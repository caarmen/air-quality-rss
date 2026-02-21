!-----------------------------------------------------------------------
! Module to provide pollutant data services, including downloading,
! parsing, and calculating pollutant averages.
!-----------------------------------------------------------------------
module prevair_pollutant_provider
   implicit none

   type prevair_pollutant_pollutant_data
      character(len=4) :: pollutant_name
      real :: average_value
      integer :: index
   end type

contains

   !-----------------------------------------------------------------------
   ! C binding for the Fortran subroutine to get pollutant pollutant data.
   ! This function retrieves pollutant data for a specific date and
   ! geographical coordinates, and returns the pollutant names and averages.
   !
   ! Arguments:
   !
   !   in  :: date_str           - Date string in the format YYYYMMDD
   !   in  :: target_lat         - Target latitude
   !   in  :: target_lon         - Target longitude
   !   in  :: max_pollutants     - Maximum number of pollutants to retrieve
   !
   !   out :: pollutant_count    - Number of pollutants found
   !   out :: pollutant_names    - Array of pollutant names
   !   out :: pollutant_averages - Array of pollutant averages
   !-----------------------------------------------------------------------
   subroutine get_prevair_pollutant_data_c( &
      date_str, &
      target_lat, &
      target_lon, &
      max_pollutants, &
      pollutant_count, &
      pollutant_names, &
      pollutant_averages, &
      pollutant_indices &
      ) &
      bind(C, name="get_prevair_pollutant_data")
      use iso_c_binding, only: c_char, c_float, c_int, c_null_char
      use stdlib_strings, only: to_c_char
      use c_string
      implicit none
      character(kind=c_char), dimension(8), intent(in) :: date_str
      real(c_float), intent(in) :: target_lat, target_lon
      integer(c_int), intent(in) :: max_pollutants
      integer(c_int), intent(out) :: pollutant_count
      real(c_float), intent(out), dimension(max_pollutants) :: pollutant_averages
      integer(c_int), intent(out), dimension(max_pollutants) :: pollutant_indices
      character(kind=c_char), dimension(5, max_pollutants), intent(out) :: pollutant_names

      character(len=8) :: date_str_f90
      integer :: i

      type(prevair_pollutant_pollutant_data), dimension(max_pollutants):: data
      ! convert C string to Fortran string
      call to_fortran_string(date_str, 8, date_str_f90)

      call get_prevair_pollutant_data( &
         date_str_f90, &
         target_lat, &
         target_lon, &
         size(data), &
         pollutant_count, &
         data &
         )

      do i = 1, pollutant_count
         pollutant_names(:, i) = to_c_char(data(i)%pollutant_name)
         pollutant_averages(i) = data(i)%average_value
         pollutant_indices(i) = data(i)%index
      end do
   end subroutine

   !-----------------------------------------------------------------------
   ! Subroutine to get pollutant pollutant data for a specific date and
   ! geographical coordinates.
   ! Arguments:
   !   in  :: date_str           - Date string in the format YYYYMMDD
   !   in  :: target_lat         - Target latitude
   !   in  :: target_lon         - Target longitude
   !   in  :: max_pollutants     - Maximum number of pollutants to retrieve
   !   out :: pollutant_count    - Number of pollutants found
   !   out :: pollutant_data     - Array of prevair_pollutant_pollutant_data
   ! Returns:
   !   pollutant_data            - Array of pollutant averages for the specified
   !                               date and coordinates.
   !-----------------------------------------------------------------------
   subroutine get_prevair_pollutant_data( &
      date_str, &
      target_lat, &
      target_lon, &
      max_pollutants, &
      pollutant_count, &
      pollutant_data &
      )
      use pollutant_netcdf_data
      use pollutant_netcdf_parser
      use pollutant_resource_parser
      use pollutant_calculator
      use file_downloader
      use env_config, only: get_pollutant_metadata_url
      implicit none

      character(len=8) :: date_str
      real, intent(in) :: target_lat, target_lon
      integer, intent(in) :: max_pollutants
      integer, intent(out) :: pollutant_count
      type(prevair_pollutant_pollutant_data), intent(out) ::  pollutant_data(*)

      integer :: i, resources_count
      type(pollutant_resource), allocatable:: resources(:)
      type(netcdf_data) :: data
      character(len=100) :: metadata_url

      ! Fetch the resource meta data for the specified date
      ! Api documentation entry point: https://www.data.gouv.fr/api/2/
      !
      ! Used https://www.data.gouv.fr/api/2/datasets/?q=qualité+de+l'air+prévision&sort=-created
      ! to manually find the dataset ID: 67f79b112ceb5b1df4c25a8b
      !
      ! Now we can use the dataset ID to fetch the resources.
      ! Api documentation for this route /datasets/{id}/resources/:
      ! https://www.data.gouv.fr/api/2/#operations-datasets-list_resources
      !
      ! Other links about the source code for the data.gouv.fr server:
      ! https://github.com/datagouv/data.gouv.fr/issues/654
      ! https://github.com/opendatateam/udata/blob/v10.7.0/udata/core/dataset/apiv2.py#L92

      metadata_url = get_pollutant_metadata_url()
      if (metadata_url == "") then
         metadata_url = "https://www.data.gouv.fr/api/2/datasets/67f79b112ceb5b1df4c25a8b"// &
                        "/resources/?&q="//date_str//".moyj0"
      end if

      call parse_resources( &
         trim(metadata_url), &
         date_str, &
         resources_count, &
         resources &
         )

      ! Loop through each resource and download the corresponding file
      ! and parse the data.
      pollutant_count = 0
      do i = 1, min(resources_count, max_pollutants)

         call download_file(resources(i)%url, resources(i)%title)

         data = parse_file(resources(i)%title)
         if (.not. allocated(data%pollutant_values)) then
            print *, "No pollutant values found in file: ", resources(i)%title
            cycle
         end if
         pollutant_count = pollutant_count + 1
         pollutant_data(pollutant_count)%pollutant_name = data%pollutant_name(1:index(data%pollutant_name, "_") - 1)
         pollutant_data(pollutant_count)%average_value = calculate_average(data, target_lat, target_lon)
         pollutant_data(pollutant_count)%index = calculate_index( &
                                                 pollutant_data(pollutant_count)%pollutant_name, &
                                                 pollutant_data(pollutant_count)%average_value &
                                                 )
      end do
   end subroutine get_prevair_pollutant_data

end module prevair_pollutant_provider
