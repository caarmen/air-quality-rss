!-----------------------------------------------------------------------
! Module to provide pollutant data services, including downloading,
! parsing, and calculating pollutant averages.
!-----------------------------------------------------------------------
module pollutant_provider
   implicit none

   type pollutant_pollutant_data
      character(len=4) :: pollutant_name
      real :: average_value
      integer :: index
   end type

contains

   !-----------------------------------------------------------------------
   ! Subroutine to get pollutant pollutant data for a specific date and
   ! geographical coordinates.
   ! Arguments:
   !   in  :: date_str           - Date string in the format YYYYMMDD
   !   in  :: target_lat         - Target latitude
   !   in  :: target_lon         - Target longitude
   !   in  :: max_pollutants     - Maximum number of pollutants to retrieve
   !   out :: pollutant_count    - Number of pollutants found
   !   out :: pollutant_data     - Array of pollutant_pollutant_data
   ! Returns:
   !   pollutant_data            - Array of pollutant averages for the specified
   !                               date and coordinates.
   !-----------------------------------------------------------------------
   subroutine get_pollutant_pollutant_data( &
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
      implicit none

      character(len=8) :: date_str
      real, intent(in) :: target_lat, target_lon
      integer, intent(in) :: max_pollutants
      integer, intent(out) :: pollutant_count
      type(pollutant_pollutant_data), intent(out) ::  pollutant_data(*)

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

      call get_environment_variable("POLLUTANT_METADATA_URL", metadata_url)
      if (metadata_url == "") then
         metadata_url = "https://www.data.gouv.fr/api/2/datasets/67f79b112ceb5b1df4c25a8b"// &
            "/resources/?&q="//date_str//".moyj0"
      endif

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
         if (.not.allocated(data%pollutant_values)) then
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
   end subroutine get_pollutant_pollutant_data

end module pollutant_provider
