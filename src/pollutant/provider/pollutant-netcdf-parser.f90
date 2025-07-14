!-----------------------------------------------------------------------
! Module to parse NetCDF files containing pollutant data.
!-----------------------------------------------------------------------
module pollutant_netcdf_parser
   implicit none

contains

   !-----------------------------------------------------------------------
   ! Subroutine to parse a NetCDF file and extract pollutant data.
   ! Arguments
   !   in  :: path             - Path to the NetCDF file
   !   out :: data             - The data extracted from the file.
   !-----------------------------------------------------------------------
   function parse_file( &
      path) result(data)
      use netcdf
      use pollutant_netcdf_data
      implicit none
      character(len=*), intent(in) :: path
      type(netcdf_data) :: data
      integer :: ncid, status
      integer :: pollutant_var_id = 4

      status = nf90_open( &
               path=path, &
               mode=nf90_nowrite, &
               ncid=ncid &
               )
      if (status /= nf90_noerr) then
         print *, 'Error reading ', path, ': ', trim(nf90_strerror(status))
         return
      end if

      ! Get the pollutant variable name
      status = nf90_inquire_variable(ncid, pollutant_var_id, data%pollutant_name)

      ! Get the latitude and longitude values

      call read_dimension( &
         ncid, &
         "lat", &
         data%lat_values, &
         data%lat_count &
         )
      call read_dimension( &
         ncid, &
         "lon", &
         data%lon_values, &
         data%lon_count &
         )

      ! Get the pollutant variable information
      allocate (data%pollutant_values(data%lon_count, data%lat_count, 1))
      status = nf90_get_var(ncid, pollutant_var_id, data%pollutant_values)
      status = nf90_close(ncid)
   end function parse_file

   !-----------------------------------------------------------------------
   ! Subroutine to read a dimension from a NetCDF file
   !
   ! Arguments:
   !   in  :: ncid        - NetCDF file ID
   !   in  :: dim_name    - Name of the dimension to read
   !   out :: dim_values  - Array to hold the dimension values
   !   out :: dim_count   - Number of values in the dimension
   !-----------------------------------------------------------------------
   subroutine read_dimension( &
      ncid, &
      dim_name, &
      dim_values, &
      dim_count &
      )
      use netcdf
      implicit none
      integer, intent(in) :: ncid
      character(len=*), intent(in) :: dim_name
      real, allocatable, intent(out) :: dim_values(:)
      integer :: status
      integer, intent(out) :: dim_count
      integer :: dim_id, var_id

      status = nf90_inq_dimid(ncid, dim_name, dim_id)
      status = nf90_inq_varid(ncid, dim_name, var_id)
      status = nf90_inquire_dimension(ncid, dim_id, len=dim_count)
      allocate (dim_values(dim_count))
      status = nf90_get_var(ncid, var_id, dim_values)
   end

   !-----------------------------------------------------------------------
   ! Subroutine to find the indices of the closest values in a dimension
   !
   ! Arguments:
   !   in  :: dim_values           - Array of dimension values
   !   in  :: dim_count            - Number of values in the dimension
   !   in  :: target_value         - Target value to find closest indices
   !                                 for
   !   out :: closest_index_before - Index of the closest value smaller
   !                                 than the target
   !   out :: closest_index_after  - Index of the closest value larger than
   !                                 the target
   !-----------------------------------------------------------------------
   subroutine find_closest_value_indices( &
      dim_values, &
      dim_count, &
      target_value, &
      closest_index_before, &
      closest_index_after &
      )
      implicit none
      real, intent(in) :: dim_values(:)
      integer, intent(in) :: dim_count
      real, intent(in) :: target_value
      integer, intent(out) :: closest_index_before, closest_index_after
      integer :: i

      closest_index_before = dim_count
      closest_index_after = 1

      do i = 1, dim_count
         if (dim_values(i) >= target_value) then
            closest_index_after = i
            if (i > 1) then
               closest_index_before = i - 1
            end if
            exit
         end if
      end do
   end subroutine find_closest_value_indices
end module pollutant_netcdf_parser
