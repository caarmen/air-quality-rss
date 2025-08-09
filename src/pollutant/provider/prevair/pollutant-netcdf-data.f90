module pollutant_netcdf_data
   implicit none

   !-----------------------------------------------------------------------
   ! Derived type to hold NetCDF data
   ! This type contains the latitude and longitude counts, their values,
   ! the pollutant name, and a 3D array of pollutant values.
   !
   ! Fields:
   !
   !   pollutant_name   - Name of the pollutant variable to extract
   !   lat_count        - Number of latitude values
   !   lon_count        - Number of longitude values
   !   lat_values       - Array of latitude values
   !   lon_values       - Array of longitude values
   !   pollutant_values - 3D array of pollutant values (latitude,
   !                      longitude, time)
   !-----------------------------------------------------------------------
   type netcdf_data
      integer :: lat_count, lon_count
      real, allocatable :: lat_values(:), lon_values(:)
      character(len=16) :: pollutant_name
      real, allocatable :: pollutant_values(:, :, :)
   end type
end module pollutant_netcdf_data
