!-----------------------------------------------------------------------
! Module to calculate pollutant data based on pollutant values
! from neighboring data points.
!-----------------------------------------------------------------------
module pollutant_calculator
   use pollutant_netcdf_data
   implicit none

   private :: find_neighboring_values
   private :: calculate_weighted_average
   public :: calculate_average
   public :: calculate_index

   !-----------------------------------------------------------------------
   ! Derived type to hold a data point with latitude, longitude, and
   ! pollutant value.
   !
   ! Fields:
   !   lat   - Latitude of the data point
   !   lon   - Longitude of the data point
   !   value - Pollutant value at the data point
   !-----------------------------------------------------------------------
   type data_point
      real :: lat
      real :: lon
      real :: value
   end type data_point
contains

   !-----------------------------------------------------------------------
   ! Function to calculate the average pollutant value at a given latitude
   ! and longitude based on the four closest data points.
   !
   ! Arguments:
   !   in  :: data                 - NetCDF file data
   !   in  :: target_lat           - Target latitude
   !   in  :: target_lon           - Target longitude
   !   out :: average_value        - Average pollutant value at the target
   !                                 latitude and longitude
   !-----------------------------------------------------------------------
   function calculate_average( &
      data, &
      target_lat, &
      target_lon &
      ) result(average_value)
      use pollutant_netcdf_data
      implicit none
      type(netcdf_data), intent(in) :: data
      real, intent(in) :: target_lat, target_lon
      real :: average_value
      type(data_point), dimension(4) :: neighboring_values

      call find_neighboring_values( &
         data, &
         target_lat, &
         target_lon, &
         neighboring_values &
         )
      average_value = calculate_weighted_average( &
                      neighboring_values, &
                      target_lat, &
                      target_lon &
                      )
   end function calculate_average

   !-----------------------------------------------------------------------
   ! Subroutine to find the four closest pollutant values to a target latitude
   ! and longitude.
   !
   ! Arguments:
   !   in  :: data                 - NetCDF file data
   !   in  :: target_lat           - Target latitude
   !   in  :: target_lon           - Target longitude
   !   out :: neighboring_values   - Array of data_point containing the four
   !                                 closest pollutant values
   !-----------------------------------------------------------------------
   subroutine find_neighboring_values( &
      data, &
      target_lat, &
      target_lon, &
      neighboring_values &
      )
      use pollutant_netcdf_parser
      use pollutant_netcdf_data
      implicit none
      type(netcdf_data), intent(in) :: data
      real, intent(in) :: target_lat, target_lon
      type(data_point), dimension(4), intent(out) :: neighboring_values
      integer :: target_lat_idx_before, target_lat_idx_after
      integer :: target_lon_idx_before, target_lon_idx_after

      call find_closest_value_indices( &
         data%lat_values, &
         data%lat_count, &
         target_lat, &
         target_lat_idx_before, &
         target_lat_idx_after &
         )

      call find_closest_value_indices( &
         data%lon_values, &
         data%lon_count, &
         target_lon, &
         target_lon_idx_before, &
         target_lon_idx_after &
         )

      neighboring_values(1)%lat = data%lat_values(target_lat_idx_before)
      neighboring_values(1)%lon = data%lon_values(target_lon_idx_before)
      neighboring_values(1)%value = data%pollutant_values(target_lon_idx_before, target_lat_idx_before, 1)
      neighboring_values(2)%lat = data%lat_values(target_lat_idx_before)
      neighboring_values(2)%lon = data%lon_values(target_lon_idx_after)
      neighboring_values(2)%value = data%pollutant_values(target_lon_idx_after, target_lat_idx_before, 1)
      neighboring_values(3)%lat = data%lat_values(target_lat_idx_after)
      neighboring_values(3)%lon = data%lon_values(target_lon_idx_before)
      neighboring_values(3)%value = data%pollutant_values(target_lon_idx_before, target_lat_idx_after, 1)
      neighboring_values(4)%lat = data%lat_values(target_lat_idx_after)
      neighboring_values(4)%lon = data%lon_values(target_lon_idx_after)
      neighboring_values(4)%value = data%pollutant_values(target_lon_idx_after, target_lat_idx_after, 1)

   end subroutine find_neighboring_values

   !-----------------------------------------------------------------------
   ! Subroutine to calculate the weighted average of pollutant values from
   ! neighboring data points.
   !
   ! The weighted average is calculated based on the distances from the
   ! target latitude and longitude to the neighboring data points.
   ! The closer the data point, the higher the weight.
   !
   ! Arguments:
   !   in  :: neighboring_values   - Array of data_point containing the four
   !                                 closest pollutant values
   !   in  :: target_lat           - Target latitude
   !   in  :: target_lon           - Target longitude
   !   out :: average_value        - Weighted average of the pollutant values
   !-----------------------------------------------------------------------
   function calculate_weighted_average( &
      neighboring_values, &
      target_lat, &
      target_lon) &
      result(average_value)

      use geo
      implicit none
      type(data_point), dimension(4), intent(in) :: neighboring_values
      real, intent(in) :: target_lat, target_lon
      real :: average_value
      integer :: i
      real :: total_weight, weight

      ! Most of this function was written by Github Copilot
      total_weight = 0.0
      average_value = 0.0

      do i = 1, 4
         ! If the data point is exactly at the target location, use its value directly.
         ! This way, we avoid a division by zero in distance calculation.
         ! Also, if we found the exact point, there's no need to calculate an average
         ! of neighboring points.
         if (neighboring_values(i)%lat == target_lat .and. &
             neighboring_values(i)%lon == target_lon) then
            average_value = neighboring_values(i)%value
            return
         end if
         weight = 1.0/calculate_distance(neighboring_values(i)%lat, neighboring_values(i)%lon, target_lat, target_lon)
         average_value = average_value + weight*neighboring_values(i)%value
         total_weight = total_weight + weight
      end do

      if (total_weight > 0.0) then
         average_value = average_value/total_weight
      else
         average_value = 0.0
      end if

   end function calculate_weighted_average

   ! -----------------------------------------------------------------------
   ! Function to calculate the pollutant index based on the pollutant
   ! value.
   !
   ! The index is based on the French pollutant index system.
   ! https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000042164835
   !
   ! Arguments:
   !   pollutant_name       - Name of the pollutant (e.g., "NO2",
   !                                 "O3", "SO2", "PM1", "PM2")
   !   pollutant_value      - Value of the pollutant
   !
   ! Returns:
   !   pollutant_index      - Calculated pollutant index (1 to 6)
   ! -----------------------------------------------------------------------
   function calculate_index( &
      pollutant_name, &
      pollutant_value &
      ) result(pollutant_index)

      implicit none

      character(len=4), intent(in) :: pollutant_name
      real, intent(in):: pollutant_value
      integer :: pollutant_index

      select case (trim(pollutant_name))
      case ("NO2")
         if (pollutant_value <= 40) then
            pollutant_index = 1
         else if (pollutant_value <= 90) then
            pollutant_index = 2
         else if (pollutant_value <= 120) then
            pollutant_index = 3
         else if (pollutant_value <= 230) then
            pollutant_index = 4
         else if (pollutant_value <= 340) then
            pollutant_index = 5
         else
            pollutant_index = 6
         end if
      case ("O3")
         if (pollutant_value <= 50) then
            pollutant_index = 1
         else if (pollutant_value <= 100) then
            pollutant_index = 2
         else if (pollutant_value <= 130) then
            pollutant_index = 3
         else if (pollutant_value <= 240) then
            pollutant_index = 4
         else if (pollutant_value <= 380) then
            pollutant_index = 5
         else
            pollutant_index = 6
         end if
      case ("SO2")
         if (pollutant_value <= 100) then
            pollutant_index = 1
         else if (pollutant_value <= 200) then
            pollutant_index = 2
         else if (pollutant_value <= 350) then
            pollutant_index = 3
         else if (pollutant_value <= 500) then
            pollutant_index = 4
         else if (pollutant_value <= 750) then
            pollutant_index = 5
         else
            pollutant_index = 6
         end if
      case ("PM10")
         if (pollutant_value <= 20) then
            pollutant_index = 1
         else if (pollutant_value <= 40) then
            pollutant_index = 2
         else if (pollutant_value <= 50) then
            pollutant_index = 3
         else if (pollutant_value <= 100) then
            pollutant_index = 4
         else if (pollutant_value <= 150) then
            pollutant_index = 5
         else
            pollutant_index = 6
         end if
      case ("PM25")
         if (pollutant_value <= 10) then
            pollutant_index = 1
         else if (pollutant_value <= 20) then
            pollutant_index = 2
         else if (pollutant_value <= 25) then
            pollutant_index = 3
         else if (pollutant_value <= 50) then
            pollutant_index = 4
         else if (pollutant_value <= 75) then
            pollutant_index = 5
         else
            pollutant_index = 6
         end if
      case default
         pollutant_index = 0
      end select
   end function calculate_index
end module pollutant_calculator
