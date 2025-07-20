module geo
   implicit none
contains

   !-----------------------------------------------------------------------
   ! Function to calculate the distance between two geographical
   ! coordinates using the Haversine formula.
   !
   ! Arguments:
   !   in  :: lat_1 - Latitude of the first point
   !   in  :: lon_1 - Longitude of the first point
   !   in  :: lat_2 - Latitude of the second point
   !   in  :: lon_2 - Longitude of the second point
   ! Returns:
   !   distance - Distance in meters between the two points
   !-----------------------------------------------------------------------
   function calculate_distance( &
      lat_1, &
      lon_1, &
      lat_2, &
      lon_2) &
      result(distance)
      implicit none
      real, intent(in) :: lat_1, lon_1, lat_2, lon_2
      real :: distance
      real, parameter :: EARTH_RADIUS_METERS = 6378137

      ! https://en.wikipedia.org/wiki/Haversine_formula
      distance = 2*EARTH_RADIUS_METERS* &
                 asin(sqrt( &
                      sind((lat_2 - lat_1)/2)**2 &
                      + (cosd(lat_1)*cosd(lat_2) &
                         *(sind((lon_2 - lon_1)/2)**2)) &
                      ))
   end function
end module geo
