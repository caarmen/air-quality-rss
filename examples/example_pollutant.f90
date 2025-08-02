!-----------------------------------------------------------------------
! Entry point for the pollutant provider.
! This program retrieves pollutant pollutant data for the current date
! and specified geographical coordinates, and prints the average
! pollutant values.
!
! Usage:
!   ./example_pollutant_f90 <latitude> <longitude>
!-----------------------------------------------------------------------
program main
   use prevair_pollutant_provider
   use datetime
   implicit none

   character(len=10) :: target_lat_str, target_lon_str
   real :: target_lat, target_lon
   type(prevair_pollutant_pollutant_data), dimension(10):: data
   integer :: data_count
   integer :: i
   character(len=8) :: date_str

   call get_command_argument(1, target_lat_str)
   call get_command_argument(2, target_lon_str)
   read (target_lat_str, *) target_lat
   read (target_lon_str, *) target_lon

   date_str = get_today_str()

   call get_prevair_pollutant_data( &
      date_str, &
      target_lat, &
      target_lon, &
      size(data), &
      data_count, &
      data &
      )

   do i = 1, data_count
      print *, data(i)%pollutant_name, ": ", data(i)%average_value, &
         " (", data(i)%index, ")"
   end do
end program main
