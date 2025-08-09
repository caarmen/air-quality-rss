!-----------------------------------------------------------------------
! Entry point for the pollutant provider.
! This program retrieves pollutant data for the current date
! and specified INSEE zone code, and prints the average
! pollutant indices.
!
! Usage:
!   ./example_pollutant_atmo_france_f90 <code_zone>
!-----------------------------------------------------------------------
program main
   use atmo_france_pollutant_provider
   use atmo_france_pollutant_data, only: pollutant_data
   use datetime
   implicit none

   character(len=5) :: code_zone
   character(len=10) :: date_str

   type(pollutant_data), dimension(10):: data
   integer :: data_count

   integer :: i

   call get_command_argument(1, code_zone)

   date_str = get_hyphenated_today_str()

   call get_atmo_france_pollutant_data( &
      date_str, &
      code_zone, &
      data, &
      data_count &
      )

   do i = 1, data_count
      print *, data(i)%pollutant_name, ": ", data(i)%index
   end do
end program main
