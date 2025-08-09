!-----------------------------------------------------------------------
! Derived type to hold pollutant data fetched from atmo france
!
! Fields:
!
!   pollutant_name   - Name of the pollutant
!   index            - The air quality index for this pollutant
!-----------------------------------------------------------------------

module atmo_france_pollutant_data
   implicit none

   type pollutant_data
      character(len=4) :: pollutant_name
      integer :: index
   end type
end module atmo_france_pollutant_data
