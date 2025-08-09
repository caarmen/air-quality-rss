module c_string
   implicit none
contains

   !-----------------------------------------------------------------------
   ! Subroutine to convert a c-string to a Fortran string
   !
   ! Arguments:
   !   in  :: c_string        - A C string.
   !   in  :: length          - Length of the C-string, excluding the null
   !                            terminating character.
   !   out  :: fortran_string - A Fortran string.
   !-----------------------------------------------------------------------
   subroutine to_fortran_string( &
      c_string, &
      length, &
      fortran_string &
      )
      use iso_c_binding, only: c_char
      implicit none
      character(kind=c_char), dimension(*), intent(in) :: c_string
      integer, intent(in) :: length
      character(len=*) :: fortran_string

      integer:: i

      do i = 1, length
         fortran_string(i:i) = trim(adjustl(c_string(i)))
      end do

   end subroutine
end module c_string
