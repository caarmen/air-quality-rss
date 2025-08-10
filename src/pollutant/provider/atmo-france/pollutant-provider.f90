
!-----------------------------------------------------------------------
! Module to provide pollutant data from atmo france.
!-----------------------------------------------------------------------
module atmo_france_pollutant_provider

contains
   !-----------------------------------------------------------------------
   ! C binding for the Fortran subroutine to get pollutant data.
   ! This function retrieves pollutant data for a specific date and
   ! INSEE zone code, and returns the pollutant names and indices.
   !
   ! Arguments:
   !
   !   in  :: date_str           - Date string in the format YYYY-MM-DD
   !   in  :: code_zone          - INSEE code de commune (not postal code).
   !
   !   out :: pollutant_count    - Number of pollutants found
   !   out :: pollutant_names    - Array of pollutant names
   !   out :: pollutant_indices  - Array of pollutant indices
   !-----------------------------------------------------------------------
   subroutine get_atmo_france_pollutant_data_c( &
      date_str, &
      code_zone, &
      pollutant_count, &
      pollutant_names, &
      pollutant_indices &
      ) &
      bind(C, name="get_atmo_france_pollutant_data")
      use atmo_france_pollutant_data, only: pollutant_data
      use iso_c_binding, only: c_char, c_float, c_int, c_null_char
      use stdlib_strings, only: to_c_char
      use c_string
      implicit none
      character(kind=c_char), dimension(10), intent(in) :: date_str
      character(kind=c_char), dimension(5), intent(in) :: code_zone
      integer, parameter:: MAX_POLLUTANT_COUNT = 10

      integer(c_int), intent(out) :: pollutant_count
      integer(c_int), intent(out), dimension(MAX_POLLUTANT_COUNT) :: pollutant_indices
      character(kind=c_char), dimension(5, MAX_POLLUTANT_COUNT), intent(out) :: pollutant_names

      character(len=10) :: date_str_f90
      character(len=5) :: code_zone_f90
      integer :: i

      type(pollutant_data), dimension(MAX_POLLUTANT_COUNT):: data
      ! convert C strings to Fortran strings
      call to_fortran_string(date_str, 10, date_str_f90)
      call to_fortran_string(code_zone, 5, code_zone_f90)

      call get_atmo_france_pollutant_data( &
         date_str_f90, &
         code_zone_f90, &
         data, &
         pollutant_count &
         )

      do i = 1, pollutant_count
         pollutant_names(:, i) = to_c_char(data(i)%pollutant_name)
         pollutant_indices(i) = data(i)%index
      end do
   end subroutine

   !-----------------------------------------------------------------------
   ! Subroutine to get pollutant data for a specific date and
   ! INSEE zone code.
   !
   ! Arguments:
   !   in  :: date_str           - Date string in the format YYYY-MM-DD
   !   in  :: code_zone          - INSEE code de commune (not postal code).
   !   out :: data               - Array of atmo france pollutant data
   !   out :: count              - The number of pollutants in the result.
   !-----------------------------------------------------------------------
   subroutine get_atmo_france_pollutant_data( &
      date_str, &
      code_zone, &
      data, &
      data_count &
      )
      use atmo_france_pollutant_data, only: pollutant_data
      use atmo_france_pollutant_parser_tabular
      use atmo_france_pollutant_fetcher_tabular
      implicit none

      character(len=4), dimension(5), parameter :: POLLUTANT_NAMES = [ &
                                                   character(len=4) :: "no2", "o3", "pm10", "pm25", "so2" &
                                                                       ]
      character(len=10), intent(in) :: date_str
      character(len=5), intent(in) :: code_zone
      type(pollutant_data), intent(out) ::  data(*)
      integer, intent(out) :: data_count

      character(len=500) :: pollutant_data_json_str

      pollutant_data_json_str = fetch_pollutant_data_tabular( &
                                date_str, &
                                code_zone, &
                                POLLUTANT_NAMES &
                                )

      call parse_pollutant_data_tabular( &
         pollutant_data_json_str, &
         POLLUTANT_NAMES, &
         data, &
         data_count &
         )

   end subroutine get_atmo_france_pollutant_data

end module atmo_france_pollutant_provider
