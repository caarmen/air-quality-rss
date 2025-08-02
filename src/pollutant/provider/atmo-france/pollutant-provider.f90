
!-----------------------------------------------------------------------
! Module to provide pollutant data from atmo france.
!-----------------------------------------------------------------------
module atmo_france_pollutant_provider

contains
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
      use atmo_france_pollutant_parser
      use atmo_france_pollutant_fetcher
      implicit none

      character(len=4), dimension(5), parameter :: POLLUTANT_NAMES = [ &
                                                   character(len=4) :: "no2", "o3", "pm10", "pm25", "so2" &
                                                                       ]
      character(len=10), intent(in) :: date_str
      character(len=5), intent(in) :: code_zone
      type(pollutant_data), intent(out) ::  data(*)
      integer, intent(out) :: data_count

      character(len=500) :: pollutant_data_json_str

      pollutant_data_json_str = fetch_pollutant_data( &
                                date_str, &
                                code_zone, &
                                POLLUTANT_NAMES &
                                )

      call parse_pollutant_data( &
         pollutant_data_json_str, &
         POLLUTANT_NAMES, &
         data, &
         data_count &
         )

   end subroutine get_atmo_france_pollutant_data

end module atmo_france_pollutant_provider
