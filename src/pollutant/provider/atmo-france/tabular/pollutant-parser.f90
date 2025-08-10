!-----------------------------------------------------------------------
! Module to parse atmo france pollutant data.
!-----------------------------------------------------------------------
module atmo_france_pollutant_parser_tabular

contains
   !-----------------------------------------------------------------------
   ! Subroutine to parse atmo france pollutant data from an atmo france
   ! tabular api response json string.
   !
   ! Arguments:
   !
   !   in  :: pollutant_names          - List of pollutant names for which
   !                                     to fetch data.
   !   in  :: pollutant_data_json_str  - The pollutant data as a raw
   !                                     string.
   !
   !   out :: data                     - The parsed pollutant data.
   !   out :: pollutant_count          - The number of pollutants in the
   !                                     the parsed result.
   !-----------------------------------------------------------------------
   subroutine parse_pollutant_data_tabular( &
      pollutant_data_json_str, &
      pollutant_names, &
      data, &
      pollutant_count &
      )
      use json_module, only: json_file, json_core, json_value
      use atmo_france_pollutant_parser
      use atmo_france_pollutant_data, only: pollutant_data
      implicit none

      character(len=*), intent(in) :: pollutant_data_json_str
      character(len=4), intent(in) :: pollutant_names(:)
      integer, intent(out) :: pollutant_count
      type(pollutant_data), intent(out) ::  data(*)

      type(json_file) :: json_response
      type(json_core) :: json_core_obj
      type(json_value), pointer :: json_root, json_pollutant_data

      ! pollutant_data_json_str:
      ! [
      !   {
      !     "code_no2": 2,
      !     "code_o3": 2,
      !     "code_pm10": 1,
      !     "code_pm25": 1,
      !     "code_so2": 1
      !   }
      ! ]
      json_response = json_file(pollutant_data_json_str)
      call json_response%get(json_root)

      call json_core_obj%get_child(json_root, 1, json_pollutant_data)
      ! json_pollutant_data:
      !   {
      !     "code_no2": 2,
      !     "code_o3": 2,
      !     "code_pm10": 1,
      !     "code_pm25": 1,
      !     "code_so2": 1
      !   }
      call parse_pollutant_data_json_object( &
         json_pollutant_data, &
         pollutant_names, &
         data, &
         pollutant_count &
         )

      call json_response%destroy()

   end subroutine parse_pollutant_data_tabular

end module atmo_france_pollutant_parser_tabular
