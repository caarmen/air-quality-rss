!-----------------------------------------------------------------------
! Module to parse atmo france pollutant data.
!-----------------------------------------------------------------------
module atmo_france_pollutant_parser_admin

contains

   !-----------------------------------------------------------------------
   ! Subroutine to parse atmo france pollutant data from an atmo france
   ! admin api response json string.
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
   subroutine parse_pollutant_data_admin( &
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
      type(json_value), pointer :: json_root, json_features, json_feature, json_properties, json_pollutant_data

      logical :: found_features, found_properties
      ! Set pollutant_count to 0. If we run into an errors and have to exit early, the
      ! caller will see that no data was fetched.
      pollutant_count = 0

      ! pollutant_data_json_str:
      ! {
      !   "features": [
      !     {
      !       "properties": {
      !         "code_no2": 1,
      !         "code_o3": 2,
      !         "code_pm10": 1,
      !         "code_pm25": 1,
      !         "code_so2": 1,
      !       },
      !    }
      !   ]
      ! }
      json_response = json_file(pollutant_data_json_str)
      call json_response%get(json_root)
      call json_core_obj%get(json_root, "features", json_features, found_features)
      if (.not. found_features) then
         return
      end if

      call json_core_obj%get_child(json_features, 1, json_feature)
      call json_core_obj%get(json_feature, "properties", json_properties, found_properties)
      if (.not. found_properties) then
         return
      end if

      call parse_pollutant_data_json_object( &
         json_properties, &
         pollutant_names, &
         data, &
         pollutant_count &
         )

      call json_response%destroy()

   end subroutine parse_pollutant_data_admin

end module atmo_france_pollutant_parser_admin
