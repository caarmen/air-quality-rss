module atmo_france_pollutant_fetcher_admin

contains

   !-----------------------------------------------------------------------
   ! Function to fetch pollutant data from atmo france, using the admin
   ! api.
   !
   ! Arguments:
   !
   !   in  :: token                    - Api token obtained from
   !                                     get_token().
   !   in  :: date_str                 - Date string in the format
   !                                     YYYY-MM-DD.
   !   in  :: code_zone                - INSEE code de commune (not postal
   !                                     code).
   !   in  :: pollutant_names          - List of pollutant names for which
   !                                     to fetch data.
   !
   !   out :: pollutant_data_json_str  - The pollutant data as a raw
   !                                     string.
   !-----------------------------------------------------------------------
   function fetch_pollutant_data_admin( &
      token, &
      date_str, &
      code_zone, &
      pollutant_names &
      ) result(pollutant_data_json_str)
      use http, only: response_type, request, pair_type
      implicit none
      character(len=*), intent(in) :: token
      character(len=*), intent(in) :: date_str
      character(len=*), intent(in) :: code_zone
      character(len=*), intent(in) :: pollutant_names(:)
      character(len=:), allocatable :: pollutant_data_json_str

      character(len=200) :: data_url
      type(response_type) :: response

      data_url = build_url( &
                 date_str, &
                 code_zone, &
                 pollutant_names &
                 )

      print *, "Fetching data from: ", data_url

      response = request( &
                 url=trim(data_url), &
                 header=[pair_type("Authorization", "Bearer "//token)] &
                 )
      pollutant_data_json_str = response%content

   end function fetch_pollutant_data_admin

   !-----------------------------------------------------------------------
   ! Function to build the atmo france url.
   !
   ! Arguments:
   !
   !   in  :: date_str            - Date string in the format YYYY-MM-DD.
   !   in  :: code_zone           - INSEE code de commune (not postal code).
   !   in  :: pollutant_names     - List of pollutant names for which to
   !                                fetch data.
   !
   !   out :: data_url            - The url from which to fetch data.
   !                                https://admindata.atmo-france.org/api/v2/data/indices/atmo?format=geojson&date=2025-08-15&code_zone=69123
   !-----------------------------------------------------------------------
   function build_url( &
      date_str, &
      code_zone, &
      pollutant_names &
      ) result(data_url)
      implicit none
      character(len=*), intent(in) :: date_str
      character(len=*), intent(in) :: code_zone
      character(len=4), intent(in) :: pollutant_names(:)
      character(len=200) :: data_url

      integer :: i

      call get_environment_variable("POLLUTANT_METADATA_URL", data_url)
      if (data_url == "") then
         data_url = "https://admindata.atmo-france.org/api/v2/data/indices/atmo"// &
                    "?format=geojson"// &
                    "&date="//date_str// &
                    "&code_zone="//code_zone
      end if
   end function build_url

end module atmo_france_pollutant_fetcher_admin
