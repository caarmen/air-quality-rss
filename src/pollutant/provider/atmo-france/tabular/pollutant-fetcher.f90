
!-----------------------------------------------------------------------
! Module to retrieve atmo france pollutant data.
!-----------------------------------------------------------------------
module atmo_france_pollutant_fetcher_tabular

contains

   !-----------------------------------------------------------------------
   ! Function to fetch pollutant data from atmo france.
   !
   ! Arguments:
   !
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
   function fetch_pollutant_data_tabular( &
      date_str, &
      code_zone, &
      pollutant_names &
      ) result(pollutant_data_json_str)
      use http, only: response_type, request
      implicit none
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

      response = request(trim(data_url))
      pollutant_data_json_str = response%content

   end function fetch_pollutant_data_tabular

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
         data_url = "https://tabular-api.data.gouv.fr/api/resources/"// &
                    "d2b9e8e6-8b0b-4bb6-9851-b4fa2efc8201/data/json/?"// &
                    "code_zone__exact="//code_zone// &
                    "&date_ech__exact="//date_str// &
                    "&columns="
         do i = 1, size(pollutant_names)
            data_url = trim(data_url)//"code_"//trim(pollutant_names(i))
            if (i < size(pollutant_names)) then
               data_url = trim(data_url)//","
            end if
         end do
      end if
   end function build_url

end module atmo_france_pollutant_fetcher_tabular
