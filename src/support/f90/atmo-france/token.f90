
module atmo_france_token
contains
   !-----------------------------------------------------------------------
   ! Subroutine to get the token for atmo france API.
   !
   ! The following environment variables must be defined:
   !
   ! ATMO_FRANCE_USERNAME
   ! ATMO_FRANCE_PASSWORD
   !
   ! To create the username and password, visit the api doc:
   ! https://admindata.atmo-france.org/api/doc/v2
   !
   ! Arguments:
   !   out :: token - The token to use for subsequent API calls.
   !-----------------------------------------------------------------------
   subroutine get_token(token)
      use http, only: response_type, request, HTTP_POST, pair_type
      use json_module
      use env_config, only: get_atmo_france_username, get_atmo_france_password
      implicit none

      ! Output variable
      character(len=:), allocatable, intent(out):: token

      ! Http variables
      type(response_type) :: response

      ! Json variables
      type(json_core) :: json
      type(json_file) :: json_response
      type(json_value), pointer :: json_request_root, json_response_root

      ! Other local variables
      character(len=:), allocatable:: username, password, request_input

      type(json_value), pointer :: p

      ! Read the username and password from the environment.
      username = get_atmo_france_username()
      password = get_atmo_france_password()

      ! Create the request payload with the username and password
      call json%create_object(json_request_root, "")
      call json%add(json_request_root, "username", username)
      call json%add(json_request_root, "password", password)
      call json%serialize(json_request_root, request_input)

      ! Call the atmo-france api route.
      response = request( &
                 method=HTTP_POST, &
                 url="https://admindata.atmo-france.org/api/login", &
                 data=request_input, &
                 header=[pair_type("Content-Type", "application/json")] &
                 )

      ! Extract the token.
      ! Same response:
      ! {
      !   "token": "eyJ...4w"
      ! }
      json_response = json_file(response%content)
      call json_response%get(json_response_root)
      call json%get(json_response_root, "token", token)

   end subroutine get_token

end module atmo_france_token
