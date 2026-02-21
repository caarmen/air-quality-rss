!-----------------------------------------------------------------------
! Module to read environment configuration values used by the app.
!-----------------------------------------------------------------------
module env_config
contains
   !-----------------------------------------------------------------------
   ! Function to read the request timeout (in seconds) from the
   ! environment.
   !
   ! Environment variables:
   !   AQRSS_REQUEST_TIMEOUT_S
   ! Returns:
   !   timeout - The configured request timeout in seconds. Defaults to 30.
   !-----------------------------------------------------------------------
   integer function get_request_timeout_s()
      implicit none
      character(len=64) :: env_buf

      env_buf = ""
      call get_environment_variable("AQRSS_REQUEST_TIMEOUT_S", env_buf)
      if (trim(env_buf) == "") then
         get_request_timeout_s = 30
      else
         read (env_buf, *) get_request_timeout_s
      end if
   end function get_request_timeout_s

   !-----------------------------------------------------------------------
   ! Function to read the pollutant metadata url from the environment.
   !
   ! Environment variables:
   !   POLLUTANT_METADATA_URL
   ! Returns:
   !   url - The configured pollutant metadata url.
   !-----------------------------------------------------------------------
   function get_pollutant_metadata_url() result(url)
      implicit none
      character(len=:), allocatable :: url
      character(len=256) :: env_buf

      env_buf = ""
      call get_environment_variable("POLLUTANT_METADATA_URL", env_buf)
      url = trim(env_buf)
   end function get_pollutant_metadata_url

   !-----------------------------------------------------------------------
   ! Function to read the atmo france api username from the environment.
   !
   ! Environment variables:
   !   ATMO_FRANCE_USERNAME
   ! Returns:
   !   username - The configured atmo france username.
   !-----------------------------------------------------------------------
   function get_atmo_france_username() result(username)
      implicit none
      character(len=:), allocatable :: username
      character(len=256) :: env_buf

      env_buf = ""
      call get_environment_variable("ATMO_FRANCE_USERNAME", env_buf)
      username = trim(env_buf)
   end function get_atmo_france_username

   !-----------------------------------------------------------------------
   ! Function to read the atmo france api password from the environment.
   !
   ! Environment variables:
   !   ATMO_FRANCE_PASSWORD
   ! Returns:
   !   password - The configured atmo france password.
   !-----------------------------------------------------------------------
   function get_atmo_france_password() result(password)
      implicit none
      character(len=:), allocatable :: password
      character(len=256) :: env_buf

      env_buf = ""
      call get_environment_variable("ATMO_FRANCE_PASSWORD", env_buf)
      password = trim(env_buf)
   end function get_atmo_france_password
end module env_config
