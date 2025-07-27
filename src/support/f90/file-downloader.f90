!-----------------------------------------------------------------------
! Module to download files from a specified URL and
! save them to a local path.
!-----------------------------------------------------------------------
module file_downloader
   implicit none
contains

   !-----------------------------------------------------------------------
   ! Subroutine to download a file from a given URL and save it to a
   ! specified path.
   !
   ! Arguments:
   !   in  :: url   - URL of the file to download
   !   out :: path  - Path where the downloaded file will be saved
   !-----------------------------------------------------------------------
   subroutine download_file( &
      url, &
      path &
      )
      use http, only: response_type, request
      use stdlib_io, only: open
      implicit none
      character(len=*), intent(in) :: url, path
      type(response_type) :: response
      integer :: file_id

      ! The given url is something like this:
      ! https://static.data.gouv.fr/resources/prevair-previsions-de-la-qualite-de-lair-pour-la-france-air-quality-forecasts-for-france/20250718-071324/prevair.prevision.20250718.moyj0.no2.public.nc
      if (access(path, "r") == 0) then
         return
      end if
      print *, "Fetching data from: ", url
      response = request(trim(url))
      file_id = open (path, "wb")
      write (file_id) response%content
      close (file_id)

   end subroutine download_file

end module file_downloader
