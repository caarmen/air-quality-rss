!-----------------------------------------------------------------------
! Module to parse pollutant resource metadata from a JSON source.
!-----------------------------------------------------------------------
module pollutant_resource_parser
   implicit none

   type pollutant_resource
      character(len=:), allocatable :: url
      character(len=:), allocatable :: title
   end type pollutant_resource

contains
   !-----------------------------------------------------------------------
   ! Subroutine to parse pollutant resource metadata from a JSON source.
   !
   ! Arguments:
   !   in  :: metadata_url       - URL of the JSON metadata file
   !   in  :: date_str           - Date string to filter resources
   !   out :: resources_count    - Number of resources found
   !   out :: resources          - Array of pollutant_resource objects
   !-----------------------------------------------------------------------
   subroutine parse_resources( &
      metadata_url, &
      date_str, &
      resources_count, &
      resources &
      )
      use json_module, only: json_file, json_core, json_value
      use http, only: response_type, request
      use env_config, only: get_request_timeout_s
      implicit none

      ! Subroutine arguments:
      character(len=*), intent(in) :: metadata_url, date_str
      integer, intent(out) :: resources_count
      type(pollutant_resource), intent(out), allocatable, dimension(:)::  resources

      ! http variables:
      type(response_type) :: response

      ! json variables:
      type(json_file) :: json_metadata_file
      type(json_core) :: json_core_obj
      type(json_value), pointer :: json_resources
      type(json_value), pointer :: json_resource
      character(len=:), allocatable :: json_resource_url, json_resource_title
      integer :: json_resources_count

      integer, parameter :: max_resources = 10
      integer :: i

      allocate (resources(max_resources))

      resources_count = 0
      print *, "Fetching data from: ", metadata_url
      response = request(metadata_url, timeout=get_request_timeout_s())
      json_metadata_file = json_file(response%content)
      call json_metadata_file%get("data", json_resources)
      json_resources_count = json_core_obj%count(json_resources)
      do i = 1, json_resources_count
         call json_core_obj%get_child(json_resources, i, json_resource)
         call json_core_obj%get(json_resource, "url", json_resource_url)
         ! Check if the URL contains the date string and the specific pattern
         ! for the pollutant forecast.
         ! Example URL pattern: "prevair.prevision.20250718.moyj0.no2.public.nc".
         if (index(json_resource_url, date_str//".moyj0") /= 0) then
            resources_count = resources_count + 1
            call json_core_obj%get(json_resource, "title", json_resource_title)
            resources(resources_count)%url = json_resource_url
            resources(resources_count)%title = json_resource_title
         end if
         if (resources_count == max_resources) then
            exit
         end if
      end do
      call json_metadata_file%destroy()
   end subroutine parse_resources
end module pollutant_resource_parser
