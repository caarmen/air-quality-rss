       identification division.
       program-id. pollen-router.

       data division.
       local-storage section.
       01 query-param-latitude pic x(16) value spaces.
       01 query-param-longitude pic x(16) value spaces.
       01 latitude pic s9(3)v9(8).
       01 longitude pic s9(3)v9(8).
       01 query-param-value usage pointer.
       01 query-param-size usage pointer.
       linkage section.
       01 connection-ptr                             usage pointer.
       01 http-method pic x(8).
       01 url pic x(100).
       01 response.
           05 status-code pic 999.
           05 body pic x(10000) value spaces.

       procedure division
       using
           by value connection-ptr
           by reference http-method
           by reference url
           by reference response
       .
           display "incoming " http-method " request for " url "."
           if function trim(http-method) = "GET" 
               and function trim(url) = "/pollen-rss"
           then
               move 200 to status-code
               *> TODO Make utility function for parsing query params.
               call "MHD_lookup_connection_value_n" using
                   by value connection-ptr
                   by value 8 *> MHD_GET_ARGUMENT_KIND
                   by value "longitude"
                   by value length of "longitude"
                   by reference query-param-value
                   by reference query-param-size
               call "c-string" using
                   by value query-param-value
                   by reference query-param-longitude
               move query-param-longitude to longitude
               call "MHD_lookup_connection_value_n" using
                   by value connection-ptr
                   by value 8 *> MHD_GET_ARGUMENT_KIND
                   by value "latitude"
                   by value length of "latitude"
                   by reference query-param-value
                   by reference query-param-size
               call "c-string" using
                   by value query-param-value
                   by reference query-param-latitude
               move query-param-latitude to latitude

               call "pollen-service" using
                   by reference latitude
                   by reference longitude
                   by reference body
           else
               move 404 to status-code
               move "Not Found" to body
           end-if
       goback.
       end program pollen-router.