       identification division.
       program-id. pollen-router.

       data division.
       linkage section.
       01 http-method pic x(8).
       01 url pic x(100).
       01 response.
           05 status-code pic 999.
           05 body pic x(10000) value spaces.

       procedure division
       using
           by reference http-method
           by reference url
           by reference response
       .
           display "incoming " http-method " request for " url "."
           if function trim(http-method) = "GET" 
               and function trim(url) = "/pollen-rss"
           then
               move 200 to status-code
               call "pollen-service" using
                   by reference body
           else
               move 404 to status-code
               move "Not Found" to body
           end-if
       goback.
       end program pollen-router.