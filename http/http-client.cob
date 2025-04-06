       identification division.
       program-id. http-client-get.
       data division.
       working-storage section.
       01 curl-code usage binary-long.
       *> https://github.com/curl/curl/blob/master/packages/OS400/curl.inc.in#L1073
       01 CURLOPT_URL constant as 10002.
       01 CURLOPT_WRITEFUNCTION constant as 20011.
       01 CURLOPT_WRITEDATA constant as 10001.
       01 curl-write-callback usage program-pointer.
       01 curl-handle-ptr usage pointer.

       linkage section.
       01 request-url pic x(1000).
       01 response.
           05 response-data pic x(10000).
           05 response-length-bytes pic 9(5) comp-5.

       *> Perform an http get request at the given url and store
       *> the response in the response variable.
       procedure division using
           request-url
           by reference response.

       set curl-write-callback 
           to entry "curl-write-callback"

       call "curl_easy_init"
           returning curl-handle-ptr

       call "curl_easy_setopt" using
           by value curl-handle-ptr
           by value CURLOPT_URL
           by content function trim(request-url)

       call "curl_easy_setopt" using
           by value curl-handle-ptr
           by value CURLOPT_WRITEFUNCTION
           by value curl-write-callback

       call "curl_easy_setopt" using
           by value curl-handle-ptr
           by value CURLOPT_WRITEDATA
           by reference response

       *> https://curl.se/libcurl/c/curl_easy_perform.html
       call "curl_easy_perform" using
           by value curl-handle-ptr
           returning curl-code
        
       call "curl_easy_cleanup" using
           by value curl-handle-ptr
       goback.
       end program http-client-get.
       

       