       identification division.
       program-id. curl-write-callback.
       *> https://curl.se/libcurl/c/CURLOPT_WRITEFUNCTION.html
       data division.
       working-storage section.
       01 response-string pic x(10000) value spaces.
       01 response-length-bytes pic 9(5) value 0.
       linkage section.
       *> We implement the curl write callback function, which
       *> has this signature:
       *> size_t write_callback(
       *>   void *ptr,
       *>   size_t size,
       *>   size_t nmemb,
       *>   void *userdata
       *>)
       01 ptr usage pointer.
       01 size-memb pic 9(5) comp-5.
       01 nmemb pic 9(5) comp-5.

       01 userdata.
           05 buffer-data pic x(10000).
           05 buffer-length-bytes pic 9(5) comp-5.


       *> Libcurl calls this procedure.
       procedure division with C linkage using
           by value ptr
           by value size-memb
           by value nmemb
           by reference userdata.

       *> Calculate the length of the response data.
       compute response-length-bytes = size-memb * nmemb.

       *> Put the response data into the response-string so
       *> we can manipulate it as a string.
       call "memcpy" using
           by reference response-string
           by value ptr
           by value response-length-bytes.

       *> Append the response data to the data we already
       *> have in the buffer.
       string buffer-data(1:buffer-length-bytes)
           response-string(1:response-length-bytes)
           into buffer-data
       end-string.
       compute buffer-length-bytes = 
           buffer-length-bytes + response-length-bytes.

       move response-length-bytes to return-code.
       goback.
       end program curl-write-callback.


       
