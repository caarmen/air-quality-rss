       IDENTIFICATION DIVISION.
       PROGRAM-ID. CURL-WRITE-CALLBACK.
       *> https://curl.se/libcurl/c/CURLOPT_WRITEFUNCTION.html

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  RESPONSE-STRING            PIC X(10000) VALUE SPACES.
           01  RESPONSE-LENGTH-BYTES      PIC 9(5)      VALUE 0.

       LINKAGE SECTION.

       *> We implement the curl write callback function, which
       *> has this signature:
       *>   size_t write_callback(
       *>       void *ptr,
       *>       size_t size,
       *>       size_t nmemb,
       *>       void *userdata
       *>   )

           01  PTR                        USAGE POINTER.
           01  SIZE-MEMB                  PIC 9(5) COMP-5.
           01  NMEMB                      PIC 9(5) COMP-5.

           01  USERDATA.
               05  BUFFER-DATA            PIC X(10000).
               05  BUFFER-LENGTH-BYTES    PIC 9(5) COMP-5.

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE     PTR
           BY VALUE     SIZE-MEMB
           BY VALUE     NMEMB
           BY REFERENCE USERDATA.

       *> Calculate the length of the response data.
           COMPUTE RESPONSE-LENGTH-BYTES = SIZE-MEMB * NMEMB

       *> Put the response data into the response-string so
       *> we can manipulate it as a string.
           CALL "memcpy" USING
               BY REFERENCE RESPONSE-STRING
               BY VALUE     PTR
               BY VALUE     RESPONSE-LENGTH-BYTES

       *> Append the response data to the data we already have in the buffer.
           STRING
               RESPONSE-STRING(1:RESPONSE-LENGTH-BYTES)
               INTO BUFFER-DATA(BUFFER-LENGTH-BYTES + 1:)
           END-STRING

           COMPUTE BUFFER-LENGTH-BYTES =
               BUFFER-LENGTH-BYTES + RESPONSE-LENGTH-BYTES

           MOVE RESPONSE-LENGTH-BYTES TO RETURN-CODE

           GOBACK.
       END PROGRAM CURL-WRITE-CALLBACK.
