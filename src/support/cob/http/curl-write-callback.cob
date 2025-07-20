      *> ===============================================================
      *> PROGRAM: CURL-WRITE-CALLBACK
      *> PURPOSE: This program is called by libcurl when it receives
      *>          data from the server.
      *>          It saves the data into a buffer and returns the number
      *>          of bytes written.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CURL-WRITE-CALLBACK.
       *> https://curl.se/libcurl/c/CURLOPT_WRITEFUNCTION.html

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           COPY remote-service-response IN "support/cob/http".

       LINKAGE SECTION.

       *> We implement the curl write callback function, which
       *> has this signature:
       *>   size_t write_callback(
       *>       void *ptr,
       *>       size_t size,
       *>       size_t nmemb,
       *>       void *userdata
       *>   )

           01  IN-PTR                      USAGE POINTER.
           01  IN-SIZE-MEMB                PIC 9(5) COMP-5.
           01  IN-NMEMB                    PIC 9(5) COMP-5.

           01  OUT-USERDATA.
               05  OUT-BUFFER-DATA         PIC X(10000).
               05  OUT-BUFFER-LENGTH-BYTES PIC 9(5) COMP-5.

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE     IN-PTR
           BY VALUE     IN-SIZE-MEMB
           BY VALUE     IN-NMEMB
           BY REFERENCE OUT-USERDATA.

       *> Calculate the length of the response data.
           COMPUTE OUT-RESPONSE-LENGTH-BYTES = IN-SIZE-MEMB * IN-NMEMB

       *> Put the response data into the response-string so
       *> we can manipulate it as a string.
           CALL "memcpy" USING
               BY REFERENCE OUT-RESPONSE-DATA
               BY VALUE     IN-PTR
               BY VALUE     OUT-RESPONSE-LENGTH-BYTES

       *> Append the response data to the data we already have in the buffer.
           STRING
               OUT-RESPONSE-DATA(1:OUT-RESPONSE-LENGTH-BYTES)
               INTO OUT-BUFFER-DATA(OUT-BUFFER-LENGTH-BYTES + 1:)
           END-STRING

           COMPUTE OUT-BUFFER-LENGTH-BYTES =
               OUT-BUFFER-LENGTH-BYTES + OUT-RESPONSE-LENGTH-BYTES

           MOVE OUT-RESPONSE-LENGTH-BYTES TO RETURN-CODE

           GOBACK.
       END PROGRAM CURL-WRITE-CALLBACK.
