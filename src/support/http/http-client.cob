
      *> ===============================================================
      *> PROGRAM: HTTP-CLIENT-GET
      *> PURPOSE: Performs an HTTP GET request using libcurl.
      *>          The response is stored in the RESPONSE variable.
      *>          This program is called by the POLLEN-DATA-SOURCE
      *>          program to fetch the pollen data.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-CLIENT-GET.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-CURL-CODE               USAGE BINARY-LONG.

       *> https://github.com/curl/curl/blob/master/packages/OS400/curl.inc.in#L1073
           01  C-CURLOPT-URL              CONSTANT AS 10002.
           01  C-CURLOPT-WRITEFUNCTION    CONSTANT AS 20011.
           01  C-CURLOPT-WRITEDATA        CONSTANT AS 10001.

           01  LS-CURL-WRITE-CALLBACK     USAGE PROGRAM-POINTER.
           01  LS-CURL-HANDLE-PTR         USAGE POINTER.

       LINKAGE SECTION.
           01  IN-REQUEST-URL             PIC X(1000).
           COPY remote-service-response IN "support/http".

       *> Perform an HTTP GET request at the given URL
       *> and store the response in the RESPONSE variable.
       PROCEDURE DIVISION USING
           IN-REQUEST-URL
           BY REFERENCE OUT-RESPONSE.

           SET LS-CURL-WRITE-CALLBACK TO
               ENTRY "CURL-WRITE-CALLBACK"

           CALL "curl_easy_init"
               RETURNING LS-CURL-HANDLE-PTR

           CALL "curl_easy_setopt" USING
               BY VALUE    LS-CURL-HANDLE-PTR
               BY VALUE    C-CURLOPT-URL
               BY CONTENT  FUNCTION TRIM(IN-REQUEST-URL)

           CALL "curl_easy_setopt" USING
               BY VALUE    LS-CURL-HANDLE-PTR
               BY VALUE    C-CURLOPT-WRITEFUNCTION
               BY VALUE    LS-CURL-WRITE-CALLBACK

           CALL "curl_easy_setopt" USING
               BY VALUE    LS-CURL-HANDLE-PTR
               BY VALUE    C-CURLOPT-WRITEDATA
               BY REFERENCE OUT-RESPONSE

       *> https://curl.se/libcurl/c/curl_easy_perform.html
           CALL "curl_easy_perform" USING
               BY VALUE    LS-CURL-HANDLE-PTR
               RETURNING   LS-CURL-CODE

           CALL "curl_easy_cleanup" USING
               BY VALUE LS-CURL-HANDLE-PTR

           GOBACK.
       END PROGRAM HTTP-CLIENT-GET.
