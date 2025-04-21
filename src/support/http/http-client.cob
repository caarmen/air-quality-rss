
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
           01  CURL-CODE                   USAGE BINARY-LONG.

       *> https://github.com/curl/curl/blob/master/packages/OS400/curl.inc.in#L1073
           01  CURLOPT-URL                CONSTANT AS 10002.
           01  CURLOPT-WRITEFUNCTION      CONSTANT AS 20011.
           01  CURLOPT-WRITEDATA          CONSTANT AS 10001.

           01  CURL-WRITE-CALLBACK        USAGE PROGRAM-POINTER.
           01  CURL-HANDLE-PTR            USAGE POINTER.

       LINKAGE SECTION.
           01  REQUEST-URL                PIC X(1000).
           01  RESPONSE.
               05  RESPONSE-DATA          PIC X(10000).
               05  RESPONSE-LENGTH-BYTES  PIC 9(5) COMP-5.

       *> Perform an HTTP GET request at the given URL
       *> and store the response in the RESPONSE variable.
       PROCEDURE DIVISION USING
           REQUEST-URL
           BY REFERENCE RESPONSE.

           SET CURL-WRITE-CALLBACK TO
               ENTRY "CURL-WRITE-CALLBACK"

           CALL "curl_easy_init"
               RETURNING CURL-HANDLE-PTR

           CALL "curl_easy_setopt" USING
               BY VALUE    CURL-HANDLE-PTR
               BY VALUE    CURLOPT-URL
               BY CONTENT  FUNCTION TRIM(REQUEST-URL)

           CALL "curl_easy_setopt" USING
               BY VALUE    CURL-HANDLE-PTR
               BY VALUE    CURLOPT-WRITEFUNCTION
               BY VALUE    CURL-WRITE-CALLBACK

           CALL "curl_easy_setopt" USING
               BY VALUE    CURL-HANDLE-PTR
               BY VALUE    CURLOPT-WRITEDATA
               BY REFERENCE RESPONSE

       *> https://curl.se/libcurl/c/curl_easy_perform.html
           CALL "curl_easy_perform" USING
               BY VALUE    CURL-HANDLE-PTR
               RETURNING   CURL-CODE

           CALL "curl_easy_cleanup" USING
               BY VALUE CURL-HANDLE-PTR

           GOBACK.
       END PROGRAM HTTP-CLIENT-GET.
