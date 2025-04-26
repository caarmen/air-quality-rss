      *> ===============================================================
      *> PROGRAM: POLLEN-SERVER
      *> PURPOSE: Starts the HTTP server and binds the request handler.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-SERVER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  MHD-USE-SELECT-INTERNALLY  CONSTANT AS 8.
           01  MHD-OPTION-END             CONSTANT AS 0.

           01  DAEMON-PTR                 USAGE POINTER.
           01  CONNECTION-HANDLER-ENTRY   USAGE PROGRAM-POINTER.
           01  SERVER-COMMAND             PIC X(80).

       PROCEDURE DIVISION.
           SET CONNECTION-HANDLER-ENTRY TO
               ENTRY "MICROHTTPD-ACCESS-HANDLER".

           CALL "MHD_start_daemon" USING
               BY VALUE    MHD-USE-SELECT-INTERNALLY
               BY VALUE    8888
               BY VALUE    0
               BY VALUE    0
               BY VALUE    CONNECTION-HANDLER-ENTRY
               BY VALUE    0
               BY VALUE    MHD-OPTION-END
               RETURNING   DAEMON-PTR
           END-CALL
           DISPLAY "Pollen server started, waiting for requests..."

           GOBACK.
       END PROGRAM POLLEN-SERVER.

      *> ===============================================================
      *> PROGRAM: MICROHTTPD-ACCESS-HANDLER
      *> PURPOSE: Handles incoming HTTP requests using libmicrohttpd.
      *>          Dispatches routing and formats HTTP responses.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MICROHTTPD-ACCESS-HANDLER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  MHD-HTTP-OK                CONSTANT AS 200.
           01  MHD-RESPMEM-PERSISTENT     CONSTANT AS 0.

           01  RESPONSE.
               05  STATUS-CODE            PIC 999.
               05  BODY                   PIC X(10000) VALUE SPACES.

           01  RESPONSE-PTR               USAGE POINTER.
           01  MHD-RESULT                 USAGE BINARY-LONG.

           01  HTTP-METHOD                PIC X(8).
           01  URL                        PIC X(100).

       LINKAGE SECTION.
           01  CLS-PTR                    USAGE POINTER.
           01  CONNECTION-PTR             USAGE POINTER.
           01  URL-PTR                    USAGE POINTER.
           01  METHOD-PTR                 USAGE POINTER.
           01  VERSION-PTR                USAGE POINTER.
           01  UPLOAD-DATA-PTR            USAGE POINTER.
           01  UPLOAD-DATA-SIZE-PTR       USAGE POINTER.
           01  CON-CLS-PTR-PTR            USAGE POINTER.

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE    CLS-PTR
           BY VALUE    CONNECTION-PTR
           BY VALUE    URL-PTR
           BY VALUE    METHOD-PTR
           BY VALUE    VERSION-PTR
           BY VALUE    UPLOAD-DATA-PTR
           BY VALUE    UPLOAD-DATA-SIZE-PTR
           BY REFERENCE CON-CLS-PTR-PTR.

           CALL "C-STRING" USING
               BY VALUE    METHOD-PTR
               BY REFERENCE HTTP-METHOD

           CALL "C-STRING" USING
               BY VALUE    URL-PTR
               BY REFERENCE URL

           CALL "POLLEN-ROUTER" USING
               BY VALUE    CONNECTION-PTR
               BY REFERENCE HTTP-METHOD
               BY REFERENCE URL
               BY REFERENCE RESPONSE


           CALL "MHD_create_response_from_buffer" USING
               BY VALUE    LENGTH OF FUNCTION TRIM(BODY)
               BY VALUE    FUNCTION TRIM(BODY)
               BY VALUE    MHD-RESPMEM-PERSISTENT
               RETURNING   RESPONSE-PTR
           END-CALL
           CALL "MHD_add_response_header" USING
               BY VALUE    RESPONSE-PTR
               BY VALUE    "Content-Type"
               BY VALUE    "application/xml"
           END-CALL

           CALL "MHD_queue_response" USING
               BY VALUE    CONNECTION-PTR
               BY VALUE    STATUS-CODE
               BY VALUE    RESPONSE-PTR
               RETURNING   MHD-RESULT
           END-CALL

           CALL "MHD_destroy_response" USING
               BY VALUE    RESPONSE-PTR
           END-CALL

           MOVE MHD-RESULT TO RETURN-CODE

           GOBACK.
       END PROGRAM MICROHTTPD-ACCESS-HANDLER.
