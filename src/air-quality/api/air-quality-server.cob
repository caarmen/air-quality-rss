      *> ===============================================================
      *> PROGRAM: AIR-QUALITY-SERVER
      *> PURPOSE: Starts the HTTP server and binds the request handler.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. AIR-QUALITY-SERVER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  C-MHD-USE-SELECT-INTERNALLY   CONSTANT AS 8.
           01  C-MHD-OPTION-END              CONSTANT AS 0.
           01  C-PORT_NUMBER                 CONSTANT AS 8888.

           01  LS-DAEMON-PTR                 USAGE POINTER.
           01  LS-CONNECTION-HANDLER-ENTRY   USAGE PROGRAM-POINTER.
           01  LS-SERVER-COMMAND             PIC X(80).

       PROCEDURE DIVISION.
           SET LS-CONNECTION-HANDLER-ENTRY TO
               ENTRY "MICROHTTPD-ACCESS-HANDLER".

           CALL "MHD_start_daemon" USING
               BY VALUE    C-MHD-USE-SELECT-INTERNALLY
               BY VALUE    C-PORT_NUMBER
               BY VALUE    0
               BY VALUE    0
               BY VALUE    LS-CONNECTION-HANDLER-ENTRY
               BY VALUE    0
               BY VALUE    C-MHD-OPTION-END
               RETURNING   LS-DAEMON-PTR
           END-CALL
           DISPLAY "Air quality server started, waiting for requests..."

           GOBACK.
       END PROGRAM AIR-QUALITY-SERVER.

      *> ===============================================================
      *> PROGRAM: MICROHTTPD-ACCESS-HANDLER
      *> PURPOSE: Handles incoming HTTP requests using libmicrohttpd.
      *>          Dispatches routing and formats HTTP responses.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MICROHTTPD-ACCESS-HANDLER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  C-MHD-HTTP-OK             CONSTANT AS 200.
           01  C-MHD-RESPMEM-PERSISTENT  CONSTANT AS 0.

           01  C-CONTENT-TYPE-TEXT-PLAIN CONSTANT "text/plain".
           01  C-CONTENT-TYPE-RSS        CONSTANT "application/rss+xml".

           01  LS-RESPONSE.
               05  LS-STATUS-CODE         PIC 999.
               05  LS-BODY                PIC X(10000) VALUE SPACES.

           01  LS-RESPONSE-PTR            USAGE POINTER.
           01  LS-MHD-RESULT              USAGE BINARY-LONG.

           01  LS-HTTP-METHOD             PIC X(8).
           01  LS-URL                     PIC X(100).
           01  LS-CONTENT-TYPE            PIC X(20).

       LINKAGE SECTION.
           01  UNUSED-CLS-PTR              USAGE POINTER.
           01  IN-CONNECTION-PTR           USAGE POINTER.
           01  IN-URL-PTR                  USAGE POINTER.
           01  IN-METHOD-PTR               USAGE POINTER.
           01  UNUSED-VERSION-PTR          USAGE POINTER.
           01  UNUSED-UPLOAD-DATA-PTR      USAGE POINTER.
           01  UNUSED-UPLOAD-DATA-SIZE-PTR USAGE POINTER.
           01  UNUSED-CON-CLS-PTR-PTR      USAGE POINTER.

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE    UNUSED-CLS-PTR
           BY VALUE    IN-CONNECTION-PTR
           BY VALUE    IN-URL-PTR
           BY VALUE    IN-METHOD-PTR
           BY VALUE    UNUSED-VERSION-PTR
           BY VALUE    UNUSED-UPLOAD-DATA-PTR
           BY VALUE    UNUSED-UPLOAD-DATA-SIZE-PTR
           BY REFERENCE UNUSED-CON-CLS-PTR-PTR.

           CALL "C-STRING" USING
               BY VALUE     IN-METHOD-PTR
               BY REFERENCE LS-HTTP-METHOD

           CALL "C-STRING" USING
               BY VALUE     IN-URL-PTR
               BY REFERENCE LS-URL

           CALL "AIR-QUALITY-ROUTER" USING
               BY VALUE     IN-CONNECTION-PTR
               BY REFERENCE LS-HTTP-METHOD
               BY REFERENCE LS-URL
               BY REFERENCE LS-RESPONSE


           CALL "MHD_create_response_from_buffer" USING
               BY VALUE    LENGTH OF FUNCTION TRIM(LS-BODY)
               BY VALUE    FUNCTION TRIM(LS-BODY)
               BY VALUE    C-MHD-RESPMEM-PERSISTENT
               RETURNING   LS-RESPONSE-PTR
           END-CALL

           EVALUATE LS-STATUS-CODE
               WHEN GREATER THAN OR EQUAL TO 200 AND LESS THAN 300
                   MOVE C-CONTENT-TYPE-RSS TO LS-CONTENT-TYPE
               WHEN OTHER
                   MOVE C-CONTENT-TYPE-TEXT-PLAIN TO LS-CONTENT-TYPE
           END-EVALUATE

           CALL "MHD_add_response_header" USING
               BY VALUE    LS-RESPONSE-PTR
               BY VALUE    "Content-Type"
               BY VALUE    LS-CONTENT-TYPE
           END-CALL

           CALL "MHD_queue_response" USING
               BY VALUE    IN-CONNECTION-PTR
               BY VALUE    LS-STATUS-CODE
               BY VALUE    LS-RESPONSE-PTR
               RETURNING   LS-MHD-RESULT
           END-CALL

           CALL "MHD_destroy_response" USING
               BY VALUE    LS-RESPONSE-PTR
           END-CALL

           MOVE LS-MHD-RESULT TO RETURN-CODE

           GOBACK.
       END PROGRAM MICROHTTPD-ACCESS-HANDLER.
