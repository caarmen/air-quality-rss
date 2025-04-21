       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-SERVER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  MHD-USE-SELECT-INTERNALLY  CONSTANT AS 8.
           01  MHD-OPTION-END             CONSTANT AS 0.

           01  STAR-DAEMON                USAGE POINTER.
           01  CONNECTION-HANDLER-ENTRY   USAGE PROGRAM-POINTER.
           01  SERVER-COMMAND             PIC X(80).

       *> ***************************************************************

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
               RETURNING   STAR-DAEMON
           END-CALL

           GOBACK.
       END PROGRAM POLLEN-SERVER.

       *> ***************************************************************


       IDENTIFICATION DIVISION.
       PROGRAM-ID. MICROHTTPD-ACCESS-HANDLER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  MHD-HTTP-OK                CONSTANT AS 200.
           01  MHD-RESPMEM-PERSISTENT     CONSTANT AS 0.

           01  RESPONSE.
               05  STATUS-CODE            PIC 999.
               05  BODY                   PIC X(10000) VALUE SPACES.

           01  STAR-RESPONSE              USAGE POINTER.
           01  MHD-RESULT                 USAGE BINARY-LONG.

           01  HTTP-METHOD                PIC X(8).
           01  URL                        PIC X(100).

       LINKAGE SECTION.
           01  STAR-CLS                   USAGE POINTER.
           01  STAR-CONNECTION            USAGE POINTER.
           01  STAR-URL                   USAGE POINTER.
           01  STAR-METHOD                USAGE POINTER.
           01  STAR-VERSION               USAGE POINTER.
           01  STAR-UPLOAD-DATA           USAGE POINTER.
           01  STAR-UPLOAD-DATA-SIZE      USAGE POINTER.
           01  STAR-STAR-CON-CLS          USAGE POINTER.

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE    STAR-CLS
           BY VALUE    STAR-CONNECTION
           BY VALUE    STAR-URL
           BY VALUE    STAR-METHOD
           BY VALUE    STAR-VERSION
           BY VALUE    STAR-UPLOAD-DATA
           BY VALUE    STAR-UPLOAD-DATA-SIZE
           BY REFERENCE STAR-STAR-CON-CLS.

           CALL "C-STRING" USING
               BY VALUE    STAR-METHOD
               BY REFERENCE HTTP-METHOD

           CALL "C-STRING" USING
               BY VALUE    STAR-URL
               BY REFERENCE URL

           CALL "POLLEN-ROUTER" USING
               BY VALUE    STAR-CONNECTION
               BY REFERENCE HTTP-METHOD
               BY REFERENCE URL
               BY REFERENCE RESPONSE

           CALL "MHD_create_response_from_buffer" USING
               BY VALUE    LENGTH OF BODY
               BY VALUE    FUNCTION TRIM(BODY)
               BY VALUE    MHD-RESPMEM-PERSISTENT
               RETURNING   STAR-RESPONSE
           END-CALL

           CALL "MHD_queue_response" USING
               BY VALUE    STAR-CONNECTION
               BY VALUE    STATUS-CODE
               BY VALUE    STAR-RESPONSE
               RETURNING   MHD-RESULT
           END-CALL

           CALL "MHD_destroy_response" USING
               BY VALUE    STAR-RESPONSE
           END-CALL

           MOVE MHD-RESULT TO RETURN-CODE

           GOBACK.
       END PROGRAM MICROHTTPD-ACCESS-HANDLER.
