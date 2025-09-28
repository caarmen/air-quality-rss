      *> ===============================================================
      *> PROGRAM: MY-HTTP-SERVER
      *> PURPOSE: Starts the HTTP server and binds the request handler.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-HTTP-SERVER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  C-MHD-USE-INTERNAL-POLLING-THREAD   CONSTANT AS 8.
           01  C-MHD-OPTION-THREAD-POOL-SIZE CONSTANT AS 14.
           01  C-MHD-OPTION-END              CONSTANT AS 0.
           01  C-PORT-NUMBER                 CONSTANT AS 8889.

           01  LS-DAEMON-PTR                 USAGE POINTER.
           01  LS-CONNECTION-HANDLER-ENTRY   USAGE PROGRAM-POINTER.
           01  LS-COUNTER                    PIC 9(8).

       PROCEDURE DIVISION.
           SET LS-CONNECTION-HANDLER-ENTRY TO
               ENTRY "MICROHTTPD-REQUEST-HANDLER".

        *>    CALL "MHD_start_daemon" USING
        *>        BY VALUE    C-MHD-USE-INTERNAL-POLLING-THREAD
        *>        BY VALUE    C-PORT-NUMBER
        *>        BY VALUE    0
        *>        BY VALUE    0
        *>        BY VALUE    LS-CONNECTION-HANDLER-ENTRY
        *>        BY VALUE    0
        *>        BY VALUE    C-MHD-OPTION-THREAD-POOL-SIZE
        *>        BY VALUE    1
        *>        BY VALUE    C-MHD-OPTION-END
        *>        RETURNING   LS-DAEMON-PTR
        *>    END-CALL
        *>    DISPLAY
        *>        "Server started, waiting for connections on port "
        *>        C-PORT-NUMBER

           *> Wait for incoming connections
        *>    PERFORM FOREVER
        *>        CALL "sleep" USING BY VALUE 1
        *>    END-PERFORM.
           PERFORM VARYING LS-COUNTER FROM 1 BY 1 UNTIL 
               LS-COUNTER = 10000
               DISPLAY "Iteration " LS-COUNTER
               CALL "MY-SERVER-LOGIC"
           END-PERFORM
           .
       END PROGRAM MY-HTTP-SERVER.

      *> ===============================================================
      *> PROGRAM: MICROHTTPD-REQUEST-HANDLER
      *> PURPOSE: Handles incoming HTTP requests using libmicrohttpd.
      *>          Dispatches routing and formats HTTP responses.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MICROHTTPD-REQUEST-HANDLER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  C-MHD-HTTP-OK             CONSTANT AS 200.
           01  C-MHD-RESPMEM-PERSISTENT  CONSTANT AS 0.

           01  C-STATIC-RESPONSE-BODY    CONSTANT "Hello world!".

           01  LS-RESPONSE.
               05  LS-STATUS-CODE         PIC 999.
               05  LS-BODY                PIC X(10000) VALUE SPACES.

           01  LS-RESPONSE-PTR            USAGE POINTER.
           01  LS-MHD-RESULT              USAGE BINARY-LONG.


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

           DISPLAY "Incoming request"

           CALL "MY-SERVER-LOGIC"

           CALL "MHD_create_response_from_buffer" USING
               BY VALUE    LENGTH OF FUNCTION
                   TRIM(C-STATIC-RESPONSE-BODY)
               BY VALUE    FUNCTION TRIM(C-STATIC-RESPONSE-BODY)
               BY VALUE    C-MHD-RESPMEM-PERSISTENT
               RETURNING   LS-RESPONSE-PTR

           CALL "MHD_add_response_header" USING
               BY VALUE    LS-RESPONSE-PTR
               BY VALUE    "Content-Type"
               BY VALUE    "text/plain"

           CALL "MHD_queue_response" USING
               BY VALUE    IN-CONNECTION-PTR
               BY VALUE    C-MHD-HTTP-OK
               BY VALUE    LS-RESPONSE-PTR
               RETURNING   LS-MHD-RESULT

           CALL "MHD_destroy_response" USING
               BY VALUE    LS-RESPONSE-PTR


           MOVE LS-MHD-RESULt TO RETURN-CODE
           DISPLAY "Server processed request."
           .
       END PROGRAM MICROHTTPD-REQUEST-HANDLER.

      *> ===============================================================
      *> PROGRAM: MY-SERVER-LOGIC
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-SERVER-LOGIC.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT               PIC X(32) VALUE "Ok then".
           01  LS-SOME-MODIFIED-TEXT      PIC X(64) VALUE SPACES.
           01  LS-DELAY-MS                USAGE BINARY-LONG.
           01  LS-CALLBACK-PTR            USAGE PROGRAM-POINTER.

       PROCEDURE DIVISION.
           CALL "ONE-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT

           SET LS-CALLBACK-PTR TO
               ENTRY "MY-CALLBACK".

        .  COMPUTE LS-DELAY-MS = FUNCTION RANDOM * 5000
           display "sleeping for " ls-delay-ms
           CALL "usleep" USING BY VALUE LS-DELAY-MS

           CALL "call_me_back" USING
               BY VALUE LS-CALLBACK-PTR

           CALL "TWO-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
               BY REFERENCE LS-SOME-MODIFIED-TEXT

           .
       END PROGRAM MY-SERVER-LOGIC.
       
      *> ===============================================================
      *> PROGRAM: ONE-ARG-PROGRAM
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ONE-ARG-PROGRAM.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ARG-1           PIC X(32) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ARG-1.

           DISPLAY "One arg program called with arg '" IN-ARG-1 "'"
           .
       END PROGRAM ONE-ARG-PROGRAM.
       
      *> ===============================================================
      *> PROGRAM: TWO-ARG-PROGRAM
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO-ARG-PROGRAM.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ARG-1           PIC X(32) VALUE SPACES.
       01  OUT-ARG-2          PIC X(64) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ARG-1
           BY REFERENCE OUT-ARG-2.

           STRING IN-ARG-1 IN-ARG-1 INTO OUT-ARG-2
           DISPLAY "Two-arg program called with arg '" IN-ARG-1 "'"
               ". Returning '" OUT-ARG-2 "'"
           .
       END PROGRAM TWO-ARG-PROGRAM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CALLBACK.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT             PIC X(32) VALUE "callback text".
           01  LS-SOME-MODIFIED-TEXT    PIC X(64) VALUE SPACES.
       PROCEDURE DIVISION.
           DISPLAY "Got called back!"
           CALL "ONE-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
           CALL "TWO-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
               BY REFERENCE LS-SOME-MODIFIED-TEXT
           .
       END PROGRAM MY-CALLBACK.