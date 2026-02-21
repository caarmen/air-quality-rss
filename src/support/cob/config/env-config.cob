      *> ===============================================================
      *> PROGRAM: ENV-CONFIG
      *> PURPOSE: Centralized access to environment configuration.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENV-CONFIG.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-ENV-VALUE               PIC X(32) VALUE SPACES.

       LINKAGE SECTION.
       01  OUT-BASE-FEED-URL          PIC X(1000).
       01  OUT-POLLEN-BASE-URL        PIC X(1000).
       01  OUT-REQUEST-TIMEOUT-SEC    USAGE BINARY-LONG.

       PROCEDURE DIVISION.

      *> Read the BASE_FEED_URL environment variable.
      *> Returns spaces if unset.
       ENTRY "GET-BASE-FEED-URL" USING
           BY REFERENCE OUT-BASE-FEED-URL.
           MOVE SPACES TO OUT-BASE-FEED-URL
           ACCEPT OUT-BASE-FEED-URL FROM ENVIRONMENT "BASE_FEED_URL"
           GOBACK.

      *> Read the POLLEN_BASE_URL environment variable.
      *> Returns spaces if unset.
       ENTRY "GET-POLLEN-BASE-URL" USING
           BY REFERENCE OUT-POLLEN-BASE-URL.
           MOVE SPACES TO OUT-POLLEN-BASE-URL
           ACCEPT OUT-POLLEN-BASE-URL FROM ENVIRONMENT "POLLEN_BASE_URL"
           GOBACK.

      *> Read the AQRSS_REQUEST_TIMEOUT_S environment variable.
      *> Returns 30 if unset.
       ENTRY "GET-REQUEST-TIMEOUT-S" USING
           BY REFERENCE OUT-REQUEST-TIMEOUT-SEC.
           MOVE 30 TO OUT-REQUEST-TIMEOUT-SEC
           MOVE SPACES TO LS-ENV-VALUE
           ACCEPT LS-ENV-VALUE
               FROM ENVIRONMENT "AQRSS_REQUEST_TIMEOUT_S"
           IF FUNCTION TRIM(LS-ENV-VALUE) NOT = SPACES
           THEN
               MOVE FUNCTION NUMVAL(LS-ENV-VALUE)
                   TO OUT-REQUEST-TIMEOUT-SEC
           END-IF
           GOBACK.

       END PROGRAM ENV-CONFIG.
