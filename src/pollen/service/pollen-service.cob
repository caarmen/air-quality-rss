      *> ===============================================================
      *> PROGRAM: POLLEN-SERVICE
      *> PURPOSE: Orchestrates the pollen data fetching, parsing, and
      *>          rendering process.
      *>          This program is called by the pollen router.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-SERVICE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 LS-BUFFER                PIC X(10000).
       01 LS-DATA-URL              PIC X(1000) VALUE SPACES.
       COPY "pollen-data" IN "pollen/service".

       LINKAGE SECTION.
       01 IN-LATITUDE-DEGREES      PIC S9(3)V9(8).
       01 IN-LONGITUDE-DEGREES     PIC S9(3)V9(8).
       01 OUT-POLLEN-RSS           PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LATITUDE-DEGREES
           BY REFERENCE IN-LONGITUDE-DEGREES
           BY REFERENCE OUT-POLLEN-RSS.

           CALL "POLLEN-DATA-SOURCE" USING
               BY REFERENCE IN-LATITUDE-DEGREES
               BY REFERENCE IN-LONGITUDE-DEGREES
               BY REFERENCE LS-DATA-URL
               BY REFERENCE LS-BUFFER

           CALL "POLLEN-PARSER" USING
               BY REFERENCE LS-BUFFER
               POLLEN-GRP
               RETURNING RETURN-CODE
           IF RETURN-CODE NOT = 0
           THEN
               DISPLAY "Error parsing pollen data"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           CALL "POLLEN-RENDER" USING
               BY REFERENCE LS-DATA-URL
               BY REFERENCE POLLEN-GRP
               BY REFERENCE OUT-POLLEN-RSS

           MOVE 0 TO RETURN-CODE
           GOBACK.

       END PROGRAM POLLEN-SERVICE.
