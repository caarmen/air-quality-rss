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
       01 BUFFER                   PIC X(10000).
       01 DATA-URL                 PIC X(1000) VALUE SPACES.

       LINKAGE SECTION.
       01 LATITUDE                 PIC S9(3)V9(8).
       01 LONGITUDE                PIC S9(3)V9(8).
       01 POLLEN-OUTPUT            PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE LATITUDE
           BY REFERENCE LONGITUDE
           BY REFERENCE POLLEN-OUTPUT.

           CALL "POLLEN-DATA-SOURCE" USING
               BY REFERENCE LATITUDE
               BY REFERENCE LONGITUDE
               BY REFERENCE DATA-URL
               BY REFERENCE BUFFER

           CALL "POLLEN-PARSER" USING
               BY REFERENCE BUFFER
               RETURNING RETURN-CODE
           IF RETURN-CODE NOT = 0
           THEN
               DISPLAY "Error parsing pollen data"
               MOVE 1 TO RETURN-CODE
               GOBACK
           END-IF

           CALL "POLLEN-RENDER" USING
               BY REFERENCE DATA-URL
               BY REFERENCE POLLEN-OUTPUT

           MOVE 0 TO RETURN-CODE
           GOBACK.

       END PROGRAM POLLEN-SERVICE.
