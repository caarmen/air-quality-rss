      *> ===============================================================
      *> PROGRAM: POLLUTANT-SERVICE
      *> PURPOSE: Orchestrates the pollutant data fetching, parsing, and
      *>          rendering process.
      *>          This program is called by the air-quality router.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLUTANT-SERVICE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  LS-CURRENT-DATE-AND-TIME.
           05  LS-CDT-YEAR                 PIC 9(4).
           05  LS-CDT-MONTH                PIC 9(2). *> 01-12
           05  LS-CDT-DAY                  PIC 9(2). *> 01-31
       01  LS-DATE-STR                     PIC X(8).
       COPY "pollutant-data" IN "pollutant/service".

       LINKAGE SECTION.
       01 IN-URL                   PIC X(100).
       01 IN-LATITUDE-DEGREES      PIC S9(3)V9(8).
       01 IN-LONGITUDE-DEGREES     PIC S9(3)V9(8).
       01 OUT-POLLUTANT-RSS        PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           IN-URL
           IN-LATITUDE-DEGREES
           IN-LONGITUDE-DEGREES
           OUT-POLLUTANT-RSS.

           MOVE FUNCTION CURRENT-DATE
               TO LS-CURRENT-DATE-AND-TIME

           STRING
               LS-CDT-YEAR LS-CDT-MONTH LS-CDT-DAY
               INTO LS-DATE-STR
           END-STRING
           CALL "get_prevair_pollutant_data" USING
               LS-DATE-STR
               IN-LATITUDE-DEGREES
               IN-LONGITUDE-DEGREES
               C-POLLUTANT-MAX-COUNT
               POLLUTANT-COUNT
               POLLUTANT-NAMES-GRP
               POLLUTANT-AVERAGES-GRP
               POLLUTANT-INDICES-GRP

           CALL "POLLUTANT-RENDER" USING
               LS-DATE-STR
               IN-URL
               IN-LATITUDE-DEGREES
               IN-LONGITUDE-DEGREES
               POLLUTANT-GRP
               OUT-POLLUTANT-RSS

           MOVE 0 TO RETURN-CODE
           GOBACK.

       END PROGRAM POLLUTANT-SERVICE.
