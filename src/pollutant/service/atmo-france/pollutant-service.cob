      *> ===============================================================
      *> PROGRAM: ATMO-FRANCE-POLLUTANT-SERVICE
      *> PURPOSE: Orchestrates the pollutant data fetching, parsing, and
      *>          rendering process.
      *>          This program is called by the air-quality router.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMO-FRANCE-POLLUTANT-SERVICE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-API-TABULAR                   CONSTANT 1.
       LOCAL-STORAGE SECTION.
       01  LS-CURRENT-DATE-AND-TIME.
           05  LS-CDT-YEAR                 PIC 9(4).
           05  LS-CDT-MONTH                PIC 9(2). *> 01-12
           05  LS-CDT-DAY                  PIC 9(2). *> 01-31
       01  LS-DATE-STR                     PIC X(10).
       COPY "pollutant-data" IN "pollutant/service/atmo-france".

       LINKAGE SECTION.
       01 IN-URL                           PIC X(100).
       01 IN-CODE-ZONE                     PIC X(5).
       01 OUT-POLLUTANT-RSS                PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           IN-URL
           IN-CODE-ZONE
           OUT-POLLUTANT-RSS.

           MOVE FUNCTION CURRENT-DATE
               TO LS-CURRENT-DATE-AND-TIME

           STRING
               LS-CDT-YEAR "-" LS-CDT-MONTH "-" LS-CDT-DAY
               INTO LS-DATE-STR
           END-STRING

           CALL "get_atmo_france_pollutant_data" USING
               LS-DATE-STR
               IN-CODE-ZONE
               C-API-TABULAR
               POLLUTANT-COUNT
               POLLUTANT-NAMES-GRP
               POLLUTANT-INDICES-GRP

           CALL "ATMO-FRANCE-POLLUTANT-RENDER" USING
               LS-DATE-STR
               IN-URL
               IN-CODE-ZONE
               POLLUTANT-GRP
               OUT-POLLUTANT-RSS

           MOVE 0 TO RETURN-CODE
           GOBACK.

       END PROGRAM ATMO-FRANCE-POLLUTANT-SERVICE.
