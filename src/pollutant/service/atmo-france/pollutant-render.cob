
      *> ===============================================================
      *> PROGRAM: ATMO-FRANCE-POLLUTANT-RENDER
      *> PURPOSE: Read data from the pollutant provider and render it
      *>          to a string in the format of an RSS feed.
      *> ===============================================================
       IDENTIFICATION DIVISION.

       PROGRAM-ID. ATMO-FRANCE-POLLUTANT-RENDER.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 LS-POLLUTANT-NAME-DISPLAY     PIC X(16).
       01 LS-POLLUTANT-INDEX-DISPLAY    PIC X(11).
       01 LS-AUTHOR                     PIC X(100) VALUE "Atmo France".
       01 LS-FEED-TITLE                 PIC X(100)
                                        VALUE "Polluants aujourd'hui".
       01 LS-ENTRY-TITLE                PIC X(100)
                                        VALUE "Rapport de polluants".
       01 LS-POLLUTANT-UPDATED-AT       PIC X(24).
       01 LS-POLLUTANT-OUTPUT           PIC X(10000) VALUE SPACES.
       01 LS-POLLUTANT-REPORT-ID        PIC X(100) VALUE SPACES.
       01 LS-DATA-URL                   PIC X(1000).
       01 LS-FEED-URL                   PIC X(1000).

       LINKAGE SECTION.
       01 IN-URL                        PIC X(100).
       01 IN-CODE-ZONE                  PIC X(5).
       01 IN-DATE-STR                   PIC X(10).
       COPY pollutant-data IN "pollutant/service/atmo-france".
       01 OUT-POLLUTANT-RSS             PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           IN-DATE-STR
           IN-URL
           IN-CODE-ZONE
           POLLUTANT-GRP
           OUT-POLLUTANT-RSS.

           *> Add the date to the report id.
           MOVE IN-DATE-STR TO LS-POLLUTANT-REPORT-ID

           STRING
               IN-DATE-STR(1:10) "T00:00:00Z"
               INTO LS-POLLUTANT-UPDATED-AT
           END-STRING

           STRING
               "https://explore.data.gouv.fr/fr/datasets/"
               "6149925a2ff0ab6cebdd6fe8/?code_zone__exact="
               IN-CODE-ZONE
               "#/resources/d2b9e8e6-8b0b-4bb6-9851-b4fa2efc8201"
               INTO LS-DATA-URL
           END-STRING
           PERFORM VARYING IDX-POLLUTANT-NAME FROM 1 BY 1
               UNTIL IDX-POLLUTANT-NAME > POLLUTANT-COUNT

               *> Get display values for pollutant name and index.
               CALL "ATMO-FRANCE-POLLUTANT-DISP-NAME" USING
                   POLLUTANT-NAMES(IDX-POLLUTANT-NAME)
                   LS-POLLUTANT-NAME-DISPLAY
               CALL "POLLUTANT-INDEX-DISPLAY" USING
                   POLLUTANT-INDICES(IDX-POLLUTANT-NAME)
                   LS-POLLUTANT-INDEX-DISPLAY

               *> Add the pollutant name and index to the report id.
               STRING
                   FUNCTION TRIM(LS-POLLUTANT-REPORT-ID) ","
                   FUNCTION TRIM(LS-POLLUTANT-NAME-DISPLAY)
                   FUNCTION TRIM(LS-POLLUTANT-INDEX-DISPLAY)
                   INTO LS-POLLUTANT-REPORT-ID
               END-STRING

               *> Format the pollutant output
               STRING
                   FUNCTION TRIM(LS-POLLUTANT-OUTPUT)
                   FUNCTION TRIM(LS-POLLUTANT-NAME-DISPLAY)
                   ": "
                   FUNCTION TRIM(LS-POLLUTANT-INDEX-DISPLAY) X"0A"
                   INTO LS-POLLUTANT-OUTPUT
               END-STRING

           END-PERFORM

           *> Build the RSS feed url
           ACCEPT LS-FEED-URL FROM ENVIRONMENT "BASE_FEED_URL"
           STRING FUNCTION TRIM(LS-FEED-URL)
               FUNCTION TRIM(IN-URL)
               "?code_zone=" IN-CODE-ZONE
               INTO LS-FEED-URL
           END-STRING

           *> Render the RSS feed
           CALL "RENDER-RSS" USING
               BY REFERENCE LS-POLLUTANT-REPORT-ID
               BY REFERENCE LS-DATA-URL
               BY REFERENCE LS-FEED-URL
               BY REFERENCE LS-POLLUTANT-UPDATED-AT
               BY REFERENCE LS-AUTHOR
               BY REFERENCE LS-FEED-TITLE
               BY REFERENCE LS-ENTRY-TITLE
               BY REFERENCE LS-POLLUTANT-OUTPUT
               BY REFERENCE OUT-POLLUTANT-RSS
           END-CALL
           GOBACK.

       END PROGRAM ATMO-FRANCE-POLLUTANT-RENDER.

      *> ===============================================================
      *> PROGRAM: ATMO-FRANCE-POLLUTANT-DISP-NAME
      *> PURPOSE: Return the display name of the pollutant from the name
      *>          of the pollen.
      *> ===============================================================
       PROGRAM-ID. ATMO-FRANCE-POLLUTANT-DISP-NAME.

       DATA DIVISION.
       LINKAGE SECTION.
       01 IN-POLLUTANT-NAME                   PIC X(4).
       01 OUT-POLLUTANT-DISPLAY-NAME          PIC X(16).

       PROCEDURE DIVISION USING
           IN-POLLUTANT-NAME,
           OUT-POLLUTANT-DISPLAY-NAME.

           MOVE IN-POLLUTANT-NAME TO OUT-POLLUTANT-DISPLAY-NAME
           INSPECT OUT-POLLUTANT-DISPLAY-NAME
               REPLACING ALL X"00" BY SPACE

           EVALUATE FUNCTION TRIM(OUT-POLLUTANT-DISPLAY-NAME)
               WHEN "no2"
                   MOVE "NO2" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "o3"
                   MOVE "O3" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "pm10"
                   MOVE "PM10" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "pm25"
                   MOVE "PM2.5" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "so2"
                   MOVE "SO2" TO OUT-POLLUTANT-DISPLAY-NAME
           END-EVALUATE

           GOBACK.

       END PROGRAM ATMO-FRANCE-POLLUTANT-DISP-NAME.
