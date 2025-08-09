
      *> ===============================================================
      *> PROGRAM: PREVAIR-POLLUTANT-RENDER
      *> PURPOSE: Read data from the pollutant provider and render it
      *>          to a string in the format of an RSS feed.
      *> ===============================================================
       IDENTIFICATION DIVISION.

       PROGRAM-ID. PREVAIR-POLLUTANT-RENDER.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 LS-POLLUTANT-NAME-DISPLAY     PIC X(16).
       01 LS-POLLUTANT-INDEX-DISPLAY    PIC X(11).
       01 LS-POLLUTANT-AVERAGE-DISPLAY  PIC ZZ9.9.
       01 LS-LATITUDE-DISPLAY           PIC -ZZ9.999999.
       01 LS-LONGITUDE-DISPLAY          PIC -ZZ9.999999.
       01 LS-AUTHOR                     PIC X(100) VALUE "PREV'AIR".
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
       01 IN-LATITUDE-DEGREES           PIC S9(3)V9(8).
       01 IN-LONGITUDE-DEGREES          PIC S9(3)V9(8).
       01  IN-DATE-STR                  PIC X(8).
       COPY pollutant-data IN "pollutant/service/prevair".
       01 OUT-POLLUTANT-RSS             PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           IN-DATE-STR
           IN-URL
           IN-LATITUDE-DEGREES
           IN-LONGITUDE-DEGREES
           POLLUTANT-GRP
           OUT-POLLUTANT-RSS.

           *> Add the date to the report id.
           MOVE IN-DATE-STR TO LS-POLLUTANT-REPORT-ID

           STRING
               IN-DATE-STR(1:4) "-"
               IN-DATE-STR(5:2) "-"
               IN-DATE-STR(7:2) "T00:00:00Z"
               INTO LS-POLLUTANT-UPDATED-AT
           END-STRING


           PERFORM VARYING IDX-POLLUTANT-NAME FROM 1 BY 1
               UNTIL IDX-POLLUTANT-NAME > POLLUTANT-COUNT

               *> Get display values for pollutant name, average,
               *> and index
               CALL "PREVAIR-POLLUTANT-DISPLAY-NAME" USING
                   POLLUTANT-NAMES(IDX-POLLUTANT-NAME)
                   LS-POLLUTANT-NAME-DISPLAY
               MOVE POLLUTANT-AVERAGES(IDX-POLLUTANT-NAME)
                   TO LS-POLLUTANT-AVERAGE-DISPLAY
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
                   ": " FUNCTION TRIM(LS-POLLUTANT-AVERAGE-DISPLAY)
                   " µg/m³ ("
                   FUNCTION TRIM(LS-POLLUTANT-INDEX-DISPLAY)
                   ")" X"0A"
                   INTO LS-POLLUTANT-OUTPUT
               END-STRING

           END-PERFORM

           *> Build the RSS feed url
           ACCEPT LS-FEED-URL FROM ENVIRONMENT "BASE_FEED_URL"
           MOVE IN-LATITUDE-DEGREES TO LS-LATITUDE-DISPLAY
           MOVE IN-LONGITUDE-DEGREES TO LS-LONGITUDE-DISPLAY
           STRING FUNCTION TRIM(LS-FEED-URL)
               FUNCTION TRIM(IN-URL)
               "?latitude=" LS-LATITUDE-DISPLAY
               "&longitude=" LS-LONGITUDE-DISPLAY
               INTO LS-FEED-URL
           END-STRING

           *> Build the data url
           CALL "CREATE-PREVAIR-DATA-URL" USING
               BY REFERENCE IN-LATITUDE-DEGREES
               BY REFERENCE IN-LONGITUDE-DEGREES
               BY REFERENCE LS-DATA-URL

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

       END PROGRAM PREVAIR-POLLUTANT-RENDER.

      *> ===============================================================
      *> PROGRAM: CREATE-PREVAIR-DATA-URL
      *> PURPOSE: Construct a URL for the pollutant data based on the
      *>          latitude and longitude.  
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-PREVAIR-DATA-URL.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 LS-LATITUDE-DISPLAY      PIC -ZZ9.999999.
       01 LS-LONGITUDE-DISPLAY     PIC -ZZ9.999999.
       01 LS-MIN-LATITUDE-DEGREES  PIC S9(3)V9(8).
       01 LS-MAX-LATITUDE-DEGREES  PIC S9(3)V9(8).
       01 LS-MIN-LONGITUDE-DEGREES PIC S9(3)V9(8).
       01 LS-MAX-LONGITUDE-DEGREES PIC S9(3)V9(8).

       LINKAGE SECTION.
       01 IN-LATITUDE-DEGREES      PIC S9(3)V9(8).
       01 IN-LONGITUDE-DEGREES     PIC S9(3)V9(8).
       01 OUT-DATA-URL             PIC X(1000) VALUE SPACES.

       PROCEDURE DIVISION USING
           IN-LATITUDE-DEGREES
           IN-LONGITUDE-DEGREES
           OUT-DATA-URL.


           SET OUT-DATA-URL TO
               "https://www.atmo-france.org/indiceatmo?bbox="

           COMPUTE LS-MIN-LATITUDE-DEGREES = IN-LATITUDE-DEGREES - 0.01
           COMPUTE LS-MAX-LATITUDE-DEGREES = IN-LATITUDE-DEGREES + 0.01
           COMPUTE LS-MIN-LONGITUDE-DEGREES = IN-LONGITUDE-DEGREES
               - 0.01
           COMPUTE LS-MAX-LONGITUDE-DEGREES = IN-LONGITUDE-DEGREES
               + 0.01

           MOVE LS-MAX-LATITUDE-DEGREES TO LS-LATITUDE-DISPLAY
           MOVE LS-MIN-LONGITUDE-DEGREES TO LS-LONGITUDE-DISPLAY

           STRING FUNCTION TRIM(OUT-DATA-URL)
               LS-LONGITUDE-DISPLAY "," LS-LATITUDE-DISPLAY
               INTO OUT-DATA-URL
           END-STRING

           MOVE LS-MIN-LATITUDE-DEGREES TO LS-LATITUDE-DISPLAY
           MOVE LS-MAX-LONGITUDE-DEGREES TO LS-LONGITUDE-DISPLAY
           STRING FUNCTION TRIM(OUT-DATA-URL)
               "," LS-LONGITUDE-DISPLAY "," LS-LATITUDE-DISPLAY
               INTO OUT-DATA-URL
           END-STRING

           GOBACK.
       END PROGRAM CREATE-PREVAIR-DATA-URL.

      *> ===============================================================
      *> PROGRAM: PREVAIR-POLLUTANT-DISPLAY-NAME
      *> PURPOSE: Return the display name of the pollutant from the name
      *>          of the pollen.
      *> ===============================================================
       PROGRAM-ID. PREVAIR-POLLUTANT-DISPLAY-NAME.

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
               WHEN "NO2"
                   MOVE "NO2" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "O3"
                   MOVE "O3" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "PM10"
                   MOVE "PM10" TO OUT-POLLUTANT-DISPLAY-NAME
               WHEN "PM25"
                   MOVE "PM2.5" TO OUT-POLLUTANT-DISPLAY-NAME
           END-EVALUATE

           GOBACK.

       END PROGRAM PREVAIR-POLLUTANT-DISPLAY-NAME.
