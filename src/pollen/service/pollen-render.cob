
      *> ===============================================================
      *> PROGRAM: POLLEN-RENDER
      *> PURPOSE: Read data from the pollen.dat file and render it to a
      *>          string in the format of an RSS feed.
      *> ===============================================================
       IDENTIFICATION DIVISION.

       PROGRAM-ID. POLLEN-RENDER.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 LS-RESPONSE                 PIC X(10000) VALUE SPACES.
       01 LS-LATITUDE-DISPLAY         PIC -ZZ9.999999.
       01 LS-LONGITUDE-DISPLAY        PIC -ZZ9.999999.
       01 LS-POLLEN-UPDATED-AT        PIC X(24).
       01 LS-AUTHOR                   PIC X(100) VALUE "Atmo France".
       01 LS-FEED-TITLE               PIC X(100)
                                      VALUE "Pollens aujourd'hui".
       01 LS-ENTRY-TITLE              PIC X(100)
                                      VALUE "Rapport de pollens".
       01 LS-POLLEN-DISPLAY-NAME      PIC X(16).
       01 LS-POLLEN-INDEX-EMOJI       PIC X(9).
       01 LS-POLLEN-OUTPUT            PIC X(10000) VALUE SPACES.
       *> LS-POLLEN-REPORT-ID: string which is unique for each
       *> combination of pollen data fields: date_maj (day component
       *> only), responsible pollen, and the code and value of
       01 LS-POLLEN-REPORT-ID         PIC X(200) VALUE SPACES.
       01 LS-FEED-URL                 PIC X(1000).

       LINKAGE SECTION.
       01 IN-DATA-URL                 PIC X(1000) VALUE SPACES.
       01 IN-URL                        PIC X(100).
       01 IN-LATITUDE-DEGREES           PIC S9(3)V9(8).
       01 IN-LONGITUDE-DEGREES          PIC S9(3)V9(8).
       COPY pollen-data IN "pollen/service".
       01 OUT-POLLEN-RSS              PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-DATA-URL
           BY REFERENCE IN-URL
           BY REFERENCE IN-LATITUDE-DEGREES
           BY REFERENCE IN-LONGITUDE-DEGREES
           POLLEN-GRP
           BY REFERENCE OUT-POLLEN-RSS.

           *> First read the responsible-pollen
           *> Then read all of the pollen-records until the end of file
           STRING POLLEN-DATE-MAJ INTO LS-POLLEN-UPDATED-AT
           END-STRING

           *> Add the date to the pollen report id.
           STRING
               LS-POLLEN-UPDATED-AT(1:10)
               INTO LS-POLLEN-REPORT-ID
           END-STRING

           STRING
               "Pollen responsable: "
               FUNCTION TRIM(POLLEN-RESPONSIBLE) X"0A"
               INTO LS-POLLEN-OUTPUT
           END-STRING

           *> Add the responsible pollen to the pollen report id.
           STRING
               FUNCTION TRIM(LS-POLLEN-REPORT-ID)
               ","
               FUNCTION TRIM(POLLEN-RESPONSIBLE)
               INTO LS-POLLEN-REPORT-ID
           END-STRING

           PERFORM VARYING POLLEN-CODE-INDEX FROM 1
               BY 1 UNTIL POLLEN-CODE-INDEX > POLLEN-CODE-COUNT

               CALL "POLLEN-DISPLAY-NAME" USING
                   BY REFERENCE POLLEN-CODE-NAME(POLLEN-CODE-INDEX)
                   BY REFERENCE LS-POLLEN-DISPLAY-NAME
               END-CALL
               CALL "INDEX-EMOJI" USING
                   POLLEN-CODE-VALUE(POLLEN-CODE-INDEX)
                   LS-POLLEN-INDEX-EMOJI
               STRING
                   FUNCTION TRIM(LS-POLLEN-OUTPUT)
                   FUNCTION TRIM(LS-POLLEN-DISPLAY-NAME)
                   ": "
                   POLLEN-CODE-VALUE(POLLEN-CODE-INDEX)
                   " "
                   LS-POLLEN-INDEX-EMOJI X"0A"
                   INTO LS-POLLEN-OUTPUT
               END-STRING
               *> Add the pollen name and code to the pollen
               *> report id.
               STRING
                   FUNCTION TRIM(LS-POLLEN-REPORT-ID)
                   ","
                   FUNCTION TRIM(POLLEN-CODE-NAME(
                       POLLEN-CODE-INDEX)(6:10)
                   )
                   POLLEN-CODE-VALUE(POLLEN-CODE-INDEX)
                   INTO LS-POLLEN-REPORT-ID
               END-STRING
           END-PERFORM

           INSPECT LS-POLLEN-OUTPUT
               REPLACING ALL X"00" BY SPACE

           ACCEPT LS-FEED-URL FROM ENVIRONMENT "BASE_FEED_URL"
           MOVE IN-LATITUDE-DEGREES TO LS-LATITUDE-DISPLAY
           MOVE IN-LONGITUDE-DEGREES TO LS-LONGITUDE-DISPLAY
           STRING FUNCTION TRIM(LS-FEED-URL)
               FUNCTION TRIM(IN-URL)
               "?latitude=" LS-LATITUDE-DISPLAY
               "&longitude=" LS-LONGITUDE-DISPLAY
               INTO LS-FEED-URL
           END-STRING

           CALL "RENDER-RSS" USING
               BY REFERENCE LS-POLLEN-REPORT-ID
               BY REFERENCE IN-DATA-URL
               BY REFERENCE LS-FEED-URL
               BY REFERENCE LS-POLLEN-UPDATED-AT
               BY REFERENCE LS-AUTHOR
               BY REFERENCE LS-FEED-TITLE
               BY REFERENCE LS-ENTRY-TITLE
               BY REFERENCE LS-POLLEN-OUTPUT
               BY REFERENCE OUT-POLLEN-RSS
           END-CALL

           GOBACK.

       END PROGRAM POLLEN-RENDER.

      *> ===============================================================
      *> PROGRAM: POLLEN-DISPLAY-NAME
      *> PURPOSE: Return the display name of the pollen from the code
      *>          of the pollen.
      *> ===============================================================

       PROGRAM-ID. POLLEN-DISPLAY-NAME.

       DATA DIVISION.
       LINKAGE SECTION.
       01 IN-POLLEN-NAME                   PIC X(16).
       01 OUT-POLLEN-DISPLAY-NAME          PIC X(16).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-POLLEN-NAME,
           BY REFERENCE OUT-POLLEN-DISPLAY-NAME.

           IF IN-POLLEN-NAME(1:9) = "code_ambr"
           THEN
               MOVE "Ambroise" TO OUT-POLLEN-DISPLAY-NAME
           ELSE IF IN-POLLEN-NAME(1:8) = "code_arm"
               THEN
                   MOVE "Armoise" TO OUT-POLLEN-DISPLAY-NAME
           ELSE IF IN-POLLEN-NAME(1:8) = "code_aul"
               THEN
                   MOVE "Aulne" TO OUT-POLLEN-DISPLAY-NAME
           ELSE IF IN-POLLEN-NAME(1:9) = "code_boul"
               THEN
                   MOVE "Bouleau" TO OUT-POLLEN-DISPLAY-NAME
           ELSE IF IN-POLLEN-NAME(1:9) = "code_gram"
               THEN
                   MOVE "Graminées" TO OUT-POLLEN-DISPLAY-NAME
           ELSE IF IN-POLLEN-NAME(1:9) = "code_oliv"
               THEN
                   MOVE "Olivier" TO OUT-POLLEN-DISPLAY-NAME
           ELSE IF IN-POLLEN-NAME(1:5) = "code_"
               MOVE IN-POLLEN-NAME(6:10)
               TO OUT-POLLEN-DISPLAY-NAME
           ELSE
               MOVE IN-POLLEN-NAME TO OUT-POLLEN-DISPLAY-NAME
           END-IF

           GOBACK.

       END PROGRAM POLLEN-DISPLAY-NAME.
