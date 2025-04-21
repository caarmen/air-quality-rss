
      *> ===============================================================
      *> PROGRAM: POLLEN-RENDER
      *> PURPOSE: Read data from the pollen.dat file and render it to a
      *>          string in the format of an RSS feed.
      *> ===============================================================
       IDENTIFICATION DIVISION.

       PROGRAM-ID. POLLEN-RENDER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLLEN-FILE ASSIGN TO "pollen.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY pollen-data IN "pollen/service".

       LOCAL-STORAGE SECTION.
       01 RESPONSE                    PIC X(10000) VALUE SPACES.
       01 POLLEN-UPDATED-AT           PIC X(24).
       01 POLLEN-DISPLAY-NAME         PIC X(16).
       01 POLLEN-OUTPUT               PIC X(10000) VALUE SPACES.

       LINKAGE SECTION.
       01 DATA-URL                    PIC X(1000) VALUE SPACES.
       01 POLLEN-RSS-OUTPUT           PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE DATA-URL
           BY REFERENCE POLLEN-RSS-OUTPUT.

           OPEN INPUT POLLEN-FILE

           *> First read the responsible-pollen
           *> Then read all of the pollen-records until the end of file
           READ POLLEN-FILE INTO DATE-MAJ
           STRING DATE-MAJ INTO POLLEN-UPDATED-AT
           END-STRING

           READ POLLEN-FILE INTO RESPONSIBLE-POLLEN
           STRING
               "Responsible pollen: "
               FUNCTION TRIM(RESPONSIBLE-POLLEN) X"0A"
               INTO POLLEN-OUTPUT
           END-STRING

           PERFORM UNTIL EXIT
               READ POLLEN-FILE INTO POLLEN-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       CALL "POLLEN-DISPLAY-NAME" USING
                           BY REFERENCE POLLEN-NAME
                           BY REFERENCE POLLEN-DISPLAY-NAME
                       END-CALL
                       STRING
                           FUNCTION TRIM(POLLEN-OUTPUT)
                           FUNCTION TRIM(POLLEN-DISPLAY-NAME)
                           ": "
                           POLLEN-CODE X"0A"
                           INTO POLLEN-OUTPUT
                       END-STRING
               END-READ
           END-PERFORM

           CLOSE POLLEN-FILE

           INSPECT POLLEN-OUTPUT
               REPLACING ALL X"00" BY SPACE

           CALL "RENDER-RSS" USING
               BY REFERENCE DATA-URL
               BY REFERENCE POLLEN-UPDATED-AT
               BY REFERENCE POLLEN-OUTPUT
               BY REFERENCE POLLEN-RSS-OUTPUT
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
       01 POLLEN-NAME                  PIC X(16).
       01 POLLEN-DISPLAY-NAME          PIC X(16).

       PROCEDURE DIVISION USING
           BY REFERENCE POLLEN-NAME,
           BY REFERENCE POLLEN-DISPLAY-NAME.

           IF POLLEN-NAME(1:9) = "code_ambr"
           THEN
               MOVE "Ambroise" TO POLLEN-DISPLAY-NAME
           ELSE IF POLLEN-NAME(1:8) = "code_arm"
               THEN
                   MOVE "Armoise" TO POLLEN-DISPLAY-NAME
           ELSE IF POLLEN-NAME(1:8) = "code_aul"
               THEN
                   MOVE "Aulne" TO POLLEN-DISPLAY-NAME
           ELSE IF POLLEN-NAME(1:9) = "code_boul"
               THEN
                   MOVE "Bouleau" TO POLLEN-DISPLAY-NAME
           ELSE IF POLLEN-NAME(1:9) = "code_gram"
               THEN
                   MOVE "Graminées" TO POLLEN-DISPLAY-NAME
           ELSE IF POLLEN-NAME(1:9) = "code_oliv"
               THEN
                   MOVE "Olivier" TO POLLEN-DISPLAY-NAME
           ELSE
               MOVE POLLEN-NAME TO POLLEN-DISPLAY-NAME
           END-IF

           GOBACK.

       END PROGRAM POLLEN-DISPLAY-NAME.

      *> ===============================================================
      *> PROGRAM: RENDER-RSS
      *> PURPOSE: Render the given FEED-CONTENT to an RSS feed format.
      *>          The SOURCE-URL is escaped to be used in the RSS feed.
      *>          The DATE-MAJ is used to set the updated date of the
      *>          feed.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENDER-RSS.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 FEED-URL                  PIC X(1000).
       01 ESCAPED-SOURCE-URL        PIC X(1000) VALUE SPACES.
       01 ESCAPED-FEED-URL          PIC X(1000) VALUE SPACES.

       LINKAGE SECTION.
       01 SOURCE-URL                 PIC X(1000).
       01 DATE-MAJ                   PIC X(24).
       01 FEED-CONTENT               PIC X(10000) VALUE SPACES.
       01 RSS-CONTENT                PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE SOURCE-URL
           BY REFERENCE DATE-MAJ
           BY REFERENCE FEED-CONTENT
           BY REFERENCE RSS-CONTENT.

           ACCEPT FEED-URL FROM ENVIRONMENT "POLLEN_FEED_URL"

           *> Escape & from the URL
           CALL "XML-ENCODE" USING
               BY REFERENCE SOURCE-URL
               BY REFERENCE ESCAPED-SOURCE-URL
           END-CALL
           CALL "XML-ENCODE" USING
               BY REFERENCE FEED-URL
               BY REFERENCE ESCAPED-FEED-URL
           END-CALL

           STRING
               '<?xml version="1.0" encoding="utf-8"?>'            X"0A"
               '<feed xmlns="http://www.w3.org/2005/Atom"'         X"0A"
               ' xmlns:dc="http://purl.org/dc/elements/1.1/">'     X"0A"
               " <updated>" DATE-MAJ "</updated>"                  X"0A"
               " <dc:date>" DATE-MAJ "</dc:date>"                  X"0A"
               " <title>Pollenes aujourd'hui</title>"              X"0A"
               " <subtitle>Pollenes aujourd'hui</subtitle>"        X"0A"
               ' <link rel="alternate" '                           X"0A"
               '  href="' FUNCTION TRIM(ESCAPED-FEED-URL) '" />'   X"0A"
               " <id>" FUNCTION TRIM(ESCAPED-FEED-URL) "</id>"     X"0A"
               " <entry>"                                          X"0A"
               "  <title>Rapport de pollens</title>"               X"0A"
               '  <link rel="alternate" '                          X"0A"
               '   href="' FUNCTION TRIM(ESCAPED-SOURCE-URL) '"/>' X"0A"
               "  <id>" FUNCTION TRIM(ESCAPED-SOURCE-URL) "</id>"  X"0A"
               '  <content type="text/plain">'                     X"0A"
                   FUNCTION TRIM(FEED-CONTENT)
               "  </content>"                                      X"0A"
               "  <author><name>Atmo France</name></author>"       X"0A"
               "  <dc:creator>Atmo France</dc:creator>"            X"0A"
               "  <published>" DATE-MAJ "</published>"             X"0A"
               "  <updated>" DATE-MAJ "</updated>"                 X"0A"
               "  <dc:date>" DATE-MAJ "</dc:date>"                 X"0A"
               " </entry>"                                         X"0A"
               "</feed>"
               INTO RSS-CONTENT
           END-STRING

           GOBACK.

       END PROGRAM RENDER-RSS.
