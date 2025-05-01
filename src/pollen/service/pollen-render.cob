
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
           SELECT FD-POLLEN-FILE ASSIGN TO "pollen.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY pollen-data IN "pollen/service".

       LOCAL-STORAGE SECTION.
       01 LS-RESPONSE                 PIC X(10000) VALUE SPACES.
       01 LS-POLLEN-UPDATED-AT        PIC X(24).
       01 LS-POLLEN-DISPLAY-NAME      PIC X(16).
       01 LS-POLLEN-OUTPUT            PIC X(10000) VALUE SPACES.
       *> LS-POLLEN-DATA-HASH: string which is unique for each
       *> combination of pollen data fields: date_maj (day component
       *> only), and the code and value of each pollen.
       01 LS-POLLEN-DATA-HASH         PIC X(100) VALUE SPACES.

       LINKAGE SECTION.
       01 IN-DATA-URL                 PIC X(1000) VALUE SPACES.
       01 OUT-POLLEN-RSS              PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-DATA-URL
           BY REFERENCE OUT-POLLEN-RSS.

           OPEN INPUT FD-POLLEN-FILE

           *> First read the responsible-pollen
           *> Then read all of the pollen-records until the end of file
           READ FD-POLLEN-FILE INTO F-DATE-MAJ
           STRING F-DATE-MAJ INTO LS-POLLEN-UPDATED-AT
           END-STRING
           *> Add the date to the pollen data hash.
           STRING
               F-DATE-MAJ(1:10)
               INTO LS-POLLEN-DATA-HASH
           END-STRING

           READ FD-POLLEN-FILE INTO F-RESPONSIBLE-POLLEN
           STRING
               "Pollen responsable: "
               FUNCTION TRIM(F-RESPONSIBLE-POLLEN) X"0A"
               INTO LS-POLLEN-OUTPUT
           END-STRING

           PERFORM UNTIL EXIT
               READ FD-POLLEN-FILE INTO F-POLLEN-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       CALL "POLLEN-DISPLAY-NAME" USING
                           BY REFERENCE F-POLLEN-NAME
                           BY REFERENCE LS-POLLEN-DISPLAY-NAME
                       END-CALL
                       STRING
                           FUNCTION TRIM(LS-POLLEN-OUTPUT)
                           FUNCTION TRIM(LS-POLLEN-DISPLAY-NAME)
                           ": "
                           F-POLLEN-CODE X"0A"
                           INTO LS-POLLEN-OUTPUT
                       END-STRING
                       *> Add the pollen name and code to the pollen
                       *> data hash.
                       STRING
                           FUNCTION TRIM(LS-POLLEN-DATA-HASH)
                           FUNCTION TRIM(F-POLLEN-NAME)
                           F-POLLEN-CODE
                           INTO LS-POLLEN-DATA-HASH
                       END-STRING
               END-READ
           END-PERFORM

           CLOSE FD-POLLEN-FILE

           INSPECT LS-POLLEN-OUTPUT
               REPLACING ALL X"00" BY SPACE

           CALL "RENDER-RSS" USING
               *> Use the pollen data hash as the RSS ID
               BY REFERENCE LS-POLLEN-DATA-HASH
               BY REFERENCE IN-DATA-URL
               BY REFERENCE LS-POLLEN-UPDATED-AT
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
           ELSE
               MOVE IN-POLLEN-NAME TO OUT-POLLEN-DISPLAY-NAME
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
       01 LS-FEED-URL               PIC X(1000).
       01 LS-ESCAPED-SOURCE-URL     PIC X(1000) VALUE SPACES.
       01 LS-ESCAPED-FEED-URL       PIC X(1000) VALUE SPACES.

       01 LS-UPDATED-AT             PIC X(24).

       LINKAGE SECTION.
       01 IN-ID                     PIC X(100).
       01 IN-SOURCE-URL             PIC X(1000).
       01 IN-DATE-MAJ               PIC X(24).
       01 IN-FEED-CONTENT           PIC X(10000) VALUE SPACES.
       01 OUT-RSS-CONTENT           PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ID
           BY REFERENCE IN-SOURCE-URL
           BY REFERENCE IN-DATE-MAJ
           BY REFERENCE IN-FEED-CONTENT
           BY REFERENCE OUT-RSS-CONTENT.

           ACCEPT LS-FEED-URL FROM ENVIRONMENT "POLLEN_FEED_URL"

           *> Escape & from the URL
           CALL "XML-ENCODE" USING
               BY REFERENCE IN-SOURCE-URL
               BY REFERENCE LS-ESCAPED-SOURCE-URL
           END-CALL
           CALL "XML-ENCODE" USING
               BY REFERENCE LS-FEED-URL
               BY REFERENCE LS-ESCAPED-FEED-URL
           END-CALL

           STRING IN-DATE-MAJ(1:10) "T00:00:00.000Z"
                INTO LS-UPDATED-AT
           END-STRING

           STRING
               '<?xml version="1.0" encoding="utf-8"?>'            X"0A"
               '<feed xmlns="http://www.w3.org/2005/Atom"'         X"0A"
               ' xmlns:dc="http://purl.org/dc/elements/1.1/">'     X"0A"
               " <updated>" LS-UPDATED-AT "</updated>"             X"0A"
               " <dc:date>" LS-UPDATED-AT "</dc:date>"             X"0A"
               " <title>Pollens aujourd'hui</title>"               X"0A"
               " <subtitle>Pollens aujourd'hui</subtitle>"         X"0A"
               ' <link rel="alternate" '                           X"0A"
               '  href="' FUNCTION TRIM(LS-ESCAPED-FEED-URL)
               '" />'                                              X"0A"
               " <id>" FUNCTION TRIM(LS-ESCAPED-FEED-URL) "</id>"  X"0A"
               " <entry>"                                          X"0A"
               "  <title>Rapport de pollens</title>"               X"0A"
               '  <link rel="alternate" '                          X"0A"
               '   href="' FUNCTION TRIM(LS-ESCAPED-SOURCE-URL)
               '"/>'                                               X"0A"
               "  <id>" FUNCTION TRIM(IN-ID) "</id>"               X"0A"
               '  <content type="text/plain">'                     X"0A"
                   FUNCTION TRIM(IN-FEED-CONTENT)
               "  </content>"                                      X"0A"
               "  <author><name>Atmo France</name></author>"       X"0A"
               "  <dc:creator>Atmo France</dc:creator>"            X"0A"
               "  <published>" LS-UPDATED-AT "</published>"        X"0A"
               "  <updated>" LS-UPDATED-AT "</updated>"            X"0A"
               "  <dc:date>" LS-UPDATED-AT "</dc:date>"            X"0A"
               " </entry>"                                         X"0A"
               "</feed>"
               INTO OUT-RSS-CONTENT
           END-STRING

           GOBACK.

       END PROGRAM RENDER-RSS.
