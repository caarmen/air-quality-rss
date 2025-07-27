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
       01 LS-ESCAPED-SOURCE-URL     PIC X(1000) VALUE SPACES.
       01 LS-ESCAPED-FEED-URL       PIC X(1000) VALUE SPACES.

       01 LS-UPDATED-AT             PIC X(24).

       01 LS-FEED-ENTRY             PIC X(10000) VALUE SPACES.

       LINKAGE SECTION.
       01 IN-ID                     PIC X(100).
       01 IN-SOURCE-URL             PIC X(1000).
       01 IN-FEED-URL               PIC X(1000).
       01 IN-DATE-MAJ               PIC X(24).
       01 IN-AUTHOR                 PIC X(100).
       01 IN-FEED-TITLE             PIC X(100) VALUE SPACES.
       01 IN-ENTRY-TITLE            PIC X(100) VALUE SPACES.
       01 IN-FEED-CONTENT           PIC X(10000) VALUE SPACES.
       01 OUT-RSS-CONTENT           PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ID
           BY REFERENCE IN-SOURCE-URL
           BY REFERENCE IN-FEED-URL
           BY REFERENCE IN-DATE-MAJ
           BY REFERENCE IN-AUTHOR
           BY REFERENCE IN-FEED-TITLE
           BY REFERENCE IN-ENTRY-TITLE
           BY REFERENCE IN-FEED-CONTENT
           BY REFERENCE OUT-RSS-CONTENT.

           *> Escape & from the URL
           CALL "XML-ENCODE" USING
               BY REFERENCE IN-SOURCE-URL
               BY REFERENCE LS-ESCAPED-SOURCE-URL
           END-CALL
           CALL "XML-ENCODE" USING
               BY REFERENCE IN-FEED-URL
               BY REFERENCE LS-ESCAPED-FEED-URL
           END-CALL

           *> Build the <entry>:
           IF IN-FEED-CONTENT = SPACES
           THEN
           *> If we have no content, set the date to exactly midnight,
           *> and don't build an <entry>.
               STRING IN-DATE-MAJ(1:10) "T00:00:00.000Z"
                    INTO LS-UPDATED-AT
               END-STRING
           ELSE
           *> If we have content, create the <entry>.
           *> Set the date to 00:00:01 UTC. This is to avoid too many
           *> updates in the RSS feed.
               STRING IN-DATE-MAJ(1:10) "T00:00:01.000Z"
                    INTO LS-UPDATED-AT
               END-STRING
               STRING
               " <entry>"                                          X"0A"
               "  <title>"FUNCTION TRIM(IN-ENTRY-TITLE)"</title>"  X"0A"
               '  <link rel="alternate" '                          X"0A"
               '   href="' FUNCTION TRIM(LS-ESCAPED-SOURCE-URL)
               '"/>'                                               X"0A"
               "  <id>" FUNCTION TRIM(IN-ID) "</id>"               X"0A"
               '  <content type="text/plain">'                     X"0A"
                   FUNCTION TRIM(IN-FEED-CONTENT)
               "  </content>"                                      X"0A"
               "  <author><name>" FUNCTION TRIM(IN-AUTHOR)
               "</name></author>"                                  X"0A"
               "  <dc:creator>"FUNCTION TRIM(IN-AUTHOR)
               "</dc:creator>"                                     X"0A"
               "  <published>" LS-UPDATED-AT "</published>"        X"0A"
               "  <updated>" LS-UPDATED-AT "</updated>"            X"0A"
               "  <dc:date>" LS-UPDATED-AT "</dc:date>"            X"0A"
               " </entry>"                                         X"0A"
               INTO LS-FEED-ENTRY
               END-STRING
           END-IF

           *> Build the entire RSS feed.
           STRING
               '<?xml version="1.0" encoding="utf-8"?>'            X"0A"
               '<feed xmlns="http://www.w3.org/2005/Atom"'         X"0A"
               ' xmlns:dc="http://purl.org/dc/elements/1.1/">'     X"0A"
               " <updated>" LS-UPDATED-AT "</updated>"             X"0A"
               " <dc:date>" LS-UPDATED-AT "</dc:date>"             X"0A"
               " <title>" FUNCTION TRIM(IN-FEED-TITLE) "</title>"  X"0A"
               " <subtitle>" FUNCTION TRIM(IN-FEED-TITLE)
               "</subtitle>"                                       X"0A"
               ' <link rel="alternate" '                           X"0A"
               '  href="' FUNCTION TRIM(LS-ESCAPED-FEED-URL)
               '" />'                                              X"0A"
               " <id>" FUNCTION TRIM(LS-ESCAPED-FEED-URL) "</id>"  X"0A"
               FUNCTION TRIM(LS-FEED-ENTRY, TRAILING)
               "</feed>"
               INTO OUT-RSS-CONTENT
           END-STRING

           GOBACK.

       END PROGRAM RENDER-RSS.
