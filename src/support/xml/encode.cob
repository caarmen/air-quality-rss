
      *> ===============================================================
      *> PROGRAM: XML-ENCODE
      *> PURPOSE: Convert the given input string to a format that can
      *>          be used in an XML document.
      *>          For now, this only replaces the '&' character with
      *>          '&amp;'.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. XML-ENCODE.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01 LS-I                         PIC 9(3) VALUE 1.

       LINKAGE SECTION.
           01 IN-RAW-TEXT                  PIC X(1000).
           01 OUT-ESCAPED-TEXT             PIC X(1000) VALUE SPACES.


       PROCEDURE DIVISION USING
           BY REFERENCE IN-RAW-TEXT
           BY REFERENCE OUT-ESCAPED-TEXT.

           *> This could be done more robustly with a thin wrapper to
           *> libxml2 APIs.
           PERFORM VARYING LS-I FROM 1 BY 1 
               UNTIL LS-I > LENGTH OF FUNCTION TRIM(IN-RAW-TEXT)
               EVALUATE IN-RAW-TEXT(LS-I:1)
                   WHEN "&"
                       STRING
                           FUNCTION TRIM(OUT-ESCAPED-TEXT)
                           "&amp;"
                           INTO OUT-ESCAPED-TEXT
                       END-STRING
                   WHEN OTHER
                       STRING
                           FUNCTION TRIM(OUT-ESCAPED-TEXT)
                           IN-RAW-TEXT(LS-I:1)
                           INTO OUT-ESCAPED-TEXT
                       END-STRING
               END-EVALUATE
           END-PERFORM

           GOBACK.
       END PROGRAM XML-ENCODE.
