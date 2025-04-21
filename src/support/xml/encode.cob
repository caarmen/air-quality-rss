
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
           01 I                            PIC 9(3) VALUE 1.

       LINKAGE SECTION.
           01 RAW-TEXT                     PIC X(1000).
           01 ESCAPED-TEXT                 PIC X(1000) VALUE SPACES.


       PROCEDURE DIVISION USING
           BY REFERENCE RAW-TEXT
           BY REFERENCE ESCAPED-TEXT.

           *> This could be done more robustly with a thin wrapper to
           *> libxml2 APIs.
           PERFORM VARYING I FROM 1 BY 1 
               UNTIL I > LENGTH OF FUNCTION TRIM(RAW-TEXT)
               EVALUATE RAW-TEXT(I:1)
                   WHEN "&"
                       STRING
                           FUNCTION TRIM(ESCAPED-TEXT)
                           "&amp;"
                           INTO ESCAPED-TEXT
                       END-STRING
                   WHEN OTHER
                       STRING
                           FUNCTION TRIM(ESCAPED-TEXT)
                           RAW-TEXT(I:1)
                           INTO ESCAPED-TEXT
                       END-STRING
               END-EVALUATE
           END-PERFORM

           GOBACK.
       END PROGRAM XML-ENCODE.
