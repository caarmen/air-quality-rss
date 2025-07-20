
      *> ===============================================================
      *> PROGRAM: C-STRING
      *> PURPOSE: Converts a C-STRING TO A COBOL STRING
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C-STRING.

       DATA DIVISION.

       LINKAGE SECTION.
           01  IN-C-STRING              USAGE POINTER.
           01  OUT-COBOL-STRING         PIC X(10000).

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE     IN-C-STRING
           BY REFERENCE OUT-COBOL-STRING.

           CALL "strcpy" USING
               BY REFERENCE OUT-COBOL-STRING
               BY VALUE     IN-C-STRING

           INSPECT OUT-COBOL-STRING
               REPLACING FIRST X"00" BY SPACE

           GOBACK.
       END PROGRAM C-STRING.
