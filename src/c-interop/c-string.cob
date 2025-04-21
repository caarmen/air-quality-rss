
      *> ===============================================================
      *> PROGRAM: C-STRING
      *> PURPOSE: Converts a C-STRING TO A COBOL STRING
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C-STRING.

       DATA DIVISION.

       LINKAGE SECTION.
           01  C-STRING                 USAGE POINTER.
           01  COBOL-STRING             PIC X(10000) VALUE SPACES.

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY VALUE     C-STRING
           BY REFERENCE COBOL-STRING.

           CALL "strcpy" USING
               BY REFERENCE COBOL-STRING
               BY VALUE     C-STRING

           INSPECT COBOL-STRING
               REPLACING FIRST X"00" BY SPACE

           GOBACK.
       END PROGRAM C-STRING.
