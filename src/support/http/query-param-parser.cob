      *> ===============================================================
      *> PROGRAM: PARSE-QUERY-PARAM
      *> PURPOSE: Fill the value of the given query parameter provided
      *>          in an incoming request.
      *>          Return 0 if the parameter is found, 1 otherwise.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-QUERY-PARAM.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-QUERY-PARAM-VALUE-PTR   USAGE POINTER.
           01  LS-QUERY-PARAM-SIZE-PTR    USAGE POINTER.
           01  MHD-GET-ARGUMENT-KIND      CONSTANT AS 8.

       LINKAGE SECTION.
           01  CONNECTION-PTR             USAGE POINTER.
           01  QUERY-PARAM-NAME           PIC X(16)        VALUE SPACES.
           01  QUERY-PARAM-VALUE          PIC X(100)       VALUE SPACES.

       PROCEDURE DIVISION USING
           BY VALUE CONNECTION-PTR
           BY REFERENCE QUERY-PARAM-NAME
           BY REFERENCE QUERY-PARAM-VALUE
       .
           CALL "MHD_lookup_connection_value_n" USING
               BY VALUE    CONNECTION-PTR
               BY VALUE    MHD-GET-ARGUMENT-KIND
               BY VALUE    FUNCTION TRIM(QUERY-PARAM-NAME)
               BY VALUE    LENGTH OF FUNCTION TRIM(QUERY-PARAM-NAME)
               BY REFERENCE LS-QUERY-PARAM-VALUE-PTR
               BY REFERENCE LS-QUERY-PARAM-SIZE-PTR

           IF LS-QUERY-PARAM-SIZE-PTR = NULL
           THEN
               MOVE 1 TO RETURN-CODE *> Failure
           ELSE
               CALL "C-STRING" USING
                   BY VALUE     LS-QUERY-PARAM-VALUE-PTR
                   BY REFERENCE QUERY-PARAM-VALUE
               MOVE 0 TO RETURN-CODE *> Success
           END-IF

           GOBACK.
       END PROGRAM PARSE-QUERY-PARAM.

      *> ===============================================================
      *> PROGRAM: PARSE-NUMERIC-QUERY-PARAM
      *> PURPOSE: Fill the numeric value of the given query
      *>          parameter provided in an incoming request.
      *>          Return 0 if the parameter is found, 1 otherwise.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSE-NUMERIC-QUERY-PARAM.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-QUERY-PARAM-VALUE-STR   PIC X(100)       VALUE SPACES.

       LINKAGE SECTION.
           01  CONNECTION-PTR             USAGE POINTER.
           01  QUERY-PARAM-NAME           PIC X(16)        VALUE SPACES.
           01  QUERY-PARAM-VALUE          PIC S9(3)V9(8).

       PROCEDURE DIVISION USING
           BY VALUE CONNECTION-PTR
           BY REFERENCE QUERY-PARAM-NAME
           BY REFERENCE QUERY-PARAM-VALUE.

           CALL "PARSE-QUERY-PARAM" USING
               BY VALUE     CONNECTION-PTR
               BY REFERENCE QUERY-PARAM-NAME
               BY REFERENCE LS-QUERY-PARAM-VALUE-STR
               RETURNING RETURN-CODE
           IF RETURN-CODE = 0
           THEN
               MOVE LS-QUERY-PARAM-VALUE-STR TO QUERY-PARAM-VALUE
           END-IF

           GOBACK.
       END PROGRAM PARSE-NUMERIC-QUERY-PARAM.