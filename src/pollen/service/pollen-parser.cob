      *> ===============================================================
      *> PROGRAM: POLLEN-PARSER
      *> PURPOSE: This program parses a JSON string containing pollen
      *>          data and writes the relevant information to a file.
      *>          We write to a file because the number of pollen
      *>          records is unknown.
      *>
      *>          TODO: maybe the number of pollen records is indeed
      *>          known. If so, we can use a table instead of a file.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-PARSER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLLEN-FILE ASSIGN TO "pollen.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       COPY pollen-data IN "pollen/service".

       LOCAL-STORAGE SECTION.
       01 JSON-ROOT                    USAGE POINTER.
       01 JSON-FEATURES                USAGE POINTER.
       01 JSON-FIRST-FEATURE           USAGE POINTER.
       01 JSON-PROPERTIES              USAGE POINTER.
       01 JSON-STR-VAL                 PIC X(1000) VALUE SPACES.
       01 JSON-ERROR                   USAGE POINTER.
       01 PROPERTY-ATTR-INDEX          PIC 999 VALUE 1.
       01 PROPERTY-ATTR                USAGE POINTER.
       01 PROPERTY-NAME-VAL            PIC X(50).
       01 PROPERTY-VALUE-VAL           PIC 9 VALUE 0.
       01 JSON-POLLEN-DATE-MAJ-PTR     USAGE POINTER.
       01 JSON-POLLEN-RESP-PTR         USAGE POINTER.
       01 JSON-PROPERTIES-SIZE         USAGE BINARY-LONG.
       01 FEATURES-ATTRIBUTE           PIC X(50) VALUE "features".
       01 PROPERTIES-ATTRIBUTE         PIC X(50) 
                                           VALUE "properties" & X"00".
       01 DATE-MAJ-ATTRIBUTE           PIC X(50) 
                                           VALUE "date_maj" & X"00".
       01 POLLEN-RESP-ATTRIBUTE        PIC X(50) 
                                           VALUE "pollen_resp" & X"00".

       LINKAGE SECTION.
       01 POLLEN-JSON-INPUT            PIC X(10000).

       PROCEDURE DIVISION WITH C LINKAGE USING
           BY REFERENCE POLLEN-JSON-INPUT.

           CALL "cJSON_Parse" USING
               BY CONTENT FUNCTION TRIM(POLLEN-JSON-INPUT)
               RETURNING JSON-ROOT

           CALL "JSON-GET-OBJECT" USING
               BY CONTENT FEATURES-ATTRIBUTE
               BY VALUE JSON-ROOT
               BY REFERENCE JSON-FEATURES

           CALL "cJSON_GetArrayItem" USING
               BY VALUE JSON-FEATURES
               0
               RETURNING JSON-FIRST-FEATURE

           IF JSON-FIRST-FEATURE NOT = NULL
           THEN
               CALL "cJSON_GetObjectItem" USING
                   BY VALUE JSON-FIRST-FEATURE
                   BY CONTENT PROPERTIES-ATTRIBUTE
                   RETURNING JSON-PROPERTIES

               IF JSON-PROPERTIES NOT = NULL
               THEN
                   OPEN OUTPUT POLLEN-FILE

                   CALL "cJSON_GetObjectItem" USING
                       BY VALUE JSON-PROPERTIES
                       BY CONTENT DATE-MAJ-ATTRIBUTE
                       RETURNING JSON-POLLEN-DATE-MAJ-PTR

                   CALL "JSON-GET-STRING-VALUE" USING
                       BY VALUE JSON-POLLEN-DATE-MAJ-PTR
                       BY REFERENCE JSON-STR-VAL
                   STRING JSON-STR-VAL
                       INTO DATE-MAJ
                   WRITE DATE-MAJ

                   MOVE SPACES TO JSON-STR-VAL
                   CALL "cJSON_GetObjectItem" USING
                       BY VALUE JSON-PROPERTIES
                       BY CONTENT POLLEN-RESP-ATTRIBUTE
                       RETURNING JSON-POLLEN-RESP-PTR

                   CALL "JSON-GET-STRING-VALUE" USING
                       BY VALUE JSON-POLLEN-RESP-PTR
                       BY REFERENCE JSON-STR-VAL
                   STRING JSON-STR-VAL
                       INTO RESPONSIBLE-POLLEN
                   END-STRING
                   WRITE RESPONSIBLE-POLLEN

                   CALL "cJSON_GetArraySize" USING
                       BY VALUE JSON-PROPERTIES
                       RETURNING JSON-PROPERTIES-SIZE

                   PERFORM VARYING PROPERTY-ATTR-INDEX FROM 0 BY 1 
                       UNTIL PROPERTY-ATTR-INDEX = JSON-PROPERTIES-SIZE
                           MOVE " " TO PROPERTY-NAME-VAL
                           CALL "cJSON_GetArrayItem" USING
                               BY VALUE JSON-PROPERTIES
                               PROPERTY-ATTR-INDEX
                               RETURNING PROPERTY-ATTR

                           CALL "JSON-GET-OBJECT-NAME" USING
                               BY VALUE PROPERTY-ATTR
                               BY REFERENCE PROPERTY-NAME-VAL

                           IF PROPERTY-NAME-VAL(1:5) = "code_"
                               AND PROPERTY-NAME-VAL(1:9) 
                                   NOT = "code_qual"
                               AND PROPERTY-NAME-VAL(1:9) 
                                   NOT = "code_zone"
                           THEN
                               CALL "cJSON_GetIntValue" USING
                                   BY VALUE PROPERTY-ATTR
                                   RETURNING PROPERTY-VALUE-VAL
                               MOVE PROPERTY-NAME-VAL TO POLLEN-NAME
                               MOVE PROPERTY-VALUE-VAL TO POLLEN-CODE
                               WRITE POLLEN-RECORD
                           END-IF
                   END-PERFORM
                   CLOSE POLLEN-FILE
               END-IF
           END-IF
           GOBACK.

       END PROGRAM POLLEN-PARSER.
