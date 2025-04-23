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
       01 JSON-ERROR-PTR               USAGE POINTER.
       01 JSON-ERROR-MSG               PIC X(10000).
       01 JSON-ROOT-PTR                USAGE POINTER.
       01 JSON-FEATURES-PTR            USAGE POINTER.
       01 JSON-FIRST-FEATURE-PTR       USAGE POINTER.
       01 JSON-PROPERTIES-PTR          USAGE POINTER.
       01 JSON-POLLEN-DATE-MAJ-PTR     USAGE POINTER.
       01 JSON-POLLEN-RESP-PTR         USAGE POINTER.
       01 PROPERTY-ATTR-INDEX          PIC 999 VALUE 1.
       01 PROPERTY-ATTR-PTR            USAGE POINTER.
       01 PROPERTY-NAME-VAL            PIC X(50).
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

      *> ===============================================================
      *> The json input looks like this:
      *>   {
      *>       "features": [
      *>         {
      *>           "properties": {
      *>             "date_maj": "2025-04-21T10:01:02.983Z",
      *>             "code_qual": 2,
      *>             "code_aul": 1,
      *>             "code_boul": 2,
      *>             "code_oliv": 1,
      *>             "code_gram": 2,
      *>             "code_arm": 1,
      *>             "code_ambr": 1,
      *>             "pollen_resp": "BETULA GRAMINEE",
      *>             "code_zone": "07079",
      *>           },
      *>         }
      *>       ]
      *>     }
      *> It actually has many more fields, but these are the ones we're
      *> interested in for now.
      *> ===============================================================

      *> Parse the raw txt and get a handle to the JSON root element.
           CALL "cJSON_Parse" USING
               BY CONTENT FUNCTION TRIM(POLLEN-JSON-INPUT)
               RETURNING JSON-ROOT-PTR
           PERFORM CHECK-JSON-ERROR

      *> Get the "features" attribute, which is an array:
           CALL "JSON-GET-OBJECT" USING
               BY CONTENT FEATURES-ATTRIBUTE
               BY VALUE JSON-ROOT-PTR
               BY REFERENCE JSON-FEATURES-PTR
           PERFORM CHECK-JSON-ERROR

      *> Get the first feature (there's only ever one it seems).
           CALL "cJSON_GetArrayItem" USING
               BY VALUE JSON-FEATURES-PTR
               0
               RETURNING JSON-FIRST-FEATURE-PTR
           PERFORM CHECK-JSON-ERROR

           IF JSON-FIRST-FEATURE-PTR = NULL
           THEN
                *> No features found, exit.
                DISPLAY "No features found in JSON data."
                MOVE 1 TO RETURN-CODE
                GOBACK
           END-IF
           *> Get the "properties" attribute, which is an object:
           CALL "cJSON_GetObjectItem" USING
               BY VALUE JSON-FIRST-FEATURE-PTR
               BY CONTENT PROPERTIES-ATTRIBUTE
               RETURNING JSON-PROPERTIES-PTR
           PERFORM CHECK-JSON-ERROR

           IF JSON-PROPERTIES-PTR NOT = NULL
           THEN
               OPEN OUTPUT POLLEN-FILE

               *> Get the "date_maj" attribute, which is a datetime
               *> string. We don't have any datetime logic for
               *> this attribute. We just store it as a string
               *> to use it in the rss date fields.
               CALL "JSON-GET-PROPERTY-STRING-VALUE" USING
                   BY VALUE JSON-PROPERTIES-PTR
                   BY REFERENCE DATE-MAJ-ATTRIBUTE
                   BY REFERENCE DATE-MAJ
               PERFORM CHECK-JSON-ERROR
               WRITE DATE-MAJ

               *> Get the "pollen_resp" attribute, which is a
               *> string containing potentially multiple pollen
               *> names separated by spaces. For now we just store
               *> it as is without any parsing.
               CALL "JSON-GET-PROPERTY-STRING-VALUE" USING
                   BY VALUE JSON-PROPERTIES-PTR
                   BY REFERENCE POLLEN-RESP-ATTRIBUTE
                   BY REFERENCE RESPONSIBLE-POLLEN
               PERFORM CHECK-JSON-ERROR
               WRITE RESPONSIBLE-POLLEN

               CALL "cJSON_GetArraySize" USING
                   BY VALUE JSON-PROPERTIES-PTR
                   RETURNING JSON-PROPERTIES-SIZE
               PERFORM CHECK-JSON-ERROR

               *> Iterate over all the properties, looking for
               *> the ones prefixed with code_. These are the pollen
               *> codes (except for code_qual and code_zone).
               PERFORM VARYING PROPERTY-ATTR-INDEX FROM 0 BY 1 
                   UNTIL PROPERTY-ATTR-INDEX = JSON-PROPERTIES-SIZE
                       MOVE " " TO PROPERTY-NAME-VAL
                       *> PROPERTY-ATTR-PTR points to an object
                       *> containing the name (code_boul) and value
                       *> (2) of one of the properties.
                       CALL "cJSON_GetArrayItem" USING
                           BY VALUE JSON-PROPERTIES-PTR
                           PROPERTY-ATTR-INDEX
                           RETURNING PROPERTY-ATTR-PTR
                       PERFORM CHECK-JSON-ERROR

                       *> PROPERTY-NAME-VAL will be like "code_boul"
                       CALL "JSON-GET-OBJECT-NAME" USING
                           BY VALUE PROPERTY-ATTR-PTR
                           BY REFERENCE PROPERTY-NAME-VAL
                       PERFORM CHECK-JSON-ERROR

                       *> Ignore code_qual and code_zone which
                       *> aren't pollen codes. All other code_
                       *> attributes are pollen codes.
                       IF PROPERTY-NAME-VAL(1:5) = "code_"
                           AND PROPERTY-NAME-VAL(1:9) 
                               NOT = "code_qual"
                           AND PROPERTY-NAME-VAL(1:9) 
                               NOT = "code_zone"
                       THEN
                           MOVE PROPERTY-NAME-VAL TO POLLEN-NAME
                           *> POLLEN-CODE will be like 2
                           CALL "cJSON_GetIntValue" USING
                               BY VALUE PROPERTY-ATTR-PTR
                               RETURNING POLLEN-CODE
                       PERFORM CHECK-JSON-ERROR
                           WRITE POLLEN-RECORD
                       END-IF
               END-PERFORM
               CLOSE POLLEN-FILE
           END-IF
           MOVE 0 TO RETURN-CODE
           GOBACK.

      *> ===============================================================
      *> PARAGRAPH: CHECK-JSON-ERROR
      *> PURPOSE: Check if the cJSON library has "raised an error".
      *>          This is done by setting the JSON-ERROR-PTR to a
      *>          position in the json input where the error starts.
      *>          If this happens:
      *>            - log the error information,
      *>            - set an error RETURN-CODE,
      *>            - Stop execution of the POLLEN-PARSER program.
      *> ===============================================================
       CHECK-JSON-ERROR.
           CALL "cJSON_GetErrorPtr"
               RETURNING JSON-ERROR-PTR
           IF JSON-ERROR-PTR NOT = NULL
           THEN
               DISPLAY "Json error occurred"
               CALL "C-STRING" USING
                   BY VALUE     JSON-ERROR-PTR
                   BY REFERENCE JSON-ERROR-MSG
               DISPLAY "Json error ptr: " FUNCTION TRIM (JSON-ERROR-MSG)
               MOVE 1 to RETURN-CODE
               GOBACK
           END-IF.

       END PROGRAM POLLEN-PARSER.
