
      *> ===============================================================
      *> PROGRAM: JSON-GET-OBJECT-NAME
      *> PURPOSE: For a given JSON object, which is a value of a
      *>          property of a parent object, this program returns
      *>          the property name of the object inside its
      *>          parent object.
      *>          TODO this looks tordu, maybe this can be simplified.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-GET-OBJECT-NAME.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  PROPERTY-NAME-PTR               USAGE POINTER.

       LINKAGE SECTION.
           01  JSON-HANDLE-PTR                 USAGE POINTER.
           01  OBJECT-NAME                     PIC X(50).

       PROCEDURE DIVISION USING
           BY VALUE     JSON-HANDLE-PTR
           BY REFERENCE OBJECT-NAME.

           CALL "cJSON_GetObjectName" USING
               BY VALUE JSON-HANDLE-PTR
               RETURNING PROPERTY-NAME-PTR

           CALL "strcpy" USING 
               BY REFERENCE OBJECT-NAME
               BY VALUE     PROPERTY-NAME-PTR

           GOBACK.
       END PROGRAM JSON-GET-OBJECT-NAME.

      *> ===============================================================
      *> PROGRAM: JSON-GET-STRING-VALUE
      *> PURPOSE: For a given JSON string property, return the value
      *>          of the property as a COBOL string.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-GET-STRING-VALUE.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  PROPERTY-VALUE-PTR              USAGE POINTER.

       LINKAGE SECTION.
           01  JSON-HANDLE-PTR                 USAGE POINTER.
           01  STRING-VALUE                    PIC X(50).

       PROCEDURE DIVISION USING
           BY VALUE     JSON-HANDLE-PTR
           BY REFERENCE STRING-VALUE.

           CALL "cJSON_GetStringValue" USING
               BY VALUE JSON-HANDLE-PTR
               RETURNING PROPERTY-VALUE-PTR

           CALL "strcpy" USING 
               BY REFERENCE STRING-VALUE
               BY VALUE     PROPERTY-VALUE-PTR

           GOBACK.
       END PROGRAM JSON-GET-STRING-VALUE.

      *> ===============================================================
      *> PROGRAM: JSON-GET-OBJECT
      *> PURPOSE: Return the first value of a JSON object's properties
      *>          whose name matches the given name.
      *>
      *>          This is a workaround:
      *>          The GetObject API from cJSON doesn't seem to work when
      *>          the attribute value is an array. So we implement an
      *>          alternative here which iterates over all the object's
      *>          attributes until it finds the one with the given name.
      *> ===============================================================


       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-GET-OBJECT.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  ATTRIBUTE-INDEX                PIC 9       VALUE 1.
           01  ATTRIBUTE-COUNT                USAGE BINARY-LONG.
           01  ITER-ATTRIBUTE-HANDLE-PTR      USAGE POINTER.
           01  ITER-ATTRIBUTE-NAME            PIC X(50).

       LINKAGE SECTION.
           01  JSON-SOURCE-HANDLE-PTR         USAGE POINTER.
           01  ATTRIBUTE-NAME                 PIC X(50) VALUE SPACES.
           01  JSON-FOUND-OBJECT-HANDLE-PTR   USAGE POINTER.

       PROCEDURE DIVISION USING
           ATTRIBUTE-NAME
           BY VALUE     JSON-SOURCE-HANDLE-PTR
           BY REFERENCE JSON-FOUND-OBJECT-HANDLE-PTR.

           CALL "cJSON_GetArraySize" USING
               BY VALUE JSON-SOURCE-HANDLE-PTR
               RETURNING ATTRIBUTE-COUNT

           MOVE NULL TO JSON-FOUND-OBJECT-HANDLE-PTR

           PERFORM VARYING ATTRIBUTE-INDEX FROM 0 BY 1 
               UNTIL ATTRIBUTE-INDEX = ATTRIBUTE-COUNT 
                 OR JSON-FOUND-OBJECT-HANDLE-PTR NOT = NULL

               CALL "cJSON_GetArrayItem" USING
                   BY VALUE JSON-SOURCE-HANDLE-PTR
                   BY VALUE ATTRIBUTE-INDEX
                   RETURNING ITER-ATTRIBUTE-HANDLE-PTR

               CALL "JSON-GET-OBJECT-NAME" USING
                   BY VALUE     ITER-ATTRIBUTE-HANDLE-PTR
                   BY REFERENCE ITER-ATTRIBUTE-NAME

               IF ITER-ATTRIBUTE-NAME(1:8) = ATTRIBUTE-NAME
                   MOVE ITER-ATTRIBUTE-HANDLE-PTR TO
                       JSON-FOUND-OBJECT-HANDLE-PTR
               END-IF

           END-PERFORM

           GOBACK.
       END PROGRAM JSON-GET-OBJECT.
