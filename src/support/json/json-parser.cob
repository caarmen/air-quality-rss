
      *> ===============================================================
      *> PROGRAM: JSON-GET-OBJECT-NAME
      *> PURPOSE: For a given JSON object, which is a value of a
      *>          property of a parent object, this program returns
      *>          the property name of the object inside its
      *>          parent object.
      *>
      *>          TODO this looks tordu, maybe this can be simplified.
      *>
      *>          We do this because we iterate over all of the
      *>          attributes of the pollen structure's
      *>          features[0].properties: like code_aul, code_boul,
      *>          code_oliv, etc.
      *>          The api from cJSON is:
      *>
      *>          To iterate over an object, you can use the
      *>          cJSON_ArrayForEach macro the same way as for arrays.
      *> https://github.com/DaveGamble/cJSON?tab=readme-ov-file#objects
      *>
      *>          For each iteration, we want to get both the name
      *>          (ex: code_aul) and the value (ex: 1).
      *>          This program returns the name (ex: code_aul).
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-GET-OBJECT-NAME.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-PROPERTY-NAME-PTR            USAGE POINTER.

       LINKAGE SECTION.
           01  IN-JSON-HANDLE-PTR              USAGE POINTER.
           01  OUT-OBJECT-NAME                 PIC X(50).

       PROCEDURE DIVISION USING
           BY VALUE     IN-JSON-HANDLE-PTR
           BY REFERENCE OUT-OBJECT-NAME.

           CALL "cJSON_GetObjectName" USING
               BY VALUE IN-JSON-HANDLE-PTR
               RETURNING LS-PROPERTY-NAME-PTR

           CALL "C-STRING" USING
               BY VALUE     LS-PROPERTY-NAME-PTR
               BY REFERENCE OUT-OBJECT-NAME

           GOBACK.
       END PROGRAM JSON-GET-OBJECT-NAME.

      *> ===============================================================
      *> PROGRAM: JSON-GET-PROPERTY-STRING-VALUE
      *> PURPOSE: For a given JSON object and property name, get the
      *>          value of the property as a COBOL string.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-GET-PROPERTY-STRING-VALUE.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-PROPERTY-VALUE-OBJECT-PTR    USAGE POINTER.
           01  LS-PROPERTY-VALUE-STRING-PTR    USAGE POINTER.

       LINKAGE SECTION.
           01  IN-JSON-OBJECT-PTR              USAGE POINTER.
           01  IN-PROPERTY-NAME                PIC X(50).
           01  OUT-PROPERTY-VALUE              PIC X(50).

       PROCEDURE DIVISION USING
           BY VALUE     IN-JSON-OBJECT-PTR
           BY REFERENCE IN-PROPERTY-NAME
           BY REFERENCE OUT-PROPERTY-VALUE.

           *> Get the value of the object's property as a cJSON object.
           CALL "cJSON_GetObjectItem" USING
               BY VALUE IN-JSON-OBJECT-PTR
               BY CONTENT IN-PROPERTY-NAME
               RETURNING LS-PROPERTY-VALUE-OBJECT-PTR

           *> We assume the value is a string: get the object's property
           *> value as a c-string.
           CALL "cJSON_GetStringValue" USING
               BY VALUE LS-PROPERTY-VALUE-OBJECT-PTR
               RETURNING LS-PROPERTY-VALUE-STRING-PTR

           *> Finally convert the c-string to a cobol string.

           MOVE SPACES TO OUT-PROPERTY-VALUE
           CALL "C-STRING" USING
               BY VALUE     LS-PROPERTY-VALUE-STRING-PTR
               BY REFERENCE OUT-PROPERTY-VALUE

           GOBACK.
       END PROGRAM JSON-GET-PROPERTY-STRING-VALUE.

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
           01  LS-ATTRIBUTE-INDEX                PIC 9 VALUE 1.
           01  LS-ATTRIBUTE-COUNT                USAGE BINARY-LONG.
           01  LS-ITER-ATTRIBUTE-HANDLE-PTR      USAGE POINTER.
           01  LS-ITER-ATTRIBUTE-NAME            PIC X(50).

       LINKAGE SECTION.
           01  IN-JSON-SOURCE-HANDLE-PTR         USAGE POINTER.
           01  IN-ATTRIBUTE-NAME                 PIC X(50) VALUE SPACES.
           01  OUT-JSON-FOUND-OBJECT-HANDLE-PTR  USAGE POINTER.

       PROCEDURE DIVISION USING
           IN-ATTRIBUTE-NAME
           BY VALUE     IN-JSON-SOURCE-HANDLE-PTR
           BY REFERENCE OUT-JSON-FOUND-OBJECT-HANDLE-PTR.

           CALL "cJSON_GetArraySize" USING
               BY VALUE IN-JSON-SOURCE-HANDLE-PTR
               RETURNING LS-ATTRIBUTE-COUNT

           MOVE NULL TO OUT-JSON-FOUND-OBJECT-HANDLE-PTR

           PERFORM VARYING LS-ATTRIBUTE-INDEX FROM 0 BY 1 
               UNTIL LS-ATTRIBUTE-INDEX = LS-ATTRIBUTE-COUNT 
                 OR OUT-JSON-FOUND-OBJECT-HANDLE-PTR NOT = NULL

               CALL "cJSON_GetArrayItem" USING
                   BY VALUE IN-JSON-SOURCE-HANDLE-PTR
                   BY VALUE LS-ATTRIBUTE-INDEX
                   RETURNING LS-ITER-ATTRIBUTE-HANDLE-PTR

               CALL "JSON-GET-OBJECT-NAME" USING
                   BY VALUE     LS-ITER-ATTRIBUTE-HANDLE-PTR
                   BY REFERENCE LS-ITER-ATTRIBUTE-NAME

               IF LS-ITER-ATTRIBUTE-NAME(1:8) = IN-ATTRIBUTE-NAME
                   MOVE LS-ITER-ATTRIBUTE-HANDLE-PTR TO
                       OUT-JSON-FOUND-OBJECT-HANDLE-PTR
               END-IF

           END-PERFORM

           GOBACK.
       END PROGRAM JSON-GET-OBJECT.
