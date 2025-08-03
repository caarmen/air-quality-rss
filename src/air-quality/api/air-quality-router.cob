      *> ===============================================================
      *> PROGRAM: AIR-QUALITY-ROUTER
      *> PURPOSE: Handles incoming HTTP requests for air quality data.
      *>          Parses query parameters and calls the service.
      *>          Returns the response to the caller.
      *>          This program is called by the
      *>          MICROHTTPD-ACCESS-HANDLER.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. AIR-QUALITY-ROUTER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-LATITUDE-DEGREES        PIC S9(3)V9(8).
           01  LS-LONGITUDE-DEGREES       PIC S9(3)V9(8).
           01  LS-CODE-ZONE               PIC X(5).
           01  LS-QUERY-PARAM-LATITUDE    PIC X(16) VALUE "latitude".
           01  LS-QUERY-PARAM-LONGITUDE   PIC X(16) VALUE "longitude".
           01  LS-QUERY-PARAM-CODE-ZONE   PIC X(16) VALUE "code_zone".

       LINKAGE SECTION.
           01  IN-CONNECTION-PTR          USAGE POINTER.
           01  IN-HTTP-METHOD             PIC X(8).
           01  IN-URL                     PIC X(100).
           01  OUT-RESPONSE.
               05  OUT-STATUS-CODE        PIC 999.
               05  OUT-BODY               PIC X(10000)     VALUE SPACES.

       PROCEDURE DIVISION
           USING
               BY VALUE     IN-CONNECTION-PTR
               BY REFERENCE IN-HTTP-METHOD
               BY REFERENCE IN-URL
               BY REFERENCE OUT-RESPONSE.

           DISPLAY "Incoming " IN-HTTP-METHOD " request for "
               IN-URL ".".

           EVALUATE FUNCTION TRIM(IN-HTTP-METHOD)
               ALSO FUNCTION TRIM(IN-URL)
           WHEN "GET" ALSO "/pollen-rss"
           WHEN "GET" ALSO "/pollutant-rss/prevair"
               MOVE 200 TO OUT-STATUS-CODE

               *> Parse the latitude query parameter

               CALL "PARSE-NUMERIC-QUERY-PARAM" USING
                   BY VALUE     IN-CONNECTION-PTR
                   BY REFERENCE LS-QUERY-PARAM-LATITUDE
                   BY REFERENCE LS-LATITUDE-DEGREES
                   RETURNING RETURN-CODE
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 400 TO OUT-STATUS-CODE
                   MOVE "Bad Request: missing latitude query param"
                       TO OUT-BODY
                   GOBACK
               END-IF

               *> Parse the longitude query parameter

               CALL "PARSE-NUMERIC-QUERY-PARAM" USING
                   BY VALUE     IN-CONNECTION-PTR
                   BY REFERENCE LS-QUERY-PARAM-LONGITUDE
                   BY REFERENCE LS-LONGITUDE-DEGREES
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 400 TO OUT-STATUS-CODE
                   MOVE "Bad Request: missing longitude query param"
                       TO OUT-BODY
                   GOBACK
               END-IF

               EVALUATE FUNCTION TRIM(IN-URL)
                   WHEN "/pollen-rss"
                       CALL "POLLEN-SERVICE" USING
                           BY REFERENCE IN-URL
                           BY REFERENCE LS-LATITUDE-DEGREES
                           BY REFERENCE LS-LONGITUDE-DEGREES
                           BY REFERENCE OUT-BODY
                           RETURNING RETURN-CODE
                   WHEN "/pollutant-rss/prevair"
                       CALL "PREVAIR-POLLUTANT-SERVICE" USING
                           BY REFERENCE IN-URL
                           BY REFERENCE LS-LATITUDE-DEGREES
                           BY REFERENCE LS-LONGITUDE-DEGREES
                           BY REFERENCE OUT-BODY
                           RETURNING RETURN-CODE
               END-EVALUATE
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 500 TO OUT-STATUS-CODE
                   STRING "Internal Server Error: "
                       FUNCTION TRIM(IN-URL) " failed" INTO OUT-BODY
                   END-STRING
               END-IF
           WHEN "GET" ALSO "/pollutant-rss/atmo-france"
               MOVE 200 TO OUT-STATUS-CODE
               CALL "PARSE-QUERY-PARAM" USING
                   BY VALUE     IN-CONNECTION-PTR
                   BY REFERENCE LS-QUERY-PARAM-CODE-ZONE
                   BY REFERENCE LS-CODE-ZONE
                   RETURNING RETURN-CODE
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 400 TO OUT-STATUS-CODE
                   MOVE "Bad Request: missing code_zone query param"
                       TO OUT-BODY
                   GOBACK
               END-IF
               CALL "ATMO-FRANCE-POLLUTANT-SERVICE" USING
                   BY REFERENCE IN-URL
                   BY REFERENCE LS-CODE-ZONE
                   BY REFERENCE OUT-BODY
                   RETURNING RETURN-CODE
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 500 TO OUT-STATUS-CODE
                   STRING "Internal Server Error: "
                       FUNCTION TRIM(IN-URL) " failed" INTO OUT-BODY
                   END-STRING
               END-IF

           WHEN OTHER
               MOVE 404 TO OUT-STATUS-CODE
               MOVE "Not Found" TO OUT-BODY
           END-EVALUATE.

           GOBACK.
       END PROGRAM AIR-QUALITY-ROUTER.
