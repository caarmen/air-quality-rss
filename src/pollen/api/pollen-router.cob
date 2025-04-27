      *> ===============================================================
      *> PROGRAM: POLLEN-ROUTER
      *> PURPOSE: Handles incoming HTTP requests for pollen data.
      *>          Parses query parameters and calls the pollen service.
      *>          Returns the response to the caller.
      *>          This program is called by the
      *>          MICROHTTPD-ACCESS-HANDLER.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-ROUTER.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-LATITUDE-DEGREES        PIC S9(3)V9(8).
           01  LS-LONGITUDE-DEGREES       PIC S9(3)V9(8).
           01  LS-QUERY-PARAM-LATITUDE    PIC X(16) VALUE "latitude".
           01  LS-QUERY-PARAM-LONGITUDE   PIC X(16) VALUE "longitude".

       LINKAGE SECTION.
           01  CONNECTION-PTR             USAGE POINTER.
           01  HTTP-METHOD                PIC X(8).
           01  URL                        PIC X(100).
           01  RESPONSE.
               05  STATUS-CODE            PIC 999.
               05  BODY                   PIC X(10000)     VALUE SPACES.

       PROCEDURE DIVISION
           USING
               BY VALUE    CONNECTION-PTR
               BY REFERENCE HTTP-METHOD
               BY REFERENCE URL
               BY REFERENCE RESPONSE.

           DISPLAY "Incoming " HTTP-METHOD " request for " URL ".".

           IF FUNCTION TRIM(HTTP-METHOD) = "GET"
               AND FUNCTION TRIM(URL) = "/pollen-rss"
           THEN
               MOVE 200 TO STATUS-CODE

               *> Parse the latitude query parameter

               CALL "PARSE-NUMERIC-QUERY-PARAM" USING
                   BY VALUE     CONNECTION-PTR
                   BY REFERENCE LS-QUERY-PARAM-LATITUDE
                   BY REFERENCE LS-LATITUDE-DEGREES
                   RETURNING RETURN-CODE
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 400 TO STATUS-CODE
                   MOVE "Bad Request: missing latitude query param"
                       TO BODY
                   GOBACK
               END-IF

               *> Parse the longitude query parameter

               CALL "PARSE-NUMERIC-QUERY-PARAM" USING
                   BY VALUE     CONNECTION-PTR
                   BY REFERENCE LS-QUERY-PARAM-LONGITUDE
                   BY REFERENCE LS-LONGITUDE-DEGREES
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 400 TO STATUS-CODE
                   MOVE "Bad Request: missing longitude query param"
                       TO BODY
                   GOBACK
               END-IF

               *> We have all we need, call the pollen service.

               CALL "POLLEN-SERVICE" USING
                   BY REFERENCE LS-LATITUDE-DEGREES
                   BY REFERENCE LS-LONGITUDE-DEGREES
                   BY REFERENCE BODY
                   RETURNING RETURN-CODE
               IF RETURN-CODE NOT = 0
               THEN
                   MOVE 500 TO STATUS-CODE
                   MOVE "Internal Server Error: pollen service failed"
                       TO BODY
               END-IF

           ELSE
               MOVE 404 TO STATUS-CODE
               MOVE "Not Found" TO BODY
           END-IF.

           GOBACK.
       END PROGRAM POLLEN-ROUTER.
