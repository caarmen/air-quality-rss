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
           01  QUERY-PARAM-LATITUDE       PIC X(16)        VALUE SPACES.
           01  QUERY-PARAM-LONGITUDE      PIC X(16)        VALUE SPACES.
           01  LATITUDE                   PIC S9(3)V9(8).
           01  LONGITUDE                  PIC S9(3)V9(8).
           01  QUERY-PARAM-VALUE          USAGE POINTER.
           01  QUERY-PARAM-SIZE           USAGE POINTER.

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

               *> TODO: Make utility function for parsing query params.

               CALL "MHD_lookup_connection_value_n" USING
                   BY VALUE    CONNECTION-PTR
                   BY VALUE    8 *> MHD_GET_ARGUMENT_KIND
                   BY VALUE    "longitude"
                   BY VALUE    LENGTH OF "longitude"
                   BY REFERENCE QUERY-PARAM-VALUE
                   BY REFERENCE QUERY-PARAM-SIZE
               IF QUERY-PARAM-SIZE = NULL
               THEN
                   MOVE 400 TO STATUS-CODE
                   MOVE "Bad Request" TO BODY
                   GOBACK
               END-IF

               CALL "C-STRING" USING
                   BY VALUE    QUERY-PARAM-VALUE
                   BY REFERENCE QUERY-PARAM-LONGITUDE

               MOVE QUERY-PARAM-LONGITUDE TO LONGITUDE

               CALL "MHD_lookup_connection_value_n" USING
                   BY VALUE    CONNECTION-PTR
                   BY VALUE    8 *> MHD_GET_ARGUMENT_KIND
                   BY VALUE    "latitude"
                   BY VALUE    LENGTH OF "latitude"
                   BY REFERENCE QUERY-PARAM-VALUE
                   BY REFERENCE QUERY-PARAM-SIZE
               IF QUERY-PARAM-SIZE = NULL
               THEN
                   MOVE 400 TO STATUS-CODE
                   MOVE "Bad Request" TO BODY
                   GOBACK
               END-IF

               CALL "C-STRING" USING
                   BY VALUE    QUERY-PARAM-VALUE
                   BY REFERENCE QUERY-PARAM-LATITUDE

               MOVE QUERY-PARAM-LATITUDE TO LATITUDE

               CALL "POLLEN-SERVICE" USING
                   BY REFERENCE LATITUDE
                   BY REFERENCE LONGITUDE
                   BY REFERENCE BODY

           ELSE
               MOVE 404 TO STATUS-CODE
               MOVE "Not Found" TO BODY
           END-IF.

           GOBACK.
       END PROGRAM POLLEN-ROUTER.
