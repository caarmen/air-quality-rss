       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-DATA-SOURCE.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  RESPONSE.
               05  RESPONSE-DATA                PIC X(10000).
               05  RESPONSE-LENGTH-BYTES        PIC 9(5) COMP-5.

       LINKAGE SECTION.
           01  LATITUDE                       PIC S9(3)V9(8).
           01  LONGITUDE                      PIC S9(3)V9(8).
           01  DATA-URL                       PIC X(1000) VALUE SPACES.
           01  RESPONSE-BODY                  PIC X(10000).

       PROCEDURE DIVISION USING
           BY REFERENCE LATITUDE
           BY REFERENCE LONGITUDE
           BY REFERENCE DATA-URL
           BY REFERENCE RESPONSE-BODY.

           CALL "SOURCE-URL" USING
               BY REFERENCE LATITUDE
               BY REFERENCE LONGITUDE
               BY REFERENCE DATA-URL
           DISPLAY "Fetching data from " DATA-URL

           CALL "HTTP-CLIENT-GET" USING
               BY REFERENCE DATA-URL
               BY REFERENCE RESPONSE

           MOVE RESPONSE-DATA(1:RESPONSE-LENGTH-BYTES)
               TO RESPONSE-BODY
           GOBACK.

       END PROGRAM POLLEN-DATA-SOURCE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOURCE-URL.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  CURRENT-DATE-AND-TIME.
               05  CDT-YEAR                    PIC 9(4).
               05  CDT-MONTH                   PIC 9(2). *> 01-12
               05  CDT-DAY                     PIC 9(2). *> 01-31
           01  DATE-AND-TIME-STR               PIC X(10).
           01  DATA-URL                        PIC X(1000) VALUE
               "https://data.atmo-france.org/geoserver/ind_pol/ows?" &
               "&REQUEST=GetFeatureInfo&SERVICE=WMS&SRS=EPSG%3A3857" &
               "&STYLES=&VERSION=1.3&FILTER=%3CPropertyIsEqualTo" &
               "%20matchCase%3D%22true%22%3E" &
               "%3CPropertyName%3Edate_ech%3C" &
               "%2FPropertyName%3E%3CLiteral" &
               "%3EYYYY-MM-DD%3C%2FLiteral%3E" &
               "%3C%2FPropertyIsEqualTo%3E&SORTBY=date_dif%20D" &
               "&LAYERS=ind_pol%3Aind_national_pol" &
               "&QUERY_LAYERS=ind_pol%3Aind_national_pol" &
               "&INFO_FORMAT=application%2Fjson" &
               "&X=535&Y=284".

           01  BBOX                           PIC X(1000) VALUE SPACES.

       LINKAGE SECTION.
           01  LATITUDE                       PIC S9(3)V9(8).
           01  LONGITUDE                      PIC S9(3)V9(8).
           01  DATA-URL-OUT                   PIC X(1000).

       PROCEDURE DIVISION USING
           BY REFERENCE LATITUDE
           BY REFERENCE LONGITUDE
           BY REFERENCE DATA-URL-OUT.

           MOVE FUNCTION CURRENT-DATE
               TO CURRENT-DATE-AND-TIME

           STRING
               CDT-YEAR "-" CDT-MONTH "-" CDT-DAY
               INTO DATE-AND-TIME-STR
           END-STRING

           CALL "BOUNDING-BOX-STR" USING
               BY REFERENCE LATITUDE
               BY REFERENCE LONGITUDE
               BY REFERENCE BBOX

           MOVE DATA-URL TO DATA-URL-OUT.

           INSPECT DATA-URL-OUT
               REPLACING ALL "YYYY-MM-DD" BY DATE-AND-TIME-STR

           STRING FUNCTION TRIM(DATA-URL-OUT) "&" BBOX
               INTO DATA-URL-OUT
           END-STRING

           GOBACK.

       END PROGRAM SOURCE-URL.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOUNDING-BOX-STR.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  X                              PIC S9(7)V9(8).
           01  Y                              PIC S9(7)V9(8).
           01  BBOX-LEFT                      PIC +9(7).9(8).
           01  BBOX-RIGHT                     PIC +9(7).9(8).
           01  BBOX-TOP                       PIC +9(7).9(8).
           01  BBOX-BOTTOM                    PIC +9(7).9(8).

       LINKAGE SECTION.
           01  LATITUDE                       PIC S9(3)V9(8).
           01  LONGITUDE                      PIC S9(3)V9(8).
           01  BOUNDING-BOX                   PIC X(1000).

       PROCEDURE DIVISION USING
           BY REFERENCE LATITUDE
           BY REFERENCE LONGITUDE
           BY REFERENCE BOUNDING-BOX.

           CALL "LAT-LONG-TO-WEB-MERCATOR" USING
               BY REFERENCE LATITUDE
               BY REFERENCE LONGITUDE
               BY REFERENCE X
               BY REFERENCE Y

           COMPUTE BBOX-LEFT = X - 20000
           COMPUTE BBOX-RIGHT = X + 20000
           COMPUTE BBOX-TOP = Y + 10000
           COMPUTE BBOX-BOTTOM = Y - 10000

           STRING
               "BBOX="
               BBOX-LEFT "%2C" BBOX-BOTTOM "%2C"
               BBOX-RIGHT "%2C" BBOX-TOP
               "&HEIGHT=500&WIDTH=1000"
               INTO BOUNDING-BOX
           END-STRING

           GOBACK.

       END PROGRAM BOUNDING-BOX-STR.
