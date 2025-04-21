       *> https://en.wikipedia.org/wiki/Equirectangular_projection
       *> https://proj.org/en/stable/operations/projections/webmerc.html
       *> https://mathworld.wolfram.com/MercatorProjection.html

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAT-LONG-TO-WEB-MERCATOR.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           01  PI                           PIC S9(3)V99999999 
                                               VALUE 3.14159265.
           01  EARTH-RADIUS                 PIC S9(7) VALUE 6378137.

       LINKAGE SECTION.
           01  LATITUDE                     PIC S9(3)V9(8).
           01  LONGITUDE                    PIC S9(3)V9(8).
           01  X                            PIC S9(7)V9(8).
           01  Y                            PIC S9(7)V9(8).

       PROCEDURE DIVISION USING
           BY REFERENCE LATITUDE
           BY REFERENCE LONGITUDE
           BY REFERENCE X
           BY REFERENCE Y.

           *> Convert longitude to X coordinate
           COMPUTE X = EARTH-RADIUS * LONGITUDE * PI / 180.

           *> Convert latitude to Y coordinate using Mercator formula
           COMPUTE Y = EARTH-RADIUS * 
               FUNCTION LOG(FUNCTION TAN(
                   ((LATITUDE * PI / 180) / 2) + (PI / 4))).

           GOBACK.
       END PROGRAM LAT-LONG-TO-WEB-MERCATOR.
