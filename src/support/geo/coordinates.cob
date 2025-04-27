
      *> ===============================================================
      *> PROGRAM: LAT-LONG-TO-WEB-MERCATOR
      *> PURPOSE: Convert latitude and longitude to Web Mercator
      *>          coordinates.
      *>          References:
      *> https://en.wikipedia.org/wiki/Equirectangular_projection
      *> https://proj.org/en/stable/operations/projections/webmerc.html
      *> https://mathworld.wolfram.com/MercatorProjection.html
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAT-LONG-TO-WEB-MERCATOR.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
           01  C-PI                         CONSTANT AS 3.14159265.
           01  C-EARTH-RADIUS               CONSTANT AS 6378137.

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
           COMPUTE X = C-EARTH-RADIUS * LONGITUDE * C-PI / 180.

           *> Convert latitude to Y coordinate using Mercator formula
           COMPUTE Y = C-EARTH-RADIUS *
               FUNCTION LOG(FUNCTION TAN(
                   ((LATITUDE * C-PI / 180) / 2) + (C-PI / 4))).

           GOBACK.
       END PROGRAM LAT-LONG-TO-WEB-MERCATOR.
