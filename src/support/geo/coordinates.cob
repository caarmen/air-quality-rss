
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
           01  IN-LATITUDE                  PIC S9(3)V9(8).
           01  IN-LONGITUDE                 PIC S9(3)V9(8).
           01  OUT-X                        PIC S9(7)V9(8).
           01  OUT-Y                        PIC S9(7)V9(8).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LATITUDE
           BY REFERENCE IN-LONGITUDE
           BY REFERENCE OUT-X
           BY REFERENCE OUT-Y.

           *> Convert longitude to X coordinate
           COMPUTE OUT-X = C-EARTH-RADIUS * IN-LONGITUDE * C-PI / 180.

           *> Convert latitude to Y coordinate using Mercator formula
           COMPUTE OUT-Y = C-EARTH-RADIUS *
               FUNCTION LOG(FUNCTION TAN(
                   ((IN-LATITUDE * C-PI / 180) / 2) + (C-PI / 4))).

           GOBACK.
       END PROGRAM LAT-LONG-TO-WEB-MERCATOR.
