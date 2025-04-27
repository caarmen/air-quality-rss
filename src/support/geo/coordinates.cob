
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
           01  C-EARTH-RADIUS-METERS        CONSTANT AS 6378137.

       LINKAGE SECTION.
           01  IN-LATITUDE-DEGREES          PIC S9(3)V9(8).
           01  IN-LONGITUDE-DEGREES         PIC S9(3)V9(8).
           01  OUT-X_METERS                 PIC S9(7)V9(8).
           01  OUT-Y-METERS                 PIC S9(7)V9(8).

       PROCEDURE DIVISION USING
           BY REFERENCE IN-LATITUDE-DEGREES
           BY REFERENCE IN-LONGITUDE-DEGREES
           BY REFERENCE OUT-X_METERS
           BY REFERENCE OUT-Y-METERS.

           *> Convert longitude to X coordinate
           COMPUTE OUT-X_METERS = C-EARTH-RADIUS-METERS
               * IN-LONGITUDE-DEGREES * C-PI / 180.

           *> Convert latitude to Y coordinate using Mercator formula
           COMPUTE OUT-Y-METERS = C-EARTH-RADIUS-METERS *
               FUNCTION LOG(FUNCTION TAN(
                   ((IN-LATITUDE-DEGREES * C-PI / 180) / 2)
                       + (C-PI / 4))).

           GOBACK.
       END PROGRAM LAT-LONG-TO-WEB-MERCATOR.
