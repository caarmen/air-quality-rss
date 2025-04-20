      *> https://en.wikipedia.org/wiki/Equirectangular_projection
      *> https://proj.org/en/stable/operations/projections/webmerc.html
      *> https://mathworld.wolfram.com/MercatorProjection.html
       identification division.
       program-id. lat-long-to-web-mercator. 
       data division.
       working-storage section.
       01 pi pic s9(3)v99999999 value 3.14159265.
       01 earth-radius pic s9(7) value 6378137.

       linkage section.
       01 latitude pic s9(3)v9(8).
       01 longitude pic s9(3)v9(8).
       01 x pic s9(7)v9(8).
       01 y pic s9(7)v9(8).

       procedure division using
           by reference latitude
           by reference longitude
           by reference x
           by reference y.
       compute x = earth-radius * longitude * pi / 180.
       compute y = earth-radius *
           function log(function tan(((latitude * pi/180)/2) 
           + (pi / 4))).

       goback.
       end program lat-long-to-web-mercator.
