       identification division.
       program-id. pollen-service.

       data division.
       local-storage section.
       01 buffer pic x(10000).
       01 DATA-URL                  pic x(1000) VALUE SPACES.

       linkage section.
       01 latitude pic s9(3)v9(8).
       01 longitude pic s9(3)v9(8).
       01 pollen-output pic x(10000) value spaces.

       procedure division using
           by reference latitude
           by reference longitude
           by reference pollen-output
       .

       call "pollen-data-source" using
           by reference latitude
           by reference longitude
           by reference DATA-URL
           by reference buffer

       call "pollen-parser" using
           by reference buffer

       call "pollen-render" using
           by reference DATA-URL
           by reference pollen-output


       goback.
       end program pollen-service.

