       identification division.
       program-id. pollen-service.

       data division.
       local-storage section.
       01 buffer pic x(10000).

       linkage section.
       01 pollen-output pic x(10000) value spaces.

       procedure division using
           by reference pollen-output
       .

       call "pollen-data-source" using
           by reference buffer

       call "pollen-parser" using
           by reference buffer

       call "pollen-render" using
           by reference pollen-output


       goback.
       end program pollen-service.

