       identification division.
       program-id. c-string.

       data division.
       linkage section.
       01 c-string usage pointer.
       01 cobol-string pic x(10000) value spaces.

       procedure division with C linkage using
           by value c-string
           by reference cobol-string.
           call "strcpy" using
               by reference cobol-string
               by value c-string
           inspect cobol-string
               replacing first x"00" by space
           goback.
       end program c-string.
