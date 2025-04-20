       identification division.
       program-id. pollen-rss.

      *> ***************************************************************
       procedure division.
       call "pollen-server".

       *> Wait for incoming connections
       perform forever
           call "sleep" using by value 1
       end-perform

