       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLLEN-RSS.

       *> ***************************************************************
       PROCEDURE DIVISION.

           CALL "POLLEN-SERVER".

           *> Wait for incoming connections
           PERFORM FOREVER
               CALL "sleep" USING BY VALUE 1
           END-PERFORM.
