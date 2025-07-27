
      *> ===============================================================
      *> PROGRAM: AIR-QUALITY-RSS
      *> PURPOSE: Entry point for the air quality RSS server.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. AIR-QUALITY-RSS.

       PROCEDURE DIVISION.

           CALL "AIR-QUALITY-SERVER".

           *> Wait for incoming connections
           PERFORM FOREVER
               CALL "sleep" USING BY VALUE 1
           END-PERFORM.
