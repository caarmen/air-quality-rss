      *> ===============================================================
      *> PROGRAM: MY-MAIN.
      *> Launches the "my-logic" program many times, trying to
      *> reproduce a race condition.
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-MAIN.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-COUNTER                    PIC 9(8).

       PROCEDURE DIVISION.

           PERFORM VARYING LS-COUNTER FROM 1 BY 1 UNTIL 
               LS-COUNTER = 100000000
      *>DISPLAY "Iteration " LS-COUNTER
               CALL "MY-LOGIC"
           END-PERFORM
           .
       END PROGRAM MY-MAIN.


      *> ===============================================================
      *> PROGRAM: MY-LOGIC.
      *> Try a few actions in concurrency to try to reproduce a race
      *> condition:
      *> From the main thread:
      *>  - call a one-arg program
      *>  - call a two-arg program
      *> From another thread:
      *>  - call MY-CALLBACK, which:
      *>    - calls the two-arg program
      *>    - calls the one-arg program
      *>
      *> We hope to have some concurrent attempts at calling the one-arg
      *> and two-arg functions, to see if we'll get a bug
      *> where the second arg of the two-arg function is set to a
      *> null pointer.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-LOGIC.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT               PIC X(32) VALUE "Ok then".
           01  LS-SOME-MODIFIED-TEXT      PIC X(64) VALUE SPACES.
           01  LS-DELAY-MS                USAGE BINARY-LONG.
           01  LS-CALLBACK-PTR            USAGE PROGRAM-POINTER.
           01  LS-CALLBACK-PTR2            USAGE PROGRAM-POINTER.

       PROCEDURE DIVISION.
           SET LS-CALLBACK-PTR TO ENTRY "MY-CALLBACK"
           SET LS-CALLBACK-PTR2 TO ENTRY "MY-CALLBACK2"

        *>    CALL "ONE-ARG-PROGRAM" USING
        *>        BY REFERENCE LS-SOME-TEXT


        *>    COMPUTE LS-DELAY-MS = FUNCTION RANDOM * 5000
        *>    display "sleeping for " ls-delay-ms
        *>    CALL "usleep" USING BY VALUE LS-DELAY-MS

           CALL "call_me_back" USING
               BY VALUE LS-CALLBACK-PTR
               BY VALUE LS-CALLBACK-PTR2

        *>    CALL "TWO-ARG-PROGRAM" USING
        *>        BY REFERENCE LS-SOME-TEXT
        *>        BY REFERENCE LS-SOME-MODIFIED-TEXT

           .
       END PROGRAM MY-LOGIC.
       
      *> ===============================================================
      *> PROGRAM: ONE-ARG-PROGRAM
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ONE-ARG-PROGRAM.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ARG-1           PIC X(32) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ARG-1.
           CONTINUE

      *>DISPLAY "One arg program called with arg '" IN-ARG-1 "'"
           .
       END PROGRAM ONE-ARG-PROGRAM.
       
      *> ===============================================================
      *> PROGRAM: TWO-ARG-PROGRAM
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO-ARG-PROGRAM.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ARG-1           PIC X(32) VALUE SPACES.
       01  OUT-ARG-2          PIC X(64) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ARG-1
           BY REFERENCE OUT-ARG-2.

           STRING
               IN-ARG-1 IN-ARG-1
               INTO OUT-ARG-2
           END-STRING
      *>DISPLAY "Two-arg program called with arg '" IN-ARG-1 "'"
      *>        ". Returning '" OUT-ARG-2 "'"
           .
       END PROGRAM TWO-ARG-PROGRAM.

      *> ===============================================================
      *> PROGRAM: THREE-ARG-PROGRAM
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. THREE-ARG-PROGRAM.

       DATA DIVISION.
       LINKAGE SECTION.
       01  IN-ARG-1           PIC X(32) VALUE SPACES.
       01  IN-ARG-2           PIC X(32) VALUE SPACES.
       01  OUT-ARG-3          PIC X(96) VALUE SPACES.

       PROCEDURE DIVISION USING
           BY REFERENCE IN-ARG-1
           BY REFERENCE IN-ARG-2
           BY REFERENCE OUT-ARG-3
           .

           STRING 
               IN-ARG-1 IN-ARG-2
               INTO OUT-ARG-3
           END-STRING
      *>DISPLAY "Three arg program called with args '" IN-ARG-1 "'"
      *>         ", '" IN-ARG-2 "', '" OUT-ARG-3 "'"
           .
       END PROGRAM THREE-ARG-PROGRAM.
       

      *> ===============================================================
      *> PROGRAM: MY-CALLBACK.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CALLBACK.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT             PIC X(32) VALUE "callback text".
           01  LS-SOME-MODIFIED-TEXT    PIC X(64) VALUE SPACES.
           01  LS-DELAY-MS                USAGE BINARY-LONG.
       PROCEDURE DIVISION.
      *>DISPLAY "Got called back!"
           CALL "TWO-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
               BY REFERENCE LS-SOME-MODIFIED-TEXT
        .  COMPUTE LS-DELAY-MS = FUNCTION RANDOM * 500
      *>display "sleeping for " ls-delay-ms
           CALL "usleep" USING BY VALUE LS-DELAY-MS
           CALL "ONE-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
           .
       END PROGRAM MY-CALLBACK.

      *> ===============================================================
      *> PROGRAM: MY-CALLBACK2.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CALLBACK2.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT             PIC X(32) VALUE "callback text".
           01  LS-SOME-OTHER-TEXT       PIC X(32) VALUE "another text".
           01  LS-SOME-MODIFIED-TEXT    PIC X(96) VALUE SPACES.
       PROCEDURE DIVISION.
      *>DISPLAY "Got called back!"
           CALL "THREE-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
               BY REFERENCE LS-SOME-OTHER-TEXT
               BY REFERENCE LS-SOME-MODIFIED-TEXT
           .
       END PROGRAM MY-CALLBACK2.
