      *> ===============================================================
      *> PROGRAM: MY-MAIN.
      *> Try a few actions in concurrency to try to reproduce a race
      *> condition:
      *>
      *> Call, in a separate thread, two programs:
      *>   MY-CALLBACK1, which:
      *>    - calls one-arg-program
      *>   MY-CALLBACK2, which:
      *>    - calls three-arg program
      *>
      *> We hope to have some concurrent attempts at calling the one-arg
      *> and three-arg programs, to see if we'll get a bug
      *> where the last argument(s) passed to the three-arg program
      *> are passed as null (0x0).
      *> ===============================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-MAIN.

       DATA DIVISION.

       LOCAL-STORAGE SECTION.
           01  LS-MAX-ITERATIONS          PiC 9(16).
           01  LS-COUNTER                 PIC 9(8).
           01  LS-SOME-TEXT               PIC X(32) VALUE "Ok then".
           01  LS-SOME-MODIFIED-TEXT      PIC X(64) VALUE SPACES.
           01  LS-CALLBACK-PTR1           USAGE PROGRAM-POINTER.
           01  LS-CALLBACK-PTR2           USAGE PROGRAM-POINTER.

       PROCEDURE DIVISION.

           ACCEPT LS-MAX-ITERATIONS FROM COMMAND-LINE

           DISPLAY "Performing " LS-MAX-ITERATIONS " iterations."

           PERFORM VARYING LS-COUNTER FROM 1 BY 1 UNTIL
               LS-COUNTER = LS-MAX-ITERATIONS
               SET LS-CALLBACK-PTR1 TO ENTRY "MY-CALLBACK1"
               SET LS-CALLBACK-PTR2 TO ENTRY "MY-CALLBACK2"

               CALL "call_me_back" USING
                   BY VALUE LS-CALLBACK-PTR1
                   BY VALUE LS-CALLBACK-PTR2
           END-PERFORM
           DISPLAY "Finished."
           .
       END PROGRAM MY-MAIN.


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
           *> Nothing interesting to do.
           CONTINUE
           .
       END PROGRAM ONE-ARG-PROGRAM.

      *> ===============================================================
      *> PROGRAM: THREE-ARG-PROGRAM
      *> Concatenate the first two arguments and store the result in
      *> the third argument.
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
           .
       END PROGRAM THREE-ARG-PROGRAM.


      *> ===============================================================
      *> PROGRAM: MY-CALLBACK1.
      *>
      *> Calls one-arg-program.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CALLBACK1.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT             PIC X(32) VALUE "callback text".
       PROCEDURE DIVISION.
           CALL "ONE-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
           .
       END PROGRAM MY-CALLBACK1.

      *> ===============================================================
      *> PROGRAM: MY-CALLBACK2.
      *>
      *> Calls three-arg program.
      *> ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-CALLBACK2.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
           01  LS-SOME-TEXT             PIC X(32) VALUE "callback text".
           01  LS-SOME-OTHER-TEXT       PIC X(32) VALUE "another text".
           01  LS-SOME-MODIFIED-TEXT    PIC X(96) VALUE SPACES.
       PROCEDURE DIVISION.
           CALL "THREE-ARG-PROGRAM" USING
               BY REFERENCE LS-SOME-TEXT
               BY REFERENCE LS-SOME-OTHER-TEXT
               BY REFERENCE LS-SOME-MODIFIED-TEXT
           .
       END PROGRAM MY-CALLBACK2.
