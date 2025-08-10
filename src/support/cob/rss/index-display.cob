      *> ===============================================================
      *> PROGRAM: INDEX-DISPLAY
      *> PURPOSE: Chooses an emoji, suitable for xml encoding,
      *>          of a square whose color corresponds to the given
      *>          air quality index.
      *>          Returns the index and emoji separated by a space.
      *> ===============================================================
       PROGRAM-ID. INDEX-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-WHITE-CIRCLE                    CONSTANT "&#x26aa;".
       01  C-BLUE-CIRCLE                     CONSTANT "&#x1f535;".
       01  C-GREEN-CIRCLE                    CONSTANT "&#x1f7e2;".
       01  C-YELLOW-CIRCLE                   CONSTANT "&#x1f7e1;".
       01  C-ORANGE-CIRCLE                   CONSTANT "&#x1f7e0;".
       01  C-RED-CIRCLE                      CONSTANT "&#x1f534;".
       01  C-PURPLE-CIRCLE                   CONSTANT "&#x1f7e3;".

       LOCAL-STORAGE SECTION.
       01  LS-INDEX-NUMBER-DISP              PIC 9(1).
       01  LS-EMOJI                          PIC X(9) VALUE SPACES.

       LINKAGE SECTION.
       01  IN-INDEX-NUMERIC                  PIC 9(9) COMP-5.
       01  OUT-INDEX-DISPLAY                 PIC X(11) VALUE SPACES.
       PROCEDURE DIVISION USING
           IN-INDEX-NUMERIC,
           OUT-INDEX-DISPLAY.

           MOVE IN-INDEX-NUMERIC TO LS-INDEX-NUMBER-DISP
           CALL "INDEX-EMOJI" USING
               LS-INDEX-NUMBER-DISP
               LS-EMOJI
           STRING
               LS-INDEX-NUMBER-DISP " " LS-EMOJI
               INTO OUT-INDEX-DISPLAY
           END-STRING
           .
       END PROGRAM INDEX-DISPLAY.

      *> ===============================================================
      *> PROGRAM: INDEX-EMOJI
      *> PURPOSE: Returns an emoji, suitable for xml encoding,
      *>          of a square whose color corresponds to the given
      *>          air quality index.
      *> ===============================================================
       PROGRAM-ID. INDEX-EMOJI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-WHITE-CIRCLE                    CONSTANT "&#x26aa;".
       01  C-BLUE-CIRCLE                     CONSTANT "&#x1f535;".
       01  C-GREEN-CIRCLE                    CONSTANT "&#x1f7e2;".
       01  C-YELLOW-CIRCLE                   CONSTANT "&#x1f7e1;".
       01  C-ORANGE-CIRCLE                   CONSTANT "&#x1f7e0;".
       01  C-RED-CIRCLE                      CONSTANT "&#x1f534;".
       01  C-PURPLE-CIRCLE                   CONSTANT "&#x1f7e3;".

       LINKAGE SECTION.
       01  IN-INDEX                          PIC 9(1).
       01  OUT-EMOJI                         PIC X(9) VALUE SPACES.

       PROCEDURE DIVISION USING
           IN-INDEX,
           OUT-EMOJI.

           EVALUATE IN-INDEX
               WHEN 1
                   MOVE C-BLUE-CIRCLE TO OUT-EMOJI
               WHEN 2
                   MOVE C-GREEN-CIRCLE TO OUT-EMOJI
               WHEN 3
                   MOVE C-YELLOW-CIRCLE TO OUT-EMOJI
               WHEN 4
                   MOVE C-ORANGE-CIRCLE TO OUT-EMOJI
               WHEN 5
                   MOVE C-RED-CIRCLE TO OUT-EMOJI
               WHEN 6
                   MOVE C-PURPLE-CIRCLE TO OUT-EMOJI
               WHEN OTHER
                   MOVE C-WHITE-CIRCLE TO OUT-EMOJI
           END-EVALUATE
           .
       END PROGRAM INDEX-EMOJI.
