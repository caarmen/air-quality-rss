      *> ===============================================================
      *> PROGRAM: POLLUTANT-INDEX-DISPLAY
      *> PURPOSE: Chooses an emoji, suitable for xml encoding,
      *>          of a square whose color corresponds to the given
      *>          air quality index.
      *>          Returns the index and emoji separated by a space.
      *> ===============================================================
       PROGRAM-ID. POLLUTANT-INDEX-DISPLAY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  C-BLUE-SQUARE                     CONSTANT "&#x1f7e6;".
       01  C-GREEN-SQUARE                    CONSTANT "&#x1f7e9;".
       01  C-YELLOW-SQUARE                   CONSTANT "&#x1f7e8;".
       01  C-ORANGE-SQUARE                   CONSTANT "&#x1f7e7;".
       01  C-RED-SQUARE                      CONSTANT "&#x1f7e5;".
       01  C-PURPLE-SQUARE                   CONSTANT "&#x1f7eA;".

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
           EVALUATE IN-INDEX-NUMERIC
               WHEN 1
                   MOVE C-BLUE-SQUARE TO LS-EMOJI
               WHEN 2
                   MOVE C-GREEN-SQUARE TO LS-EMOJI
               WHEN 3
                   MOVE C-YELLOW-SQUARE TO LS-EMOJI
               WHEN 4
                   MOVE C-ORANGE-SQUARE TO LS-EMOJI
               WHEN 5
                   MOVE C-RED-SQUARE TO LS-EMOJI
               WHEN 6
                   MOVE C-PURPLE-SQUARE TO LS-EMOJI
           END-EVALUATE

           STRING
               LS-INDEX-NUMBER-DISP " " LS-EMOJI
               INTO OUT-INDEX-DISPLAY
           END-STRING
           .
       END PROGRAM POLLUTANT-INDEX-DISPLAY.