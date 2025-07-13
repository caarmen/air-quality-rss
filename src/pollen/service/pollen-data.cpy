       01  C-POLLEN-MAX-CODES           CONSTANT AS 10.
       01  POLLEN-GRP.
           05  POLLEN-DATE-MAJ          PIC X(24) VALUE SPACES.
           05  POLLEN-RESPONSIBLE       PIC X(64) VALUE SPACES.
           05  POLLEN-CODE-COUNT        PIC 9(2) VALUE 0.
           *> Can't use the C-POLLEN-MAX-CODES constant in the OCCURS
           *> clause. :/ Still good to have the constant to reference it
           *> elsewhere.
           05  POLLEN-CODES OCCURS 1 TO 10 TIMES
               DEPENDING ON POLLEN-CODE-COUNT
               INDEXED BY POLLEN-CODE-INDEX.
               10  POLLEN-CODE-NAME       PIC X(16).
               10  POLLEN-CODE-VALUE      PIC 9(1).
