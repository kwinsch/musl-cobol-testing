       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-STRINGS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME   PIC X(10) VALUE "John".
       01 LAST-NAME    PIC X(10) VALUE "Doe".
       01 FULL-NAME    PIC X(25).
       01 UPPER-NAME   PIC X(25).

       PROCEDURE DIVISION.
           STRING FIRST-NAME DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  LAST-NAME DELIMITED BY SPACE
                  INTO FULL-NAME
           END-STRING.
           DISPLAY "Full name: " FULL-NAME.

           MOVE FULL-NAME TO UPPER-NAME.
           INSPECT UPPER-NAME CONVERTING
               "abcdefghijklmnopqrstuvwxyz" TO
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           DISPLAY "Uppercase: " UPPER-NAME.

           STOP RUN.
