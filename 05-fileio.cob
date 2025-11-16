       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-FILEIO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-FILE ASSIGN TO "test-data.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TEST-FILE.
       01 FILE-RECORD PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-RECORD PIC X(50).
       01 EOF-FLAG  PIC X VALUE "N".

       PROCEDURE DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "Line 1: Testing file I/O" TO FILE-RECORD.
           WRITE FILE-RECORD.
           MOVE "Line 2: Static linking works!" TO FILE-RECORD.
           WRITE FILE-RECORD.
           CLOSE TEST-FILE.
           DISPLAY "Wrote 2 lines".

           OPEN INPUT TEST-FILE.
           PERFORM UNTIL EOF-FLAG = "Y"
               READ TEST-FILE INTO WS-RECORD
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END DISPLAY "Read: " WS-RECORD
               END-READ
           END-PERFORM.
           CLOSE TEST-FILE.

           STOP RUN.
