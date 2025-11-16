       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIXED-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FILENAME        PIC X(20) VALUE "sales-data.txt".
       01 OUTPUT-FILE     PIC X(20) VALUE "processed.txt".
       01 LINE-BUFFER     PIC X(80).
       01 TOTAL-SALES     PIC 9(6)V99 VALUE 0.
       01 AMOUNT          PIC 9(4)V99.
       01 RESULT          PIC S9(4) COMP.
       01 LINE-COUNT      PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Processing sales data using C file I/O..."

           PERFORM READ-AND-PROCESS

           DISPLAY "Total sales: " TOTAL-SALES
           DISPLAY "Lines processed: " LINE-COUNT

           STOP RUN.

       READ-AND-PROCESS.
           PERFORM UNTIL RESULT = -1
               CALL "c_read_line" USING
                   BY REFERENCE FILENAME
                   BY REFERENCE LINE-BUFFER
                   BY VALUE 80
                   RETURNING RESULT

               IF RESULT NOT = -1 THEN
                   ADD 1 TO LINE-COUNT

                   MOVE LINE-BUFFER(1:7) TO AMOUNT
                   ADD AMOUNT TO TOTAL-SALES

                   CALL "c_write_line" USING
                       BY REFERENCE OUTPUT-FILE
                       BY REFERENCE LINE-BUFFER
                       BY VALUE 80
                       RETURNING RESULT
               END-IF
           END-PERFORM.
