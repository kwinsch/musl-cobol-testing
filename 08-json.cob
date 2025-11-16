       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-DEMO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 JSON-FILE       PIC X(50) VALUE "customers.json".
       01 CUSTOMER-COUNT  PIC S9(4) COMP.
       01 RESULT          PIC S9(4) COMP.
       01 INDEX-VAR       PIC S9(4) COMP VALUE 0.

       01 CUSTOMER-DATA.
          05 CUST-NAME    PIC X(50).
          05 CUST-STATUS  PIC X(20).
          05 CUST-AGE     PIC 9(4).
          05 CUST-BALANCE PIC 9(8)V99.

       01 BALANCE-DISPLAY PIC Z,ZZZ,ZZ9.99.
       01 AGE-DISPLAY     PIC ZZ9.

       01 TOTAL-BALANCE   PIC S9(10)V99 VALUE 0.
       01 TOTAL-DISPLAY   PIC Z,ZZZ,ZZZ,ZZ9.99.

       01 ACTIVE-COUNT    PIC 9(4) VALUE 0.
       01 INACTIVE-COUNT  PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "JSON Customer Processing Demo"
           DISPLAY "=============================="
           DISPLAY " "

           CALL "json_load_file" USING
               BY REFERENCE JSON-FILE
           RETURNING CUSTOMER-COUNT

           IF CUSTOMER-COUNT = -1 THEN
               DISPLAY "Error: Could not load " JSON-FILE
               STOP RUN
           END-IF

           CALL "json_get_count"
           RETURNING CUSTOMER-COUNT

           DISPLAY "Loaded " CUSTOMER-COUNT " customers from JSON"
           DISPLAY " "

           PERFORM PROCESS-CUSTOMERS

           DISPLAY " "
           DISPLAY "Summary:"
           DISPLAY "--------"
           DISPLAY "Active customers:   " ACTIVE-COUNT
           DISPLAY "Inactive customers: " INACTIVE-COUNT

           MOVE TOTAL-BALANCE TO TOTAL-DISPLAY
           DISPLAY "Total balance:      $" TOTAL-DISPLAY

           STOP RUN.

       PROCESS-CUSTOMERS.
           PERFORM VARYING INDEX-VAR FROM 0 BY 1
               UNTIL INDEX-VAR >= CUSTOMER-COUNT

               CALL "json_get_customer" USING
                   BY VALUE INDEX-VAR
                   BY REFERENCE CUST-NAME
                   BY REFERENCE CUST-STATUS
                   BY REFERENCE CUST-AGE
                   BY REFERENCE CUST-BALANCE
                   RETURNING RESULT

               IF RESULT = 0 THEN
                   MOVE CUST-AGE TO AGE-DISPLAY
                   MOVE CUST-BALANCE TO BALANCE-DISPLAY

                   DISPLAY CUST-NAME " | Age: " AGE-DISPLAY
                       " | Balance: $" BALANCE-DISPLAY
                       " | " CUST-STATUS

                   ADD CUST-BALANCE TO TOTAL-BALANCE

                   IF CUST-STATUS(1:6) = "active" OR
                      CUST-STATUS(1:6) = "Active" THEN
                       ADD 1 TO ACTIVE-COUNT
                   ELSE
                       ADD 1 TO INACTIVE-COUNT
                   END-IF
               END-IF
           END-PERFORM.
