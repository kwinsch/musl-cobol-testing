       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CONDITIONALS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1     PIC 9(3) VALUE 100.
       01 NUM2     PIC 9(3) VALUE 50.
       01 GRADE    PIC 99 VALUE 85.

       PROCEDURE DIVISION.
           IF NUM1 > NUM2
               DISPLAY "100 > 50: TRUE"
           ELSE
               DISPLAY "100 > 50: FALSE"
           END-IF.

           IF NUM1 = NUM2
               DISPLAY "100 = 50: TRUE"
           ELSE
               DISPLAY "100 = 50: FALSE"
           END-IF.

           EVALUATE GRADE
               WHEN 90 THRU 100
                   DISPLAY "Grade: A"
               WHEN 80 THRU 89
                   DISPLAY "Grade: B"
               WHEN 70 THRU 79
                   DISPLAY "Grade: C"
               WHEN OTHER
                   DISPLAY "Grade: F"
           END-EVALUATE.

           STOP RUN.
