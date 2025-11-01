      ***********************************************************
      * Program name:    ARITH1
      * Description:
      *   Demonstrates arithmetic operations in COBOL.
      ***********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARITH1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(3) VALUE 25.
       01 NUM2 PIC 9(3) VALUE 5.
       01 RESULT PIC 9(4)V9(2) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-Main.
           ADD NUM1 TO NUM2 GIVING RESULT.
           DISPLAY "ADD Result: " RESULT.

           SUBTRACT NUM2 FROM NUM1 GIVING RESULT.
           DISPLAY "SUBTRACT Result: " RESULT.

           MULTIPLY NUM1 BY NUM2 GIVING RESULT.
           DISPLAY "MULTIPLY Result: " RESULT.

           DIVIDE NUM1 BY NUM2 GIVING RESULT.
           DISPLAY "DIVIDE Result: " RESULT.

           COMPUTE RESULT = (NUM1 ** 2 + NUM2 ** 2) / 2.
           DISPLAY "COMPUTE Result: " RESULT.
           STOP RUN.
