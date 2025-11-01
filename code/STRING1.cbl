      ***********************************************************
      * Program name:    STRING1
      * Description:
      *   Demonstrates STRING and UNSTRING operations in COBOL.
      ***********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME    PIC X(10) VALUE "Alice".
       01 LAST-NAME     PIC X(10) VALUE "Johnson".
       01 FULL-NAME     PIC X(25).
       01 ADDRESS-LINE  PIC X(40) VALUE "42 Wallaby Way, Sydney".
       01 CITY          PIC X(15).
       01 STREET        PIC X(25).

       PROCEDURE DIVISION.
       0000-Main.
           STRING FIRST-NAME DELIMITED BY SPACE
                  " " DELIMITED BY SIZE
                  LAST-NAME DELIMITED BY SIZE
                  INTO FULL-NAME
           END-STRING.

           DISPLAY "Full Name: " FULL-NAME.

           UNSTRING ADDRESS-LINE
               DELIMITED BY ","
               INTO STREET, CITY
           END-UNSTRING.

           DISPLAY "Street: " STREET.
           DISPLAY "City: " CITY.
           STOP RUN.
