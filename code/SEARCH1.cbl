      ***********************************************************
      * Program name:    SEARCH1
      * Description:
      *   Demonstrates COBOL's SEARCH verb with a simple table.
      *   Looks up an employee name based on ID input.
      ***********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EMPLOYEE-TABLE.
          05 EMP-ENTRY OCCURS 5 TIMES INDEXED BY IDX.
             10 EMP-ID   PIC 9(3).
             10 EMP-NAME PIC X(15).

       01 WS-TARGET-ID  PIC 9(3) VALUE 103.
       01 WS-FOUND-FLAG PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       0000-Init.
           MOVE 101 TO EMP-ID(1)
           MOVE "Alice" TO EMP-NAME(1)
           MOVE 102 TO EMP-ID(2)
           MOVE "Bob" TO EMP-NAME(2)
           MOVE 103 TO EMP-ID(3)
           MOVE "Charlie" TO EMP-NAME(3)
           MOVE 104 TO EMP-ID(4)
           MOVE "Diana" TO EMP-NAME(4)
           MOVE 105 TO EMP-ID(5)
           MOVE "Evan" TO EMP-NAME(5).

       1000-Search.
           SET IDX TO 1.
           SEARCH EMP-ENTRY
               AT END DISPLAY "Employee not found."
               WHEN EMP-ID(IDX) = WS-TARGET-ID
                   DISPLAY "Employee Found: " EMP-NAME(IDX)
                   MOVE 'Y' TO WS-FOUND-FLAG
           END-SEARCH.
           STOP RUN.
