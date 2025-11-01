       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-DEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "students.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUDENT-ID     PIC 9(3).
           05 STUDENT-NAME   PIC A(20).
           05 STUDENT-MARKS  PIC 999.

       WORKING-STORAGE SECTION.
       01 WS-OPTION         PIC 9 VALUE 0.
       01 WS-MORE           PIC A VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM UNTIL WS-OPTION = 3
               DISPLAY "1. Write to file"
               DISPLAY "2. Read from file"
               DISPLAY "3. Exit"
               DISPLAY "Enter your choice: " WITH NO ADVANCING
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                   WHEN 1
                       PERFORM WRITE-FILE
                   WHEN 2
                       PERFORM READ-FILE
                   WHEN 3
                       DISPLAY "Exiting program..."
                   WHEN OTHER
                       DISPLAY "Invalid choice!"
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       WRITE-FILE.
           OPEN OUTPUT STUDENT-FILE
           PERFORM UNTIL WS-MORE NOT = "Y"
               DISPLAY "Enter Student ID (3 digits): " WITH NO ADVANCING
               ACCEPT STUDENT-ID
               DISPLAY "Enter Student Name: " WITH NO ADVANCING
               ACCEPT STUDENT-NAME
               DISPLAY "Enter Marks (out of 100): " WITH NO ADVANCING
               ACCEPT STUDENT-MARKS
               WRITE STUDENT-RECORD
               DISPLAY "Add another record? (Y/N): " WITH NO ADVANCING
               ACCEPT WS-MORE
           END-PERFORM
           CLOSE STUDENT-FILE
           DISPLAY "Records written successfully!".

       READ-FILE.
           OPEN INPUT STUDENT-FILE
           DISPLAY "Reading records from file..."
           PERFORM UNTIL EOF
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO EOF
                   NOT AT END
                       DISPLAY "ID: " STUDENT-ID
                       DISPLAY "Name: " STUDENT-NAME
                       DISPLAY "Marks: " STUDENT-MARKS
                       DISPLAY "-----------------------------"
               END-READ
           END-PERFORM
           CLOSE STUDENT-FILE
           DISPLAY "End of file reached.".

       WORKING-STORAGE SECTION.
       01 EOF PIC A VALUE "N".
