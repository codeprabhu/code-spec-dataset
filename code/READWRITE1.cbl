      ***********************************************************
      * Program name:    READWRITE1
      * Original author: ChatGPT (Educational Demo)
      *
      * Description:
      *   Demonstrates basic file reading and writing.
      *   Reads names from an input file, converts to uppercase,
      *   and writes them to an output file.
      ***********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READWRITE1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "names-in.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "names-out.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 IN-RECORD PIC X(30).

       FD OUTFILE.
       01 OUT-RECORD PIC X(30).

       WORKING-STORAGE SECTION.
       01 WS-EOF-FLAG PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       0000-Main.
           OPEN INPUT INFILE
                OUTPUT OUTFILE.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
              READ INFILE
                 AT END MOVE 'Y' TO WS-EOF-FLAG
                 NOT AT END
                    MOVE FUNCTION UPPER-CASE(IN-RECORD)
                       TO OUT-RECORD
                    WRITE OUT-RECORD
              END-READ
           END-PERFORM.
           CLOSE INFILE OUTFILE.
           DISPLAY "Processing complete.".
           STOP RUN.
