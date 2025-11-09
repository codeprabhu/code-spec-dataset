      *> =========================================================
      *> --- HTTP-HANDLER ---
      *> HTTP request parser and response generator module
      *> =========================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-HANDLER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Include HTTP request/response data structures
           COPY "http-structs.cpy".
      *> Include file handling data structures
           COPY "file-structs.cpy".

      *> General purpose variables
           01  WS-INDEX                 PIC 9(4) COMP.
           01  WS-SPACE-POS             PIC 9(4) COMP.
           01  WS-PATH-LEN              PIC 9(4) COMP.
           01  WS-RETURN-CODE           PIC 9.
           01  WS-SIZE-STR              PIC X(10).
           01  WS-CRLF                  PIC XX VALUE X"0D0A".
           01  WS-DECODED-PATH          PIC X(512).

       LINKAGE SECTION.
           01  LS-REQUEST-BUF           PIC X(8192).
           01  LS-RESPONSE-BUF          PIC X(65536).
           01  LS-RESPONSE-LEN          PIC 9(8) COMP-5.

       PROCEDURE DIVISION USING LS-REQUEST-BUF LS-RESPONSE-BUF LS-RESPONSE-LEN.

      *> =========================================================
      *> --- MAIN LOGIC ---
      *> =========================================================
       MAIN-LOGIC.
           MOVE SPACES TO REQUEST-METHOD
           MOVE SPACES TO REQUEST-PATH
           MOVE 0 TO LS-RESPONSE-LEN

      *> Find first space to extract HTTP method
           MOVE 0 TO WS-SPACE-POS
           INSPECT LS-REQUEST-BUF
               TALLYING WS-SPACE-POS FOR CHARACTERS BEFORE INITIAL SPACE

           IF WS-SPACE-POS > 0 AND WS-SPACE-POS <= 10
               MOVE LS-REQUEST-BUF(1:WS-SPACE-POS) TO REQUEST-METHOD
           END-IF

      *> Find start of path
           COMPUTE WS-INDEX = WS-SPACE-POS + 2
           MOVE 0 TO WS-PATH-LEN
           PERFORM VARYING WS-SPACE-POS FROM WS-INDEX BY 1
               UNTIL WS-SPACE-POS > 8192
               IF LS-REQUEST-BUF(WS-SPACE-POS:1) = SPACE OR
                  LS-REQUEST-BUF(WS-SPACE-POS:1) = X"0D" OR
                  LS-REQUEST-BUF(WS-SPACE-POS:1) = X"0A"
                   COMPUTE WS-PATH-LEN = WS-SPACE-POS - WS-INDEX
                   EXIT PERFORM
               END-IF
           END-PERFORM

      *> Extract path
           IF WS-PATH-LEN > 0 AND WS-PATH-LEN <= 512
               MOVE LS-REQUEST-BUF(WS-INDEX:WS-PATH-LEN)
                    TO REQUEST-PATH
           END-IF

      *> Decode URL-encoded path
           CALL "URL-DECODE" USING REQUEST-PATH WS-DECODED-PATH

      *> Sanitize and validate path
           CALL "PATH-UTILS" USING WS-DECODED-PATH SANITIZED-PATH WS-RETURN-CODE

      *> If invalid, return 403
           IF WS-RETURN-CODE NOT = 0
               PERFORM BUILD-403-RESPONSE
               GOBACK
           END-IF

      *> Read file
           CALL "FILE-OPS" USING SANITIZED-PATH FILE-BUFFER FILE-SIZE WS-RETURN-CODE

      *> Too large → 413
           IF WS-RETURN-CODE = 2
               PERFORM BUILD-413-RESPONSE
               GOBACK
           END-IF

      *> Not found → 404
           IF WS-RETURN-CODE NOT = 0
               PERFORM BUILD-404-RESPONSE
               GOBACK
           END-IF

      *> Determine MIME type
           CALL "MIME-TYPES" USING SANITIZED-PATH MIME-TYPE

      *> Build success response
           PERFORM BUILD-200-RESPONSE
           GOBACK.

      *> =========================================================
      *> --- BUILD 200 OK RESPONSE ---
      *> =========================================================
       BUILD-200-RESPONSE.
           MOVE FILE-SIZE TO WS-SIZE-STR
           MOVE LOW-VALUE TO LS-RESPONSE-BUF

           STRING "HTTP/1.1 200 OK" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: " DELIMITED BY SIZE
                  MIME-TYPE DELIMITED BY SPACE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: " DELIMITED BY SIZE
                  WS-SIZE-STR DELIMITED BY SPACE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

           MOVE 0 TO LS-RESPONSE-LEN
           INSPECT LS-RESPONSE-BUF
               TALLYING LS-RESPONSE-LEN FOR CHARACTERS BEFORE INITIAL LOW-VALUE

           IF LS-RESPONSE-LEN > 0 AND FILE-SIZE > 0
               MOVE FILE-BUFFER(1:FILE-SIZE)
                    TO LS-RESPONSE-BUF(LS-RESPONSE-LEN + 1:FILE-SIZE)
               ADD FILE-SIZE TO LS-RESPONSE-LEN
           END-IF
           .

      *> =========================================================
      *> --- BUILD 404 NOT FOUND RESPONSE ---
      *> =========================================================
       BUILD-404-RESPONSE.
           STRING "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 47" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>404 Not Found</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

           INSPECT LS-RESPONSE-BUF
               TALLYING LS-RESPONSE-LEN FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .

      *> =========================================================
      *> --- BUILD 403 FORBIDDEN RESPONSE ---
      *> =========================================================
       BUILD-403-RESPONSE.
           STRING "HTTP/1.1 403 Forbidden" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 47" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>403 Forbidden</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

           INSPECT LS-RESPONSE-BUF
               TALLYING LS-RESPONSE-LEN FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .

      *> =========================================================
      *> --- BUILD 413 PAYLOAD TOO LARGE RESPONSE ---
      *> =========================================================
       BUILD-413-RESPONSE.
           STRING "HTTP/1.1 413 Payload Too Large" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Type: text/html" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "Content-Length: 59" DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  WS-CRLF DELIMITED BY SIZE
                  "<html><body><h1>413 Payload Too Large</h1></body></html>"
                      DELIMITED BY SIZE
                  INTO LS-RESPONSE-BUF
           END-STRING

           INSPECT LS-RESPONSE-BUF
               TALLYING LS-RESPONSE-LEN FOR CHARACTERS BEFORE INITIAL LOW-VALUE
           .
