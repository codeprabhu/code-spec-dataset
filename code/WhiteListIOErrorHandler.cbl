      *> =========================================================
      *> --- Whitelist-Read ---
      *> Read and parse the whitelist.json file into memory.
      *> =========================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Whitelist-Read.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  WHITELIST-FILENAME        PIC X(64)
               VALUE "whitelist.json".
           01  EMPTY-UUID                PIC X(16)
               VALUE X"00000000000000000000000000000000".

      *> Shared whitelist data
           COPY DD-WHITELIST.

      *> External buffer shared with write module
           01  WHITELIST-BUFFER          PIC X(64000) EXTERNAL.
           01  WHITELIST-BUFFER-LEN      BINARY-LONG UNSIGNED.
           01  WHITELIST-BUFFER-POS      BINARY-LONG UNSIGNED.
           01  FLAG                      BINARY-CHAR UNSIGNED.
           01  OBJECT-KEY                PIC X(255).
           01  TEMP-UUID-STR             PIC X(36).
           01  TEMP-UUID                 PIC X(16).
           01  TEMP-PLAYER-NAME          PIC X(16).

       LINKAGE SECTION.
           01  LK-FAILURE                BINARY-CHAR UNSIGNED.

       PROCEDURE DIVISION USING LK-FAILURE.
           MOVE 0 TO LK-FAILURE
           MOVE 0 TO WHITELIST-LENGTH

           CALL "Files-ReadAll"
               USING WHITELIST-FILENAME WHITELIST-BUFFER
                     WHITELIST-BUFFER-LEN FLAG

           IF FLAG NOT = 0
      *> Could not read file — create new whitelist
               CALL "Whitelist-Write" USING LK-FAILURE
               GOBACK
           END-IF

           MOVE 1 TO WHITELIST-BUFFER-POS

      *> Start parsing JSON array
           CALL "JsonParse-ArrayStart"
               USING WHITELIST-BUFFER WHITELIST-BUFFER-POS FLAG
           IF FLAG NOT = 0
               MOVE 1 TO LK-FAILURE
               GOBACK
           END-IF

           PERFORM UNTIL EXIT
               CALL "JsonParse-ObjectStart"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS FLAG
               IF FLAG NOT = 0
                   IF WHITELIST-LENGTH > 0
                       MOVE 1 TO LK-FAILURE
                       GOBACK
                   END-IF
                   EXIT PERFORM
               END-IF

               MOVE EMPTY-UUID TO TEMP-UUID
               MOVE SPACES TO TEMP-PLAYER-NAME

               PERFORM UNTIL EXIT
                   CALL "JsonParse-ObjectKey"
                       USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
                             FLAG OBJECT-KEY
                   IF FLAG NOT = 0
                       MOVE 1 TO LK-FAILURE
                       GOBACK
                   END-IF

                   EVALUATE OBJECT-KEY
                       WHEN "uuid"
                           CALL "JsonParse-String"
                               USING WHITELIST-BUFFER
                                     WHITELIST-BUFFER-POS FLAG TEMP-UUID-STR
                           CALL "UUID-FromString"
                               USING TEMP-UUID-STR TEMP-UUID
                       WHEN "name"
                           CALL "JsonParse-String"
                               USING WHITELIST-BUFFER
                                     WHITELIST-BUFFER-POS FLAG TEMP-PLAYER-NAME
                       WHEN OTHER
                           CALL "JsonParse-SkipValue"
                               USING WHITELIST-BUFFER
                                     WHITELIST-BUFFER-POS FLAG
                   END-EVALUATE

                   IF FLAG NOT = 0
                       MOVE 1 TO LK-FAILURE
                       GOBACK
                   END-IF

                   CALL "JsonParse-Comma"
                       USING WHITELIST-BUFFER WHITELIST-BUFFER-POS FLAG
                   IF FLAG NOT = 0
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               CALL "JsonParse-ObjectEnd"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS FLAG
               IF FLAG NOT = 0
                   MOVE 1 TO LK-FAILURE
                   EXIT PERFORM
               END-IF

               IF TEMP-UUID NOT = EMPTY-UUID
                  AND TEMP-PLAYER-NAME NOT = SPACES
                   ADD 1 TO WHITELIST-LENGTH
                   MOVE TEMP-UUID TO WHITELIST-UUID(WHITELIST-LENGTH)
                   MOVE TEMP-PLAYER-NAME
                        TO WHITELIST-NAME(WHITELIST-LENGTH)
               END-IF

               CALL "JsonParse-Comma"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS FLAG
               IF FLAG NOT = 0
                   EXIT PERFORM
               END-IF
           END-PERFORM

           CALL "JsonParse-ArrayEnd"
               USING WHITELIST-BUFFER WHITELIST-BUFFER-POS FLAG
           IF FLAG NOT = 0
               MOVE 1 TO LK-FAILURE
           END-IF

           GOBACK.
       END PROGRAM Whitelist-Read.

      *> =========================================================
      *> --- Whitelist-Write ---
      *> Write the in-memory whitelist to whitelist.json.
      *> =========================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Whitelist-Write.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  WHITELIST-FILENAME        PIC X(64)
               VALUE "whitelist.json".

           COPY DD-WHITELIST.

           01  WHITELIST-BUFFER          PIC X(64000) EXTERNAL.
           01  WHITELIST-BUFFER-POS      BINARY-LONG UNSIGNED.
           01  WHITELIST-BUFFER-LEN      BINARY-LONG UNSIGNED.
           01  WHITELIST-INDEX           BINARY-LONG UNSIGNED.
           01  TEMP-STR                  PIC X(36).
           01  TEMP-STR-LEN              BINARY-LONG UNSIGNED.

       LINKAGE SECTION.
           01  LK-FAILURE                BINARY-CHAR UNSIGNED.

       PROCEDURE DIVISION USING LK-FAILURE.
           MOVE 0 TO LK-FAILURE
           MOVE 1 TO WHITELIST-BUFFER-POS

           CALL "JsonEncode-ArrayStart"
               USING WHITELIST-BUFFER WHITELIST-BUFFER-POS

           PERFORM VARYING WHITELIST-INDEX FROM 1 BY 1
               UNTIL WHITELIST-INDEX > WHITELIST-LENGTH

               IF WHITELIST-INDEX > 1
                   CALL "JsonEncode-Comma"
                       USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
               END-IF

               CALL "JsonEncode-ObjectStart"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS

               *> UUID field
               MOVE 4 TO TEMP-STR-LEN
               MOVE "uuid" TO TEMP-STR(1:TEMP-STR-LEN)
               CALL "JsonEncode-ObjectKey"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
                         TEMP-STR TEMP-STR-LEN

               MOVE 36 TO TEMP-STR-LEN
               CALL "UUID-ToString"
                   USING WHITELIST-UUID(WHITELIST-INDEX) TEMP-STR
               CALL "JsonEncode-String"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
                         TEMP-STR TEMP-STR-LEN

               CALL "JsonEncode-Comma"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS

               *> Name field
               MOVE 4 TO TEMP-STR-LEN
               MOVE "name" TO TEMP-STR(1:TEMP-STR-LEN)
               CALL "JsonEncode-ObjectKey"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
                         TEMP-STR TEMP-STR-LEN

               MOVE WHITELIST-NAME(WHITELIST-INDEX) TO TEMP-STR
               MOVE FUNCTION STORED-CHAR-LENGTH(TEMP-STR)
                    TO TEMP-STR-LEN
               CALL "JsonEncode-String"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
                         TEMP-STR TEMP-STR-LEN

               CALL "JsonEncode-ObjectEnd"
                   USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
           END-PERFORM

           CALL "JsonEncode-ArrayEnd"
               USING WHITELIST-BUFFER WHITELIST-BUFFER-POS
           COMPUTE WHITELIST-BUFFER-LEN = WHITELIST-BUFFER-POS - 1

           CALL "Files-WriteAll"
               USING WHITELIST-FILENAME WHITELIST-BUFFER
                     WHITELIST-BUFFER-LEN LK-FAILURE

           GOBACK.
       END PROGRAM Whitelist-Write.
