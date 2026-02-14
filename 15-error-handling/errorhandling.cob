       IDENTIFICATION DIVISION.
       PROGRAM-ID. ERRORHANDLING.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 15: Error Handling
      *> ============================================
      *> Production COBOL programs must handle errors
      *> gracefully. Here are the key techniques.
      *>
      *> NEW CONCEPTS:
      *>   - FILE STATUS: a 2-char code set after
      *>     every file operation.
      *>     "00" = success
      *>     "10" = end of file
      *>     "22" = duplicate key
      *>     "23" = record not found
      *>     "30" = permanent I/O error
      *>     "35" = file not found (OPEN)
      *>     "39" = file attribute mismatch
      *>     "41" = file already open
      *>     "42" = file not open
      *>     "46" = read failed, no valid next
      *>     "47" = read on file not opened INPUT/I-O
      *>
      *>   - ON SIZE ERROR: catches arithmetic
      *>     overflow (result too big for variable).
      *>     COMPUTE X = A / B
      *>       ON SIZE ERROR
      *>         DISPLAY "Overflow!"
      *>       NOT ON SIZE ERROR
      *>         DISPLAY "Result: " X
      *>     END-COMPUTE
      *>
      *>   - INVALID KEY: catches key errors on
      *>     indexed file operations.
      *>
      *>   - AT END: detects end of file during READ.
      *>
      *>   - DECLARATIVES: special error-handling
      *>     sections that run automatically when
      *>     file errors occur.
      *>     DECLARATIVES.
      *>     sect-name SECTION.
      *>       USE AFTER ERROR PROCEDURE ON file.
      *>     para-name.
      *>       ... error handling code ...
      *>     END DECLARATIVES.
      *>
      *>   - Best practices:
      *>     * Always check FILE STATUS after I/O.
      *>     * Always use ON SIZE ERROR for division.
      *>     * Log errors with enough context to debug.
      *>     * Have a clear error recovery strategy.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x errorhandling.cob -o errorhandling
      *>   ./errorhandling
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-FILE
               ASSIGN TO "testfile.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD TEST-FILE.
       01 TEST-RECORD       PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS    PIC XX.
       01 WS-NUM1           PIC 9(3).
       01 WS-NUM2           PIC 9(3).
       01 WS-RESULT         PIC 9(3).
       01 WS-BIG-RESULT     PIC 9(6)V99.
       01 WS-DISP           PIC Z(5)9.99.

       01 WS-STATUS-MSG     PIC X(40).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM 1000-FILE-STATUS-DEMO.
           PERFORM 2000-SIZE-ERROR-DEMO.
           PERFORM 3000-ROBUST-FILE-READ.
           STOP RUN.

      *> --- Demo: FILE STATUS checking ---
       1000-FILE-STATUS-DEMO.
           DISPLAY "=== FILE STATUS Demo ===".

      *>   Try to open a file that doesn't exist
           DISPLAY "Opening nonexistent file...".
           OPEN INPUT TEST-FILE.
           PERFORM 9000-CHECK-FILE-STATUS.

      *>   Create the file so next open works
           DISPLAY "Creating test file...".
           OPEN OUTPUT TEST-FILE.
           PERFORM 9000-CHECK-FILE-STATUS.

           MOVE "Record one"   TO TEST-RECORD.
           WRITE TEST-RECORD.
           MOVE "Record two"   TO TEST-RECORD.
           WRITE TEST-RECORD.
           MOVE "Record three" TO TEST-RECORD.
           WRITE TEST-RECORD.
           CLOSE TEST-FILE.
           DISPLAY "File created with 3 records.".
           DISPLAY SPACES.

      *> --- Demo: ON SIZE ERROR ---
       2000-SIZE-ERROR-DEMO.
           DISPLAY "=== ON SIZE ERROR Demo ===".

      *>   Normal arithmetic - fits in PIC 9(3)
           MOVE 50 TO WS-NUM1.
           MOVE 30 TO WS-NUM2.
           COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2
               ON SIZE ERROR
                   DISPLAY "OVERFLOW on " WS-NUM1
                       " + " WS-NUM2 "!"
               NOT ON SIZE ERROR
                   DISPLAY WS-NUM1 " + " WS-NUM2
                       " = " WS-RESULT
           END-COMPUTE.

      *>   Overflow - too big for PIC 9(3) (max 999)
           MOVE 600 TO WS-NUM1.
           MOVE 500 TO WS-NUM2.
           COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2
               ON SIZE ERROR
                   DISPLAY WS-NUM1 " + " WS-NUM2
                       " = OVERFLOW! (max 999)"
               NOT ON SIZE ERROR
                   DISPLAY WS-NUM1 " + " WS-NUM2
                       " = " WS-RESULT
           END-COMPUTE.

      *>   Division by zero
           MOVE 100 TO WS-NUM1.
           MOVE 0   TO WS-NUM2.
           COMPUTE WS-BIG-RESULT = WS-NUM1 / WS-NUM2
               ON SIZE ERROR
                   DISPLAY WS-NUM1 " / " WS-NUM2
                       " = DIVISION BY ZERO!"
               NOT ON SIZE ERROR
                   MOVE WS-BIG-RESULT TO WS-DISP
                   DISPLAY WS-NUM1 " / " WS-NUM2
                       " = " WS-DISP
           END-COMPUTE.
           DISPLAY SPACES.

      *> --- Demo: Robust file reading ---
       3000-ROBUST-FILE-READ.
           DISPLAY "=== Robust File Reading ===".

           OPEN INPUT TEST-FILE.
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Cannot open file: " WS-FILE-STATUS
               GO TO 3000-EXIT
           END-IF.

           PERFORM UNTIL WS-FILE-STATUS NOT = "00"
               READ TEST-FILE
               EVALUATE WS-FILE-STATUS
                   WHEN "00"
                       DISPLAY "  Read: " TEST-RECORD
                   WHEN "10"
                       DISPLAY "  (End of file reached)"
                   WHEN OTHER
                       DISPLAY "  Read error: "
                           WS-FILE-STATUS
               END-EVALUATE
           END-PERFORM.

           CLOSE TEST-FILE.
       3000-EXIT.
           DISPLAY "Done.".

      *> --- Helper: interpret FILE STATUS ---
       9000-CHECK-FILE-STATUS.
           EVALUATE WS-FILE-STATUS
               WHEN "00"
                   MOVE "Success" TO WS-STATUS-MSG
               WHEN "10"
                   MOVE "End of file" TO WS-STATUS-MSG
               WHEN "22"
                   MOVE "Duplicate key" TO WS-STATUS-MSG
               WHEN "23"
                   MOVE "Record not found"
                       TO WS-STATUS-MSG
               WHEN "35"
                   MOVE "File not found" TO WS-STATUS-MSG
               WHEN "41"
                   MOVE "File already open"
                       TO WS-STATUS-MSG
               WHEN OTHER
                   MOVE "Unknown error" TO WS-STATUS-MSG
           END-EVALUATE.
           DISPLAY "  Status " WS-FILE-STATUS ": "
               WS-STATUS-MSG.
