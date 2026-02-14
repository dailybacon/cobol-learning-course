       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 18: Real-World Batch Processing
      *> ============================================
      *> This is the capstone lesson. It combines
      *> everything you've learned into a realistic
      *> batch processing program - the kind that
      *> runs nightly on mainframes worldwide.
      *>
      *> THE PATTERN:
      *>   1. Read a transaction file
      *>   2. Validate each record
      *>   3. Apply business rules
      *>   4. Write results to output files
      *>   5. Produce a summary report
      *>   6. Handle errors gracefully
      *>
      *> CONCEPTS COMBINED:
      *>   - File I/O (lessons 10-11)
      *>   - Record validation (lesson 15)
      *>   - Report generation (lesson 14)
      *>   - Paragraphs for structure (lesson 6)
      *>   - Tables for accumulators (lesson 8)
      *>   - Conditionals and loops (lessons 4-5)
      *>
      *> SCENARIO:
      *>   Process daily bank transactions.
      *>   Read transactions, validate them, apply
      *>   them, and produce an end-of-day report.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x batch.cob -o batch
      *>   ./batch
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE
               ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.

           SELECT VALID-FILE
               ASSIGN TO "valid-trans.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-VALID-STATUS.

           SELECT ERROR-FILE
               ASSIGN TO "error-trans.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ERROR-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "daily-report.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.

      *> Input: daily transactions
       FD TRANS-FILE.
       01 TRANS-RECORD.
          05 TR-ACCT-NO      PIC X(8).
          05 FILLER          PIC X VALUE ",".
          05 TR-TYPE          PIC X.
      *>     D=Deposit, W=Withdrawal, T=Transfer
          05 FILLER          PIC X VALUE ",".
          05 TR-AMOUNT        PIC 9(7)V99.
          05 FILLER          PIC X VALUE ",".
          05 TR-DESC          PIC X(20).

      *> Output: valid transactions
       FD VALID-FILE.
       01 VALID-RECORD       PIC X(50).

      *> Output: rejected transactions
       FD ERROR-FILE.
       01 ERROR-RECORD.
          05 ER-DATA          PIC X(42).
          05 ER-REASON        PIC X(30).

      *> Output: daily report
       FD REPORT-FILE.
       01 RPT-LINE           PIC X(70).

       WORKING-STORAGE SECTION.
      *> File statuses
       01 WS-TRANS-STATUS    PIC XX.
       01 WS-VALID-STATUS    PIC XX.
       01 WS-ERROR-STATUS    PIC XX.
       01 WS-RPT-STATUS      PIC XX.

       01 WS-EOF             PIC X VALUE "N".
          88 END-OF-FILE     VALUE "Y".
          88 NOT-EOF         VALUE "N".

      *> Counters and accumulators
       01 WS-TOTAL-READ      PIC 9(5) VALUE 0.
       01 WS-TOTAL-VALID     PIC 9(5) VALUE 0.
       01 WS-TOTAL-ERROR     PIC 9(5) VALUE 0.
       01 WS-TOTAL-DEPOSITS  PIC 9(9)V99 VALUE 0.
       01 WS-TOTAL-WITHDRAWALS PIC 9(9)V99 VALUE 0.
       01 WS-TOTAL-TRANSFERS PIC 9(9)V99 VALUE 0.
       01 WS-DEP-COUNT       PIC 9(5) VALUE 0.
       01 WS-WDR-COUNT       PIC 9(5) VALUE 0.
       01 WS-TFR-COUNT       PIC 9(5) VALUE 0.

      *> Validation
       01 WS-VALID-FLAG      PIC X VALUE "Y".
          88 RECORD-VALID    VALUE "Y".
          88 RECORD-INVALID  VALUE "N".
       01 WS-ERROR-MSG       PIC X(30).

      *> Display fields
       01 WS-DISP-AMT        PIC $Z(6),ZZ9.99.
       01 WS-DISP-CNT        PIC Z(4)9.
       01 WS-RPT-DETAIL      PIC X(70).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-TRANSACTIONS.
           PERFORM 3000-WRITE-REPORT.
           PERFORM 4000-CLEANUP.
           STOP RUN.

      *> --- Setup: create sample data and open files ---
       1000-INITIALIZE.
           DISPLAY "=== Daily Batch Processing ===".
           DISPLAY "Creating sample transactions...".
           PERFORM 1100-CREATE-SAMPLE-DATA.

           OPEN INPUT  TRANS-FILE.
           OPEN OUTPUT VALID-FILE.
           OPEN OUTPUT ERROR-FILE.
           OPEN OUTPUT REPORT-FILE.

           IF WS-TRANS-STATUS NOT = "00"
               DISPLAY "FATAL: Cannot open transaction "
                   "file: " WS-TRANS-STATUS
               STOP RUN
           END-IF.

           DISPLAY "Processing started.".

      *> --- Create test transaction file ---
       1100-CREATE-SAMPLE-DATA.
           OPEN OUTPUT TRANS-FILE.

           MOVE "ACCT0001,D,000500000,Payroll deposit"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "ACCT0002,W,000015099,ATM withdrawal"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "ACCT0003,T,000250000,Wire transfer"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "ACCT0004,D,000075000,Check deposit"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "BADACCT!,D,000100000,Bad account num"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "ACCT0005,X,000050000,Invalid type"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "ACCT0006,W,999999999,Amount too large"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           MOVE "ACCT0007,D,000000000,Zero amount"
               TO TRANS-RECORD.
           WRITE TRANS-RECORD.

           CLOSE TRANS-FILE.

      *> --- Main processing loop ---
       2000-PROCESS-TRANSACTIONS.
           SET NOT-EOF TO TRUE.

           READ TRANS-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-TOTAL-READ
               PERFORM 2100-VALIDATE-RECORD
               IF RECORD-VALID
                   PERFORM 2200-PROCESS-VALID
               ELSE
                   PERFORM 2300-PROCESS-ERROR
               END-IF
               READ TRANS-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

      *> --- Validate a transaction record ---
       2100-VALIDATE-RECORD.
           SET RECORD-VALID TO TRUE.
           MOVE SPACES TO WS-ERROR-MSG.

      *>   Check account number format (must be
      *>   letters/digits only, start with ACCT)
           IF TR-ACCT-NO(1:4) NOT = "ACCT"
               SET RECORD-INVALID TO TRUE
               MOVE "Invalid account number"
                   TO WS-ERROR-MSG
           END-IF.

      *>   Check transaction type
           IF RECORD-VALID
               IF TR-TYPE NOT = "D" AND
                  TR-TYPE NOT = "W" AND
                  TR-TYPE NOT = "T"
                   SET RECORD-INVALID TO TRUE
                   MOVE "Invalid transaction type"
                       TO WS-ERROR-MSG
               END-IF
           END-IF.

      *>   Check amount (must be > 0 and < 100000.00)
           IF RECORD-VALID
               IF TR-AMOUNT = 0
                   SET RECORD-INVALID TO TRUE
                   MOVE "Zero amount" TO WS-ERROR-MSG
               END-IF
               IF TR-AMOUNT > 9999999
                   SET RECORD-INVALID TO TRUE
                   MOVE "Amount exceeds limit"
                       TO WS-ERROR-MSG
               END-IF
           END-IF.

      *> --- Process valid transaction ---
       2200-PROCESS-VALID.
           ADD 1 TO WS-TOTAL-VALID.
           MOVE TRANS-RECORD TO VALID-RECORD.
           WRITE VALID-RECORD.

           EVALUATE TR-TYPE
               WHEN "D"
                   ADD TR-AMOUNT TO WS-TOTAL-DEPOSITS
                   ADD 1 TO WS-DEP-COUNT
               WHEN "W"
                   ADD TR-AMOUNT TO WS-TOTAL-WITHDRAWALS
                   ADD 1 TO WS-WDR-COUNT
               WHEN "T"
                   ADD TR-AMOUNT TO WS-TOTAL-TRANSFERS
                   ADD 1 TO WS-TFR-COUNT
           END-EVALUATE.

      *> --- Process error transaction ---
       2300-PROCESS-ERROR.
           ADD 1 TO WS-TOTAL-ERROR.
           MOVE TRANS-RECORD TO ER-DATA.
           MOVE WS-ERROR-MSG TO ER-REASON.
           WRITE ERROR-RECORD.

      *> --- Generate end-of-day report ---
       3000-WRITE-REPORT.
           MOVE "=================================="
               & "==================================="
               TO RPT-LINE.
           WRITE RPT-LINE.

           MOVE "       DAILY TRANSACTION PROCESSING"
               & " REPORT" TO RPT-LINE.
           WRITE RPT-LINE.

           MOVE "=================================="
               & "==================================="
               TO RPT-LINE.
           WRITE RPT-LINE.

           MOVE SPACES TO RPT-LINE.
           WRITE RPT-LINE.

      *>   Summary counts
           MOVE "PROCESSING SUMMARY:" TO RPT-LINE.
           WRITE RPT-LINE.

           MOVE WS-TOTAL-READ TO WS-DISP-CNT.
           STRING "  Records read:     " WS-DISP-CNT
               DELIMITED BY SIZE INTO WS-RPT-DETAIL
           END-STRING.
           MOVE WS-RPT-DETAIL TO RPT-LINE.
           WRITE RPT-LINE.
           MOVE SPACES TO WS-RPT-DETAIL.

           MOVE WS-TOTAL-VALID TO WS-DISP-CNT.
           STRING "  Records accepted: " WS-DISP-CNT
               DELIMITED BY SIZE INTO WS-RPT-DETAIL
           END-STRING.
           MOVE WS-RPT-DETAIL TO RPT-LINE.
           WRITE RPT-LINE.
           MOVE SPACES TO WS-RPT-DETAIL.

           MOVE WS-TOTAL-ERROR TO WS-DISP-CNT.
           STRING "  Records rejected: " WS-DISP-CNT
               DELIMITED BY SIZE INTO WS-RPT-DETAIL
           END-STRING.
           MOVE WS-RPT-DETAIL TO RPT-LINE.
           WRITE RPT-LINE.
           MOVE SPACES TO WS-RPT-DETAIL.

           MOVE SPACES TO RPT-LINE.
           WRITE RPT-LINE.

      *>   Transaction type breakdown
           MOVE "TRANSACTION BREAKDOWN:" TO RPT-LINE.
           WRITE RPT-LINE.

           MOVE WS-DEP-COUNT TO WS-DISP-CNT.
           MOVE WS-TOTAL-DEPOSITS TO WS-DISP-AMT.
           STRING "  Deposits:    " WS-DISP-CNT
                  "  Total: " WS-DISP-AMT
               DELIMITED BY SIZE INTO WS-RPT-DETAIL
           END-STRING.
           MOVE WS-RPT-DETAIL TO RPT-LINE.
           WRITE RPT-LINE.
           MOVE SPACES TO WS-RPT-DETAIL.

           MOVE WS-WDR-COUNT TO WS-DISP-CNT.
           MOVE WS-TOTAL-WITHDRAWALS TO WS-DISP-AMT.
           STRING "  Withdrawals: " WS-DISP-CNT
                  "  Total: " WS-DISP-AMT
               DELIMITED BY SIZE INTO WS-RPT-DETAIL
           END-STRING.
           MOVE WS-RPT-DETAIL TO RPT-LINE.
           WRITE RPT-LINE.
           MOVE SPACES TO WS-RPT-DETAIL.

           MOVE WS-TFR-COUNT TO WS-DISP-CNT.
           MOVE WS-TOTAL-TRANSFERS TO WS-DISP-AMT.
           STRING "  Transfers:   " WS-DISP-CNT
                  "  Total: " WS-DISP-AMT
               DELIMITED BY SIZE INTO WS-RPT-DETAIL
           END-STRING.
           MOVE WS-RPT-DETAIL TO RPT-LINE.
           WRITE RPT-LINE.
           MOVE SPACES TO WS-RPT-DETAIL.

           MOVE "=================================="
               & "==================================="
               TO RPT-LINE.
           WRITE RPT-LINE.

      *> --- Close everything ---
       4000-CLEANUP.
           CLOSE TRANS-FILE.
           CLOSE VALID-FILE.
           CLOSE ERROR-FILE.
           CLOSE REPORT-FILE.

           DISPLAY SPACES.
           DISPLAY "Processing complete!".
           MOVE WS-TOTAL-READ TO WS-DISP-CNT.
           DISPLAY "  Read:     " WS-DISP-CNT.
           MOVE WS-TOTAL-VALID TO WS-DISP-CNT.
           DISPLAY "  Accepted: " WS-DISP-CNT.
           MOVE WS-TOTAL-ERROR TO WS-DISP-CNT.
           DISPLAY "  Rejected: " WS-DISP-CNT.
           DISPLAY SPACES.
           DISPLAY "See daily-report.txt for full report.".
           DISPLAY "See error-trans.dat for rejected records.".
