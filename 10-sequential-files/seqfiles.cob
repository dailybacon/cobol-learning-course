       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQFILES.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 10: Sequential File I/O
      *> ============================================
      *> This is the heart of COBOL. Most business
      *> programs read records from files, process
      *> them, and write results to other files.
      *>
      *> NEW CONCEPTS:
      *>   - ENVIRONMENT DIVISION:
      *>     INPUT-OUTPUT SECTION / FILE-CONTROL
      *>     Maps logical file names to physical files.
      *>
      *>     SELECT file-name ASSIGN TO "path"
      *>       ORGANIZATION IS LINE SEQUENTIAL.
      *>
      *>   - FILE SECTION (in DATA DIVISION):
      *>     FD (File Description) defines the record
      *>     layout for each file.
      *>
      *>   - File operations:
      *>     OPEN INPUT file   (read only)
      *>     OPEN OUTPUT file  (write, creates new)
      *>     OPEN EXTEND file  (append)
      *>     READ file INTO var
      *>       AT END action
      *>     END-READ
      *>     WRITE record FROM var
      *>     CLOSE file
      *>
      *>   - The READ loop pattern:
      *>     Read first record before the loop.
      *>     Process + read next inside the loop.
      *>     (This is the standard COBOL pattern.)
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x seqfiles.cob -o seqfiles
      *>   ./seqfiles
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *> Output file: we'll write employee data here
           SELECT EMPLOYEE-FILE
               ASSIGN TO "employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

      *> Input file: we'll read it back
           SELECT REPORT-FILE
               ASSIGN TO "report.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      *> Record layout for employee file
       FD EMPLOYEE-FILE.
       01 EMP-RECORD.
          05 EMP-ID          PIC X(4).
          05 FILLER          PIC X VALUE ",".
          05 EMP-NAME        PIC X(20).
          05 FILLER          PIC X VALUE ",".
          05 EMP-SALARY      PIC 9(6)V99.

      *> Record layout for report file
       FD REPORT-FILE.
       01 REPORT-RECORD      PIC X(60).

       WORKING-STORAGE SECTION.
       01 WS-EOF             PIC X VALUE "N".
          88 END-OF-FILE     VALUE "Y".
          88 NOT-END-OF-FILE VALUE "N".

       01 WS-EMP-COUNT       PIC 9(3) VALUE 0.
       01 WS-TOTAL-SAL       PIC 9(8)V99 VALUE 0.
       01 WS-DISPLAY-SAL     PIC $Z(5)9.99.
       01 WS-DISPLAY-CNT     PIC Z(2)9.
       01 WS-REPORT-LINE     PIC X(60).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM 1000-CREATE-FILE.
           PERFORM 2000-READ-AND-REPORT.
           STOP RUN.

      *> --- Write sample employee records ---
       1000-CREATE-FILE.
           OPEN OUTPUT EMPLOYEE-FILE.

           MOVE "E001" TO EMP-ID.
           MOVE "Alice Johnson"  TO EMP-NAME.
           MOVE 65000.00 TO EMP-SALARY.
           WRITE EMP-RECORD.

           MOVE "E002" TO EMP-ID.
           MOVE "Bob Williams"   TO EMP-NAME.
           MOVE 72000.00 TO EMP-SALARY.
           WRITE EMP-RECORD.

           MOVE "E003" TO EMP-ID.
           MOVE "Carol Davis"    TO EMP-NAME.
           MOVE 58000.00 TO EMP-SALARY.
           WRITE EMP-RECORD.

           MOVE "E004" TO EMP-ID.
           MOVE "Dan Miller"     TO EMP-NAME.
           MOVE 81000.00 TO EMP-SALARY.
           WRITE EMP-RECORD.

           CLOSE EMPLOYEE-FILE.
           DISPLAY "Created employees.dat with 4 records.".

      *> --- Read the file and produce a report ---
       2000-READ-AND-REPORT.
           OPEN INPUT EMPLOYEE-FILE.
           OPEN OUTPUT REPORT-FILE.

      *>   Write report header
           MOVE "=== EMPLOYEE SALARY REPORT ==="
               TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           MOVE "ID   Name                 Salary"
               TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           MOVE "---- -------------------- ----------"
               TO REPORT-RECORD.
           WRITE REPORT-RECORD.

      *>   Standard COBOL read loop:
      *>   Read first, then loop until EOF.
           SET NOT-END-OF-FILE TO TRUE.
           READ EMPLOYEE-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-EMP-COUNT
               ADD EMP-SALARY TO WS-TOTAL-SAL
               MOVE EMP-SALARY TO WS-DISPLAY-SAL
               STRING EMP-ID "  "
                      EMP-NAME
                      WS-DISPLAY-SAL
                   DELIMITED BY SIZE
                   INTO WS-REPORT-LINE
               END-STRING
               MOVE WS-REPORT-LINE TO REPORT-RECORD
               WRITE REPORT-RECORD
               MOVE SPACES TO WS-REPORT-LINE

               READ EMPLOYEE-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

      *>   Write summary
           MOVE SPACES TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           MOVE WS-TOTAL-SAL TO WS-DISPLAY-SAL.
           MOVE WS-EMP-COUNT TO WS-DISPLAY-CNT.
           STRING "Total employees: " WS-DISPLAY-CNT
               DELIMITED BY SIZE INTO WS-REPORT-LINE
           END-STRING.
           MOVE WS-REPORT-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           MOVE SPACES TO WS-REPORT-LINE.
           STRING "Total salary:    " WS-DISPLAY-SAL
               DELIMITED BY SIZE INTO WS-REPORT-LINE
           END-STRING.
           MOVE WS-REPORT-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           CLOSE EMPLOYEE-FILE.
           CLOSE REPORT-FILE.

           DISPLAY "Report written to report.txt".
           DISPLAY SPACES.

      *>   Show the report on screen too
           DISPLAY "=== Report Contents ===".
           OPEN INPUT REPORT-FILE.
           SET NOT-END-OF-FILE TO TRUE.
           READ REPORT-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.
           PERFORM UNTIL END-OF-FILE
               DISPLAY REPORT-RECORD
               READ REPORT-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.
           CLOSE REPORT-FILE.
