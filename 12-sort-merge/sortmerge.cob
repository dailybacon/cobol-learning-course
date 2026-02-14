       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTMERGE.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 12: SORT and MERGE
      *> ============================================
      *> COBOL has built-in SORT and MERGE verbs.
      *> These are used constantly in batch processing
      *> to order data before reporting or processing.
      *>
      *> NEW CONCEPTS:
      *>   - SD (Sort Description): like FD but for
      *>     a temporary sort work file.
      *>
      *>   - SORT work-file
      *>       ON ASCENDING KEY field
      *>       USING input-file
      *>       GIVING output-file
      *>     Reads input, sorts it, writes output.
      *>
      *>   - SORT with INPUT/OUTPUT PROCEDURE:
      *>     Instead of USING/GIVING, you can write
      *>     custom code to feed records in (RELEASE)
      *>     and retrieve sorted records (RETURN).
      *>
      *>     SORT work-file
      *>       ON ASCENDING KEY field
      *>       INPUT PROCEDURE IS para-1
      *>       OUTPUT PROCEDURE IS para-2
      *>
      *>     In INPUT PROCEDURE: use RELEASE record
      *>     In OUTPUT PROCEDURE: use RETURN file
      *>
      *>   - ASCENDING / DESCENDING KEY
      *>     Sort direction for each key field.
      *>     Multiple keys = sort by first, then
      *>     by second within ties, etc.
      *>
      *>   - MERGE: combines multiple sorted files
      *>     into one sorted output (like merge sort).
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x sortmerge.cob -o sortmerge
      *>   ./sortmerge
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNSORTED-FILE
               ASSIGN TO "unsorted.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORTED-FILE
               ASSIGN TO "sorted.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-WORK
               ASSIGN TO "sort-work.tmp".

       DATA DIVISION.
       FILE SECTION.

       FD UNSORTED-FILE.
       01 UNSORTED-RECORD.
          05 UR-NAME       PIC X(20).
          05 UR-DEPT        PIC X(15).
          05 UR-SALARY      PIC 9(6)V99.

       FD SORTED-FILE.
       01 SORTED-RECORD.
          05 SR-NAME       PIC X(20).
          05 SR-DEPT        PIC X(15).
          05 SR-SALARY      PIC 9(6)V99.

      *> SD = Sort Description (temporary work file)
       SD SORT-WORK.
       01 SORT-RECORD.
          05 SORT-NAME     PIC X(20).
          05 SORT-DEPT      PIC X(15).
          05 SORT-SALARY    PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01 WS-EOF           PIC X VALUE "N".
          88 END-OF-FILE   VALUE "Y".
       01 WS-DISP-SAL      PIC $Z(5)9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM 1000-CREATE-DATA.
           PERFORM 2000-SORT-BY-SALARY.
           PERFORM 3000-DISPLAY-RESULTS.
           STOP RUN.

      *> --- Create unsorted test data ---
       1000-CREATE-DATA.
           OPEN OUTPUT UNSORTED-FILE.

           MOVE "Charlie Brown"   TO UR-NAME.
           MOVE "Sales"           TO UR-DEPT.
           MOVE 55000.00          TO UR-SALARY.
           WRITE UNSORTED-RECORD.

           MOVE "Alice Johnson"   TO UR-NAME.
           MOVE "Engineering"     TO UR-DEPT.
           MOVE 85000.00          TO UR-SALARY.
           WRITE UNSORTED-RECORD.

           MOVE "Eve Williams"    TO UR-NAME.
           MOVE "Engineering"     TO UR-DEPT.
           MOVE 72000.00          TO UR-SALARY.
           WRITE UNSORTED-RECORD.

           MOVE "Bob Davis"       TO UR-NAME.
           MOVE "Sales"           TO UR-DEPT.
           MOVE 61000.00          TO UR-SALARY.
           WRITE UNSORTED-RECORD.

           MOVE "Diana Prince"    TO UR-NAME.
           MOVE "Marketing"       TO UR-DEPT.
           MOVE 68000.00          TO UR-SALARY.
           WRITE UNSORTED-RECORD.

           CLOSE UNSORTED-FILE.
           DISPLAY "Created unsorted data file.".

      *> --- Sort by department, then salary descending ---
       2000-SORT-BY-SALARY.
           SORT SORT-WORK
               ON ASCENDING KEY SORT-DEPT
               ON DESCENDING KEY SORT-SALARY
               USING UNSORTED-FILE
               GIVING SORTED-FILE.
           DISPLAY "Sorted by dept (asc), salary (desc).".

      *> --- Display sorted results ---
       3000-DISPLAY-RESULTS.
           OPEN INPUT SORTED-FILE.
           MOVE "N" TO WS-EOF.

           DISPLAY SPACES.
           DISPLAY "=== Sorted Employee List ===".
           DISPLAY "Name                 Department"
                   "      Salary".
           DISPLAY "-------------------- ----------"
                   "----- ----------".

           READ SORTED-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.

           PERFORM UNTIL END-OF-FILE
               MOVE SR-SALARY TO WS-DISP-SAL
               DISPLAY SR-NAME SR-DEPT WS-DISP-SAL
               READ SORTED-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.

           CLOSE SORTED-FILE.
