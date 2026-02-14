       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 14: Report Formatting
      *> ============================================
      *> Generating formatted reports is one of
      *> COBOL's most common tasks. This lesson
      *> covers manual report writing with headers,
      *> detail lines, and footers.
      *>
      *> NEW CONCEPTS:
      *>   - Report layout with separate record
      *>     definitions for headers, detail lines,
      *>     and footers.
      *>
      *>   - Line counters for page breaks.
      *>
      *>   - Edited picture clauses for display:
      *>     Z = suppress leading zeros
      *>     $ = floating currency symbol
      *>     , = insert comma
      *>     . = decimal point
      *>     - = trailing minus for negatives
      *>     CR/DB = credit/debit symbols
      *>     B = insert blank space
      *>     0 = insert zero
      *>     / = insert slash (great for dates)
      *>
      *>   - WRITE record AFTER ADVANCING
      *>     Controls vertical spacing in output.
      *>     AFTER ADVANCING 1 LINE = single space
      *>     AFTER ADVANCING 2 LINES = double space
      *>     AFTER ADVANCING PAGE = new page
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x reports.cob -o reports
      *>   ./reports
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE
               ASSIGN TO "payroll-report.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD REPORT-FILE.
       01 PRINT-LINE         PIC X(70).

       WORKING-STORAGE SECTION.

      *> --- Report line templates ---
       01 WS-TITLE-LINE.
          05 FILLER           PIC X(20) VALUE SPACES.
          05 FILLER           PIC X(30)
              VALUE "MONTHLY PAYROLL REPORT".

       01 WS-DATE-LINE.
          05 FILLER           PIC X(10) VALUE "Date: ".
          05 WS-RPT-DATE      PIC X(10).

       01 WS-HEADER-LINE.
          05 FILLER           PIC X(5) VALUE "ID".
          05 FILLER           PIC X(22) VALUE "Name".
          05 FILLER           PIC X(13) VALUE "Department".
          05 FILLER           PIC X(12) VALUE "Gross Pay".
          05 FILLER           PIC X(12) VALUE "Net Pay".

       01 WS-DASH-LINE.
          05 FILLER           PIC X(65)
              VALUE ALL "-".

       01 WS-DETAIL-LINE.
          05 WS-DT-ID         PIC X(5).
          05 WS-DT-NAME       PIC X(22).
          05 WS-DT-DEPT       PIC X(13).
          05 WS-DT-GROSS      PIC $ZZ,ZZ9.99.
          05 FILLER           PIC X(2) VALUE SPACES.
          05 WS-DT-NET        PIC $ZZ,ZZ9.99.

       01 WS-TOTAL-LINE.
          05 FILLER           PIC X(27) VALUE SPACES.
          05 FILLER           PIC X(13) VALUE "TOTALS:".
          05 WS-TOT-GROSS     PIC $ZZZ,ZZ9.99.
          05 WS-TOT-NET       PIC $ZZZ,ZZ9.99.

       01 WS-COUNT-LINE.
          05 FILLER           PIC X(27) VALUE SPACES.
          05 FILLER           PIC X(20)
              VALUE "Employee count:".
          05 WS-TOT-COUNT     PIC Z(3)9.

      *> --- Accumulators ---
       01 WS-TOTAL-GROSS     PIC 9(7)V99 VALUE 0.
       01 WS-TOTAL-NET       PIC 9(7)V99 VALUE 0.
       01 WS-EMP-COUNT       PIC 9(4) VALUE 0.
       01 WS-GROSS           PIC 9(6)V99.
       01 WS-NET             PIC 9(6)V99.
       01 WS-LINE-COUNT      PIC 9(3) VALUE 99.
       01 WS-PAGE-LIMIT      PIC 9(3) VALUE 20.
       01 WS-I               PIC 9(2).

      *> --- Sample employee data table ---
       01 WS-EMP-DATA.
          05 FILLER PIC X(46)
              VALUE "E001 Sarah Connor       Engineering  085000".
          05 FILLER PIC X(46)
              VALUE "E002 John Smith         Sales        062000".
          05 FILLER PIC X(46)
              VALUE "E003 Lisa Park          Marketing    071000".
          05 FILLER PIC X(46)
              VALUE "E004 Mike Chen          Engineering  093000".
          05 FILLER PIC X(46)
              VALUE "E005 Anna Lee           Sales        058000".
          05 FILLER PIC X(46)
              VALUE "E006 Tom Harris         Marketing    067000".

       01 WS-EMP-TABLE REDEFINES WS-EMP-DATA.
          05 WS-EMP-ENTRY OCCURS 6 TIMES.
             10 WS-E-ID      PIC X(5).
             10 WS-E-NAME    PIC X(16).
             10 WS-E-DEPT    PIC X(13).
             10 WS-E-SAL     PIC 9(6).
             10 FILLER       PIC X(6).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN OUTPUT REPORT-FILE.
           PERFORM 1000-WRITE-HEADER.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 6
               PERFORM 2000-WRITE-DETAIL
           END-PERFORM.

           PERFORM 3000-WRITE-FOOTER.
           CLOSE REPORT-FILE.

           DISPLAY "Report written to payroll-report.txt".

      *>   Show it on screen
           DISPLAY SPACES.
           PERFORM 4000-DISPLAY-REPORT.
           STOP RUN.

      *> --- Page header ---
       1000-WRITE-HEADER.
           WRITE PRINT-LINE FROM WS-TITLE-LINE
               AFTER ADVANCING 1 LINE.
           MOVE "2026-02-13" TO WS-RPT-DATE.
           WRITE PRINT-LINE FROM WS-DATE-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM WS-DASH-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM WS-HEADER-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRINT-LINE FROM WS-DASH-LINE
               AFTER ADVANCING 1 LINE.
           MOVE 5 TO WS-LINE-COUNT.

      *> --- Detail line for each employee ---
       2000-WRITE-DETAIL.
           IF WS-LINE-COUNT >= WS-PAGE-LIMIT
               PERFORM 1000-WRITE-HEADER
           END-IF.

           MOVE WS-E-ID(WS-I)   TO WS-DT-ID.
           MOVE WS-E-NAME(WS-I) TO WS-DT-NAME.
           MOVE WS-E-DEPT(WS-I) TO WS-DT-DEPT.

           MOVE WS-E-SAL(WS-I)  TO WS-GROSS.
           COMPUTE WS-NET = WS-GROSS * 0.75.

           MOVE WS-GROSS TO WS-DT-GROSS.
           MOVE WS-NET   TO WS-DT-NET.

           WRITE PRINT-LINE FROM WS-DETAIL-LINE
               AFTER ADVANCING 1 LINE.

           ADD WS-GROSS TO WS-TOTAL-GROSS.
           ADD WS-NET   TO WS-TOTAL-NET.
           ADD 1        TO WS-EMP-COUNT.
           ADD 1        TO WS-LINE-COUNT.

      *> --- Report footer ---
       3000-WRITE-FOOTER.
           WRITE PRINT-LINE FROM WS-DASH-LINE
               AFTER ADVANCING 1 LINE.
           MOVE WS-TOTAL-GROSS TO WS-TOT-GROSS.
           MOVE WS-TOTAL-NET   TO WS-TOT-NET.
           WRITE PRINT-LINE FROM WS-TOTAL-LINE
               AFTER ADVANCING 1 LINE.
           MOVE WS-EMP-COUNT TO WS-TOT-COUNT.
           WRITE PRINT-LINE FROM WS-COUNT-LINE
               AFTER ADVANCING 1 LINE.

      *> --- Display the report file on screen ---
       4000-DISPLAY-REPORT.
           OPEN INPUT REPORT-FILE.
           MOVE "N" TO WS-I.
           PERFORM UNTIL WS-I = "Y"
               READ REPORT-FILE INTO PRINT-LINE
                   AT END MOVE "Y" TO WS-I
                   NOT AT END DISPLAY PRINT-LINE
               END-READ
           END-PERFORM.
           CLOSE REPORT-FILE.
