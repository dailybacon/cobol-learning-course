       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 8: Tables and Arrays
      *> ============================================
      *> COBOL calls arrays "tables." They use the
      *> OCCURS clause to define repeating elements.
      *>
      *> NEW CONCEPTS:
      *>   - OCCURS n TIMES: defines a table.
      *>     01 WS-SCORES.
      *>        05 WS-SCORE PIC 9(3) OCCURS 10 TIMES.
      *>     Access with subscript: WS-SCORE(1), (2)...
      *>     Subscripts start at 1, not 0!
      *>
      *>   - Multi-level tables (2D arrays):
      *>     01 WS-MATRIX.
      *>        05 WS-ROW OCCURS 3 TIMES.
      *>           10 WS-COL PIC 9 OCCURS 3 TIMES.
      *>     Access: WS-COL(row, col)
      *>
      *>   - INDEXED BY: attach an index to a table.
      *>     More efficient than subscripts.
      *>     Use SET index TO value (not MOVE).
      *>
      *>   - SEARCH: linear search through a table.
      *>     SEARCH table-item
      *>       WHEN condition action
      *>     END-SEARCH
      *>
      *>   - SEARCH ALL: binary search (table must
      *>     be sorted, needs KEY IS clause).
      *>
      *>   - VALUE clause with OCCURS for
      *>     pre-initialized tables.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x tables.cob -o tables
      *>   ./tables
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Simple table: 5 student scores
       01 WS-SCORE-TABLE.
          05 WS-SCORE       PIC 9(3) OCCURS 5 TIMES.

       01 WS-COUNTER        PIC 9(2).
       01 WS-TOTAL          PIC 9(5) VALUE 0.
       01 WS-AVERAGE        PIC 9(3)V99.
       01 WS-DISP           PIC Z(2)9.99.
       01 WS-DISP-INT       PIC Z(2)9.

      *> Pre-initialized table: month names
       01 WS-MONTH-DATA.
          05 FILLER PIC X(9) VALUE "January".
          05 FILLER PIC X(9) VALUE "February".
          05 FILLER PIC X(9) VALUE "March".
          05 FILLER PIC X(9) VALUE "April".
          05 FILLER PIC X(9) VALUE "May".
          05 FILLER PIC X(9) VALUE "June".
          05 FILLER PIC X(9) VALUE "July".
          05 FILLER PIC X(9) VALUE "August".
          05 FILLER PIC X(9) VALUE "September".
          05 FILLER PIC X(9) VALUE "October".
          05 FILLER PIC X(9) VALUE "November".
          05 FILLER PIC X(9) VALUE "December".
       01 WS-MONTH-TABLE REDEFINES WS-MONTH-DATA.
          05 WS-MONTH-NAME   PIC X(9) OCCURS 12 TIMES
                             INDEXED BY WS-MONTH-IDX.

       01 WS-INPUT-MONTH     PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *>   --- Basic table: enter and average scores ---
           DISPLAY "=== Student Scores ===".
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               DISPLAY "Enter score " WS-COUNTER ": "
               ACCEPT WS-SCORE(WS-COUNTER)
               ADD WS-SCORE(WS-COUNTER) TO WS-TOTAL
           END-PERFORM.

           COMPUTE WS-AVERAGE = WS-TOTAL / 5.
           DISPLAY SPACES.
           DISPLAY "Scores entered:".
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               MOVE WS-SCORE(WS-COUNTER) TO WS-DISP-INT
               DISPLAY "  Student " WS-COUNTER
                       ": " WS-DISP-INT
           END-PERFORM.

           MOVE WS-AVERAGE TO WS-DISP.
           DISPLAY "Average: " WS-DISP.
           DISPLAY SPACES.

      *>   --- Pre-initialized table with SEARCH ---
           DISPLAY "=== Month Lookup ===".
           DISPLAY "Enter month number (1-12): ".
           ACCEPT WS-INPUT-MONTH.

           IF WS-INPUT-MONTH >= 1 AND
              WS-INPUT-MONTH <= 12
               SET WS-MONTH-IDX TO WS-INPUT-MONTH
               DISPLAY "Month: "
                   WS-MONTH-NAME(WS-MONTH-IDX)
           ELSE
               DISPLAY "Invalid month number!"
           END-IF.

           STOP RUN.
