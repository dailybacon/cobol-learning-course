       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMBEDSQL.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 16: Embedded SQL
      *> ============================================
      *> In the real world, COBOL programs often
      *> talk to databases (DB2, Oracle, etc.) using
      *> embedded SQL. SQL statements are placed
      *> directly in the COBOL source code.
      *>
      *> IMPORTANT: This lesson is REFERENCE ONLY.
      *> Embedded SQL requires a database precompiler
      *> (like DB2's DBRM or Oracle Pro*COBOL) which
      *> isn't available in GnuCOBOL by default.
      *>
      *> This file shows the PATTERNS you'll use on
      *> mainframes and enterprise systems. The
      *> runnable demo at the bottom uses regular
      *> COBOL to simulate the concepts.
      *>
      *> KEY CONCEPTS:
      *>   - EXEC SQL ... END-EXEC
      *>     Wraps any SQL statement in COBOL.
      *>
      *>   - Host variables: COBOL variables used
      *>     in SQL, prefixed with colon (:).
      *>     SELECT NAME INTO :WS-NAME
      *>       FROM EMPLOYEES WHERE ID = :WS-ID
      *>
      *>   - SQLCODE: return code after each SQL.
      *>     0    = success
      *>     100  = no rows found
      *>     < 0  = error occurred
      *>
      *>   - CURSORS: for multi-row results.
      *>     DECLARE cursor-name CURSOR FOR SELECT...
      *>     OPEN cursor-name
      *>     FETCH cursor-name INTO :var1, :var2
      *>     CLOSE cursor-name
      *>
      *>   - COMMIT / ROLLBACK: transaction control.
      *>
      *>   - WHENEVER: automatic error handling.
      *>     EXEC SQL WHENEVER SQLERROR
      *>       GO TO error-para END-EXEC
      *>
      *> TO COMPILE THIS DEMO (no SQL needed):
      *>   cobc -x embedsql.cob -o embedsql
      *>   ./embedsql
      *> ============================================

      *> ==================================================
      *> REFERENCE: What real embedded SQL looks like
      *> (This code won't compile - it's for reading only)
      *> ==================================================
      *>
      *> WORKING-STORAGE SECTION.
      *>     EXEC SQL INCLUDE SQLCA END-EXEC.
      *>
      *>     EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *> 01  WS-EMP-ID      PIC X(4).
      *> 01  WS-EMP-NAME    PIC X(30).
      *> 01  WS-EMP-SALARY  PIC S9(7)V99 COMP-3.
      *>     EXEC SQL END DECLARE SECTION END-EXEC.
      *>
      *> PROCEDURE DIVISION.
      *>
      *> --- Single row query ---
      *>     MOVE "E001" TO WS-EMP-ID.
      *>     EXEC SQL
      *>       SELECT EMP_NAME, SALARY
      *>         INTO :WS-EMP-NAME, :WS-EMP-SALARY
      *>         FROM EMPLOYEES
      *>         WHERE EMP_ID = :WS-EMP-ID
      *>     END-EXEC.
      *>     IF SQLCODE = 0
      *>       DISPLAY "Found: " WS-EMP-NAME
      *>     END-IF.
      *>
      *> --- Cursor for multiple rows ---
      *>     EXEC SQL
      *>       DECLARE EMP-CURSOR CURSOR FOR
      *>       SELECT EMP_ID, EMP_NAME, SALARY
      *>         FROM EMPLOYEES
      *>         WHERE SALARY > :WS-MIN-SAL
      *>         ORDER BY SALARY DESC
      *>     END-EXEC.
      *>
      *>     EXEC SQL OPEN EMP-CURSOR END-EXEC.
      *>     PERFORM UNTIL SQLCODE = 100
      *>       EXEC SQL
      *>         FETCH EMP-CURSOR
      *>           INTO :WS-EMP-ID, :WS-EMP-NAME,
      *>                :WS-EMP-SALARY
      *>       END-EXEC
      *>       IF SQLCODE = 0
      *>         DISPLAY WS-EMP-ID " " WS-EMP-NAME
      *>       END-IF
      *>     END-PERFORM.
      *>     EXEC SQL CLOSE EMP-CURSOR END-EXEC.
      *>
      *> --- Insert ---
      *>     EXEC SQL
      *>       INSERT INTO EMPLOYEES
      *>         (EMP_ID, EMP_NAME, SALARY)
      *>       VALUES
      *>         (:WS-EMP-ID, :WS-EMP-NAME,
      *>          :WS-EMP-SALARY)
      *>     END-EXEC.
      *>     EXEC SQL COMMIT END-EXEC.
      *>
      *> ==================================================

      *> ==================================================
      *> RUNNABLE DEMO: Simulates the SQL patterns above
      *> using COBOL tables (so you can see the flow).
      *> ==================================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Simulated "database" as a COBOL table
       01 WS-DB.
          05 WS-DB-ROWS.
             10 FILLER PIC X(38)
                VALUE "E001Alice Johnson        065000".
             10 FILLER PIC X(38)
                VALUE "E002Bob Williams         072000".
             10 FILLER PIC X(38)
                VALUE "E003Carol Davis          058000".
             10 FILLER PIC X(38)
                VALUE "E004Dan Miller           081000".
          05 WS-DB-TABLE REDEFINES WS-DB-ROWS.
             10 WS-ROW OCCURS 4 TIMES.
                15 WS-DB-ID     PIC X(4).
                15 WS-DB-NAME   PIC X(20).
                15 WS-DB-SAL    PIC 9(6).

       01 WS-SEARCH-ID    PIC X(4).
       01 WS-FOUND        PIC X VALUE "N".
       01 WS-I            PIC 9(2).
       01 WS-DISP-SAL     PIC $ZZ,ZZ9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *>   Simulate: SELECT ... WHERE ID = :search-id
           DISPLAY "=== Simulated SQL Query ===".
           DISPLAY "Enter employee ID (E001-E004): ".
           ACCEPT WS-SEARCH-ID.

           MOVE "N" TO WS-FOUND.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 4
               IF WS-DB-ID(WS-I) = WS-SEARCH-ID
                   MOVE "Y" TO WS-FOUND
                   DISPLAY "  Found: " WS-DB-NAME(WS-I)
                   MOVE WS-DB-SAL(WS-I) TO WS-DISP-SAL
                   DISPLAY "  Salary: " WS-DISP-SAL
               END-IF
           END-PERFORM.

           IF WS-FOUND = "N"
               DISPLAY "  SQLCODE 100: No rows found"
           END-IF.

           DISPLAY SPACES.

      *>   Simulate: SELECT ... WHERE SALARY > 65000
      *>   (like a cursor fetch loop)
           DISPLAY "=== Simulated Cursor ===".
           DISPLAY "Employees earning > $65,000:".

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 4
               IF WS-DB-SAL(WS-I) > 65000
                   MOVE WS-DB-SAL(WS-I) TO WS-DISP-SAL
                   DISPLAY "  " WS-DB-ID(WS-I) " "
                       WS-DB-NAME(WS-I) WS-DISP-SAL
               END-IF
           END-PERFORM.

           DISPLAY SPACES.
           DISPLAY "In production, these would be real".
           DISPLAY "EXEC SQL statements talking to DB2!".

           STOP RUN.
