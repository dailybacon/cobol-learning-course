       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYBOOKS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 9: Copybooks (COPY and REPLACE)
      *> ============================================
      *> Copybooks are COBOL's version of #include
      *> or import. They let you reuse data layouts
      *> and code across multiple programs.
      *>
      *> NEW CONCEPTS:
      *>   - COPY "filename".
      *>     Inserts the contents of a copybook file
      *>     at compile time. Like C's #include.
      *>
      *>   - COPY "filename" REPLACING
      *>       ==old-text== BY ==new-text==.
      *>     Inserts the copybook but substitutes text.
      *>     Useful for generic templates.
      *>
      *>   - Copybook files typically use extensions
      *>     like .cpy, .cbl, or .copy
      *>
      *>   - Common uses:
      *>     * Record layouts (so multiple programs
      *>       read the same file format)
      *>     * Constants and configuration values
      *>     * Shared paragraphs
      *>
      *>   - REPLACE statement: like COPY REPLACING
      *>     but works on the current source code.
      *>     REPLACE ==old== BY ==new==.
      *>     REPLACE OFF.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x copybooks.cob -o copybooks
      *>   ./copybooks
      *>
      *> NOTE: The -I flag tells the compiler where
      *>   to find copybooks:
      *>   cobc -x -I . copybooks.cob -o copybooks
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> This pulls in the employee record layout
      *> from the employee.cpy copybook file.
           COPY "employee.cpy"
               REPLACING ==WS-EMP-RECORD== BY ==WS-EMP1==
                         ==WS-EMP-ID==      BY ==WS-EMP-ID==
                         ==WS-EMP-NAME==    BY ==WS-EMP-NAME==
                         ==WS-EMP-DEPT==    BY ==WS-EMP-DEPT==
                         ==WS-EMP-SALARY==  BY ==WS-EMP-SALARY==.

      *> We can also use COPY with REPLACING to
      *> create a second record with different names.
           COPY "employee.cpy"
               REPLACING ==WS-EMP-RECORD== BY ==WS-MGR-RECORD==
                         ==WS-EMP-ID==      BY ==WS-MGR-ID==
                         ==WS-EMP-NAME==    BY ==WS-MGR-NAME==
                         ==WS-EMP-DEPT==    BY ==WS-MGR-DEPT==
                         ==WS-EMP-SALARY==  BY ==WS-MGR-SALARY==.

       01 WS-DISPLAY-PAY  PIC $Z(5)9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *>   --- Fill in employee data ---
           MOVE "E001"          TO WS-EMP-ID.
           MOVE "Jane Smith"    TO WS-EMP-NAME.
           MOVE "Engineering"   TO WS-EMP-DEPT.
           MOVE 75000.00        TO WS-EMP-SALARY.

      *>   --- Fill in manager data (from REPLACING) ---
           MOVE "M001"          TO WS-MGR-ID.
           MOVE "Bob Johnson"   TO WS-MGR-NAME.
           MOVE "Engineering"   TO WS-MGR-DEPT.
           MOVE 95000.00        TO WS-MGR-SALARY.

      *>   --- Display both ---
           DISPLAY "=== Employee Record ===".
           DISPLAY "  ID:     " WS-EMP-ID.
           DISPLAY "  Name:   " WS-EMP-NAME.
           DISPLAY "  Dept:   " WS-EMP-DEPT.
           MOVE WS-EMP-SALARY TO WS-DISPLAY-PAY.
           DISPLAY "  Salary: " WS-DISPLAY-PAY.
           DISPLAY SPACES.

           DISPLAY "=== Manager Record ===".
           DISPLAY "  ID:     " WS-MGR-ID.
           DISPLAY "  Name:   " WS-MGR-NAME.
           DISPLAY "  Dept:   " WS-MGR-DEPT.
           MOVE WS-MGR-SALARY TO WS-DISPLAY-PAY.
           DISPLAY "  Salary: " WS-DISPLAY-PAY.

           STOP RUN.
