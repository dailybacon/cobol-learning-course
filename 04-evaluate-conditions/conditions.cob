       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDITIONS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 4: EVALUATE and 88-Level Conditions
      *> ============================================
      *> IF/ELSE works, but COBOL has better tools
      *> for complex decisions.
      *>
      *> NEW CONCEPTS:
      *>   - EVALUATE: COBOL's version of switch/case.
      *>     Much cleaner than nested IF statements.
      *>
      *>     EVALUATE variable
      *>       WHEN value-1  action-1
      *>       WHEN value-2  action-2
      *>       WHEN OTHER    default-action
      *>     END-EVALUATE
      *>
      *>   - EVALUATE TRUE: test multiple conditions
      *>     (like chained if/else-if)
      *>
      *>   - 88-level condition names:
      *>     These let you give meaningful names to
      *>     specific values a variable can hold.
      *>
      *>     01 WS-STATUS    PIC 9.
      *>        88 IS-ACTIVE  VALUE 1.
      *>        88 IS-CLOSED  VALUE 2.
      *>
      *>     Now you can write: IF IS-ACTIVE ...
      *>     instead of:        IF WS-STATUS = 1 ...
      *>
      *>   - SET condition TO TRUE
      *>     Assigns the value associated with an
      *>     88-level to its parent variable.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x conditions.cob -o conditions
      *>   ./conditions
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> 88-levels give names to values
       01 WS-GRADE        PIC X(1).
          88 GRADE-A       VALUE "A".
          88 GRADE-B       VALUE "B".
          88 GRADE-C       VALUE "C".
          88 GRADE-D       VALUE "D".
          88 GRADE-F       VALUE "F".
          88 GRADE-VALID   VALUE "A" "B" "C" "D" "F".

       01 WS-SCORE         PIC 9(3).
       01 WS-DAY           PIC 9.
       01 WS-DAY-NAME      PIC X(9).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *>   --- EVALUATE with specific values ---
      *>   Like a switch/case statement.
           DISPLAY "Enter day number (1-7): ".
           ACCEPT WS-DAY.

           EVALUATE WS-DAY
               WHEN 1 MOVE "Monday"    TO WS-DAY-NAME
               WHEN 2 MOVE "Tuesday"   TO WS-DAY-NAME
               WHEN 3 MOVE "Wednesday" TO WS-DAY-NAME
               WHEN 4 MOVE "Thursday"  TO WS-DAY-NAME
               WHEN 5 MOVE "Friday"    TO WS-DAY-NAME
               WHEN 6 MOVE "Saturday"  TO WS-DAY-NAME
               WHEN 7 MOVE "Sunday"    TO WS-DAY-NAME
               WHEN OTHER
                   MOVE "Unknown" TO WS-DAY-NAME
           END-EVALUATE.

           DISPLAY "Day: " WS-DAY-NAME.
           DISPLAY SPACES.

      *>   --- EVALUATE TRUE with ranges ---
      *>   Like chained if/else-if.
           DISPLAY "Enter test score (0-100): ".
           ACCEPT WS-SCORE.

           EVALUATE TRUE
               WHEN WS-SCORE >= 90
                   SET GRADE-A TO TRUE
               WHEN WS-SCORE >= 80
                   SET GRADE-B TO TRUE
               WHEN WS-SCORE >= 70
                   SET GRADE-C TO TRUE
               WHEN WS-SCORE >= 60
                   SET GRADE-D TO TRUE
               WHEN OTHER
                   SET GRADE-F TO TRUE
           END-EVALUATE.

      *>   --- Using 88-level conditions in IF ---
           DISPLAY "Grade: " WS-GRADE.

           IF GRADE-A
               DISPLAY "Excellent work!"
           END-IF.

           IF GRADE-F
               DISPLAY "You need to study more."
           END-IF.

           IF GRADE-VALID
               DISPLAY "Valid grade recorded."
           ELSE
               DISPLAY "Something went wrong."
           END-IF.

           STOP RUN.
