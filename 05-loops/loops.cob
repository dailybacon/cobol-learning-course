       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOOPS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 5: Loops with PERFORM
      *> ============================================
      *> COBOL doesn't have "for" or "while" loops.
      *> Instead, everything uses PERFORM.
      *>
      *> NEW CONCEPTS:
      *>   - PERFORM paragraph-name
      *>     Executes a paragraph once (like a
      *>     function call).
      *>
      *>   - PERFORM paragraph-name N TIMES
      *>     Repeats a paragraph N times.
      *>
      *>   - PERFORM paragraph-name
      *>       UNTIL condition
      *>     Repeats until the condition is true.
      *>     (Like a while loop, but tests BEFORE
      *>     each iteration by default.)
      *>
      *>   - PERFORM paragraph-name
      *>       WITH TEST AFTER
      *>       UNTIL condition
      *>     Tests AFTER each iteration (do-while).
      *>
      *>   - PERFORM VARYING counter
      *>       FROM start BY step
      *>       UNTIL condition
      *>     COBOL's version of a for loop.
      *>
      *>   - Inline PERFORM: put statements between
      *>     PERFORM and END-PERFORM instead of
      *>     calling a paragraph.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x loops.cob -o loops
      *>   ./loops
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER    PIC 9(3) VALUE 0.
       01 WS-LIMIT      PIC 9(3).
       01 WS-TOTAL      PIC 9(5) VALUE 0.
       01 WS-DISPLAY    PIC Z(4)9.
       01 WS-I          PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *>   --- PERFORM N TIMES ---
           DISPLAY "=== Countdown (3 TIMES) ===".
           MOVE 3 TO WS-COUNTER.
           PERFORM SHOW-COUNTDOWN 3 TIMES.
           DISPLAY SPACES.

      *>   --- PERFORM UNTIL ---
           DISPLAY "=== Count up to 5 (UNTIL) ===".
           MOVE 1 TO WS-COUNTER.
           PERFORM COUNT-UP
               UNTIL WS-COUNTER > 5.
           DISPLAY SPACES.

      *>   --- PERFORM VARYING (for loop) ---
           DISPLAY "=== Multiplication table for 7 ===".
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 10
               COMPUTE WS-TOTAL = 7 * WS-I
               MOVE WS-TOTAL TO WS-DISPLAY
               DISPLAY "  7 x " WS-I " = " WS-DISPLAY
           END-PERFORM.
           DISPLAY SPACES.

      *>   --- Inline PERFORM UNTIL ---
           DISPLAY "=== Sum numbers until 0 ===".
           DISPLAY "Enter numbers (0 to stop):".
           MOVE 0 TO WS-TOTAL.
           PERFORM WITH TEST AFTER
               UNTIL WS-COUNTER = 0
               ACCEPT WS-COUNTER
               ADD WS-COUNTER TO WS-TOTAL
           END-PERFORM.
           MOVE WS-TOTAL TO WS-DISPLAY.
           DISPLAY "Total: " WS-DISPLAY.

           STOP RUN.

      *> --- Paragraphs called by PERFORM ---

       SHOW-COUNTDOWN.
           DISPLAY "  T-minus " WS-COUNTER.
           SUBTRACT 1 FROM WS-COUNTER.

       COUNT-UP.
           DISPLAY "  Count: " WS-COUNTER.
           ADD 1 TO WS-COUNTER.
