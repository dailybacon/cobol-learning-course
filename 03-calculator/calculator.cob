       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 3: Arithmetic and Conditionals
      *> ============================================
      *> This program takes two numbers and performs
      *> addition, subtraction, multiplication, and
      *> division.
      *>
      *> NEW CONCEPTS:
      *>   - Numeric variables with decimals:
      *>     PIC 9(5)V99 = 5 whole digits, 2 decimal
      *>     (V marks the implied decimal point)
      *>
      *>   - Arithmetic statements:
      *>     ADD A TO B        (B = B + A)
      *>     SUBTRACT A FROM B (B = B - A)
      *>     MULTIPLY A BY B   (B = B * A)
      *>     DIVIDE A INTO B   (B = B / A)
      *>     COMPUTE C = A + B (like normal math)
      *>
      *>   - Edited picture for display:
      *>     PIC Z(5)9.99 = suppress leading zeros,
      *>     show decimal point
      *>
      *>   - IF / ELSE / END-IF conditionals
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x calculator.cob -o calculator
      *>   ./calculator
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1       PIC 9(5)V99.
       01 WS-NUM2       PIC 9(5)V99.
       01 WS-RESULT     PIC 9(5)V99.
       01 WS-DISPLAY    PIC Z(5)9.99.

       PROCEDURE DIVISION.
           DISPLAY "Enter first number: ".
           ACCEPT WS-NUM1.

           DISPLAY "Enter second number: ".
           ACCEPT WS-NUM2.

      *>   --- Addition ---
           COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2.
           MOVE WS-RESULT TO WS-DISPLAY.
           DISPLAY "Sum:        " WS-DISPLAY.

      *>   --- Subtraction ---
           COMPUTE WS-RESULT = WS-NUM1 - WS-NUM2.
           MOVE WS-RESULT TO WS-DISPLAY.
           DISPLAY "Difference: " WS-DISPLAY.

      *>   --- Multiplication ---
           COMPUTE WS-RESULT = WS-NUM1 * WS-NUM2.
           MOVE WS-RESULT TO WS-DISPLAY.
           DISPLAY "Product:    " WS-DISPLAY.

      *>   --- Division (with zero check) ---
           IF WS-NUM2 = 0
               DISPLAY "Quotient:   Cannot divide by zero!"
           ELSE
               COMPUTE WS-RESULT = WS-NUM1 / WS-NUM2
               MOVE WS-RESULT TO WS-DISPLAY
               DISPLAY "Quotient:   " WS-DISPLAY
           END-IF.

           STOP RUN.
