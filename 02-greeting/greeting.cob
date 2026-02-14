       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREETING.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 2: Variables and User Input
      *> ============================================
      *> Now we add the DATA DIVISION to declare
      *> variables, and use ACCEPT to read input.
      *>
      *> NEW CONCEPTS:
      *>   - DATA DIVISION / WORKING-STORAGE SECTION
      *>     This is where you declare your variables.
      *>
      *>   - PIC (PICTURE) clause:
      *>     PIC X(30) = alphanumeric, 30 chars wide
      *>     PIC 9(3)  = numeric, 3 digits wide
      *>
      *>   - ACCEPT variable-name
      *>     Reads a line of input from the user.
      *>
      *>   - DISPLAY can combine literals and variables:
      *>     DISPLAY "Hello, " WS-NAME "!"
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x greeting.cob -o greeting
      *>   ./greeting
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(30).
       01 WS-AGE  PIC 9(3).

       PROCEDURE DIVISION.
           DISPLAY "What is your name? ".
           ACCEPT WS-NAME.

           DISPLAY "How old are you? ".
           ACCEPT WS-AGE.

           DISPLAY "Hello, " WS-NAME "!".
           DISPLAY "You are " WS-AGE " years old.".

           STOP RUN.
