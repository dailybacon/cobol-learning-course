       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARAGRAPHS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 6: Paragraphs and Program Organization
      *> ============================================
      *> Real COBOL programs are organized into
      *> paragraphs and sections. This is how you
      *> structure larger programs.
      *>
      *> NEW CONCEPTS:
      *>   - Paragraphs: named blocks of code.
      *>     A paragraph starts with a name in Area A
      *>     (columns 8-11) followed by a period.
      *>     It ends when the next paragraph begins.
      *>
      *>   - Sections: groups of related paragraphs.
      *>     Declared with SECTION keyword.
      *>
      *>   - PERFORM paragraph-name
      *>     Executes the paragraph then returns.
      *>
      *>   - PERFORM para-1 THRU para-3
      *>     Executes para-1 through para-3 in order.
      *>
      *>   - GO TO paragraph-name
      *>     Jumps to a paragraph (use sparingly!).
      *>
      *>   - STOP RUN vs. GO BACK
      *>     STOP RUN ends the whole program.
      *>     GO BACK returns to the caller (used in
      *>     subprograms, covered in lesson 13).
      *>
      *>   - Best practice: structure your program
      *>     with a main paragraph that PERFORMs
      *>     other paragraphs in sequence.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x paragraphs.cob -o paragraphs
      *>   ./paragraphs
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME        PIC X(30).
       01 WS-HOURS        PIC 9(3).
       01 WS-RATE         PIC 9(3)V99.
       01 WS-GROSS-PAY    PIC 9(5)V99.
       01 WS-TAX          PIC 9(5)V99.
       01 WS-NET-PAY      PIC 9(5)V99.
       01 WS-DISPLAY      PIC $Z(4)9.99.

       PROCEDURE DIVISION.

      *> --- Main paragraph: controls the flow ---
       0000-MAIN.
           PERFORM 1000-GET-INPUT.
           PERFORM 2000-CALCULATE-PAY.
           PERFORM 3000-DISPLAY-RESULTS.
           STOP RUN.

      *> --- Input paragraph ---
       1000-GET-INPUT.
           DISPLAY "=== Payroll Calculator ===".
           DISPLAY "Employee name: ".
           ACCEPT WS-NAME.
           DISPLAY "Hours worked: ".
           ACCEPT WS-HOURS.
           DISPLAY "Hourly rate: ".
           ACCEPT WS-RATE.

      *> --- Calculation paragraph ---
       2000-CALCULATE-PAY.
           COMPUTE WS-GROSS-PAY = WS-HOURS * WS-RATE.
           PERFORM 2100-CALCULATE-TAX.
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TAX.

      *> --- Sub-paragraph for tax ---
       2100-CALCULATE-TAX.
           EVALUATE TRUE
               WHEN WS-GROSS-PAY > 5000
                   COMPUTE WS-TAX = WS-GROSS-PAY * 0.30
               WHEN WS-GROSS-PAY > 2000
                   COMPUTE WS-TAX = WS-GROSS-PAY * 0.20
               WHEN OTHER
                   COMPUTE WS-TAX = WS-GROSS-PAY * 0.10
           END-EVALUATE.

      *> --- Output paragraph ---
       3000-DISPLAY-RESULTS.
           DISPLAY SPACES.
           DISPLAY "=== Pay Stub for " WS-NAME " ===".
           MOVE WS-GROSS-PAY TO WS-DISPLAY.
           DISPLAY "Gross Pay:  " WS-DISPLAY.
           MOVE WS-TAX TO WS-DISPLAY.
           DISPLAY "Tax:        " WS-DISPLAY.
           MOVE WS-NET-PAY TO WS-DISPLAY.
           DISPLAY "Net Pay:    " WS-DISPLAY.
           DISPLAY "================================".
