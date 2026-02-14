       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 13: Subprograms (CALL Statement)
      *> ============================================
      *> Large COBOL systems are split into separate
      *> programs that CALL each other. This is how
      *> you build modular, reusable code.
      *>
      *> NEW CONCEPTS:
      *>   - CALL "program-name" USING var1 var2
      *>     Calls another compiled COBOL program,
      *>     passing variables to it.
      *>
      *>   - BY REFERENCE (default):
      *>     The called program can modify the
      *>     variable. Changes are visible to caller.
      *>
      *>   - BY CONTENT:
      *>     Passes a copy. The called program can
      *>     modify its copy but the original is safe.
      *>
      *>   - BY VALUE:
      *>     Like BY CONTENT but for interop with C.
      *>
      *>   - LINKAGE SECTION (in called program):
      *>     Declares variables received from caller.
      *>     Same as WORKING-STORAGE but memory is
      *>     owned by the caller.
      *>
      *>   - PROCEDURE DIVISION USING var1 var2
      *>     The called program lists its parameters.
      *>
      *>   - GOBACK: returns to the calling program.
      *>     (STOP RUN would end everything.)
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x mainprog.cob taxcalc.cob -o mainprog
      *>   ./mainprog
      *>
      *> Or compile separately:
      *>   cobc -c taxcalc.cob
      *>   cobc -x mainprog.cob taxcalc.o -o mainprog
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-GROSS-PAY     PIC 9(6)V99.
       01 WS-TAX-RATE      PIC V999.
       01 WS-TAX-AMOUNT    PIC 9(6)V99.
       01 WS-NET-PAY       PIC 9(6)V99.
       01 WS-DISP          PIC $Z(5)9.99.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "=== Payroll with Subprogram ===".
           DISPLAY "Enter gross pay: ".
           ACCEPT WS-GROSS-PAY.

      *>   Call the tax calculator subprogram.
      *>   BY REFERENCE: it can set WS-TAX-RATE
      *>   and WS-TAX-AMOUNT for us.
           CALL "TAXCALC" USING
               BY CONTENT   WS-GROSS-PAY
               BY REFERENCE WS-TAX-RATE
               BY REFERENCE WS-TAX-AMOUNT
           END-CALL.

           COMPUTE WS-NET-PAY =
               WS-GROSS-PAY - WS-TAX-AMOUNT.

           DISPLAY SPACES.
           MOVE WS-GROSS-PAY TO WS-DISP.
           DISPLAY "Gross Pay:  " WS-DISP.

           DISPLAY "Tax Rate:   " WS-TAX-RATE.

           MOVE WS-TAX-AMOUNT TO WS-DISP.
           DISPLAY "Tax Amount: " WS-DISP.

           MOVE WS-NET-PAY TO WS-DISP.
           DISPLAY "Net Pay:    " WS-DISP.

           STOP RUN.
