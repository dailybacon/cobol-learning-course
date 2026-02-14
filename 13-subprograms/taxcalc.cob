       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAXCALC.
       AUTHOR. LEARNER.

      *> ============================================
      *> SUBPROGRAM: Tax Calculator
      *> ============================================
      *> This is a called subprogram. It receives
      *> parameters from the caller via the
      *> LINKAGE SECTION.
      *>
      *> Key differences from a main program:
      *>   1. Uses LINKAGE SECTION instead of
      *>      (or in addition to) WORKING-STORAGE.
      *>   2. PROCEDURE DIVISION has USING clause.
      *>   3. Ends with GOBACK, not STOP RUN.
      *> ============================================

       DATA DIVISION.

      *> LINKAGE SECTION: variables passed by caller
       LINKAGE SECTION.
       01 LS-GROSS-PAY      PIC 9(6)V99.
       01 LS-TAX-RATE        PIC V999.
       01 LS-TAX-AMOUNT      PIC 9(6)V99.

      *> Parameters listed in same order as CALL USING
       PROCEDURE DIVISION USING
           LS-GROSS-PAY
           LS-TAX-RATE
           LS-TAX-AMOUNT.

       CALC-TAX.
           EVALUATE TRUE
               WHEN LS-GROSS-PAY > 100000
                   MOVE 0.350 TO LS-TAX-RATE
               WHEN LS-GROSS-PAY > 50000
                   MOVE 0.250 TO LS-TAX-RATE
               WHEN LS-GROSS-PAY > 25000
                   MOVE 0.150 TO LS-TAX-RATE
               WHEN OTHER
                   MOVE 0.100 TO LS-TAX-RATE
           END-EVALUATE.

           COMPUTE LS-TAX-AMOUNT =
               LS-GROSS-PAY * LS-TAX-RATE.

      *>   GOBACK returns to caller.
      *>   STOP RUN would end the whole program!
           GOBACK.
