       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT-TEMP.
       AUTHOR. LEARNER.

      *> ============================================
      *> SUBPROGRAM: Temperature Converter
      *> ============================================
      *> Called by homework-main.cob.
      *> See that file for full assignment details.
      *>
      *> Parameters (in order):
      *>   1. LS-INPUT-TEMP   PIC S9(5)V99  (BY CONTENT)
      *>   2. LS-DIRECTION    PIC X         (BY CONTENT)
      *>      "F" = Fahrenheit to Celsius
      *>      "C" = Celsius to Fahrenheit
      *>   3. LS-OUTPUT-TEMP  PIC S9(5)V99  (BY REFERENCE)
      *>   4. LS-STATUS       PIC X         (BY REFERENCE)
      *>      "S" = success, "E" = error
      *>
      *> Remember:
      *>   - Use LINKAGE SECTION for parameters
      *>   - Use PROCEDURE DIVISION USING
      *>   - End with GOBACK, not STOP RUN
      *> ============================================

       DATA DIVISION.
       LINKAGE SECTION.
      *>   DECLARE YOUR LINKAGE PARAMETERS HERE


       PROCEDURE DIVISION USING
      *>   LIST YOUR PARAMETERS HERE
           .
       CALC-TEMP.
      *>   YOUR CODE GOES HERE

           GOBACK.
