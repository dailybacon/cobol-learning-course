       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT-DIST.
       AUTHOR. LEARNER.

      *> ============================================
      *> SUBPROGRAM: Distance Converter
      *> ============================================
      *> Called by homework-main.cob.
      *> See that file for full assignment details.
      *>
      *> Parameters (in order):
      *>   1. LS-INPUT-DIST  PIC 9(7)V99  (BY CONTENT)
      *>   2. LS-UNIT-CODE   PIC 9        (BY CONTENT)
      *>      1 = Miles to Kilometers
      *>      2 = Kilometers to Miles
      *>      3 = Feet to Meters
      *>      4 = Meters to Feet
      *>   3. LS-OUTPUT-DIST PIC 9(7)V99  (BY REFERENCE)
      *>   4. LS-UNIT-LABEL  PIC X(12)    (BY REFERENCE)
      *>
      *> Conversion factors:
      *>   1 mile = 1.60934 km
      *>   1 foot = 0.3048 meters
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
       CALC-DIST.
      *>   YOUR CODE GOES HERE

           GOBACK.
