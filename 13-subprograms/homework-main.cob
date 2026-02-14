       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-MAIN.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 13: Subprograms (CALL Statement)
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a unit conversion system using
      *>   subprograms. The main program handles
      *>   the menu and I/O. Each conversion type
      *>   is a separate called subprogram.
      *>
      *>   YOU MUST CREATE 3 FILES:
      *>     homework-main.cob  (this file - the menu)
      *>     convert-temp.cob   (temperature converter)
      *>     convert-dist.cob   (distance converter)
      *>
      *>   MAIN PROGRAM MENU:
      *>     1. Temperature Conversion
      *>     2. Distance Conversion
      *>     3. Exit
      *>
      *>   TEMPERATURE SUBPROGRAM (convert-temp.cob):
      *>   Receives:
      *>     - Input value     PIC S9(5)V99  (BY CONTENT)
      *>     - Direction       PIC X         (BY CONTENT)
      *>       "F" = Fahrenheit to Celsius
      *>       "C" = Celsius to Fahrenheit
      *>   Returns:
      *>     - Output value    PIC S9(5)V99  (BY REFERENCE)
      *>     - Status          PIC X         (BY REFERENCE)
      *>       "S" = success, "E" = error
      *>
      *>   Formulas:
      *>     C = (F - 32) * 5 / 9
      *>     F = C * 9 / 5 + 32
      *>
      *>   DISTANCE SUBPROGRAM (convert-dist.cob):
      *>   Receives:
      *>     - Input value     PIC 9(7)V99   (BY CONTENT)
      *>     - Unit code       PIC 9         (BY CONTENT)
      *>       1 = Miles to Kilometers
      *>       2 = Kilometers to Miles
      *>       3 = Feet to Meters
      *>       4 = Meters to Feet
      *>   Returns:
      *>     - Output value    PIC 9(7)V99   (BY REFERENCE)
      *>     - Unit label      PIC X(12)     (BY REFERENCE)
      *>
      *>   Conversion factors:
      *>     1 mile = 1.60934 km
      *>     1 foot = 0.3048 meters
      *>
      *>   Example interaction:
      *>
      *>   === Unit Converter ===
      *>   1. Temperature
      *>   2. Distance
      *>   3. Exit
      *>   Choice: 1
      *>
      *>   Enter temperature: 212
      *>   Convert (F)ahrenheit->Celsius
      *>       or (C)elsius->Fahrenheit? F
      *>   Result: 100.00 Celsius
      *>
      *>   Choice: 2
      *>   Enter distance: 26.20
      *>   1=Mi->Km 2=Km->Mi 3=Ft->M 4=M->Ft
      *>   Unit: 1
      *>   Result: 42.16 Kilometers
      *>
      *> REQUIREMENTS:
      *>   1. Main program uses CALL "CONVERT-TEMP"
      *>      and CALL "CONVERT-DIST".
      *>   2. Use BY CONTENT for input parameters
      *>      (protect the caller's data).
      *>   3. Use BY REFERENCE for output parameters
      *>      (let the subprogram return results).
      *>   4. Subprograms use LINKAGE SECTION and
      *>      PROCEDURE DIVISION USING.
      *>   5. Subprograms end with GOBACK, not
      *>      STOP RUN.
      *>   6. Handle invalid direction/unit codes
      *>      in the subprograms (return error status).
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework-main.cob convert-temp.cob
      *>     convert-dist.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
