       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-15.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 15: Error Handling
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a "bullet-proof" data import program
      *>   that reads a messy CSV file and handles
      *>   every possible thing that can go wrong.
      *>
      *>   INPUT FILE: "import.csv"
      *>   Each line should have:
      *>     ProductID,Name,Price,Quantity
      *>     e.g. "P001,Widget,19.99,100"
      *>
      *>   But the data is MESSY. Include these
      *>   intentional problems in your test file:
      *>     - A blank line
      *>     - A line with missing fields ("P002,,5.00,10")
      *>     - A line with invalid price (letters)
      *>     - A line with negative quantity
      *>     - A line with price > 99999.99 (overflow)
      *>     - Several valid lines
      *>
      *>   YOUR PROGRAM MUST:
      *>   1. Attempt to OPEN the file. If it doesn't
      *>      exist, display the FILE STATUS code and
      *>      a friendly message, then stop.
      *>
      *>   2. Read each record. For each one:
      *>      a. Validate product ID is not blank.
      *>      b. Validate name is not blank.
      *>      c. Validate price is numeric and > 0.
      *>      d. Validate quantity is numeric and > 0.
      *>      e. Use ON SIZE ERROR when computing
      *>         extended value (price * qty) in case
      *>         the result overflows.
      *>
      *>   3. Write valid records to "clean-data.dat".
      *>      Write invalid records to "errors.log"
      *>      with the reason for rejection.
      *>
      *>   4. At the end, display a summary:
      *>      - Total records read
      *>      - Records accepted
      *>      - Records rejected
      *>      - Rejection breakdown by reason:
      *>        * Blank product ID: X
      *>        * Blank name: X
      *>        * Invalid price: X
      *>        * Invalid quantity: X
      *>        * Overflow: X
      *>
      *>   Example output:
      *>
      *>   Opening import.csv... Status: 00 (OK)
      *>   Processing record 1... VALID
      *>   Processing record 2... ERROR: Blank name
      *>   Processing record 3... ERROR: Invalid price
      *>   ...
      *>
      *>   === IMPORT SUMMARY ===
      *>   Records read:       10
      *>   Records accepted:    6
      *>   Records rejected:    4
      *>
      *>   Rejection reasons:
      *>     Blank product ID:  1
      *>     Blank name:        1
      *>     Invalid price:     1
      *>     Overflow:          1
      *>
      *>   See errors.log for details.
      *>
      *> REQUIREMENTS:
      *>   1. Check FILE STATUS after EVERY file
      *>      operation (OPEN, READ, WRITE, CLOSE).
      *>   2. Use ON SIZE ERROR on at least one
      *>      COMPUTE statement.
      *>   3. Write a 9000-CHECK-STATUS paragraph
      *>      that translates status codes to
      *>      human-readable messages.
      *>   4. Use 88-level conditions for the
      *>      end-of-file and valid/invalid flags.
      *>   5. Never let the program crash - handle
      *>      every error gracefully.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   DEFINE YOUR FILES HERE (input, clean, error)


       DATA DIVISION.
       FILE SECTION.
      *>   DEFINE YOUR RECORD LAYOUTS HERE


       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES, COUNTERS,
      *>   AND ERROR TRACKING FIELDS HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
