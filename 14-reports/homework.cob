       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-14.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 14: Report Formatting
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a monthly invoice generator.
      *>   Read line items from a data file and
      *>   produce a professionally formatted invoice
      *>   written to an output file.
      *>
      *>   STEP 1 - CREATE INPUT DATA:
      *>   Write "invoice-items.dat" with at least
      *>   8 line items in this format:
      *>     Item Code    PIC X(6)
      *>     Description  PIC X(25)
      *>     Quantity     PIC 9(3)
      *>     Unit Price   PIC 9(5)V99
      *>
      *>   STEP 2 - GENERATE INVOICE:
      *>   Read the items and write "invoice.txt"
      *>   with this layout:
      *>
      *>   +--------------------------------------------------+
      *>   |              ACME CORPORATION                     |
      *>   |              INVOICE #2026-0213                   |
      *>   |              Date: 02/13/2026                     |
      *>   +--------------------------------------------------+
      *>   | Bill To: [hardcode a customer name/address]       |
      *>   +--------------------------------------------------+
      *>   | Code   Description               Qty  Price  Ext |
      *>   | ------ ------------------------- --- ------- -----|
      *>   | WDG-01 Standard Widget             10  $19.99  ...|
      *>   | GDG-02 Deluxe Gadget                5  $49.99  ...|
      *>   | ...                                               |
      *>   +--------------------------------------------------+
      *>   |                          Subtotal:    $X,XXX.XX   |
      *>   |                          Tax (8.5%):     $XXX.XX  |
      *>   |                          TOTAL:       $X,XXX.XX   |
      *>   +--------------------------------------------------+
      *>   | Thank you for your business!                      |
      *>   +--------------------------------------------------+
      *>
      *>   KEY COLUMNS:
      *>   - Extended price = Quantity * Unit Price
      *>   - Subtotal = sum of all extended prices
      *>   - Tax = Subtotal * 0.085
      *>   - Total = Subtotal + Tax
      *>
      *> REQUIREMENTS:
      *>   1. Use separate 01-level records for each
      *>      line type (header, detail, totals).
      *>   2. Use edited PIC clauses for currency:
      *>      PIC $Z(3),ZZ9.99 or similar.
      *>   3. Use PIC 99/99/9999 for the date display.
      *>   4. Use WRITE ... AFTER ADVANCING for
      *>      spacing control.
      *>   5. Read from a data file (not hardcoded
      *>      in WORKING-STORAGE).
      *>   6. Display the invoice to screen AND
      *>      write to the output file.
      *>
      *> BONUS:
      *>   Add page break logic: if more than 20
      *>   detail lines, start a new page with
      *>   headers reprinted.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   DEFINE YOUR FILES HERE


       DATA DIVISION.
       FILE SECTION.
      *>   DEFINE YOUR RECORD LAYOUTS HERE


       WORKING-STORAGE SECTION.
      *>   DEFINE YOUR REPORT LINE TEMPLATES HERE
      *>   (header lines, detail line, total lines)


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
