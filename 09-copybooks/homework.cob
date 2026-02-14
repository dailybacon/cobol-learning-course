       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-09.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 9: Copybooks (COPY & REPLACE)
      *> ============================================
      *> ASSIGNMENT:
      *>   Build an inventory system using copybooks
      *>   to share a common product record layout.
      *>
      *>   STEP 1 - CREATE A COPYBOOK:
      *>   Create a file called "product.cpy" with
      *>   this record layout:
      *>     01 WS-PROD-RECORD.
      *>        05 WS-PROD-SKU     PIC X(8).
      *>        05 WS-PROD-NAME    PIC X(20).
      *>        05 WS-PROD-PRICE   PIC 9(5)V99.
      *>        05 WS-PROD-QTY     PIC 9(4).
      *>
      *>   STEP 2 - USE THE COPYBOOK:
      *>   In this program, COPY the product layout
      *>   THREE times using REPLACING to create:
      *>     - WS-PROD1-xxx  (product 1)
      *>     - WS-PROD2-xxx  (product 2)
      *>     - WS-PROD3-xxx  (product 3)
      *>
      *>   STEP 3 - THE PROGRAM:
      *>   1. Fill all 3 products with hardcoded data.
      *>   2. Display all 3 in a formatted table.
      *>   3. Calculate and show the total inventory
      *>      value (price * qty for each, summed).
      *>
      *>   Example output:
      *>
      *>   === Inventory Report ===
      *>   SKU      Name                 Price    Qty
      *>   -------- -------------------- -------- ----
      *>   WDG-1001 Widget               $19.99    100
      *>   GDG-2001 Gadget               $49.99     50
      *>   SPR-3001 Sprocket              $5.99    500
      *>
      *>   Total Inventory Value: $6,993.50
      *>
      *> REQUIREMENTS:
      *>   1. Create product.cpy as a separate file.
      *>   2. Use COPY "product.cpy" REPLACING
      *>      three times in WORKING-STORAGE.
      *>   3. Use the REPLACING clause to give each
      *>      copy unique field names.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x -I . homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   COPY YOUR PRODUCT LAYOUT HERE (3 times)
      *>   with REPLACING for each one.


       01 WS-TOTAL-VALUE    PIC 9(7)V99.
       01 WS-DISP-VALUE     PIC $Z(5),ZZ9.99.

       PROCEDURE DIVISION.
      *>   YOUR CODE GOES HERE


           STOP RUN.
