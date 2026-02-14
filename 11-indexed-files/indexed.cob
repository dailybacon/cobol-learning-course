       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEXED.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 11: Indexed File Organization
      *> ============================================
      *> Sequential files must be read in order.
      *> Indexed files let you jump directly to any
      *> record by its key - like a database table.
      *>
      *> NEW CONCEPTS:
      *>   - ORGANIZATION IS INDEXED
      *>     File has an index allowing random access.
      *>
      *>   - RECORD KEY IS field-name
      *>     The primary key for lookups. Must be
      *>     unique for each record.
      *>
      *>   - ACCESS MODE:
      *>     SEQUENTIAL = read in key order
      *>     RANDOM     = read/write by key value
      *>     DYNAMIC    = both (switch as needed)
      *>
      *>   - FILE STATUS: a 2-char variable that
      *>     receives a status code after each I/O.
      *>     "00" = success
      *>     "23" = record not found
      *>     "22" = duplicate key
      *>     "10" = end of file
      *>
      *>   - OPEN I-O file
      *>     Opens for both reading and writing.
      *>
      *>   - READ file KEY IS field
      *>     Random read by key value.
      *>
      *>   - REWRITE record
      *>     Updates the current record in place.
      *>
      *>   - DELETE file RECORD
      *>     Removes the current record.
      *>
      *>   - START file KEY >= field
      *>     Positions for sequential reading from
      *>     a specific key value.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x indexed.cob -o indexed
      *>   ./indexed
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCT-FILE
               ASSIGN TO "products.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD PRODUCT-FILE.
       01 PROD-RECORD.
          05 PROD-ID        PIC X(5).
          05 PROD-NAME      PIC X(20).
          05 PROD-PRICE     PIC 9(5)V99.
          05 PROD-QTY       PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS    PIC XX.
       01 WS-CHOICE         PIC 9.
       01 WS-CONTINUE       PIC X VALUE "Y".
          88 USER-CONTINUES VALUE "Y" "y".
       01 WS-DISP-PRICE     PIC $Z(4)9.99.
       01 WS-DISP-QTY       PIC Z(3)9.
       01 WS-EOF             PIC X VALUE "N".
          88 END-OF-FILE     VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM 1000-LOAD-SAMPLE-DATA.
           PERFORM 2000-MENU
               UNTIL NOT USER-CONTINUES.
           STOP RUN.

      *> --- Create file with sample data ---
       1000-LOAD-SAMPLE-DATA.
           OPEN OUTPUT PRODUCT-FILE.
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error creating file: "
                   WS-FILE-STATUS
               STOP RUN
           END-IF.

           MOVE "P001" TO PROD-ID.
           MOVE "Widget"  TO PROD-NAME.
           MOVE 19.99 TO PROD-PRICE.
           MOVE 100  TO PROD-QTY.
           WRITE PROD-RECORD.

           MOVE "P002" TO PROD-ID.
           MOVE "Gadget"  TO PROD-NAME.
           MOVE 49.99 TO PROD-PRICE.
           MOVE 50   TO PROD-QTY.
           WRITE PROD-RECORD.

           MOVE "P003" TO PROD-ID.
           MOVE "Doohickey" TO PROD-NAME.
           MOVE 9.99 TO PROD-PRICE.
           MOVE 200  TO PROD-QTY.
           WRITE PROD-RECORD.

           CLOSE PRODUCT-FILE.
           DISPLAY "Loaded 3 sample products.".

      *> --- Interactive menu ---
       2000-MENU.
           DISPLAY SPACES.
           DISPLAY "=== Product Database ===".
           DISPLAY "1. Look up product by ID".
           DISPLAY "2. List all products".
           DISPLAY "3. Update product price".
           DISPLAY "4. Quit".
           DISPLAY "Choice: ".
           ACCEPT WS-CHOICE.

           EVALUATE WS-CHOICE
               WHEN 1 PERFORM 3000-LOOKUP
               WHEN 2 PERFORM 4000-LIST-ALL
               WHEN 3 PERFORM 5000-UPDATE-PRICE
               WHEN 4 MOVE "N" TO WS-CONTINUE
               WHEN OTHER DISPLAY "Invalid choice."
           END-EVALUATE.

      *> --- Random read by key ---
       3000-LOOKUP.
           OPEN I-O PRODUCT-FILE.
           DISPLAY "Enter product ID (e.g. P001): ".
           ACCEPT PROD-ID.

           READ PRODUCT-FILE
               KEY IS PROD-ID
               INVALID KEY
                   DISPLAY "Product not found!"
               NOT INVALID KEY
                   PERFORM 9000-DISPLAY-PRODUCT
           END-READ.
           CLOSE PRODUCT-FILE.

      *> --- Sequential read of all records ---
       4000-LIST-ALL.
           OPEN INPUT PRODUCT-FILE.
           MOVE "N" TO WS-EOF.

           READ PRODUCT-FILE NEXT
               AT END SET END-OF-FILE TO TRUE
           END-READ.

           PERFORM UNTIL END-OF-FILE
               PERFORM 9000-DISPLAY-PRODUCT
               READ PRODUCT-FILE NEXT
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.
           CLOSE PRODUCT-FILE.

      *> --- Update a record ---
       5000-UPDATE-PRICE.
           OPEN I-O PRODUCT-FILE.
           DISPLAY "Enter product ID to update: ".
           ACCEPT PROD-ID.

           READ PRODUCT-FILE
               KEY IS PROD-ID
               INVALID KEY
                   DISPLAY "Product not found!"
                   CLOSE PRODUCT-FILE
                   GO TO 5000-EXIT
           END-READ.

           PERFORM 9000-DISPLAY-PRODUCT.
           DISPLAY "Enter new price: ".
           ACCEPT PROD-PRICE.
           REWRITE PROD-RECORD.

           IF WS-FILE-STATUS = "00"
               DISPLAY "Price updated!"
           ELSE
               DISPLAY "Update failed: " WS-FILE-STATUS
           END-IF.
           CLOSE PRODUCT-FILE.
       5000-EXIT.
           CONTINUE.

      *> --- Helper: display one product ---
       9000-DISPLAY-PRODUCT.
           MOVE PROD-PRICE TO WS-DISP-PRICE.
           MOVE PROD-QTY TO WS-DISP-QTY.
           DISPLAY "  " PROD-ID " | "
                   PROD-NAME " | "
                   WS-DISP-PRICE " | Qty: "
                   WS-DISP-QTY.
