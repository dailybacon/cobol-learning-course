       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-03.
       AUTHOR. LESLIE.

      *> ============================================
      *> HOMEWORK 3: Arithmetic & Conditionals
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a tip calculator for a restaurant.
      *>
      *>   1. Ask for the bill amount.
      *>   2. Ask how many people are splitting it.
      *>   3. Calculate and display:
      *>      - 15% tip, 20% tip, and 25% tip
      *>      - Total with each tip amount
      *>      - Per-person cost for each tip level
      *>   4. If the bill is over $100, display
      *>      "Thanks for the generous visit!"
      *>      If under $20, display
      *>      "Quick bite today!"
      *>
      *>   Example output:
      *>
      *>   Bill amount: 80.00
      *>   Number of people: 4
      *>
      *>   --- Tip Breakdown ---
      *>   15% tip:  $12.00  Total:  $92.00
      *>   20% tip:  $16.00  Total:  $96.00
      *>   25% tip:  $20.00  Total: $100.00
      *>
      *>   --- Per Person ---
      *>   At 15%: $23.00 each
      *>   At 20%: $24.00 each
      *>   At 25%: $25.00 each
      *>
      *> REQUIREMENTS:
      *>   1. Use COMPUTE for the calculations.
      *>   2. Use PIC 9(n)V99 for money amounts.
      *>   3. Use an edited PIC for display
      *>      (e.g., PIC $Z(4)9.99).
      *>   4. Use IF/ELSE for the message.
      *>   5. Handle division (people splitting).
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-BILL-TOTAL PIC 9(5)V99.
       01 WS-PPL-TOTAL PIC 9(3).
       01 WS-PPL-TOTAL-DISPLAY PIC Z(2)9.
       01 WS-TIP PIC 9(5)V99.
       01 WS-BILL-TIP-TOTAL PIC 9(5)V99.
       01 WS-DISPLAY PIC $$$$$$9.99.
       01 WS-DISPLAY2 PIC $$$$$$9.99.


       PROCEDURE DIVISION.
           DISPLAY " ".
           DISPLAY "Bill Amount:".
           ACCEPT WS-BILL-TOTAL.
           MOVE WS-BILL-TOTAL TO WS-DISPLAY.

           DISPLAY "# of People:".
           ACCEPT WS-PPL-TOTAL.
           
           IF WS-PPL-TOTAL = 0
               DISPLAY 'CANNOT HAVE 0 PEOPLE'
           ELSE
           MOVE WS-PPL-TOTAL TO WS-PPL-TOTAL-DISPLAY
           
           DISPLAY " "

           DISPLAY "Bill amount: " WS-DISPLAY
           DISPLAY "Number of people: " WS-PPL-TOTAL-DISPLAY

           DISPLAY " "

           DISPLAY "--- Tip Breakdown ---"
           COMPUTE WS-TIP = WS-BILL-TOTAL * 0.15
           MOVE WS-TIP TO WS-DISPLAY
           COMPUTE WS-BILL-TIP-TOTAL = WS-BILL-TOTAL + WS-TIP
           MOVE WS-BILL-TIP-TOTAL TO WS-DISPLAY2
           DISPLAY "15% tip: " WS-DISPLAY " Total: " WS-DISPLAY2

           COMPUTE WS-TIP = WS-BILL-TOTAL * 0.20
           MOVE WS-TIP TO WS-DISPLAY
           COMPUTE WS-BILL-TIP-TOTAL = WS-BILL-TOTAL + WS-TIP
           MOVE WS-BILL-TIP-TOTAL TO WS-DISPLAY2
           DISPLAY "20% tip: " WS-DISPLAY " Total: " WS-DISPLAY2

           COMPUTE WS-TIP = WS-BILL-TOTAL * 0.25
           MOVE WS-TIP TO WS-DISPLAY
           COMPUTE WS-BILL-TIP-TOTAL = WS-BILL-TOTAL + WS-TIP
           MOVE WS-BILL-TIP-TOTAL TO WS-DISPLAY2
           DISPLAY "25% tip: " WS-DISPLAY " Total: " WS-DISPLAY2

           DISPLAY ' '

           DISPLAY "--- Per Person ---"
           COMPUTE WS-BILL-TIP-TOTAL = (WS-BILL-TOTAL * 1.15) /
              WS-PPL-TOTAL
           MOVE WS-BILL-TIP-TOTAL TO WS-DISPLAY
           DISPLAY 'At 15%: ' WS-DISPLAY ' each'

           COMPUTE WS-BILL-TIP-TOTAL = (WS-BILL-TOTAL * 1.20) /
              WS-PPL-TOTAL
           MOVE WS-BILL-TIP-TOTAL TO WS-DISPLAY
           DISPLAY 'At 20%: ' WS-DISPLAY ' each'

           COMPUTE WS-BILL-TIP-TOTAL = (WS-BILL-TOTAL * 1.25) /
              WS-PPL-TOTAL
           MOVE WS-BILL-TIP-TOTAL TO WS-DISPLAY
           DISPLAY 'At 25%: ' WS-DISPLAY ' each'

           DISPLAY ' '

           IF WS-BILL-TOTAL > 100
              DISPLAY 'Thanks for the generous visit!'
           END-IF
           IF WS-BILL-TOTAL < 20
              DISPLAY 'Quick bite today!'
           END-IF
           END-IF.
           STOP RUN.
