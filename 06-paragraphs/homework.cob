       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-06.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 6: Paragraphs & Program Organization
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a mini banking system with a menu.
      *>   The program maintains a single account
      *>   balance and lets the user perform actions.
      *>
      *>   MENU:
      *>     1. Check Balance
      *>     2. Deposit
      *>     3. Withdraw
      *>     4. Transaction History (last 5)
      *>     5. Exit
      *>
      *>   RULES:
      *>   - Starting balance is $1,000.00
      *>   - Cannot withdraw more than the balance
      *>   - Deposits and withdrawals must be > $0
      *>   - Keep a count of total transactions
      *>   - Track the last 5 transaction amounts
      *>     (use a simple set of 5 variables,
      *>     shift them down when a new one comes in)
      *>
      *>   Example:
      *>
      *>   === Mini Bank ===
      *>   1. Check Balance
      *>   2. Deposit
      *>   3. Withdraw
      *>   4. Transaction History
      *>   5. Exit
      *>   Choice: 2
      *>   Amount to deposit: 250.00
      *>   Deposited $250.00. New balance: $1,250.00
      *>
      *> REQUIREMENTS:
      *>   1. Use a 0000-MAIN paragraph that PERFORMs
      *>      everything else.
      *>   2. Use numbered paragraphs:
      *>      1000-DISPLAY-MENU
      *>      2000-CHECK-BALANCE
      *>      3000-DEPOSIT
      *>      4000-WITHDRAW
      *>      5000-SHOW-HISTORY
      *>   3. Each action must be its own paragraph.
      *>   4. The menu loops until user picks Exit.
      *>   5. Use EVALUATE for the menu choice.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
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
