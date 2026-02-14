       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-17.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 17: Screen Section (TUI Programs)
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a personal expense tracker with a
      *>   full text-based user interface using
      *>   SCREEN SECTION.
      *>
      *>   SCREEN 1 - MAIN MENU:
      *>   Row 2: title "EXPENSE TRACKER" in cyan,
      *>          highlighted, centered.
      *>   Row 4-8: menu options:
      *>     1. Add Expense
      *>     2. View Today's Expenses
      *>     3. View Summary
      *>     4. Exit
      *>   Row 10: "Choice: " with input field
      *>           in yellow highlight.
      *>   Row 12: status message in red (for errors)
      *>           or green (for confirmations).
      *>
      *>   SCREEN 2 - ADD EXPENSE:
      *>   Row 2: title "Add New Expense" in green.
      *>   Row 5: "Category: " with input (reverse-video)
      *>     (Food, Transport, Entertainment, Bills, Other)
      *>   Row 7: "Amount: $" with numeric input
      *>   Row 9: "Description: " with input (30 chars)
      *>   Row 12: "Press ENTER to save."
      *>
      *>   SCREEN 3 - EXPENSE LIST:
      *>   Row 2: title "Today's Expenses" in green.
      *>   Row 4: column headers (Category, Amount, Desc)
      *>   Row 5: dashes
      *>   Rows 6-15: up to 10 expense entries
      *>     (use FROM with table variables)
      *>   Row 17: "Total: $XX.XX" in yellow highlight.
      *>   Row 19: "Press ENTER to return."
      *>
      *>   SCREEN 4 - SUMMARY:
      *>   Row 2: title "Expense Summary" in green.
      *>   Show totals by category:
      *>   Row 5:  "Food:           $XXX.XX"
      *>   Row 6:  "Transport:      $XXX.XX"
      *>   Row 7:  "Entertainment:  $XXX.XX"
      *>   Row 8:  "Bills:          $XXX.XX"
      *>   Row 9:  "Other:          $XXX.XX"
      *>   Row 11: "GRAND TOTAL:    $XXX.XX" (highlight)
      *>   Row 12: "Expense count:  XX"
      *>   Row 14: "Press ENTER to return."
      *>
      *>   STORAGE:
      *>   Use a table to store up to 20 expenses:
      *>     01 WS-EXPENSE-TABLE.
      *>        05 WS-EXPENSE OCCURS 20 TIMES.
      *>           10 WS-EXP-CATEGORY PIC X(15).
      *>           10 WS-EXP-AMOUNT   PIC 9(5)V99.
      *>           10 WS-EXP-DESC     PIC X(30).
      *>
      *> REQUIREMENTS:
      *>   1. Define at least 4 screens in SCREEN
      *>      SECTION (menu, add, list, summary).
      *>   2. Use FOREGROUND-COLOR on at least 3
      *>      different colors.
      *>   3. Use REVERSE-VIDEO on input fields.
      *>   4. Use HIGHLIGHT on titles and totals.
      *>   5. Use BLANK SCREEN on each screen.
      *>   6. Use USING for input fields and FROM
      *>      for display-only fields.
      *>   7. Use DISPLAY screen / ACCEPT screen.
      *>   8. Track expenses in a COBOL table.
      *>
      *> BONUS:
      *>   Use SPECIAL-NAMES CRT STATUS to detect
      *>   if the user presses Escape and return to
      *>   the menu without saving.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-KEY-STATUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES AND TABLES HERE


       SCREEN SECTION.
      *>   DEFINE YOUR SCREENS HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
