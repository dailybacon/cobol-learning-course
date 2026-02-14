       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCREENS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 17: Screen Section (TUI Programs)
      *> ============================================
      *> The SCREEN SECTION lets you build text-based
      *> user interfaces with positioned fields,
      *> colors, and input validation - right in
      *> COBOL.
      *>
      *> NEW CONCEPTS:
      *>   - SCREEN SECTION: defines screen layouts
      *>     with row/column positioning.
      *>
      *>   - Screen item clauses:
      *>     LINE n COL n     = position on screen
      *>     VALUE "text"     = display literal text
      *>     PIC X(n)         = input/output field
      *>     USING variable   = bind to a variable
      *>                        (both display & input)
      *>     FROM variable    = display only
      *>     TO variable      = input only
      *>     HIGHLIGHT        = bold/bright text
      *>     REVERSE-VIDEO    = inverted colors
      *>     BLANK SCREEN     = clear the screen
      *>     FOREGROUND-COLOR = text color (0-7)
      *>     BACKGROUND-COLOR = background color
      *>
      *>   - Colors (0-7):
      *>     0=Black, 1=Blue, 2=Green, 3=Cyan
      *>     4=Red, 5=Magenta, 6=Yellow/Brown, 7=White
      *>
      *>   - DISPLAY screen-name / ACCEPT screen-name
      *>     Shows screen / waits for input on screen.
      *>
      *>   - SPECIAL-NAMES.
      *>       CRT STATUS IS key-status.
      *>     Captures which key the user pressed
      *>     (Enter, F1-F12, Escape, etc.)
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x screens.cob -o screens
      *>   ./screens
      *> ============================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-KEY-STATUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-KEY-STATUS     PIC 9(4).
       01 WS-NAME           PIC X(25).
       01 WS-EMAIL          PIC X(30).
       01 WS-PHONE          PIC X(12).
       01 WS-CHOICE         PIC 9 VALUE 0.
       01 WS-MSG            PIC X(40) VALUE SPACES.
       01 WS-CONTINUE       PIC X VALUE "Y".
          88 USER-DONE      VALUE "N" "n".

      *> ============================================
      *> SCREEN SECTION: define the UI layout
      *> ============================================
       SCREEN SECTION.

      *> --- Main menu screen ---
       01 MENU-SCREEN.
          05 BLANK SCREEN.
          05 LINE 2  COL 20 VALUE "==========================="
             FOREGROUND-COLOR 3.
          05 LINE 3  COL 20 VALUE "   CONTACT MANAGER v1.0"
             FOREGROUND-COLOR 3 HIGHLIGHT.
          05 LINE 4  COL 20 VALUE "==========================="
             FOREGROUND-COLOR 3.
          05 LINE 7  COL 20 VALUE "1. Enter new contact".
          05 LINE 8  COL 20 VALUE "2. View contact".
          05 LINE 9  COL 20 VALUE "3. Exit".
          05 LINE 11 COL 20 VALUE "Choice: ".
          05 LINE 11 COL 28 PIC 9 USING WS-CHOICE
             FOREGROUND-COLOR 6 HIGHLIGHT.
          05 LINE 13 COL 20 PIC X(40) FROM WS-MSG
             FOREGROUND-COLOR 4.

      *> --- Data entry screen ---
       01 ENTRY-SCREEN.
          05 BLANK SCREEN.
          05 LINE 2  COL 15 VALUE "--- Enter New Contact ---"
             FOREGROUND-COLOR 2 HIGHLIGHT.
          05 LINE 5  COL 10 VALUE "Name:  ".
          05 LINE 5  COL 17 PIC X(25) USING WS-NAME
             FOREGROUND-COLOR 7 REVERSE-VIDEO.
          05 LINE 7  COL 10 VALUE "Email: ".
          05 LINE 7  COL 17 PIC X(30) USING WS-EMAIL
             FOREGROUND-COLOR 7 REVERSE-VIDEO.
          05 LINE 9  COL 10 VALUE "Phone: ".
          05 LINE 9  COL 17 PIC X(12) USING WS-PHONE
             FOREGROUND-COLOR 7 REVERSE-VIDEO.
          05 LINE 12 COL 10
             VALUE "Press ENTER to save."
             FOREGROUND-COLOR 3.

      *> --- Display contact screen ---
       01 DISPLAY-SCREEN.
          05 BLANK SCREEN.
          05 LINE 2  COL 15 VALUE "--- Contact Details ---"
             FOREGROUND-COLOR 2 HIGHLIGHT.
          05 LINE 5  COL 10 VALUE "Name:  ".
          05 LINE 5  COL 17 PIC X(25) FROM WS-NAME
             FOREGROUND-COLOR 6 HIGHLIGHT.
          05 LINE 7  COL 10 VALUE "Email: ".
          05 LINE 7  COL 17 PIC X(30) FROM WS-EMAIL
             FOREGROUND-COLOR 6 HIGHLIGHT.
          05 LINE 9  COL 10 VALUE "Phone: ".
          05 LINE 9  COL 17 PIC X(12) FROM WS-PHONE
             FOREGROUND-COLOR 6 HIGHLIGHT.
          05 LINE 12 COL 10
             VALUE "Press ENTER to return to menu."
             FOREGROUND-COLOR 3.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE SPACES TO WS-NAME.
           MOVE SPACES TO WS-EMAIL.
           MOVE SPACES TO WS-PHONE.

           PERFORM UNTIL USER-DONE
               MOVE 0 TO WS-CHOICE
               DISPLAY MENU-SCREEN
               ACCEPT MENU-SCREEN

               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM 1000-ENTER-CONTACT
                   WHEN 2
                       PERFORM 2000-VIEW-CONTACT
                   WHEN 3
                       MOVE "N" TO WS-CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again."
                           TO WS-MSG
               END-EVALUATE
           END-PERFORM.

           DISPLAY SPACE BLANK SCREEN.
           DISPLAY "Goodbye!".
           STOP RUN.

       1000-ENTER-CONTACT.
           DISPLAY ENTRY-SCREEN.
           ACCEPT ENTRY-SCREEN.
           MOVE "Contact saved!" TO WS-MSG.

       2000-VIEW-CONTACT.
           IF WS-NAME = SPACES
               MOVE "No contact entered yet!" TO WS-MSG
           ELSE
               DISPLAY DISPLAY-SCREEN
               ACCEPT DISPLAY-SCREEN
               MOVE SPACES TO WS-MSG
           END-IF.
