       IDENTIFICATION DIVISION.
           PROGRAM-ID. BUSINESSCARD.
           AUTHOR. LESLIE.

      *> ============================================
      *> HOMEWORK 1: Program Structure & DISPLAY
      *> ============================================
      *> ASSIGNMENT:
      *>   Create a program that displays your own
      *>   business card to the screen.
      *>
      *>   Your output should look like this:
      *>
      *>   +-------------------------------+
      *>   |  [Your Name]                  |
      *>   |  [Your Job Title]             |
      *>   |  [Your Company]               |
      *>   |                               |
      *>   |  Phone: [Your Phone]          |
      *>   |  Email: [Your Email]          |
      *>   +-------------------------------+
      *>
      *> REQUIREMENTS:
      *>   1. Use ONLY the IDENTIFICATION DIVISION
      *>      and PROCEDURE DIVISION (no variables).
      *>   2. Use DISPLAY for every line of output.
      *>   3. Make the box borders line up.
      *>   4. Include at least 6 DISPLAY statements.
      *>   5. End with STOP RUN.
      *>
      *> BONUS:
      *>   Add a second business card for a coworker
      *>   with a blank line between the two cards.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       PROCEDURE DIVISION.
           DISPLAY "+----------------------------------------+".
           DISPLAY "|  Leslie                                |".
           DISPLAY "|  Desktop Technician Tier I             |".
           DISPLAY "|  Blimp Dimmy                           |".
           DISPLAY "|                                        |".
           DISPLAY "|  Phone: 648-291-9999                   |".
           DISPLAY "|  Email: leslie@email.com               |".
           DISPLAY "+----------------------------------------+".
           DISPLAY " ".
           DISPLAY "+----------------------------------------+".
           DISPLAY "|  Bim Glimbas                           |".
           DISPLAY "|  Desktop Technician Tier II            |".
           DISPLAY "|  Blimp Dimmy                           |".
           DISPLAY "|                                        |".
           DISPLAY "|  Phone: 555-123-4567                   |".
           DISPLAY "|  Email: bim@email.com                  |".
           DISPLAY "+----------------------------------------+".
           STOP RUN.
