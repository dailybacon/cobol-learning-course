       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-02.
       AUTHOR. LESLIE.

      *> ============================================
      *> HOMEWORK 2: Variables & User Input
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a "Mad Libs" style story generator.
      *>   Ask the user for several words, then
      *>   plug them into a short story.
      *>
      *>   Example interaction:
      *>
      *>   Enter an animal: dog
      *>   Enter a color: purple
      *>   Enter a food: pizza
      *>   Enter a number: 42
      *>   Enter a verb (past tense): danced
      *>
      *>   --- YOUR STORY ---
      *>   Once upon a time, a purple dog
      *>   danced all the way to the store.
      *>   It bought 42 slices of pizza
      *>   and lived happily ever after.
      *>
      *> REQUIREMENTS:
      *>   1. Declare at least 5 variables in
      *>      WORKING-STORAGE SECTION.
      *>   2. Use PIC X(n) for text variables.
      *>   3. Use PIC 9(n) for the number.
      *>   4. Use ACCEPT to read each input.
      *>   5. Use DISPLAY to show the story,
      *>      mixing literals and variables.
      *>
      *> HINTS:
      *>   - PIC X(15) holds up to 15 characters.
      *>   - You can combine text and variables:
      *>     DISPLAY "The " WS-COLOR " " WS-ANIMAL.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PROFESSION PIC X(15).
       01 WS-NAME PIC X(10).
       01 WS-MOT PIC X(10).
       01 WS-ACTION PIC X(10).
       01 WS-NUMBER PIC 9(3).
       01 WS-ANIMAL PIC X(10).

       PROCEDURE DIVISION.
           DISPLAY "Enter a profession:".
           ACCEPT WS-PROFESSION.

           DISPLAY "Enter a person's name:".
           ACCEPT WS-NAME.

           DISPLAY "Enter a method of transportation:".
           ACCEPT WS-MOT.

           DISPLAY "Enter an action:".
           ACCEPT WS-ACTION.

           DISPLAY "Enter a positive integer:".
           ACCEPT WS-NUMBER.

           DISPLAY "Enter the name of a singular animal:".
           ACCEPT WS-ANIMAL.

           DISPLAY "ONCE THERE WAS A "
               WS-PROFESSION " NAMED " WS-NAME ".".
           DISPLAY "HE WAS RIDING HIS "
               WS-MOT " ONE DAY WHEN HE " WS-ACTION ".".
           DISPLAY "THEN, " WS-NUMBER " " WS-ANIMAL
               "S ATE HIM.".


           STOP RUN.
