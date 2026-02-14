       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-05.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 5: Loops with PERFORM
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a number guessing game.
      *>
      *>   1. The "secret number" is 42 (hardcoded).
      *>   2. The player gets up to 7 guesses.
      *>   3. After each guess, tell them:
      *>      "Too high!" or "Too low!" or "Correct!"
      *>   4. Track how many guesses they used.
      *>   5. After the game, display:
      *>      - Whether they won or lost
      *>      - How many guesses they used
      *>      - A rating based on guesses:
      *>        1-2 guesses: "Lucky!"
      *>        3-4 guesses: "Nice job!"
      *>        5-6 guesses: "Cutting it close!"
      *>        7 guesses:   "Just barely!"
      *>        Didn't win:  "Better luck next time!"
      *>
      *>   Example interaction:
      *>
      *>   === Guess the Number (1-100) ===
      *>   You have 7 guesses.
      *>   Guess #1: 50
      *>   Too high!
      *>   Guess #2: 25
      *>   Too low!
      *>   Guess #3: 42
      *>   Correct! You got it in 3 guesses.
      *>   Rating: Nice job!
      *>
      *> REQUIREMENTS:
      *>   1. Use PERFORM UNTIL for the game loop.
      *>   2. Use a counter variable for guess count.
      *>   3. Use PERFORM paragraph-name for the
      *>      rating display (separate paragraph).
      *>   4. The loop must stop on correct guess
      *>      OR when guesses run out.
      *>
      *> BONUS:
      *>   Use FUNCTION RANDOM to generate a random
      *>   secret number between 1 and 100:
      *>   COMPUTE WS-SECRET =
      *>     FUNCTION RANDOM * 100 + 1.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES HERE


       PROCEDURE DIVISION.
      *>   YOUR CODE GOES HERE


           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
