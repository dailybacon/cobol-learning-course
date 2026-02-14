       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-04.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 4: EVALUATE & 88-Level Conditions
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a movie ticket pricing system.
      *>
      *>   1. Ask for the customer's age.
      *>   2. Ask what day of the week it is (1-7).
      *>   3. Ask if it's a matinee showing (Y/N).
      *>   4. Determine the ticket price using
      *>      these rules:
      *>
      *>      AGE PRICING (use 88-levels):
      *>        Child (0-12):   $8.00
      *>        Teen  (13-17):  $10.00
      *>        Adult (18-64):  $14.00
      *>        Senior (65+):   $9.00
      *>
      *>      DAY DISCOUNT (use EVALUATE):
      *>        Tuesday = half price ("Discount Tues!")
      *>        Sat/Sun = add $2.00 surcharge
      *>
      *>      MATINEE DISCOUNT:
      *>        If matinee = Y, subtract $3.00
      *>        (but never go below $5.00)
      *>
      *>   5. Display the final ticket price and
      *>      which discounts/surcharges applied.
      *>
      *> REQUIREMENTS:
      *>   1. Use 88-level conditions for age groups.
      *>   2. Use EVALUATE for the day of the week.
      *>   3. Use IF with 88-level names (not numbers)
      *>      e.g., IF IS-CHILD instead of
      *>            IF WS-AGE < 13.
      *>   4. Use SET condition-name TO TRUE.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES AND 88-LEVELS HERE


       PROCEDURE DIVISION.
      *>   YOUR CODE GOES HERE


           STOP RUN.
