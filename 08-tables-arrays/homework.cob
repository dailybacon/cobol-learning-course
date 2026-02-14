       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-08.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 8: Tables and Arrays
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a student grade tracker.
      *>
      *>   1. Ask how many students (max 10).
      *>   2. For each student, accept their name
      *>      and 3 test scores.
      *>   3. Calculate each student's average.
      *>   4. Display a grade report showing:
      *>      - Each student's name, 3 scores, avg
      *>      - The class average (all students)
      *>      - The highest and lowest averages
      *>      - Which student had the highest average
      *>
      *>   Example:
      *>
      *>   How many students? 3
      *>   Student 1 name: Alice
      *>   Score 1: 90
      *>   Score 2: 85
      *>   Score 3: 92
      *>   ...
      *>
      *>   === GRADE REPORT ===
      *>   Name       S1   S2   S3   Avg
      *>   ---------- ---- ---- ---- -----
      *>   Alice       90   85   92  89.00
      *>   Bob         78   82   80  80.00
      *>   Carol       95   91   88  91.33
      *>
      *>   Class Average: 86.78
      *>   Highest: Carol (91.33)
      *>   Lowest:  Bob (80.00)
      *>
      *> REQUIREMENTS:
      *>   1. Use a table with OCCURS for students.
      *>   2. Each student entry needs: name (X(10)),
      *>      3 scores (9(3)), and average (9(3)V99).
      *>   3. Use PERFORM VARYING to fill the table.
      *>   4. Use PERFORM VARYING to find highest
      *>      and lowest averages.
      *>   5. Use a pre-initialized table for letter
      *>      grades (optional bonus):
      *>      90+ = A, 80+ = B, 70+ = C, etc.
      *>
      *> HINTS:
      *>   - Table structure:
      *>     01 WS-STUDENT-TABLE.
      *>        05 WS-STUDENT OCCURS 10 TIMES.
      *>           10 WS-STU-NAME  PIC X(10).
      *>           10 WS-STU-SCORE PIC 9(3)
      *>              OCCURS 3 TIMES.
      *>           10 WS-STU-AVG   PIC 9(3)V99.
      *>   - Access: WS-STU-SCORE(student#, score#)
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES AND TABLES HERE


       PROCEDURE DIVISION.
      *>   YOUR CODE GOES HERE


           STOP RUN.
