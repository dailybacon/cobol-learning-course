       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-10.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 10: Sequential File I/O
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a student roster system that writes
      *>   student records to a file and then reads
      *>   them back to produce a formatted report.
      *>
      *>   PART 1 - DATA ENTRY:
      *>   1. Ask the user how many students to enter
      *>      (max 20).
      *>   2. For each student, accept:
      *>      - Student ID  (4 chars, e.g. S001)
      *>      - Name        (20 chars)
      *>      - Major       (15 chars)
      *>      - GPA         (1 digit, V, 2 digits)
      *>   3. Write each record to "roster.dat".
      *>
      *>   PART 2 - REPORT:
      *>   1. Read "roster.dat" back in.
      *>   2. Write a formatted report to
      *>      "roster-report.txt" with:
      *>      - A header with column titles
      *>      - Each student's data on one line
      *>      - A footer showing:
      *>        * Total students
      *>        * Average GPA
      *>        * Count of students with GPA >= 3.50
      *>          (Dean's List count)
      *>   3. Display the report to the screen too.
      *>
      *>   Example report:
      *>
      *>   === STUDENT ROSTER REPORT ===
      *>   ID   Name                 Major           GPA
      *>   ---- -------------------- --------------- ----
      *>   S001 Jane Smith           Computer Sci    3.80
      *>   S002 Bob Jones            Mathematics     3.20
      *>   S003 Alice Park           Engineering     3.90
      *>   ---------------------------------------------------
      *>   Total Students:   3
      *>   Average GPA:      3.63
      *>   Dean's List:      2
      *>
      *> REQUIREMENTS:
      *>   1. Use SELECT / ASSIGN TO for two files
      *>      (data file and report file).
      *>   2. Use FD and record layouts in FILE
      *>      SECTION for both files.
      *>   3. Use the standard COBOL read loop
      *>      (read first, then PERFORM UNTIL EOF).
      *>   4. Use 88-level for end-of-file flag.
      *>   5. OPEN OUTPUT to write, OPEN INPUT to
      *>      read, CLOSE when done.
      *>
      *> HINTS:
      *>   - Record layout for the data file:
      *>     FD ROSTER-FILE.
      *>     01 ROSTER-RECORD.
      *>        05 RS-ID     PIC X(4).
      *>        05 FILLER    PIC X VALUE ",".
      *>        05 RS-NAME   PIC X(20).
      *>        05 FILLER    PIC X VALUE ",".
      *>        05 RS-MAJOR  PIC X(15).
      *>        05 FILLER    PIC X VALUE ",".
      *>        05 RS-GPA    PIC 9V99.
      *>
      *>   - Remember to CLOSE files before
      *>     reopening them in a different mode.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   DEFINE YOUR FILES HERE


       DATA DIVISION.
       FILE SECTION.
      *>   DEFINE YOUR RECORD LAYOUTS HERE


       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES HERE


       PROCEDURE DIVISION.
      *>   YOUR CODE GOES HERE


           STOP RUN.
