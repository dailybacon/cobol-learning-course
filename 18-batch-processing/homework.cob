       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-18.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 18: Real-World Batch Processing
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a complete end-of-month payroll
      *>   batch processing system. This is the
      *>   capstone - use EVERYTHING you've learned.
      *>
      *> === INPUT FILES ===
      *>
      *>   FILE 1: "employees.dat" (master file)
      *>     Emp ID         PIC X(5)
      *>     Name           PIC X(25)
      *>     Department     PIC X(15)
      *>     Hourly Rate    PIC 9(3)V99
      *>     Tax Bracket    PIC 9  (1=10%, 2=20%, 3=30%)
      *>
      *>   FILE 2: "timesheets.dat" (transaction file)
      *>     Emp ID         PIC X(5)
      *>     Week Number    PIC 9(1)  (1-4)
      *>     Hours Worked   PIC 9(2)V9
      *>     Overtime Hours PIC 9(2)V9
      *>
      *>   Create both files with at least 5
      *>   employees and 4 weeks of timesheets each
      *>   (20+ timesheet records). Include some
      *>   intentional errors:
      *>     - A timesheet with an Emp ID that
      *>       doesn't exist in the master file
      *>     - A timesheet with hours > 60
      *>     - A timesheet with overtime but 0 regular
      *>
      *> === PROCESSING RULES ===
      *>
      *>   1. Read each timesheet record.
      *>   2. Look up the employee in the master file
      *>      (use a table loaded at startup, or an
      *>      indexed file - your choice).
      *>   3. Validate:
      *>      - Employee exists
      *>      - Hours <= 60
      *>      - If overtime > 0, regular hours >= 40
      *>   4. Calculate pay:
      *>      - Regular pay = hours * rate
      *>      - Overtime pay = OT hours * rate * 1.5
      *>      - Gross pay = regular + overtime
      *>      - Tax = gross * tax rate (from bracket)
      *>      - Net pay = gross - tax
      *>   5. Accumulate monthly totals per employee.
      *>
      *> === OUTPUT FILES ===
      *>
      *>   FILE 3: "paystubs.txt" (report)
      *>   A formatted pay stub for each employee:
      *>
      *>   +-----------------------------------------+
      *>   | PAY STUB - January 2026                 |
      *>   | Employee: E0001 Jane Smith              |
      *>   | Department: Engineering                 |
      *>   +-----------------------------------------+
      *>   | Week | Reg Hrs | OT Hrs | Reg Pay | OT  |
      *>   |-----------------------------------------|
      *>   |  1   |  40.0   |   5.0  | $X,XXX  | $XX |
      *>   |  2   |  40.0   |   0.0  | $X,XXX  | $0  |
      *>   |  3   |  ...    |        |         |     |
      *>   |  4   |  ...    |        |         |     |
      *>   +-----------------------------------------+
      *>   | Monthly Gross:    $XX,XXX.XX            |
      *>   | Tax (XX%):        -$X,XXX.XX            |
      *>   | NET PAY:          $XX,XXX.XX            |
      *>   +-----------------------------------------+
      *>
      *>   FILE 4: "payroll-summary.txt" (report)
      *>   A department summary report:
      *>
      *>   === MONTHLY PAYROLL SUMMARY ===
      *>   Department       Employees  Gross Pay    Net Pay
      *>   --------------- ---------- ----------- -----------
      *>   Engineering            3   $XX,XXX.XX  $XX,XXX.XX
      *>   Sales                  2   $XX,XXX.XX  $XX,XXX.XX
      *>   ------------------------------------------------
      *>   COMPANY TOTAL          5   $XX,XXX.XX  $XX,XXX.XX
      *>
      *>   FILE 5: "errors.log"
      *>   All rejected timesheet records with reasons.
      *>
      *> === PROCESSING FLOW ===
      *>   (Use this as your paragraph structure)
      *>
      *>   0000-MAIN
      *>     1000-INITIALIZE
      *>       1100-LOAD-EMPLOYEES (read master file
      *>            into a table)
      *>       1200-OPEN-FILES
      *>     2000-PROCESS-TIMESHEETS
      *>       2100-VALIDATE-TIMESHEET
      *>       2200-CALCULATE-PAY
      *>       2300-ACCUMULATE-TOTALS
      *>       2400-LOG-ERROR
      *>     3000-GENERATE-PAYSTUBS
      *>     4000-GENERATE-SUMMARY
      *>     5000-CLEANUP
      *>
      *> REQUIREMENTS:
      *>   1. Read from sequential files (lesson 10).
      *>   2. Use tables to store employee data and
      *>      accumulate weekly pay (lesson 8).
      *>   3. Use paragraphs for clear program
      *>      structure (lesson 6).
      *>   4. Use EVALUATE and 88-levels for
      *>      validation (lesson 4).
      *>   5. Use edited PIC clauses for all money
      *>      amounts in reports (lesson 14).
      *>   6. Check FILE STATUS after every I/O
      *>      and handle errors (lesson 15).
      *>   7. Use STRING for building report lines
      *>      (lesson 7).
      *>   8. Use COMPUTE with ON SIZE ERROR for
      *>      pay calculations (lesson 15).
      *>
      *> BONUS:
      *>   - SORT the timesheets by employee ID
      *>     before processing (lesson 12).
      *>   - Put the employee record layout in a
      *>     copybook (lesson 9).
      *>   - Move the pay calculation into a
      *>     subprogram (lesson 13).
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   DEFINE ALL YOUR FILES HERE


       DATA DIVISION.
       FILE SECTION.
      *>   DEFINE YOUR RECORD LAYOUTS HERE


       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR TABLES, ACCUMULATORS,
      *>   REPORT TEMPLATES, AND FLAGS HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
      *>   (Follow the structure outlined above)
