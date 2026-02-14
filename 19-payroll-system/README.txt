       ============================================
        LESSON 19: Portfolio Project
        Full-Stack Batch Payroll System
       ============================================

        OVERVIEW
       ============================================
        Build a production-grade batch payroll
        system from scratch. No starter code. No
        skeleton. You design the file layouts, the
        program structure, the reports - everything.

        This is the kind of system that runs every
        two weeks at banks, hospitals, and government
        agencies. When you finish this, you can walk
        into an interview and explain a real system
        you built yourself.


        WHAT YOU ARE BUILDING
       ============================================
        A multi-program payroll suite with:

        PROGRAM 1: PAYMAST (Master File Maintenance)
          An interactive program to manage the
          employee master file. Supports add, update,
          delete, and lookup. Uses an indexed VSAM-
          style file with employee ID as the key.

        PROGRAM 2: PAYLOAD (Timesheet Loader)
          Reads a raw timesheet CSV file submitted
          by department managers. Validates every
          field. Writes clean records to a validated
          timesheet file. Writes rejects to an error
          file with rejection codes.

        PROGRAM 3: PAYCALC (Pay Calculator - Subprogram)
          A called subprogram that receives an
          employee record and timesheet data via
          LINKAGE SECTION, computes gross pay,
          deductions, and net pay, and returns the
          results. Called by PAYRUN for each employee.

        PROGRAM 4: PAYRUN (Main Batch Run)
          The core batch program. Sorts validated
          timesheets by employee ID. Reads each
          timesheet, looks up the employee in the
          master file, CALLs PAYCALC to compute pay,
          accumulates totals, and writes pay records
          to an output file.

        PROGRAM 5: PAYRPT (Report Generator)
          Reads the pay output file and generates
          three formatted reports:
            - Individual pay stubs
            - Department summary
            - Company-wide totals with YTD tracking


        FILE LAYOUTS (Design These Yourself)
       ============================================
        You need to design record layouts for:

        1. Employee Master File (indexed)
           Think about what fields a real employer
           needs. At minimum:
             - Employee ID (key)
             - Name (first and last, or combined)
             - Department
             - Job title
             - Pay rate (hourly or salary)
             - Pay type (H=hourly, S=salary)
             - Federal tax bracket
             - State tax rate
             - Year-to-date gross
             - Year-to-date net
             - Hire date
             - Status (A=active, T=terminated)

        2. Raw Timesheet File (sequential CSV)
             - Employee ID
             - Pay period end date
             - Regular hours
             - Overtime hours
             - Sick hours used
             - Vacation hours used

        3. Validated Timesheet File (sequential)
           Same fields but in fixed-width COBOL
           format after validation.

        4. Error/Reject File (sequential)
           The bad timesheet record plus a reason
           code and description.

        5. Pay Output File (sequential)
           One record per employee per pay period:
             - All employee info
             - Hours breakdown
             - Gross pay
             - Each deduction itemized
             - Net pay

        6. Report File(s) (sequential, line format)

        PUT ALL RECORD LAYOUTS IN COPYBOOKS.
        Every program should COPY the same layouts
        so they stay in sync. Create:
          - employee.cpy
          - timesheet.cpy
          - payrecord.cpy


        VALIDATION RULES (For PAYLOAD)
       ============================================
        Reject a timesheet record if:
          - Employee ID is blank or not found in
            the master file
          - Employee status is not "A" (active)
          - Regular hours < 0 or > 80
          - Overtime hours < 0 or > 40
          - Overtime > 0 but regular hours < 40
          - Sick + vacation hours > available balance
            (bonus: track balances in master file)
          - Pay period date is not valid

        Assign each rejection a 2-character code:
          "01" = blank employee ID
          "02" = employee not found
          "03" = employee not active
          "04" = invalid regular hours
          "05" = invalid overtime hours
          "06" = overtime without full regular
          "07" = PTO exceeds balance
          "08" = invalid date

        A single record can have multiple rejection
        codes. Log them all.


        PAY CALCULATION RULES (For PAYCALC)
       ============================================
        The subprogram should handle:

        HOURLY EMPLOYEES (pay type "H"):
          Regular pay = regular hours * rate
          Overtime pay = OT hours * rate * 1.5
          Gross = regular + overtime

        SALARIED EMPLOYEES (pay type "S"):
          Gross = annual salary / 26 (biweekly)
          (Ignore hours for salary calculation,
           but still track them for reporting)

        DEDUCTIONS (compute each separately):
          Federal tax = gross * bracket rate
            Bracket 1: 10% (gross <= 1500)
            Bracket 2: 15% (gross <= 3000)
            Bracket 3: 22% (gross <= 5000)
            Bracket 4: 30% (gross > 5000)
          State tax = gross * state rate
            (from employee master record)
          Social Security = gross * 6.2%
            (cap at $160,200 YTD)
          Medicare = gross * 1.45%
          Total deductions = sum of all above

        NET PAY = Gross - Total Deductions

        UPDATE YTD fields in master file after
        each employee is processed.


        REPORT FORMATS (For PAYRPT)
       ============================================

        REPORT 1: Pay Stub (one per employee)
        Design a professional pay stub showing:
          - Company name and pay period dates
          - Employee name, ID, department
          - Hours breakdown (regular, OT, sick, vac)
          - Earnings (regular pay, OT pay, gross)
          - Deductions (each tax itemized)
          - Net pay (prominently displayed)
          - YTD totals (gross and net)
        Use box-drawing characters or dashes to
        create a clean layout.

        REPORT 2: Department Summary
          - One section per department
          - List each employee: name, gross, net
          - Department subtotals
          - Sort by department, then by name
          - Include employee count per department

        REPORT 3: Company Summary
          - Total employees processed
          - Total regular hours, OT hours
          - Total gross pay
          - Total of each deduction type
          - Total net pay
          - Comparison to previous period (bonus)


        HOW TO BUILD THIS (Suggested Order)
       ============================================
        Phase 1: Foundation
          1. Design and create all copybooks.
          2. Write PAYMAST. Test it by adding 8-10
             employees with varied pay types, rates,
             departments, and tax brackets.
          3. Create a test timesheet CSV file with
             20+ records (4 pay periods for 5+ people).
             Include 3-4 intentionally bad records.

        Phase 2: Processing Pipeline
          4. Write PAYLOAD. Run it against your test
             data. Verify rejects are caught and
             clean records pass through.
          5. Write PAYCALC as a standalone subprogram.
             Test it with a small driver program that
             feeds it hardcoded values and displays
             the results.
          6. Write PAYRUN. This is the big one. Get
             the sort working first, then add the
             master file lookup, then the CALL to
             PAYCALC, then the accumulation logic.

        Phase 3: Output
          7. Write PAYRPT. Start with the pay stubs
             (easiest to verify). Then add the
             department summary. Then company totals.
          8. Run the full pipeline end-to-end:
             PAYMAST -> create test data
             PAYLOAD -> validate timesheets
             PAYRUN  -> process payroll
             PAYRPT  -> generate reports

        Phase 4: Polish
          9. Add a JCL-style driver script (a bash
             script that runs all programs in
             sequence and checks return codes).
         10. Add a README explaining the system.
         11. Test with edge cases: zero hours,
             maximum overtime, terminated employee,
             brand new employee with no YTD.


        SKILLS DEMONSTRATED
       ============================================
        When you finish this project, you will have
        demonstrated:
          - Indexed file CRUD operations
          - Sequential file processing
          - SORT with multiple keys
          - Copybooks for shared layouts
          - Subprogram CALL with LINKAGE SECTION
          - Complex business logic
          - Multi-level report generation
          - Input validation with error codes
          - Batch processing pipeline design
          - Multi-program system architecture

        This is resume-ready. Put it on GitHub.


        COMPILATION
       ============================================
        Compile each program:
          cobc -c paycalc.cob
          cobc -x -I . paymast.cob -o paymast
          cobc -x -I . payload.cob -o payload
          cobc -x -I . payrun.cob paycalc.o -o payrun
          cobc -x -I . payrpt.cob -o payrpt

        Or create a Makefile (bonus points).
