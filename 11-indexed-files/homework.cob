       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-11.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 11: Indexed File Organization
      *> ============================================
      *> ASSIGNMENT:
      *>   Build an employee directory with full
      *>   CRUD operations (Create, Read, Update,
      *>   Delete) using an indexed file.
      *>
      *>   MENU:
      *>     1. Add Employee
      *>     2. Look Up by ID
      *>     3. Update Salary
      *>     4. Delete Employee
      *>     5. List All Employees
      *>     6. Exit
      *>
      *>   RECORD LAYOUT:
      *>     Employee ID   PIC X(5)   (key, e.g. E0001)
      *>     Name          PIC X(25)
      *>     Department    PIC X(15)
      *>     Salary        PIC 9(6)V99
      *>     Hire Year     PIC 9(4)
      *>
      *>   RULES:
      *>   - Employee ID is the primary key.
      *>   - Cannot add a duplicate ID (check for
      *>     status "22" or use INVALID KEY).
      *>   - Cannot delete or update an ID that
      *>     doesn't exist (check status "23").
      *>   - After each operation, display a
      *>     confirmation or error with the file
      *>     status code.
      *>   - "List All" should show employees in
      *>     ID order with formatted salary.
      *>
      *>   Example interaction:
      *>
      *>   Choice: 1
      *>   Employee ID: E0001
      *>   Name: Sarah Connor
      *>   Department: Engineering
      *>   Salary: 085000.00
      *>   Hire Year: 2019
      *>   Employee E0001 added successfully.
      *>
      *>   Choice: 2
      *>   Employee ID: E0001
      *>   --- Employee Found ---
      *>   ID:    E0001
      *>   Name:  Sarah Connor
      *>   Dept:  Engineering
      *>   Salary: $85,000.00
      *>   Hired:  2019
      *>
      *>   Choice: 4
      *>   Employee ID: E9999
      *>   Error: Employee E9999 not found (status 23)
      *>
      *> REQUIREMENTS:
      *>   1. Use ORGANIZATION IS INDEXED with
      *>      ACCESS MODE IS DYNAMIC.
      *>   2. Use RECORD KEY IS for the employee ID.
      *>   3. Use FILE STATUS and check it after
      *>      every I/O operation.
      *>   4. Use INVALID KEY / NOT INVALID KEY
      *>      on READ, WRITE, REWRITE, DELETE.
      *>   5. Use READ NEXT for sequential listing.
      *>   6. Use OPEN I-O for operations that
      *>      both read and write.
      *>   7. Organize with numbered paragraphs.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   DEFINE YOUR INDEXED FILE HERE


       DATA DIVISION.
       FILE SECTION.
      *>   DEFINE YOUR RECORD LAYOUT HERE


       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
