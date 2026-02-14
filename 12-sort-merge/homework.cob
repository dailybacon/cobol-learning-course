       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-12.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 12: SORT and MERGE
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a sales leaderboard generator.
      *>
      *>   STEP 1 - CREATE TEST DATA:
      *>   Write a file "sales.dat" with at least
      *>   8 sales records in this format:
      *>     Salesperson Name  PIC X(20)
      *>     Region            PIC X(10)
      *>     Quarter           PIC 9
      *>     Sales Amount      PIC 9(7)V99
      *>
      *>   Use a mix of regions (North, South, East,
      *>   West) and quarters (1-4). Include some
      *>   duplicate names with different quarters
      *>   so the same person appears multiple times.
      *>
      *>   STEP 2 - SORT AND REPORT:
      *>   Produce THREE sorted output files:
      *>
      *>   a) "by-sales.dat" - sorted by sales
      *>      amount DESCENDING (top sellers first).
      *>      Display as a "Top Sellers Leaderboard."
      *>
      *>   b) "by-region.dat" - sorted by region
      *>      ASCENDING, then by sales DESCENDING
      *>      within each region.
      *>      Display with region subtotals.
      *>
      *>   c) "by-quarter.dat" - sorted by quarter
      *>      ASCENDING, then by sales DESCENDING.
      *>      Display with quarter subtotals.
      *>
      *>   Example leaderboard output:
      *>
      *>   === TOP SELLERS LEADERBOARD ===
      *>   Rank  Name                 Sales
      *>   ----  -------------------- ----------
      *>    1    Alice Johnson        $150,000.00
      *>    2    Bob Smith            $125,000.00
      *>    3    Carol Davis          $110,000.00
      *>   ...
      *>
      *>   === SALES BY REGION ===
      *>   East
      *>     Alice Johnson        $150,000.00
      *>     Dan Wilson            $80,000.00
      *>     Region Total:        $230,000.00
      *>   North
      *>     ...
      *>
      *> REQUIREMENTS:
      *>   1. Use SD for the sort work file.
      *>   2. Use SORT with ASCENDING/DESCENDING KEY.
      *>   3. Use SORT ... USING ... GIVING for at
      *>      least one of the sorts.
      *>   4. Use multiple sort keys (e.g., region
      *>      then sales) for the grouped reports.
      *>   5. After sorting, read the output file
      *>      and display the leaderboard with rank
      *>      numbers.
      *>
      *> BONUS:
      *>   Use INPUT PROCEDURE to filter out records
      *>   with sales < $50,000 before sorting (only
      *>   RELEASE records that meet the threshold).
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
      *>   DEFINE YOUR FD AND SD RECORDS HERE


       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
