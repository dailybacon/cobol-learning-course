       ============================================
        LESSON 20: Portfolio Project
        Core Banking Transaction Processor
       ============================================

        OVERVIEW
       ============================================
        Build a simplified but realistic core
        banking system. This is the software that
        runs behind every ATM, every direct deposit,
        every wire transfer. Banks run COBOL for
        this exact use case, and most of them have
        been running it for 40+ years.

        You are building the nightly batch processor
        that takes the day's transactions and applies
        them to customer accounts.


        WHAT YOU ARE BUILDING
       ============================================

        PROGRAM 1: ACCTMGR (Account Manager)
          Interactive program to manage customer
          accounts in an indexed master file. Full
          CRUD: open account, close account, update
          customer info, look up account, list all
          accounts.

        PROGRAM 2: TXNLOAD (Transaction Loader)
          Reads the day's raw transaction feed (a
          sequential file simulating what comes from
          ATMs, branches, and online banking). Validates
          each transaction and writes clean records
          to a validated transaction file. Rejects
          go to an error file.

        PROGRAM 3: TXNSORT (Transaction Sorter)
          Sorts validated transactions by account
          number, then by transaction type (deposits
          before withdrawals - this matters!), then
          by timestamp. Uses SORT with INPUT PROCEDURE
          to filter and OUTPUT PROCEDURE to add
          sequence numbers.

        PROGRAM 4: TXNPROC (Transaction Processor)
          The main engine. Reads sorted transactions
          and the account master file. For each
          transaction:
            - Looks up the account
            - Checks business rules
            - Applies the transaction
            - Updates the account balance
            - Writes an audit trail record
          CALLs subprograms for interest calculation
          and fee assessment.

        PROGRAM 5: INTCALC (Interest Calculator - Subprogram)
          Receives account type and balance, returns
          daily interest amount. Called by TXNPROC.

        PROGRAM 6: FEECALC (Fee Calculator - Subprogram)
          Receives transaction type, amount, and
          account status. Returns any applicable fees.
          Called by TXNPROC.

        PROGRAM 7: BNKRPT (Report Generator)
          Produces the end-of-day reports that bank
          managers review every morning.


        FILE LAYOUTS (Design These Yourself)
       ============================================

        1. Account Master File (indexed)
             - Account number (key, 10 digits)
             - Customer last name
             - Customer first name
             - Account type
               (C=checking, S=savings, M=money market)
             - Current balance (signed, allow negative)
             - Available balance
             - Daily transaction count
             - Daily transaction total
             - Monthly fee total
             - Interest rate (varies by account type)
             - Interest accrued this month
             - Overdraft protection (Y/N)
             - Overdraft limit
             - Account status (A=active, F=frozen,
               C=closed)
             - Date opened (YYYYMMDD)
             - Last activity date

        2. Raw Transaction Feed (sequential)
             - Transaction ID (unique, 12 chars)
             - Account number
             - Transaction type:
               DEP = deposit
               WDR = withdrawal
               TFR = transfer (need a 2nd account #)
               FEE = fee assessment
               INT = interest credit
               ADJ = adjustment
             - Amount (9(9)V99)
             - Target account (for transfers only)
             - Channel (ATM/BRN/ONL/ACH)
             - Timestamp (YYYYMMDDHHMMSS)
             - Description

        3. Validated Transaction File (sequential)
           Clean records in fixed-width format.

        4. Sorted Transaction File (sequential)
           Same layout but sorted and sequenced.

        5. Audit Trail File (sequential)
           Every applied transaction plus:
             - Balance before
             - Balance after
             - Approval code (generated)
             - Processing timestamp

        6. Reject File (sequential)
           Bad transactions with reject reason.

        USE COPYBOOKS FOR ALL LAYOUTS.
          - account.cpy
          - transaction.cpy
          - audit.cpy


        BUSINESS RULES (Critical - Get These Right)
       ============================================

        DEPOSIT RULES:
          - Amount must be > 0 and <= 50,000.00
            (over 50K requires manual review -
             write to a "held" file instead)
          - Deposits over 10,000 must be flagged
            for reporting (think CTR - Currency
            Transaction Report)
          - Available balance increases immediately
            for cash/ACH deposits under 5,000
          - Deposits over 5,000: available balance
            doesn't update until next day (hold)

        WITHDRAWAL RULES:
          - Amount must be > 0
          - Check AVAILABLE balance, not current
          - If available balance insufficient:
            * If overdraft protection = Y and
              amount <= overdraft limit: allow it,
              charge $35 overdraft fee
            * Otherwise: reject the transaction
          - ATM withdrawals capped at $500/day
            (check daily transaction total)
          - Cannot withdraw from frozen/closed accts

        TRANSFER RULES:
          - Both accounts must exist and be active
          - Apply withdrawal rules to source account
          - Apply deposit rules to target account
          - Transfers are atomic: if either side
            fails, neither side processes

        FEE RULES:
          - Monthly maintenance: $12 for checking
            if balance < $1,500 average
          - ATM fee: $2.50 per ATM transaction
            after the first 4 per month
          - Overdraft fee: $35 per occurrence
          - Wire transfer fee: $25

        INTEREST RULES:
          - Savings: 0.50% APR (daily accrual)
          - Money Market: 1.25% APR (daily accrual)
          - Checking: 0.01% APR
          - Daily interest = balance * (APR / 365)
          - Interest credits on last day of month


        REPORT FORMATS (For BNKRPT)
       ============================================

        REPORT 1: Daily Transaction Journal
          Chronological list of every transaction
          processed today. Show:
            - Transaction ID
            - Account number
            - Type (DEP/WDR/TFR/etc.)
            - Amount
            - Balance after
            - Status (approved/rejected/held)
          Include daily totals by transaction type.

        REPORT 2: Exception Report
          Any transaction that triggered special
          handling:
            - Overdraft occurrences
            - Large deposits (>10K flag)
            - Held deposits (>5K)
            - Rejected transactions
            - Frozen account attempts
          This is what the branch manager reads
          first thing in the morning.

        REPORT 3: Daily Balance Summary
          For each account type (checking, savings,
          money market):
            - Number of accounts
            - Total deposits today
            - Total withdrawals today
            - Total fees assessed
            - Total interest accrued
            - Aggregate balance
          Company-wide totals at the bottom.

        REPORT 4: Large Transaction Report (CTR)
          All cash transactions over $10,000.
          Required by federal regulations.
            - Customer name
            - Account number
            - Transaction amount
            - Transaction type
            - Channel
          In the real world this gets filed with
          FinCEN. Here just write it to a file.


        HOW TO BUILD THIS (Suggested Order)
       ============================================

        Phase 1: Foundation (Week 1)
          1. Design all copybooks first. Spend real
             time on this. Get the field sizes right.
             Think about what reports need.
          2. Write ACCTMGR. Create 15-20 test
             accounts with variety: different types,
             balances ranging from $50 to $50,000,
             some with overdraft protection, one
             frozen, one closed.
          3. Write INTCALC and FEECALC subprograms.
             Test with a small driver program.

        Phase 2: Transaction Pipeline (Week 2)
          4. Create a test transaction file with 40+
             records. Include every transaction type.
             Include edge cases:
               - Withdrawal that triggers overdraft
               - Deposit over $10,000
               - Transfer between accounts
               - Transaction on a frozen account
               - ATM withdrawal that exceeds daily limit
               - Deposit exactly equal to $5,000
          5. Write TXNLOAD. Run it. Verify rejects.
          6. Write TXNSORT. Verify deposit-before-
             withdrawal ordering.
          7. Write TXNPROC. This is the hard one.
             Start simple: deposits only. Then add
             withdrawals. Then transfers. Then fees
             and interest. Test after each addition.

        Phase 3: Reports (Week 3)
          8. Write BNKRPT. Start with the daily
             journal (most straightforward). Then
             the exception report. Then the balance
             summary. Then the CTR report.
          9. Run the full pipeline end-to-end.
         10. Create a driver script.

        Phase 4: Polish (Week 4)
         11. Run edge cases. Try to break it.
         12. Add a README with system documentation.
         13. Draw a data flow diagram showing how
             files connect between programs.
         14. Consider adding a SCREEN SECTION front
             end to ACCTMGR for bonus points.


        SKILLS DEMONSTRATED
       ============================================
          - Real-world banking domain knowledge
          - Complex business rule implementation
          - Multi-program batch architecture
          - Transaction ordering and atomicity
          - Regulatory compliance (CTR reporting)
          - Exception handling and audit trails
          - Subprogram design (CALL/LINKAGE)
          - Indexed and sequential file processing
          - SORT with INPUT/OUTPUT PROCEDURE
          - Multi-level report generation
          - Copybook-driven system design

        This is a serious portfolio piece. A hiring
        manager at a bank or insurance company will
        recognize this as someone who understands
        how their production systems actually work.


        COMPILATION
       ============================================
        cobc -c intcalc.cob
        cobc -c feecalc.cob
        cobc -x -I . acctmgr.cob -o acctmgr
        cobc -x -I . txnload.cob -o txnload
        cobc -x -I . txnsort.cob -o txnsort
        cobc -x -I . txnproc.cob intcalc.o feecalc.o
          -o txnproc
        cobc -x -I . bnkrpt.cob -o bnkrpt
