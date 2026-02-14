       ============================================
        LESSON 23: Portfolio Project
        Legacy System Data Migration
       ============================================

        OVERVIEW
       ============================================
        Companies are spending billions migrating
        off legacy mainframe systems. But here is
        the dirty secret: they still need people
        who understand the old formats to build the
        migration. You cannot convert what you
        cannot read.

        This project simulates a real-world data
        migration: taking a "legacy" system with
        old-style COBOL data formats and converting
        it to a modernized structure. You will
        build the extraction tools, transformation
        logic, validation, and reconciliation that
        every migration project requires.

        This is not glamorous work. It is extremely
        well-paid work. Migration contracts run
        $50M-$500M at large enterprises, and they
        need COBOL developers on both sides of the
        bridge.


        THE SCENARIO
       ============================================
        LEGACY SYSTEM: "OldBank" (circa 1985)
          A banking system with data formats that
          reflect 40 years of accumulated decisions:
            - COMP-3 (packed decimal) for all money
            - COMP (binary) for counters
            - 2-digit years (Y2K was "fixed" with
              windowing, not conversion)
            - EBCDIC-era field names (6 chars max)
            - Redefines used for overloaded fields
            - Filler bytes scattered for alignment
            - Status codes are numeric, not flags
            - Account types stored as single digit
            - Dates in MMDDYY format
            - No explicit delimiters (pure fixed-width)

        TARGET SYSTEM: "NewBank" (modern)
          A cleaned-up format ready for a modern
          database or downstream system:
            - DISPLAY numeric for all money
            - Standard PIC 9 counters
            - 4-digit years (YYYYMMDD dates)
            - Meaningful field names
            - Explicit status flags (A/I/C)
            - Account type spelled out
            - ISO date format
            - Nullable fields handled with indicators
            - Comma-delimited output option for
              database loading


        WHAT YOU ARE BUILDING
       ============================================

        PROGRAM 1: LGCYGEN (Legacy Data Generator)
          Creates realistic test data in the legacy
          format. You need to build this first so
          you have something to migrate. Generate
          three legacy files:

          a) Legacy Account Master
             500+ records with the old format
             (described below). Include edge cases:
               - Accounts opened before Y2K (year 85-99)
               - Accounts opened after Y2K (year 00-26)
               - Zero-balance accounts
               - Negative balances (overdrawn)
               - Accounts with maximum field values
               - Accounts with all-zero packed fields
               - At least one of every status code
               - At least one of every account type

          b) Legacy Transaction History
             2000+ records. Historical transactions
             with the old format. Include:
               - Transactions spanning the Y2K boundary
               - Transactions with amounts that use the
                 full precision of COMP-3 fields
               - Reversals (negative amounts)
               - Multiple transactions per account
               - Accounts with no transactions

          c) Legacy Customer File
             One record per customer. Customers may
             have multiple accounts. Include:
               - Customers with 1, 2, 3+ accounts
               - Customer with no matching accounts
                 (orphan — these exist in real systems)
               - Account with no matching customer
                 (another real-world orphan)

        PROGRAM 2: MIGEXTR (Migration Extractor)
          Reads each legacy file and produces a
          human-readable extract report showing
          exactly what is in the data. This is
          critical — before you convert anything,
          you must prove you can READ it correctly.

          For packed decimal fields, display both
          the raw hex representation and the
          converted numeric value. For dates,
          show both the legacy format and the
          interpreted 4-digit year.

          This program is your "I understand the
          source data" proof.

        PROGRAM 3: MIGTRAN (Migration Transformer)
          The core migration engine. Reads legacy
          files and writes new-format files.

          For each record:
            a. Unpack COMP-3 fields to DISPLAY
            b. Convert COMP fields to PIC 9
            c. Apply date windowing:
               Year 00-40 = 2000-2040
               Year 41-99 = 1941-1999
               Convert MMDDYY to YYYYMMDD
            d. Map status codes:
               Old 1 -> New "A" (active)
               Old 2 -> New "I" (inactive)
               Old 3 -> New "C" (closed)
               Old 9 -> New "X" (error/unknown)
            e. Map account types:
               Old 1 -> New "CHECKING"
               Old 2 -> New "SAVINGS"
               Old 3 -> New "MONEY-MKT"
               Old 4 -> New "CD"
               Old 5 -> New "IRA"
            f. Handle null/missing data:
               If a field is all spaces or all zeros
               in a context where that is invalid,
               set a null indicator flag ("N" = null)
            g. Write the new-format record

          Also write a transformation log showing
          every conversion applied to every record.
          In a real migration, auditors will ask
          "why did this value change?" and you need
          an answer.

        PROGRAM 4: MIGRECN (Migration Reconciler)
          The most important program. Reads both
          the legacy file and the converted file
          in parallel and proves the migration is
          LOSSLESS. For every record:

            a. Count comparison: same number of
               records in and out.
            b. Key comparison: every legacy key
               exists in the new file and vice versa.
            c. Value comparison: every converted
               field, when converted BACK to the
               legacy format, matches the original.
               (This is the acid test.)
            d. Monetary total comparison: sum of
               all balances in legacy file must
               equal sum in new file (to the penny).
            e. Cross-reference check: every customer
               still links to the same accounts.

          Produce a reconciliation report:
            - Total records compared
            - Records matched perfectly
            - Records with differences (list each)
            - Financial totals (old vs new)
            - Orphan records (in one file but not
              the other)
            - PASS or FAIL verdict

          If this report does not say PASS with
          zero differences, the migration is NOT
          done. Period. In the real world, a single
          penny discrepancy can halt a $100M project.

        PROGRAM 5: MIGLOAD (Database Load Generator)
          Reads the new-format files and generates
          comma-delimited (CSV) output suitable for
          loading into a modern database. Include:
            - Header row with column names
            - Proper quoting for text fields
            - ISO date formatting
            - NULL keyword for null-indicated fields
            - One file per target table

          Also generate the SQL DDL (CREATE TABLE
          statements) as a separate output file,
          derived from the COBOL record layout.
          This shows you understand both sides of
          the bridge.


        LEGACY FILE LAYOUTS
       ============================================
        These use COBOL features you may not have
        seen yet. That is intentional. Real legacy
        files use them and you need to handle them.

        LEGACY ACCOUNT MASTER:
          01 LGC-ACCOUNT-RECORD.
             05 LGC-ACCT-NO       PIC X(10).
             05 LGC-CUST-NO       PIC X(8).
             05 LGC-ACCT-TYPE     PIC 9 COMP.
             05 LGC-STATUS        PIC 9 COMP.
             05 LGC-BAL           PIC S9(9)V99 COMP-3.
             05 LGC-AVAIL         PIC S9(9)V99 COMP-3.
             05 LGC-OPEN-DT       PIC 9(6).
      *>        (MMDDYY format)
             05 LGC-LAST-ACT      PIC 9(6).
      *>        (MMDDYY format)
             05 LGC-INT-RATE      PIC 9V9999 COMP-3.
             05 LGC-YTD-INT       PIC S9(7)V99 COMP-3.
             05 LGC-OD-FLAG       PIC 9 COMP.
      *>        0=no overdraft, 1=has overdraft
             05 LGC-OD-LIM        PIC S9(7)V99 COMP-3.
             05 FILLER            PIC X(20).
      *>        (reserved for future use — always
      *>         present in legacy files, never remove)

        NOTES ON COMP-3 (Packed Decimal):
          PIC S9(9)V99 COMP-3 stores 11 digits plus
          sign in 6 bytes. Each byte holds two digits
          (one per nibble) except the last byte which
          holds one digit and the sign.

          In GnuCOBOL, COMP-3 works natively. You
          can MOVE a COMP-3 field to a DISPLAY field
          and it converts automatically. But you
          need to understand what is happening
          internally because:
            a. Record lengths change when you convert
               COMP-3 to DISPLAY (6 bytes becomes 12)
            b. File offsets shift
            c. All downstream field positions move

        NOTES ON COMP (Binary):
          PIC 9 COMP stores a single digit in 2
          bytes (halfword binary). Again, GnuCOBOL
          handles conversion, but record length
          changes.

        LEGACY TRANSACTION HISTORY:
          01 LGC-TRANS-RECORD.
             05 LGC-TR-ACCT       PIC X(10).
             05 LGC-TR-SEQ        PIC 9(6) COMP.
             05 LGC-TR-DATE       PIC 9(6).
      *>        (MMDDYY)
             05 LGC-TR-TIME       PIC 9(6).
      *>        (HHMMSS)
             05 LGC-TR-TYPE       PIC 99 COMP.
      *>        01=deposit, 02=withdrawal, 03=transfer
      *>        04=fee, 05=interest, 06=adjustment
      *>        99=reversal
             05 LGC-TR-AMT        PIC S9(9)V99 COMP-3.
             05 LGC-TR-BAL        PIC S9(9)V99 COMP-3.
      *>        (balance after transaction)
             05 LGC-TR-DESC       PIC X(20).
             05 LGC-TR-BRANCH     PIC 9(4) COMP.
             05 FILLER            PIC X(10).

        LEGACY CUSTOMER FILE:
          01 LGC-CUST-RECORD.
             05 LGC-CU-NO         PIC X(8).
             05 LGC-CU-LNAME      PIC X(15).
             05 LGC-CU-FNAME      PIC X(10).
             05 LGC-CU-MI         PIC X.
             05 LGC-CU-SSN        PIC 9(9) COMP-3.
      *>        (yes, SSNs stored unencrypted in
      *>         packed decimal — that is how it was)
             05 LGC-CU-DOB        PIC 9(6).
      *>        (MMDDYY)
             05 LGC-CU-ADDR.
                10 LGC-CU-ST1     PIC X(25).
                10 LGC-CU-ST2     PIC X(25).
                10 LGC-CU-CITY    PIC X(15).
                10 LGC-CU-STATE   PIC X(2).
                10 LGC-CU-ZIP     PIC X(9).
             05 LGC-CU-NACCTS     PIC 9 COMP.
      *>        (number of accounts for this customer)
             05 LGC-CU-ACCTS.
                10 LGC-CU-ACCT    PIC X(10)
                                  OCCURS 5 TIMES.
      *>        (up to 5 linked account numbers)
             05 FILLER            PIC X(10).


        NEW-FORMAT FILE LAYOUTS (Design Yourself)
       ============================================
        Design the target layouts. Ground rules:

          - No COMP or COMP-3. All DISPLAY numeric.
          - All dates YYYYMMDD (PIC 9(8)).
          - Meaningful field names (not 6-char
            abbreviations).
          - Status and type fields use readable
            codes, not opaque numbers.
          - Add a null indicator (PIC X) before
            any field that could be missing/invalid.
          - Add a record-version field (PIC 9) for
            future format changes.
          - Add a migration-source field (PIC X(8))
            to track where each record came from.
          - SSN must be masked in the new format
            (store only last 4 digits plus a hash
            or just "***-**-1234"). Do not migrate
            raw SSNs. Ever. This is a real rule.

        Create copybooks:
          - lgc-acct.cpy     (legacy account)
          - lgc-trans.cpy    (legacy transaction)
          - lgc-cust.cpy     (legacy customer)
          - new-acct.cpy     (new account)
          - new-trans.cpy    (new transaction)
          - new-cust.cpy     (new customer)


        RECONCILIATION RULES
       ============================================
        Your reconciliation report must verify:

        RECORD COUNTS:
          Legacy accounts in:     XXX
          New accounts out:       XXX
          Difference:             0  (must be 0)

          Legacy transactions in: X,XXX
          New transactions out:   X,XXX
          Difference:             0  (must be 0)

          Legacy customers in:    XXX
          New customers out:      XXX
          Difference:             0  (must be 0)

        FINANCIAL TOTALS:
          Legacy total balances:  $X,XXX,XXX.XX
          New total balances:     $X,XXX,XXX.XX
          Difference:             $0.00  (must be 0)

          Legacy total YTD int:   $XXX,XXX.XX
          New total YTD int:      $XXX,XXX.XX
          Difference:             $0.00  (must be 0)

        KEY INTEGRITY:
          Accounts with matching customers: XXX
          Orphan accounts (no customer):    X
          Orphan customers (no accounts):   X
          (Report orphans by ID for investigation)

        DATE CONVERSION VERIFICATION:
          Sample 20 records. For each, show:
            Legacy date: MMDDYY -> Interpreted YYYYMMDD
          Manually verify the windowing logic is
          correct for dates on both sides of 2000.

        FIELD-LEVEL SPOT CHECK:
          Sample 10 records. For each, show every
          field side by side: legacy value and new
          value. Visually confirm the conversion.

        VERDICT:
          ALL COUNTS MATCH:     YES/NO
          ALL TOTALS BALANCE:   YES/NO
          ALL KEYS LINKED:      YES/NO
          MIGRATION STATUS:     PASS / FAIL


        HOW TO BUILD THIS (Suggested Order)
       ============================================

        Phase 1: Understand the Data (Week 1)
          1. Study COMP-3 and COMP. Write a tiny
             test program that declares COMP-3 and
             COMP variables, moves values in, and
             displays them. Make sure you understand
             how GnuCOBOL handles the conversion.
          2. Write LGCYGEN. Generate the legacy
             test files. This forces you to
             understand every field.
          3. Write MIGEXTR. Read the legacy files
             back and display them. Verify you can
             read what you wrote. This is your
             "I understand the source" milestone.

        Phase 2: Transform (Week 2)
          4. Design the new-format copybooks.
          5. Write MIGTRAN. Start with account
             master only. Get COMP-3 conversion,
             date windowing, and status mapping
             working. Then add transactions. Then
             customers.
          6. After each file type, eyeball the
             output. Does it look right?

        Phase 3: Prove It (Week 3)
          7. Write MIGRECN. Start with record
             counts only. Then add financial
             totals. Then key integrity. Then
             the spot check.
          8. Run it. Fix any discrepancies until
             you get PASS.
          9. Intentionally introduce a bug in
             MIGTRAN (off-by-one on date, wrong
             sign on COMP-3). Run MIGRECN. Does
             it catch it? If not, your reconciler
             is not thorough enough.

        Phase 4: Load Output (Week 4)
         10. Write MIGLOAD. Generate CSV and DDL.
         11. If you have SQLite or PostgreSQL
             available, actually load the data and
             run queries against it.
         12. Write the README. Include a data flow
             diagram and field mapping document.
         13. Document every edge case you handled
             and how.


        SKILLS DEMONSTRATED
       ============================================
          - COMP-3 (packed decimal) understanding
          - COMP (binary) field handling
          - Y2K date windowing logic
          - Data format transformation
          - Migration reconciliation and balancing
          - Legacy file format interpretation
          - SSN/PII data masking
          - CSV generation for database loading
          - SQL DDL generation
          - Orphan record detection
          - End-to-end audit trail
          - The most in-demand COBOL project type
            in the current job market

        Companies will pay a premium for developers
        who can prove a migration is lossless. This
        project IS that proof.


        COMPILATION
       ============================================
        cobc -x -I . lgcygen.cob -o lgcygen
        cobc -x -I . migextr.cob -o migextr
        cobc -x -I . migtran.cob -o migtran
        cobc -x -I . migrecn.cob -o migrecn
        cobc -x -I . migload.cob -o migload
