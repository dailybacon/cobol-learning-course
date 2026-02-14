       ============================================
        LESSON 21: Portfolio Project
        Insurance Claims Processing System
       ============================================

        OVERVIEW
       ============================================
        Build a health insurance claims processing
        system. Insurance is the other giant COBOL
        industry alongside banking. Every claim you
        file at a doctor's office gets processed by
        a system that looks a lot like this one.

        This project deals with a different domain
        than banking but the same COBOL patterns:
        master files, transaction processing,
        business rule validation, and reporting.
        Having both a banking project AND an
        insurance project shows range.


        WHAT YOU ARE BUILDING
       ============================================

        PROGRAM 1: MEMMGR (Member Manager)
          Interactive program to manage the member
          (policyholder) master file. Members have
          a policy, a plan type, deductible tracking,
          and dependent information.

        PROGRAM 2: PRVMGR (Provider Manager)
          Interactive program to manage the provider
          (doctor/hospital) master file. Providers
          have contracted rates, specialties, and
          network status.

        PROGRAM 3: CLMLOAD (Claims Loader)
          Reads raw claims submitted by providers
          (the "837" file in real life). Validates
          format, checks that member and provider
          exist, and writes clean claims to a
          validated file. Rejects go to an error
          file with denial codes.

        PROGRAM 4: CLMADJ (Claims Adjudicator - Subprogram)
          The brain of the system. Receives a claim,
          the member record, and the provider record.
          Applies pricing rules, deductible logic,
          coinsurance, and out-of-pocket maximums.
          Returns the adjudicated amounts:
            - Allowed amount
            - Deductible applied
            - Coinsurance (member share)
            - Plan pays amount
            - Member owes amount

        PROGRAM 5: CLMPROC (Claims Processor)
          Main batch program. Reads validated claims,
          looks up member and provider, CALLs CLMADJ
          to adjudicate, updates member deductible
          and out-of-pocket tracking, writes
          processed claims to output file and audit
          trail. Handles claim status: paid, denied,
          or pended for review.

        PROGRAM 6: EOBRPT (EOB Report Generator)
          Generates Explanation of Benefits statements
          for each member. This is the document
          patients receive showing what was billed,
          what insurance paid, and what they owe.

        PROGRAM 7: CLMRPT (Management Reports)
          Produces operational reports: claims
          summary by provider, by plan type, denial
          rates, and financial totals.


        FILE LAYOUTS (Design These Yourself)
       ============================================

        1. Member Master File (indexed)
             - Member ID (key, 10 chars)
             - Last name
             - First name
             - Date of birth (YYYYMMDD)
             - Plan type:
               B = Bronze (60/40 coinsurance)
               S = Silver (70/30)
               G = Gold (80/20)
               P = Platinum (90/10)
             - Annual deductible amount
               (B=$6000, S=$3000, G=$1500, P=$500)
             - Deductible met YTD
             - Out-of-pocket maximum
               (B=$8000, S=$6000, G=$4000, P=$2000)
             - Out-of-pocket paid YTD
             - Policy effective date
             - Policy termination date (0 = active)
             - Number of dependents
             - Dependent names (table of 5)
             - Member status (A=active, T=terminated)

        2. Provider Master File (indexed)
             - Provider ID (key, NPI format, 10 digits)
             - Provider name (practice or physician)
             - Specialty code:
               PCP = Primary Care
               SPC = Specialist
               URG = Urgent Care
               ER  = Emergency Room
               LAB = Laboratory
               IMG = Imaging
             - Network status (I=in-network, O=out)
             - Contracted rate percentage
               (in-network providers agree to accept
                a percentage of billed charges)
               PCP=70%, SPC=75%, URG=80%, ER=90%,
               LAB=60%, IMG=65%
               Out-of-network: 100% (no discount)
             - Provider status (A=active, S=suspended)

        3. Raw Claims File (sequential)
             - Claim ID (12 chars, unique)
             - Member ID
             - Provider ID
             - Date of service (YYYYMMDD)
             - Procedure code (CPT code, 5 chars)
             - Diagnosis code (ICD-10, 7 chars)
             - Billed amount (what the provider
               charges)
             - Units (number of procedures)
             - Place of service
               (OF=office, HO=hospital, ER=ER)

        4. Validated Claims File (sequential)
           Clean claims ready for adjudication.

        5. Processed Claims File (sequential)
           Adjudicated claims with all amounts:
             - All raw claim fields
             - Allowed amount
             - Deductible applied
             - Coinsurance amount
             - Plan paid amount
             - Member responsibility
             - Claim status (P=paid, D=denied,
               R=pended for review)
             - Denial reason code (if denied)
             - Processing date

        6. Audit Trail (sequential)
           Every adjudication decision with
           before/after deductible balances.

        7. Reject File (sequential)
           Failed claims with denial codes.

        COPYBOOKS:
          - member.cpy
          - provider.cpy
          - claim.cpy
          - adjresult.cpy (adjudication results)


        VALIDATION RULES (For CLMLOAD)
       ============================================

        Deny a claim at loading if:
          "D01" = Claim ID is blank or duplicate
          "D02" = Member ID not found
          "D03" = Member policy not active on date
                  of service
          "D04" = Member terminated before service date
          "D05" = Provider ID not found
          "D06" = Provider suspended
          "D07" = Date of service is in the future
          "D08" = Date of service > 365 days old
                  (timely filing limit)
          "D09" = Billed amount <= 0
          "D10" = Procedure code is blank
          "D11" = Diagnosis code is blank
          "D12" = Invalid place of service code


        ADJUDICATION RULES (For CLMADJ)
       ============================================
        This is the complex part. Work through it
        step by step:

        STEP 1: Determine Allowed Amount
          If provider is in-network:
            Allowed = Billed * contracted rate %
          If provider is out-of-network:
            Allowed = Billed amount (no discount)
            BUT member coinsurance goes to 50/50
            regardless of plan type.

        STEP 2: Apply Deductible
          If member has NOT met their annual
          deductible:
            Remaining deductible = annual amount
              minus YTD met amount
            Deductible applied = lesser of:
              - Remaining deductible
              - Allowed amount
            Reduce allowed amount by deductible
            Update member YTD deductible met

        STEP 3: Calculate Coinsurance
          After deductible is applied, split the
          remaining allowed amount:
            Plan share = remaining * plan %
            Member share = remaining * (100 - plan %)
          Example (Gold 80/20, $200 remaining):
            Plan pays $160, member pays $40

        STEP 4: Check Out-of-Pocket Maximum
          If member's YTD out-of-pocket + this
          claim's member share exceeds their OOP max:
            Member pays only up to the max
            Plan pays the rest
          Once OOP max is hit, plan pays 100% of
          all remaining claims for the year.

        STEP 5: Determine Claim Status
          PAID: plan pays amount > 0
          DENIED: entire claim denied (use code)
          PENDED: flag for manual review if:
            - Billed amount > $10,000
            - Out-of-network emergency room visit
            - Procedure/diagnosis mismatch (bonus)


        REPORT FORMATS (For EOBRPT and CLMRPT)
       ============================================

        REPORT 1: Explanation of Benefits (EOB)
          One per member who had claims processed.
          This is what patients actually receive.

          ================================================
           YOUR INSURANCE COMPANY
           EXPLANATION OF BENEFITS
           This is not a bill.
          ================================================
           Member: John Smith          ID: M000000001
           Plan: Gold                  Period: Jan 2026
          ================================================
           Date     Provider         Billed   Allowed
           -------- ---------------- -------- --------
           01/05/26 Dr. Jane Lee     $250.00  $175.00
                    Deductible:                ($50.00)
                    Plan pays (80%):           $100.00
                    You owe (20%):              $25.00

           01/12/26 City Lab         $180.00  $108.00
                    Deductible:                 $0.00
                    Plan pays (80%):            $86.40
                    You owe (20%):              $21.60
          ================================================
           Deductible:   $1,050 of $1,500 met
           Out-of-Pocket: $890 of $4,000 met
          ================================================
           Total you owe this period:           $46.60
          ================================================

        REPORT 2: Claims Summary by Provider
          For each provider:
            - Total claims submitted
            - Total billed amount
            - Total allowed amount
            - Total plan paid
            - Average discount percentage
          Sorted by total billed descending.

        REPORT 3: Claims Summary by Plan Type
          For each plan type (Bronze/Silver/Gold/Plat):
            - Number of members with claims
            - Total claims count
            - Total billed
            - Total allowed
            - Total deductible applied
            - Total plan paid
            - Total member responsibility
            - Average plan cost per member

        REPORT 4: Denial Report
          All denied claims grouped by denial code.
            - Denial code and description
            - Count of denials
            - Total billed amount denied
          Sorted by denial count descending.
          This tells management where the problems
          are (bad provider data, expired policies,
          timely filing issues, etc.)


        HOW TO BUILD THIS (Suggested Order)
       ============================================

        Phase 1: Master Files (Week 1)
          1. Design all copybooks. The adjudication
             result layout is the trickiest - make
             sure it has every amount you will need
             for the EOB report.
          2. Write MEMMGR. Create 10-15 test members
             across all plan types. Include:
               - Members with high and low deductibles
               - A member close to OOP max
               - A member already past OOP max
               - A terminated member
               - Members with dependents
          3. Write PRVMGR. Create 8-10 providers:
               - At least one of each specialty
               - At least 2 out-of-network providers
               - One suspended provider

        Phase 2: Claims Pipeline (Week 2)
          4. Create a test claims file with 30+
             claims. Cover every scenario:
               - Simple office visit, in-network
               - Specialist visit, in-network
               - ER visit, out-of-network
               - Lab work under deductible
               - Claim that hits the OOP max
               - Claim for terminated member
               - Claim with suspended provider
               - Claim over $10,000 (pend)
               - Duplicate claim ID
               - Claim filed too late (>365 days)
          5. Write CLMLOAD. Test thoroughly.
          6. Write CLMADJ subprogram. This is the
             hardest program. Test it with a driver
             that feeds it scenarios one at a time:
               a. Simple claim, deductible not met
               b. Claim that finishes the deductible
               c. Claim after deductible is met
               d. Claim that hits OOP max
               e. Claim after OOP max (plan pays 100%)
               f. Out-of-network claim (50/50)
          7. Write CLMPROC. Integrate everything.

        Phase 3: Reports (Week 3)
          8. Write EOBRPT. The EOB is the showcase.
             Make it look professional.
          9. Write CLMRPT. Management reports.
         10. End-to-end testing.

        Phase 4: Polish (Week 4)
         11. Driver script to run the full pipeline.
         12. README with system documentation and
             data flow diagram.
         13. Consider: what happens when you run the
             system a SECOND time? Are YTD fields
             handled correctly? Does it break?
         14. Edge case testing. Try to break it.


        SKILLS DEMONSTRATED
       ============================================
          - Insurance/healthcare domain knowledge
          - Complex multi-step business logic
            (adjudication is genuinely hard)
          - Accumulator pattern with running totals
          - Out-of-pocket and deductible tracking
            (stateful processing across claims)
          - Multi-file, multi-program architecture
          - Subprogram design for core business logic
          - Professional report generation (EOB)
          - Regulatory awareness (timely filing)
          - Denial code management
          - Master file maintenance patterns
          - Complete system from data entry to reports

        Between this and the banking project, you
        have demonstrated competency in the two
        largest COBOL employment sectors. That is
        a resume that gets interviews.


        COMPILATION
       ============================================
        cobc -c clmadj.cob
        cobc -x -I . memmgr.cob -o memmgr
        cobc -x -I . prvmgr.cob -o prvmgr
        cobc -x -I . clmload.cob -o clmload
        cobc -x -I . clmproc.cob clmadj.o -o clmproc
        cobc -x -I . eobrpt.cob -o eobrpt
        cobc -x -I . clmrpt.cob -o clmrpt
