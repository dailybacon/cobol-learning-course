       ============================================
        LESSON 22: Portfolio Project
        EDI 837/835 Healthcare Claims Processor
       ============================================

        OVERVIEW
       ============================================
        EDI (Electronic Data Interchange) is how
        healthcare providers and insurance companies
        actually exchange data. Every doctor visit,
        every hospital stay, every lab test — the
        claim travels as an EDI 837 file. The
        payment response comes back as an 835.

        These are not CSV files. They are not JSON.
        They are a format designed in the 1970s that
        looks like someone threw punctuation at a
        wall. And they are processed by COBOL
        programs at virtually every health insurer
        in the United States.

        This project is the single most employable
        thing you can build in COBOL. Healthcare
        EDI shops cannot find enough people who
        understand both COBOL and X12 formats.


        WHAT EDI ACTUALLY LOOKS LIKE
       ============================================
        Before building anything, you need to
        understand the format. Here is a simplified
        837 Professional claim:

        ISA*00*          *00*          *ZZ*SENDER    ...
        GS*HC*SENDER*RECEIVER*20260213*1200*1*X*005010X222A1~
        ST*837*0001*005010X222A1~
        BHT*0019*00*BATCH001*20260213*1200*CH~
        NM1*41*2*CITY MEDICAL GROUP*****46*1234567890~
        NM1*40*2*ACME INSURANCE*****46*9876543210~
        CLM*PAT001-2026*150.00***11:B:1*Y*A*Y*Y~
        DTP*472*D8*20260115~
        SV1*HC:99213*150.00*UN*1***1~
        NM1*IL*1*SMITH*JOHN****MI*M0000000001~
        SE*10*0001~
        GE*1*1~
        IEA*1*000000001~

        KEY THINGS TO NOTICE:
          - Segments are separated by ~ (tilde)
          - Elements within a segment are separated
            by * (asterisk)
          - Sub-elements use : (colon)
          - The file is NOT line-based. Segments can
            span lines or multiple segments can be
            on one line. You parse by delimiters.
          - ISA is always exactly 106 characters
            (this is how you find the delimiters —
            positions 4, 104, and 106 of the ISA
            tell you element, sub-element, and
            segment separators)
          - The structure is hierarchical:
            ISA/IEA = interchange envelope
            GS/GE   = functional group envelope
            ST/SE   = transaction set envelope
            CLM     = claim header
            SV1     = service line (procedure)

        An 835 remittance advice looks similar but
        flows the other direction (insurer to
        provider) with payment information.


        WHAT YOU ARE BUILDING
       ============================================

        PROGRAM 1: EDIPARSE (EDI Parser)
          The foundation. Reads a raw EDI file
          (a single long string or multi-line text)
          and breaks it into individual segments.
          For each segment, splits it into elements.
          Writes parsed segments to an intermediate
          work file in a COBOL-friendly fixed-width
          format.

          This is the hardest program because EDI
          is stream-based, not record-based. You
          need to:
            a. Read the ISA segment to discover
               the delimiters (don't hardcode * ~ :)
            b. Buffer input and scan for the segment
               terminator
            c. When you find a complete segment,
               extract the segment ID (first 2-3
               chars) and each element
            d. Write it as a parsed record

        PROGRAM 2: EDI837IN (837 Inbound Processor)
          Reads the parsed segments and assembles
          them into claim records. This is where you
          apply the X12 transaction structure:

          Loop 1000A: Submitter (NM1*41)
          Loop 1000B: Receiver (NM1*40)
          Loop 2000A: Billing Provider
          Loop 2300:  Claim Header (CLM segment)
            Loop 2400: Service Lines (SV1 segments)

          For each claim, build a COBOL record with:
            - Submitter info
            - Patient/member info (from NM1*IL)
            - Claim ID (CLM element 1)
            - Total charge (CLM element 2)
            - Date of service (DTP*472)
            - Procedure code (SV1 element 1)
            - Line charge (SV1 element 2)
            - Units (SV1 element 4)

          Write assembled claims to a claims file
          that your insurance system (lesson 21)
          could process.

        PROGRAM 3: EDI835GN (835 Outbound Generator)
          After claims are adjudicated (by your
          lesson 21 system or simulated results),
          generate an 835 remittance advice.

          Build the output EDI file segment by
          segment:
            ISA - interchange header
            GS  - functional group header
            ST  - transaction set header (835)
            BPR - financial information (total paid)
            TRN - reassociation trace number
            Loop 1000A: Payer identification
            Loop 1000B: Payee identification
            Loop 2100: Claim payment info
              CLP - claim-level data
              SVC - service line adjudication
              CAS - adjustment reason codes
            SE  - transaction set trailer
            GE  - functional group trailer
            IEA - interchange trailer

          The SE segment must contain an accurate
          count of all segments in the transaction.
          The GE must count transaction sets. The
          IEA must count functional groups. These
          counts are how the receiver validates
          the file wasn't corrupted in transit.

        PROGRAM 4: EDIRECN (Reconciliation)
          Reads the original 837 and the generated
          835 side by side. For every claim
          submitted, verify there is a corresponding
          payment or denial in the 835. Produce a
          reconciliation report showing:
            - Claims submitted vs. claims responded
            - Total billed vs. total allowed
            - Total paid vs. total member responsibility
            - Any unmatched claims (errors)

        PROGRAM 5: EDITRK (Tracking & Audit)
          Maintains an indexed tracking file that
          records every EDI interchange processed:
            - ISA control number
            - Date/time received
            - Sender/receiver IDs
            - Transaction count
            - Total dollar amount
            - Processing status
            - Error count
          Provides lookup by control number and
          reporting by date range.


        EDI SEGMENT REFERENCE
       ============================================
        You need to handle these segments for a
        basic 837 Professional:

        ENVELOPE SEGMENTS:
          ISA - Interchange Control Header
            16 elements, always fixed-width
            Element 1:  Authorization qualifier
            Element 6:  Interchange sender ID
            Element 8:  Interchange receiver ID
            Element 9:  Interchange date (YYMMDD)
            Element 10: Interchange time (HHMM)
            Element 13: Interchange control number
            Element 16: Sub-element separator

          GS - Functional Group Header
            Element 1: Functional ID code ("HC")
            Element 2: Application sender code
            Element 3: Application receiver code
            Element 4: Date (YYYYMMDD)
            Element 6: Group control number
            Element 8: Version ("005010X222A1")

          ST - Transaction Set Header
            Element 1: Transaction set ID ("837")
            Element 2: Transaction set control number

          SE - Transaction Set Trailer
            Element 1: Number of included segments
            Element 2: Transaction set control number

          GE - Functional Group Trailer
            Element 1: Number of transaction sets
            Element 2: Group control number

          IEA - Interchange Control Trailer
            Element 1: Number of functional groups
            Element 2: Interchange control number

        CLAIM SEGMENTS (837):
          BHT - Beginning of Hierarchical Transaction
            Element 2: Transaction type ("00" = original)
            Element 3: Batch/reference ID
            Element 4: Date
            Element 6: Transaction type code

          NM1 - Name
            Element 1: Entity ID qualifier
              "41" = Submitter
              "40" = Receiver
              "85" = Billing provider
              "IL" = Insured/subscriber
              "QC" = Patient
            Element 2: Entity type ("1"=person, "2"=org)
            Elements 3-5: Last, first, middle name
              (or org name in element 3 if type "2")
            Element 8: ID code qualifier
              "MI" = Member ID
              "46" = Electronic transmitter ID
            Element 9: ID code

          CLM - Claim Information
            Element 1: Patient claim ID
            Element 2: Total claim charge amount
            Element 5: Place of service (sub-elements)

          DTP - Date/Time Period
            Element 1: Date qualifier
              "472" = Service date
              "435" = Admission date
            Element 2: Date format qualifier
              "D8" = YYYYMMDD single date
              "RD8" = YYYYMMDD-YYYYMMDD range
            Element 3: Date value

          SV1 - Professional Service
            Element 1: Procedure code (composite)
              Sub 1: Product/service ID qualifier "HC"
              Sub 2: CPT/HCPCS procedure code
              Sub 3-6: Modifier codes (optional)
            Element 2: Line item charge amount
            Element 3: Unit basis code ("UN"=unit)
            Element 4: Service unit count

        PAYMENT SEGMENTS (835):
          BPR - Financial Information
            Element 1: Transaction handling code
              "I" = Remittance info only
              "H" = Payment with remittance
            Element 2: Total payment amount
            Element 4: Payment method
              "CHK" = Check
              "ACH" = Electronic
            Element 16: Payment date

          TRN - Reassociation Trace Number
            Element 2: Reference ID (check/EFT number)
            Element 3: Payer identifier

          CLP - Claim Payment Information
            Element 1: Patient claim ID (matches CLM)
            Element 2: Claim status code
              "1" = Processed as primary
              "2" = Processed as secondary
              "4" = Denied
              "22" = Reversed
            Element 3: Total claim charge
            Element 4: Total claim payment
            Element 5: Patient responsibility
            Element 6: Claim filing indicator

          SVC - Service Payment Information
            Element 1: Procedure code (composite)
            Element 2: Line item charge
            Element 3: Line item payment
            Element 5: Units paid

          CAS - Claims Adjustment
            Element 1: Adjustment group code
              "CO" = Contractual obligation
              "PR" = Patient responsibility
              "OA" = Other adjustment
            Element 2: Adjustment reason code
            Element 3: Adjustment amount
            (Can have up to 6 reason/amount pairs
             in elements 2-19)


        FILE LAYOUTS (Design These Yourself)
       ============================================

        1. Parsed Segment File (sequential)
           Each record = one segment, fixed-width:
             - Interchange control # PIC X(9)
             - Segment sequence #    PIC 9(6)
             - Segment ID            PIC X(3)
             - Element count         PIC 9(2)
             - Element 1 through 20  PIC X(50) each
           Yes, this is a wide record. That is normal
           in EDI processing.

        2. Assembled Claims File (sequential)
           Should match or feed into your lesson 21
           claim record layout.

        3. Adjudicated Claims File (sequential)
           Input to the 835 generator. Contains
           all adjudication results.

        4. EDI Tracking File (indexed)
           Key = ISA control number.

        5. Error/Reject File
           Bad segments with position and reason.

        COPYBOOKS:
          - ediseg.cpy    (parsed segment layout)
          - ediclaim.cpy  (assembled claim)
          - editrk.cpy    (tracking record)


        PARSING STRATEGY
       ============================================
        This is the part most people get wrong.
        Here is how to approach it:

        DO NOT try to parse the entire EDI file
        in one pass into claim records. That is
        the path to madness.

        Instead, use a TWO-PASS approach:

        PASS 1 (EDIPARSE):
          Read raw EDI as a stream of characters.
          Buffer into a large WORKING-STORAGE field
          (PIC X(10000) or similar).
          Scan for segment terminators.
          For each segment:
            - Extract segment ID (chars before
              first element separator)
            - Split on element separators
            - Write to parsed segment file
          This pass knows NOTHING about 837 vs 835
          or what segments mean. It just tokenizes.

        PASS 2 (EDI837IN):
          Read parsed segments sequentially.
          Maintain state: "I am in loop 2300"
          (claim header). When you see a CLM
          segment, start a new claim. When you
          see SV1, add a service line. When you
          see the next CLM or SE, the current
          claim is complete — write it out.

        This two-pass design is how production
        EDI systems actually work. The parser is
        reusable across all transaction types.
        Only the assembler (pass 2) is specific
        to 837 or 835.


        835 GENERATION STRATEGY
       ============================================
        Building the outbound 835 is the reverse:

        1. Start with envelope headers (ISA, GS, ST).
           Keep a running segment counter for SE.

        2. Write BPR with the total payment amount
           (you need to calculate this first by
           summing all claim payments — or make two
           passes: one to total, one to write).

        3. For each adjudicated claim:
           - Write CLP with claim-level totals
           - For each service line:
             Write SVC with line-level payment
             Write CAS for adjustments:
               "CO" for contractual write-off
                 (billed minus allowed)
               "PR" for patient responsibility
                 (deductible + coinsurance)

        4. Close with SE (include segment count),
           GE (include transaction count), and
           IEA (include group count).

        5. Validate your own output: does the SE
           count match? Does the GE count match?
           Do the CLP amounts sum to the BPR total?


        TEST DATA
       ============================================
        Create at least 3 test 837 files:

        FILE 1: Simple (3 claims, 1 service line each)
          - Standard office visits
          - All in-network
          - All valid
          Use this to get your parser working.

        FILE 2: Complex (5 claims, mixed)
          - Claim with multiple service lines
          - Claim with modifier codes on SV1
          - Claim with date range (RD8) instead
            of single date (D8)
          - Claim with multiple DTP segments

        FILE 3: Adversarial (8 claims, errors)
          - Missing required segments
          - Invalid segment terminator
          - CLM with zero dollar amount
          - Duplicate claim IDs
          - ISA with non-standard delimiters
            (use ^ instead of * and | instead of ~)
          - Segment count in SE doesn't match
          - Service date in the future

        IMPORTANT: Build your test files by hand.
        Understand every character. EDI debugging
        is 90% of the job — you need to be able to
        read raw EDI and spot problems by eye.


        HOW TO BUILD THIS (Suggested Order)
       ============================================

        Phase 1: Parser (Week 1)
          1. Study the segment reference above until
             you can read raw EDI without the guide.
          2. Create test file 1 (simple, by hand).
          3. Write EDIPARSE. Start with hardcoded
             delimiters (* ~ :). Get it working.
             Then refactor to read delimiters from
             the ISA segment dynamically.
          4. Verify parsed output — every element
             should be in the right position.

        Phase 2: 837 Inbound (Week 2)
          5. Write EDI837IN. Start by just handling
             CLM and SV1 segments. Get claims
             assembling correctly.
          6. Add NM1 handling (patient and provider
             identification).
          7. Add DTP handling (service dates).
          8. Test with file 2 (complex).
          9. Add validation. Test with file 3.

        Phase 3: 835 Outbound (Week 3)
         10. Create sample adjudicated claims data
             (or connect to your lesson 21 system).
         11. Write EDI835GN. Start with envelope
             segments only (ISA through IEA with
             no claims). Verify the structure.
         12. Add CLP/SVC/CAS generation.
         13. Add segment counting for SE/GE/IEA.
         14. Validate: parse your own 835 output
             with EDIPARSE. Does it parse cleanly?

        Phase 4: Reconciliation & Tracking (Week 4)
         15. Write EDIRECN. Match 837 claims to
             835 payment lines.
         16. Write EDITRK. Tracking and audit trail.
         17. End-to-end test: 837 in, process,
             835 out, reconcile.
         18. Test with adversarial data (file 3).

        Phase 5: Polish
         19. Handle the non-standard delimiter file.
         20. Write a README explaining the system.
         21. Document the segment flow with a
             diagram.
         22. If you connected this to lesson 21,
             document the full pipeline: EDI in ->
             parse -> assemble -> adjudicate ->
             generate 835 -> reconcile.


        SKILLS DEMONSTRATED
       ============================================
          - X12 EDI format expertise (837/835)
          - Stream parsing (not record-based I/O)
          - Two-pass processing architecture
          - State machine design (loop tracking)
          - Healthcare domain knowledge
          - Envelope validation (segment counts)
          - Dynamic delimiter detection
          - Reconciliation and balancing
          - Audit trail maintenance
          - The #1 most in-demand COBOL niche skill

        If you complete this project AND lesson 21,
        you can walk into any healthcare COBOL shop
        in the country and speak their language.
        Most junior hires take 6-12 months to learn
        EDI on the job. You will already know it.


        COMPILATION
       ============================================
        cobc -x -I . ediparse.cob -o ediparse
        cobc -x -I . edi837in.cob -o edi837in
        cobc -x -I . edi835gn.cob -o edi835gn
        cobc -x -I . edirecn.cob -o edirecn
        cobc -x -I . editrk.cob -o editrk
