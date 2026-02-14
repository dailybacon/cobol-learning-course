       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRINGS.
       AUTHOR. LEARNER.

      *> ============================================
      *> LESSON 7: String Handling
      *> ============================================
      *> COBOL strings are fixed-length (padded with
      *> spaces). These tools let you manipulate them.
      *>
      *> NEW CONCEPTS:
      *>   - STRING: concatenate strings together.
      *>     STRING str-1 DELIMITED BY SPACE
      *>            str-2 DELIMITED BY SIZE
      *>       INTO target
      *>     END-STRING
      *>
      *>     DELIMITED BY SPACE = stop at first space
      *>     DELIMITED BY SIZE  = use entire field
      *>     DELIMITED BY ","   = stop at comma
      *>
      *>   - UNSTRING: split a string apart.
      *>     UNSTRING source DELIMITED BY ","
      *>       INTO field-1 field-2 field-3
      *>     END-UNSTRING
      *>
      *>   - INSPECT: count or replace characters.
      *>     INSPECT str TALLYING count
      *>       FOR ALL "A"
      *>     INSPECT str REPLACING ALL "," BY " "
      *>
      *>   - Reference modification (substring):
      *>     WS-NAME(1:5) = first 5 characters
      *>     WS-NAME(3:2) = 2 chars starting at pos 3
      *>
      *>   - FUNCTION LENGTH(field)
      *>     Returns the defined length of a field.
      *>
      *>   - FUNCTION UPPER-CASE / LOWER-CASE
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x strings.cob -o strings
      *>   ./strings
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIRST-NAME    PIC X(15).
       01 WS-LAST-NAME     PIC X(15).
       01 WS-FULL-NAME     PIC X(31).
       01 WS-CSV-LINE      PIC X(50).
       01 WS-FIELD1        PIC X(15).
       01 WS-FIELD2        PIC X(15).
       01 WS-FIELD3        PIC X(15).
       01 WS-TALLY         PIC 9(3) VALUE 0.
       01 WS-SAMPLE        PIC X(30).
       01 WS-PTR           PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

      *>   --- STRING: Build a full name ---
           DISPLAY "=== STRING (concatenation) ===".
           DISPLAY "First name: ".
           ACCEPT WS-FIRST-NAME.
           DISPLAY "Last name: ".
           ACCEPT WS-LAST-NAME.

           MOVE SPACES TO WS-FULL-NAME.
           MOVE 1 TO WS-PTR.
           STRING WS-FIRST-NAME DELIMITED BY "  "
                  " "            DELIMITED BY SIZE
                  WS-LAST-NAME   DELIMITED BY "  "
             INTO WS-FULL-NAME
             WITH POINTER WS-PTR
           END-STRING.

           DISPLAY "Full name: [" WS-FULL-NAME "]".
           DISPLAY SPACES.

      *>   --- UNSTRING: Parse CSV data ---
           DISPLAY "=== UNSTRING (splitting) ===".
           MOVE "Alice,Accounting,50000" TO WS-CSV-LINE.
           DISPLAY "CSV input: " WS-CSV-LINE.

           UNSTRING WS-CSV-LINE DELIMITED BY ","
               INTO WS-FIELD1 WS-FIELD2 WS-FIELD3
           END-UNSTRING.

           DISPLAY "  Name:   [" WS-FIELD1 "]".
           DISPLAY "  Dept:   [" WS-FIELD2 "]".
           DISPLAY "  Salary: [" WS-FIELD3 "]".
           DISPLAY SPACES.

      *>   --- INSPECT: Count and replace ---
           DISPLAY "=== INSPECT (count & replace) ===".
           MOVE "Hello World, How Are You?" TO WS-SAMPLE.
           DISPLAY "Original: " WS-SAMPLE.

           MOVE 0 TO WS-TALLY.
           INSPECT WS-SAMPLE
               TALLYING WS-TALLY FOR ALL "o".
           DISPLAY "Count of 'o': " WS-TALLY.

           INSPECT WS-SAMPLE
               REPLACING ALL "o" BY "*".
           DISPLAY "After replacing 'o' with '*': "
               WS-SAMPLE.
           DISPLAY SPACES.

      *>   --- Reference modification (substring) ---
           DISPLAY "=== Reference Modification ===".
           MOVE "COBOL IS GREAT" TO WS-SAMPLE.
           DISPLAY "Full string:    " WS-SAMPLE.
           DISPLAY "Chars 1-5:      " WS-SAMPLE(1:5).
           DISPLAY "Chars 7-8:      " WS-SAMPLE(7:2).
           DISPLAY SPACES.

      *>   --- Built-in functions ---
           DISPLAY "=== UPPER/LOWER CASE ===".
           MOVE "Hello World" TO WS-SAMPLE.
           DISPLAY "Original:  " WS-SAMPLE.
           DISPLAY "Uppercase: "
               FUNCTION UPPER-CASE(WS-SAMPLE).
           DISPLAY "Lowercase: "
               FUNCTION LOWER-CASE(WS-SAMPLE).

           STOP RUN.
