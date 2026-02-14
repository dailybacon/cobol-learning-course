       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-07.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 7: String Handling
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a contact info parser and formatter.
      *>
      *>   PART 1 - PARSE:
      *>   Accept a contact string in this format:
      *>     "LastName,FirstName,555-1234,city"
      *>   Use UNSTRING to split it into 4 fields.
      *>
      *>   PART 2 - FORMAT:
      *>   Use STRING to build a formatted output:
      *>     "FirstName LastName - city (555-1234)"
      *>   (Note: first and last name are swapped
      *>    from the input order.)
      *>
      *>   PART 3 - ANALYZE:
      *>   Use INSPECT to:
      *>     - Count the vowels (a,e,i,o,u) in the
      *>       full name (first + last combined).
      *>     - Replace all dashes in the phone number
      *>       with dots (555-1234 -> 555.1234).
      *>
      *>   PART 4 - EXTRACT:
      *>   Use reference modification to:
      *>     - Display the area code (first 3 chars
      *>       of the phone number).
      *>     - Display the initials (first char of
      *>       first name + first char of last name).
      *>
      *>   Example:
      *>
      *>   Enter contact (Last,First,Phone,City):
      *>   Smith,John,555-1234,Denver
      *>
      *>   --- Parsed Fields ---
      *>   Last:  Smith
      *>   First: John
      *>   Phone: 555-1234
      *>   City:  Denver
      *>
      *>   --- Formatted ---
      *>   John Smith - Denver (555-1234)
      *>
      *>   --- Analysis ---
      *>   Vowels in name: 2
      *>   Phone with dots: 555.1234
      *>
      *>   --- Extracted ---
      *>   Area code: 555
      *>   Initials: JS
      *>
      *> REQUIREMENTS:
      *>   1. Use UNSTRING with DELIMITED BY ","
      *>   2. Use STRING with DELIMITED BY SPACE
      *>      or DELIMITED BY SIZE
      *>   3. Use INSPECT TALLYING for vowel count
      *>   4. Use INSPECT REPLACING for phone dots
      *>   5. Use reference modification for extracts
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR VARIABLES HERE


       PROCEDURE DIVISION.
      *>   YOUR CODE GOES HERE


           STOP RUN.
