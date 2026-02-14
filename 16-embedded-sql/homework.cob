       IDENTIFICATION DIVISION.
       PROGRAM-ID. HOMEWORK-16.
       AUTHOR. LEARNER.

      *> ============================================
      *> HOMEWORK 16: Embedded SQL (Simulated)
      *> ============================================
      *> ASSIGNMENT:
      *>   Build a "Hotel Reservation System" that
      *>   simulates database operations. Since we
      *>   don't have DB2, you'll use COBOL tables
      *>   to simulate a database - but write your
      *>   code as if you were using real SQL.
      *>
      *>   For each operation, include a comment
      *>   block showing what the real EXEC SQL
      *>   would look like, followed by the COBOL
      *>   table code that simulates it.
      *>
      *>   "DATABASE" TABLE - ROOMS:
      *>     Room Number    PIC 9(3)      (key)
      *>     Room Type      PIC X(10)     (Single/Double/Suite)
      *>     Rate Per Night PIC 9(4)V99
      *>     Guest Name     PIC X(25)     (spaces = vacant)
      *>     Check-In Date  PIC 9(8)      (YYYYMMDD, 0 = vacant)
      *>     Nights         PIC 9(2)      (0 = vacant)
      *>
      *>   Pre-load 10 rooms (mix of types/rates).
      *>   Rooms 1-4: Single ($89.99)
      *>   Rooms 5-7: Double ($129.99)
      *>   Rooms 8-10: Suite ($249.99)
      *>   Start with rooms 2, 5, 9 occupied.
      *>
      *>   MENU:
      *>     1. Check Room Availability
      *>        Simulates: SELECT * FROM ROOMS
      *>          WHERE GUEST_NAME = SPACES
      *>        Show all vacant rooms with type & rate.
      *>
      *>     2. Book a Room
      *>        Simulates: UPDATE ROOMS
      *>          SET GUEST_NAME = :name,
      *>              CHECKIN = :date, NIGHTS = :n
      *>          WHERE ROOM_NO = :room
      *>            AND GUEST_NAME = SPACES
      *>        Ask for room #, guest name, date, nights.
      *>        Reject if room is already occupied.
      *>
      *>     3. Check Out
      *>        Simulates: UPDATE ROOMS
      *>          SET GUEST_NAME = SPACES,
      *>              CHECKIN = 0, NIGHTS = 0
      *>          WHERE ROOM_NO = :room
      *>            AND GUEST_NAME <> SPACES
      *>        Calculate and display the total bill
      *>        (rate * nights). Clear the room.
      *>
      *>     4. Guest Lookup
      *>        Simulates: SELECT * FROM ROOMS
      *>          WHERE GUEST_NAME = :name
      *>        (Cursor pattern - could match multiple)
      *>        Search by guest name, display all
      *>        matching rooms.
      *>
      *>     5. Revenue Report
      *>        Simulates: SELECT ROOM_TYPE,
      *>          COUNT(*), SUM(RATE * NIGHTS)
      *>          FROM ROOMS WHERE GUEST <> SPACES
      *>          GROUP BY ROOM_TYPE
      *>        Show occupied rooms grouped by type
      *>        with subtotals.
      *>
      *>     6. Exit
      *>
      *>   Example:
      *>
      *>   Choice: 1
      *>   --- Vacant Rooms ---
      *>   *> SELECT ROOM_NO, ROOM_TYPE, RATE
      *>   *>   FROM ROOMS WHERE GUEST = SPACES
      *>   Room  Type       Rate
      *>   001   Single     $89.99
      *>   003   Single     $89.99
      *>   ...
      *>
      *> REQUIREMENTS:
      *>   1. Use a COBOL table with OCCURS to
      *>      simulate the database.
      *>   2. Before each operation, include a
      *>      comment block showing the equivalent
      *>      EXEC SQL statement.
      *>   3. Simulate SQLCODE: set a status variable
      *>      to 0 (found), 100 (not found), or
      *>      -1 (error) after each "query."
      *>   4. For the guest lookup, simulate a cursor
      *>      by looping through and collecting all
      *>      matches (not just the first).
      *>   5. Use paragraphs for each menu option.
      *>
      *> TO COMPILE AND RUN:
      *>   cobc -x homework.cob -o homework
      *>   ./homework
      *> ============================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *>   DECLARE YOUR "DATABASE" TABLE AND
      *>   WORKING VARIABLES HERE


       PROCEDURE DIVISION.
       0000-MAIN.
      *>   YOUR CODE GOES HERE

           STOP RUN.

      *>   ADD YOUR PARAGRAPHS HERE
