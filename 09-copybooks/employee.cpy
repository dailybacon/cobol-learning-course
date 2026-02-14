      *> ============================================
      *> COPYBOOK: employee.cpy
      *> ============================================
      *> This is a copybook - a reusable data layout.
      *> Any program can COPY this file to get the
      *> same record structure.
      *>
      *> Notice there is NO IDENTIFICATION DIVISION
      *> or PROCEDURE DIVISION. Copybooks are just
      *> fragments that get inserted into programs.
      *> ============================================

       01 WS-EMP-RECORD.
          05 WS-EMP-ID       PIC X(4).
          05 WS-EMP-NAME     PIC X(30).
          05 WS-EMP-DEPT     PIC X(20).
          05 WS-EMP-SALARY   PIC 9(6)V99.
