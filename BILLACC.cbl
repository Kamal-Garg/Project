      ******************************************************************
      * PROGRAM NAME: BILLACC
      * DESCRIPTION : Reads Billing Account Number as input and
      *               produces a report with Premise Number and Address.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BILLACC.
       AUTHOR.        PROJECT TEAM.
       DATE-WRITTEN.  2025-07-15.

      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO BILLINPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-INPUT-STATUS.

           SELECT OUTPUT-REPORT
               ASSIGN TO BILLRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WS-OUTPUT-STATUS.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

      *----------------------------------------------------------------*
      * INPUT FILE - one record per Billing Account
      *----------------------------------------------------------------*
       FD  INPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS.

       01  INPUT-RECORD.
           05  IN-BILLING-ACCOUNT     PIC X(15).
           05  IN-PREMISE-NUMBER      PIC X(10).
           05  IN-ADDRESS-LINE1       PIC X(30).
           05  IN-ADDRESS-LINE2       PIC X(20).
           05  IN-PHONE-NUMBER        PIC X(12).
           05  FILLER                 PIC X(13).

      *----------------------------------------------------------------*
      * OUTPUT REPORT FILE
      *----------------------------------------------------------------*
       FD  OUTPUT-REPORT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS.

       01  OUTPUT-RECORD              PIC X(132).

      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           05  WS-INPUT-STATUS        PIC XX  VALUE SPACES.
           05  WS-OUTPUT-STATUS       PIC XX  VALUE SPACES.

       01  WS-FLAGS.
           05  WS-END-OF-FILE         PIC X   VALUE 'N'.
               88  END-OF-FILE                VALUE 'Y'.
           05  WS-FIRST-PAGE          PIC X   VALUE 'Y'.
               88  FIRST-PAGE                 VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-RECORD-COUNT        PIC 9(6)  VALUE ZEROS.
           05  WS-LINE-COUNT          PIC 9(3)  VALUE ZEROS.
           05  WS-PAGE-NUMBER         PIC 9(4)  VALUE ZEROS.
           05  WS-MAX-LINES           PIC 9(3)  VALUE 55.

       01  WS-CURRENT-DATE.
           05  WS-YEAR                PIC 9(4).
           05  WS-MONTH               PIC 9(2).
           05  WS-DAY                 PIC 9(2).

       01  WS-FORMATTED-DATE          PIC X(10).

      *----------------------------------------------------------------*
      * REPORT HEADER LINES
      *----------------------------------------------------------------*
       01  RPT-PAGE-HEADER-1.
           05  FILLER   PIC X(10)  VALUE SPACES.
           05  FILLER   PIC X(40)
               VALUE 'BILLING ACCOUNT PREMISE & ADDRESS REPORT'.
           05  FILLER   PIC X(20)  VALUE SPACES.
           05  FILLER   PIC X(5)   VALUE 'DATE:'.
           05  RPT-DATE PIC X(10)  VALUE SPACES.
           05  FILLER   PIC X(5)   VALUE SPACES.
           05  FILLER   PIC X(5)   VALUE 'PAGE:'.
           05  RPT-PAGE PIC ZZZ9.
           05  FILLER   PIC X(27)  VALUE SPACES.

       01  RPT-PAGE-HEADER-2.
           05  FILLER   PIC X(132) VALUE SPACES.

       01  RPT-COLUMN-HEADER.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(15)  VALUE 'BILLING ACCOUNT'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(14)  VALUE 'PREMISE NUMBER'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(30)  VALUE 'ADDRESS LINE 1'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(20)  VALUE 'ADDRESS LINE 2'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(12)  VALUE 'PHONE NUMBER'.
           05  FILLER   PIC X(26)  VALUE SPACES.

       01  RPT-COLUMN-UNDERLINE.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(15)  VALUE '---------------'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(14)  VALUE '--------------'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(30)  VALUE '------------------------------'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(20)  VALUE '--------------------'.
           05  FILLER   PIC X(3)   VALUE SPACES.
           05  FILLER   PIC X(12)  VALUE '------------'.
           05  FILLER   PIC X(26)  VALUE SPACES.

      *----------------------------------------------------------------*
      * DETAIL LINE
      *----------------------------------------------------------------*
       01  RPT-DETAIL-LINE.
           05  FILLER               PIC X(3)   VALUE SPACES.
           05  RPT-BILLING-ACCOUNT  PIC X(15)  VALUE SPACES.
           05  FILLER               PIC X(3)   VALUE SPACES.
           05  RPT-PREMISE-NUMBER   PIC X(14)  VALUE SPACES.
           05  FILLER               PIC X(3)   VALUE SPACES.
           05  RPT-ADDRESS-LINE1    PIC X(30)  VALUE SPACES.
           05  FILLER               PIC X(3)   VALUE SPACES.
           05  RPT-ADDRESS-LINE2    PIC X(20)  VALUE SPACES.
           05  FILLER               PIC X(3)   VALUE SPACES.
           05  RPT-PHONE-NUMBER     PIC X(12)  VALUE SPACES.
           05  FILLER               PIC X(26)  VALUE SPACES.

      *----------------------------------------------------------------*
      * TOTAL LINE
      *----------------------------------------------------------------*
       01  RPT-TOTAL-LINE.
           05  FILLER               PIC X(3)   VALUE SPACES.
           05  FILLER               PIC X(22)
               VALUE 'TOTAL RECORDS PRINTED: '.
           05  RPT-TOTAL-COUNT      PIC ZZZ,ZZ9.
           05  FILLER               PIC X(100) VALUE SPACES.

      ******************************************************************
       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS
               UNTIL END-OF-FILE
           PERFORM 3000-TERMINATE
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INITIALIZE.
           OPEN INPUT  INPUT-FILE
           OPEN OUTPUT OUTPUT-REPORT

           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           STRING WS-YEAR  '-'
                  WS-MONTH '-'
                  WS-DAY
               DELIMITED SIZE
               INTO WS-FORMATTED-DATE

           PERFORM 1100-PRINT-PAGE-HEADER
           PERFORM 1200-READ-INPUT.

       1100-PRINT-PAGE-HEADER.
           ADD 1                    TO WS-PAGE-NUMBER
           MOVE WS-FORMATTED-DATE  TO RPT-DATE
           MOVE WS-PAGE-NUMBER     TO RPT-PAGE
           WRITE OUTPUT-RECORD FROM RPT-PAGE-HEADER-1
           WRITE OUTPUT-RECORD FROM RPT-PAGE-HEADER-2
           WRITE OUTPUT-RECORD FROM RPT-COLUMN-HEADER
           WRITE OUTPUT-RECORD FROM RPT-COLUMN-UNDERLINE
           MOVE 4                  TO WS-LINE-COUNT.

       1200-READ-INPUT.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE 'Y' TO WS-END-OF-FILE
           END-READ.

      *----------------------------------------------------------------*
       2000-PROCESS.
           IF WS-LINE-COUNT >= WS-MAX-LINES
               PERFORM 1100-PRINT-PAGE-HEADER
           END-IF

           MOVE IN-BILLING-ACCOUNT  TO RPT-BILLING-ACCOUNT
           MOVE IN-PREMISE-NUMBER   TO RPT-PREMISE-NUMBER
           MOVE IN-ADDRESS-LINE1    TO RPT-ADDRESS-LINE1
           MOVE IN-ADDRESS-LINE2    TO RPT-ADDRESS-LINE2
           MOVE IN-PHONE-NUMBER     TO RPT-PHONE-NUMBER

           WRITE OUTPUT-RECORD FROM RPT-DETAIL-LINE
           ADD 1                    TO WS-RECORD-COUNT
           ADD 1                    TO WS-LINE-COUNT

           PERFORM 1200-READ-INPUT.

      *----------------------------------------------------------------*
       3000-TERMINATE.
           MOVE WS-RECORD-COUNT     TO RPT-TOTAL-COUNT
           WRITE OUTPUT-RECORD FROM RPT-PAGE-HEADER-2
           WRITE OUTPUT-RECORD FROM RPT-TOTAL-LINE

           CLOSE INPUT-FILE
           CLOSE OUTPUT-REPORT.
