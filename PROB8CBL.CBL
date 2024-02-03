       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PROB8CBL.
       AUTHOR. COLE DOMBROWSKI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-INPUT-FILE ASSIGN TO 'INVENT8.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENTORY-OUTPUT-FILE ASSIGN TO 'TEST8DOC.DOC'
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY-INPUT-FILE RECORDING MODE IS F.
       01                            PIC X(80).
       FD  INVENTORY-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-LINE                PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  EOF-INVENTORY-DATA-WS         PIC X(3)     VALUE 'NO'.
           05  NET-GRAND-DOLLAR-TOTAL-WS     PIC S9(8)V99 VALUE ZERO.
           05  OVERALL-GRAND-RECORD-COUNT-WS PIC 999      VALUE ZERO.
           05  ACCT-DOLLAR-SUBTOTAL-WS         PIC S9(8)V99 VALUE ZERO.
           05  ACCT-RECORD-COUNT-WS   PIC 999      VALUE ZERO.
           05  LINE-COUNT-WS                 PIC 99       VALUE 18.
           05  HOLD-DATE-WS.
               10  HOLD-YR-WS               PIC XXXX.
               10  HOLD-MO-WS               PIC XX.
               10  HOLD-DY-WS               PIC XX.
               10  HOLD-HR-WS               PIC XX.
               10  HOLD-MIN-WS              PIC XX.
               10  HOLD-SC-WS               PIC XX.

       01  INVENTORY-INPUT-RECORD.
           05  INV-REC-TYPE            PIC X(01).
           05  INV-BATCH-NO            PIC X(02).
           05  INV-SUPPLIER-NO         PIC X(05).
           05  INV-VOUCHER-NO          PIC X(06).
           05  INV-INVOICE-NO          PIC X(08).
           05  INV-ACCT-NO             PIC X(04).
           05  INV-STORE-NO            PIC X(03).
           05  INV-DATE.
               10  INV-DATE-MM         PIC X(02).
               10  INV-DATE-DD         PIC X(02).
               10  INV-DATE-CCYY       PIC X(04).
           05  FILLER                  PIC X(12).
           05  INV-AMT                 PIC S9(06)V99.
           05  INV-SUPPLIER-NAME       PIC X(23).

       01  INVENTORY-PRINT-LINE.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  PRT-REC-TYPE            PIC X.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-DATE-OUT.
               10  PRT-DATE-MM         PIC XX.
               10  FILLER              PIC X     VALUE '/'.
               10  PRT-DATE-DD         PIC XX.
               10  FILLER              PIC X     VALUE '/'.
               10  PRT-DATE-CCYY       PIC X(04).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-AMT                 PIC $$$,$$9.99BCR.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-ACCT-NO             PIC X(04).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-INVOICE-NO          PIC X(08).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-BATCH-NO            PIC X(02).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-VOUCHER-NO          PIC X(06).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-STORE-NO            PIC X(03).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  PRT-SUPPLIER-NO         PIC X(05).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  PRT-SUPPLIER-NAME       PIC X(23).
           05  FILLER                  PIC X(34) VALUE SPACES.

       01  REPORT-HEADING.
           05  FILLER                  PIC X     VALUE SPACES.
           05  FILLER                  PIC X(10) VALUE 'Run Date: '.
           05  RPT-HEAD-MM             PIC XX    VALUE SPACES.
           05  FILLER                  PIC X     VALUE '/'.
           05  RPT-HEAD-DD             PIC XX    VALUE SPACES.
           05  FILLER                  PIC X     VALUE '/'.
           05  RPT-HEAD-CCYY           PIC X(04) VALUE SPACES.
           05  FILLER                  PIC X(01) VALUE ' '.
           05  RPT-HEAD-HR             PIC XX    VALUE SPACES.
           05  FILLER                  PIC X     VALUE ':'.
           05  RPT-HEAD-MIN            PIC XX    VALUE SPACES.
           05  FILLER                  PIC X     VALUE ':'.
           05  RPT-HEAD-SC             PIC XX    VALUE SPACES.
           05  FILLER                  PIC X(53) VALUE
               '    Inventory Control Report for Cole Dombrowski     '.
           05  FILLER                  PIC X(55) VALUE SPACES.

       01  PRINT-LINE1-HEADING.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE 'Rec'.
           05  FILLER                  PIC X(12) VALUE '    Date  '.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(13) VALUE '    Amount'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE 'Acct'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(08) VALUE 'Invoice '.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE 'B#'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE 'Vouchr'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE 'Sto'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(05) VALUE 'Supp#'.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(23) VALUE 'Supplier'.
           05  FILLER                  PIC X(38) VALUE SPACES.

       01  PRINT-LINE2-HEADING.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE '---'.
           05  FILLER                  PIC X(12) VALUE '    ----   '.
           05  FILLER                  PIC X(01) VALUE SPACES.
           05  FILLER                  PIC X(13) VALUE '    ------'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE '----'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(08) VALUE '------- '.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(02) VALUE '--'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(06) VALUE '------'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(03) VALUE '---'.
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  FILLER                  PIC X(05) VALUE '-----'.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(23) VALUE '--------'.
           05  FILLER                  PIC X(38) VALUE SPACES.

       01  GRAND-TOTAL-LINE-1.
           05  FILLER                  PIC X     VALUE SPACES.
           05                          PIC X(5)  VALUE SPACES.
           05                          PIC X(35) VALUE
                               'NUMBER OF RECORDS PROCESSED IS:'.
           05  RECORD-COUNT-OUT        PIC ZZZ9.


       01  GRAND-TOTAL-LINE-2.
           05  FILLER                  PIC X     VALUE SPACES.
           05                          PIC X(5)  VALUE SPACES.
           05                          PIC X(24) VALUE
                               'NET DOLLAR AMOUNT IS:'.
           05  DOLLAR-AMOUNT-OUT       PIC $$$$,$$$,$$9.99BCR.

       01  ACCOUNT-TOTAL-LINE.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  FILLER                  PIC X(30) VALUE
                               'ACCOUNT TOTAL NET DOLLARS IS:'.

           05  ACCOUNT-DOLLAR-AMOUNT-OUT PIC $$$$,$$$,$$9.99BCR.
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(35) VALUE
                               'ACCOUNT TOTAL NUMBER OF RECORDS IS:'.
           05  ACCOUNT-RECORD-COUNT-OUT PIC ZZZ9.

       01  ACCOUNT-SAVE-WS.
           05  PREVIOUS-ACCT-NO         PIC X(04) VALUE SPACES.

       PROCEDURE DIVISION.
       100-MAINLINE.
           PERFORM 200-OPEN
           PERFORM 300-PROCESS UNTIL EOF-INVENTORY-DATA-WS = 'YES'
           PERFORM 900-CLOSE
           STOP RUN.

       200-OPEN.
           OPEN INPUT INVENTORY-INPUT-FILE
                OUTPUT INVENTORY-OUTPUT-FILE
           MOVE FUNCTION CURRENT-DATE TO HOLD-DATE-WS

           MOVE HOLD-MO-WS TO RPT-HEAD-MM
           MOVE HOLD-DY-WS TO RPT-HEAD-DD
           MOVE HOLD-YR-WS TO RPT-HEAD-CCYY
           MOVE HOLD-HR-WS TO RPT-HEAD-HR
           MOVE HOLD-MIN-WS TO RPT-HEAD-MIN
           MOVE HOLD-SC-WS TO RPT-HEAD-SC

           PERFORM 250-READ-RECORD

           IF INV-REC-TYPE = '2'
               MOVE INV-ACCT-NO TO PREVIOUS-ACCT-NO
               PERFORM 500-HEADER
           END-IF.

       250-READ-RECORD.
           READ INVENTORY-INPUT-FILE INTO INVENTORY-INPUT-RECORD
               AT END MOVE 'YES' TO EOF-INVENTORY-DATA-WS
           END-READ

           PERFORM UNTIL INV-REC-TYPE = '2'
               OR EOF-INVENTORY-DATA-WS = 'YES'
           READ INVENTORY-INPUT-FILE INTO INVENTORY-INPUT-RECORD
               AT END MOVE 'YES' TO EOF-INVENTORY-DATA-WS
           END-READ
           END-PERFORM.

       300-PROCESS.
           PERFORM UNTIL EOF-INVENTORY-DATA-WS = 'YES'
           IF INV-REC-TYPE = '2'
               IF INV-ACCT-NO NOT EQUAL TO PREVIOUS-ACCT-NO
                   IF LINE-COUNT-WS > 0
                       PERFORM 600-WRITE-TOTALS
                   END-IF

                   MOVE 0 TO LINE-COUNT-WS
                   MOVE 0 TO ACCT-RECORD-COUNT-WS
                   PERFORM 500-HEADER
                   MOVE INV-ACCT-NO TO PREVIOUS-ACCT-NO
               END-IF

               PERFORM 375-PRINT

               ADD 1 TO LINE-COUNT-WS
               ADD 1 TO ACCT-RECORD-COUNT-WS
               ADD 1 TO OVERALL-GRAND-RECORD-COUNT-WS
               ADD INV-AMT TO NET-GRAND-DOLLAR-TOTAL-WS
               ADD INV-AMT TO ACCT-DOLLAR-SUBTOTAL-WS

               IF LINE-COUNT-WS >= 18
                   MOVE 0 TO LINE-COUNT-WS
                       PERFORM 500-HEADER
               END-IF
           END-IF

           PERFORM 250-READ-RECORD
               END-PERFORM.

       375-PRINT.
           MOVE INV-REC-TYPE       TO PRT-REC-TYPE
           MOVE INV-BATCH-NO       TO PRT-BATCH-NO
           MOVE INV-SUPPLIER-NO    TO PRT-SUPPLIER-NO
           MOVE INV-VOUCHER-NO     TO PRT-VOUCHER-NO
           MOVE INV-INVOICE-NO     TO PRT-INVOICE-NO
           MOVE INV-ACCT-NO        TO PRT-ACCT-NO
           MOVE INV-STORE-NO       TO PRT-STORE-NO
           MOVE INV-AMT            TO PRT-AMT
           MOVE INV-DATE-MM        TO PRT-DATE-MM
           MOVE INV-DATE-DD        TO PRT-DATE-DD
           MOVE INV-DATE-CCYY      TO PRT-DATE-CCYY
           MOVE INV-SUPPLIER-NAME  TO PRT-SUPPLIER-NAME

           WRITE PRINT-LINE FROM INVENTORY-PRINT-LINE.

       500-HEADER.
           MOVE SPACES TO PRINT-LINE
           WRITE PRINT-LINE AFTER PAGE
           WRITE PRINT-LINE FROM REPORT-HEADING AFTER PAGE
           WRITE PRINT-LINE FROM PRINT-LINE1-HEADING AFTER 1
           WRITE PRINT-LINE FROM PRINT-LINE2-HEADING AFTER 1
           WRITE PRINT-LINE FROM SPACES AFTER 1
           MOVE 0 TO LINE-COUNT-WS.

       600-WRITE-TOTALS.
           IF LINE-COUNT-WS > 0
               MOVE ACCT-DOLLAR-SUBTOTAL-WS
                   TO ACCOUNT-DOLLAR-AMOUNT-OUT
               MOVE ACCT-RECORD-COUNT-WS
                   TO ACCOUNT-RECORD-COUNT-OUT
               WRITE PRINT-LINE FROM ACCOUNT-TOTAL-LINE AFTER 2
               MOVE ZERO TO ACCT-DOLLAR-SUBTOTAL-WS
               MOVE ZERO TO ACCT-RECORD-COUNT-WS
           END-IF.

       700-FINALIZE-ACCOUNT-TOTALS.
           IF ACCT-RECORD-COUNT-WS > 0 THEN
               MOVE ACCT-DOLLAR-SUBTOTAL-WS
                   TO ACCOUNT-DOLLAR-AMOUNT-OUT
               MOVE ACCT-RECORD-COUNT-WS
                    TO ACCOUNT-RECORD-COUNT-OUT
               WRITE PRINT-LINE FROM ACCOUNT-TOTAL-LINE AFTER 1
           END-IF.

       900-CLOSE.
           PERFORM 700-FINALIZE-ACCOUNT-TOTALS

           MOVE NET-GRAND-DOLLAR-TOTAL-WS TO DOLLAR-AMOUNT-OUT
           MOVE OVERALL-GRAND-RECORD-COUNT-WS TO RECORD-COUNT-OUT
           WRITE PRINT-LINE FROM GRAND-TOTAL-LINE-1 AFTER 3
           WRITE PRINT-LINE FROM GRAND-TOTAL-LINE-2 AFTER 1
           CLOSE INVENTORY-INPUT-FILE
           CLOSE INVENTORY-OUTPUT-FILE.
