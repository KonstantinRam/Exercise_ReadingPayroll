       IDENTIFICATION DIVISION. 
       PROGRAM-ID. PayrollProcessing.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO "PAYROLL.DAT"
           ORGANIZATION IS SEQUENTIAL
      *>       Bug was in Payroll.dat format. CRLF + missing FILLER PADDING at the END.
      *>       Added LINE SEQUENTIAL version FOR ORGANIZATION IS LINE SEQUENTIAL for Windows style, but we for this project 
      *>       I follow  mainframe style.
      *>       Another bug => missing 0 in from of rates.
           FILE STATUS IS WS-FILE-STATUS. 
           
           SELECT SORT-WORK ASSIGN TO SORTWK01.

       DATA DIVISION.
       FILE SECTION.
       FD PAYROLL-FILE.
         01  PAYROLL-RECORD-RAW       PIC X(80).

       SD  SORT-WORK.
       01  SORT-RECORD.
           05  SR-SORT-KEY.
             10  SR-TOTAL-PAY     PIC S9(7)V99 COMP-3.
             10  SR-DEPT          PIC X(3).
           05  SR-EMP-ID            PIC X(8).
           05  SR-LASTNAME          PIC X(20).
           05  SR-FIRSTNAME         PIC X(15).
           05  SR-HOURS             PIC 9(3)V9.
           05  SR-RATE              PIC 9(3)V99.
           05  SR-HIRE-DATE         PIC 9(6).    



       WORKING-STORAGE SECTION. 
       01  C-WORK-HOUR-BASE         PIC 9(3) VALUE 40.
       01  C-OVERTIME-MULT          PIC 9V9 VALUE 1.5.

       01  WS-DEBUG-LEVEL           PIC 9 VALUE 0.
           88  DEBUG-OFF            VALUE 0.
           88  DEBUG-ON             VALUE 1.

       01  WS-INPUT-RECORD.
           05  WS-EMP-ID-X          PIC X(8).
           05  WS-LASTNAME-X        PIC X(20).
           05  WS-FIRSTNAME-X       PIC X(15).
           05  WS-DEPT-X            PIC X(3).
           05  WS-HOURS-X           PIC X(4).
           05  WS-HOURS-X-NUM REDEFINES WS-HOURS-X PIC 9(3)V9.
      *>           Bug was here. I have TO redifine VALUE instead OF MOVE directly.
      *>           direct move was turning 0500 into 500.0 instead of 50.0
           05  WS-RATE-X            PIC X(5).
           05  WS-RATE-X-NUM REDEFINES WS-RATE-X PIC 9(3)V99.
           05  WS-HIRE-DATE-X       PIC X(6).
           05  FILLER               PIC X(19).
 
       01  WS-VALIDATED-RECORD.
           05  WS-EMP-ID            PIC X(8) VALUE 'BLANC'.
           05  WS-LASTNAME          PIC X(20) VALUE 'BLANC'.
           05  WS-FIRSTNAME         PIC X(15) VALUE 'BLANC'.
           05  WS-DEPT              PIC X(3) VALUE SPACES.
           05  WS-HOURS             PIC 9(3)V9 VALUE 0.
           05  WS-RATE              PIC 9(3)V99 VALUE ZEROS.
           05  WS-HIRE-DATE         PIC 9(6) VALUE 000000.
           05  WS-RECORD-STATUS     PIC X VALUE 'N'.
               88  RECORD-VALID     VALUE 'Y'.
               88  RECORD-INVALID   VALUE 'N'.

       01  WS-RECORD-DSP.
           05  WS-EMP-ID-DSP        PIC X(8) VALUE 'BLANC'.
           05  WS-LASTNAME-DSP      PIC X(20) VALUE 'BLANC'.
           05  WS-FIRSTNAME-DSP     PIC X(15) VALUE 'BLANC'.
           05  WS-DEPT-DSP          PIC X(3) VALUE SPACES.
           05  WS-HOURS-DSP         PIC Z9.9 VALUE '0'.
           05  WS-RATE-DSP          PIC $$$,$$9.99.
           05  WS-HIRE-DATE-DSP     PIC X(8) VALUE SPACES.

       01 WS-CALCULATED-PAY.
           05  WS-TOTAL-PAY         PIC S9(8)V99 VALUE 0 COMP-3.
           05  WS-OVERTIME-HRS      PIC S9(3)V9 VALUE 0 COMP-3.
           05  WS-REGULAR-PAY       PIC S9(8)V99 VALUE 0 COMP-3.
           05  WS-OVERTIME-PAY      PIC S9(8)V99 VALUE 0 COMP-3.
           05  WS-OVERTIME-RATE     PIC S9(4)V99 VALUE 0 COMP-3.

       01 WS-PAYROLL-DSP.
          05  WS-TOTAL-PAY-DSP     PIC Z9.9 VALUE '0'.
          05  WS-OVERTIME-HRS-DSP  PIC Z9.9 VALUE '0'.
          05  WS-REGULAR-PAY-DSP   PIC $$$,$$9.99 VALUE '0'.
          05  WS-OVERTIME-PAY-DSP  PIC $$$,$$9.99 VALUE '0'.

       01 WS-FILE-STATUS            PIC XX.
          88 FILE-OK                VALUE "00".
          88 FILE-EOF               VALUE "10".
          88 FILE-NOT-FOUND         VALUE "35".

       01 WS-DISPLAY-LINE           PIC X(80) VALUE SPACES.
       01  WS-ERROR-ACCUMULATOR.
           05  WS-ERROR-BUFFER      PIC X(500) VALUE SPACES.
           05  WS-ERROR-PTR         PIC 999 VALUE 1.
           05  WS-ERROR-COUNT       PIC 99 VALUE ZERO.
      
       01  WS-CONTROL-FIELDS.
           05  WS-PREV-DEPT     PIC X(3) VALUE SPACES.
           05  WS-DEPT-TOTAL    PIC S9(9)V99 COMP-3 VALUE ZERO.
           05  WS-DEPT-COUNT    PIC S9(5) COMP VALUE ZERO.
           05  WS-GRAND-TOTAL   PIC S9(11)V99 COMP-3 VALUE ZERO.
           05  WS-GRAND-COUNT   PIC S9(7) COMP VALUE ZERO.

       01  WS-SORT-EOF              PIC X VALUE 'N'.
           88 SORT-EOF              VALUE 'Y'.
       PROCEDURE DIVISION.
              
           DISPLAY "Execution started" UPON CONSOLE
           SORT SORT-WORK
               ON DESCENDING KEY SR-TOTAL-PAY
               ON ASCENDING KEY SR-DEPT
               INPUT PROCEDURE IS 1000-INPUT-PROCEDURE
               OUTPUT PROCEDURE IS 4000-OUTPUT-PROCEDURE
           
               DISPLAY 'PAYROLL PROCESSING COMPLETE'
               DISPLAY 'TOTAL EMPLOYEES: ' WS-GRAND-COUNT
               DISPLAY 'TOTAL PAYROLL: ' WS-GRAND-TOTAL
                    
           DISPLAY "Execution stopped"
           GOBACK.

       1000-INPUT-PROCEDURE.
           OPEN INPUT PAYROLL-FILE

           PERFORM UNTIL FILE-EOF
               READ PAYROLL-FILE
                 AT END
                   SET FILE-EOF TO TRUE
                 NOT AT END
      *> Any other file checks?                
                   MOVE PAYROLL-RECORD-RAW TO WS-INPUT-RECORD
                   PERFORM 2000-VALIDATE-AND-MOVE
                   IF RECORD-VALID
                      PERFORM 2200-CALCULATE-AND-RELEASE
                   ELSE
      *> It would be nice to have proper error handling, but for the test task I just do DISPLAY
                     DISPLAY "Error record. EMP ID:" WS-EMP-ID
                   END-IF
               END-READ
    
               IF NOT (FILE-OK OR FILE-EOF)
                  DISPLAY "READ ERROR: " WS-FILE-STATUS
                  PERFORM 3000-ABORT-RUN
               END-IF
                       
               PERFORM 1100-DISP-RECORD-CONDITIONAL
           END-PERFORM

           CLOSE PAYROLL-FILE
           .

       1100-DISP-RECORD-CONDITIONAL.
           IF DEBUG-ON
               PERFORM 1110-DISPLAY-PAYROLL-DEBUG
           END-IF
           .

       1110-DISPLAY-PAYROLL-DEBUG.
           PERFORM 1200-CREATE-RECORD-DSP
           PERFORM 1300-CALCULATE-PAYROLL
           PERFORM 1400-CREATE-PAYROLL-DSP

           DISPLAY " "
           DISPLAY "EMP-ID: " WS-EMP-ID-DSP " [" WS-LASTNAME-DSP ", " 
                   WS-FIRSTNAME-DSP "]"
    
           MOVE SPACES TO WS-DISPLAY-LINE
           STRING "Dept: " WS-DEPT-DSP 
                  "  Hours: " WS-HOURS-DSP
                  "  Rate: " WS-RATE-DSP 
                  "  Hired: " WS-HIRE-DATE-DSP
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           DISPLAY WS-DISPLAY-LINE
    
                   IF WS-OVERTIME-HRS > 0
                       
                       DISPLAY "  Overtime: "
                               WS-OVERTIME-HRS-DSP " hr."
                   END-IF
           .

       1200-CREATE-RECORD-DSP.
           INITIALIZE WS-RECORD-DSP
           MOVE WS-EMP-ID    TO WS-EMP-ID-DSP
           MOVE WS-LASTNAME  TO WS-LASTNAME-DSP
           MOVE WS-FIRSTNAME TO WS-FIRSTNAME-DSP
           MOVE WS-DEPT      TO WS-DEPT-DSP
           MOVE WS-HOURS     TO WS-HOURS-DSP
           MOVE WS-RATE      TO WS-RATE-DSP
           STRING WS-HIRE-DATE(1:2) "/"
                  WS-HIRE-DATE(3:2) "/"
                  WS-HIRE-DATE(5:2)
                  DELIMITED BY SIZE
                  INTO WS-HIRE-DATE-DSP
           END-STRING
           .

       1300-CALCULATE-PAYROLL.
           INITIALIZE WS-CALCULATED-PAY

           IF WS-HOURS > C-WORK-HOUR-BASE
              COMPUTE WS-OVERTIME-HRS = WS-HOURS - C-WORK-HOUR-BASE
              COMPUTE WS-REGULAR-PAY = C-WORK-HOUR-BASE * WS-RATE
              COMPUTE WS-OVERTIME-RATE = WS-RATE * C-OVERTIME-MULT
              COMPUTE WS-OVERTIME-PAY = 
                       WS-OVERTIME-HRS * WS-OVERTIME-RATE
              COMPUTE WS-TOTAL-PAY = WS-REGULAR-PAY + WS-OVERTIME-PAY
           ELSE
              COMPUTE WS-TOTAL-PAY = WS-HOURS * WS-RATE
           END-IF
           .

       1400-CREATE-PAYROLL-DSP.
           INITIALIZE WS-PAYROLL-DSP
           MOVE WS-OVERTIME-HRS TO WS-OVERTIME-HRS-DSP
           MOVE WS-TOTAL-PAY    TO WS-TOTAL-PAY-DSP
           MOVE WS-REGULAR-PAY  TO WS-REGULAR-PAY-DSP
           MOVE WS-OVERTIME-PAY TO WS-OVERTIME-PAY-DSP
           .

       2000-VALIDATE-AND-MOVE.
                   INITIALIZE WS-VALIDATED-RECORD
                   INITIALIZE WS-ERROR-ACCUMULATOR
                   
                   STRING " EMP:" WS-EMP-ID-X
                          " ERRORS:"
                          DELIMITED BY SIZE
                          INTO WS-ERROR-BUFFER
                          WITH POINTER WS-ERROR-PTR
                   END-STRING
                   MOVE WS-EMP-ID-X    TO WS-EMP-ID
                   MOVE WS-LASTNAME-X  TO WS-LASTNAME
                   MOVE WS-FIRSTNAME-X TO WS-FIRSTNAME
                   MOVE WS-DEPT-X      TO WS-DEPT
      *> Here could be checks for wrong names or non existing departments, but checking NUMERIC corruption is enough IMHO for training task.

                   IF WS-HOURS-X IS NUMERIC
                       MOVE WS-HOURS-X-NUM TO WS-HOURS
                   ELSE
                       MOVE ZERO TO WS-HOURS
    
                       ADD 1 TO WS-ERROR-COUNT
                       STRING " [HOURS:" WS-HOURS-X "]"
                              DELIMITED BY SIZE
                              INTO WS-ERROR-BUFFER
                              WITH POINTER WS-ERROR-PTR
                       END-STRING
      
                   END-IF
                   
                   IF WS-RATE-X IS NUMERIC
                       MOVE WS-RATE-X-NUM TO WS-RATE
                   ELSE
                       ADD 1 TO WS-ERROR-COUNT
                       STRING " [RATE:" WS-RATE-X "]"
                       DELIMITED BY SIZE
                       INTO WS-ERROR-BUFFER
                       WITH POINTER WS-ERROR-PTR
                   END-IF
    
                   IF WS-HIRE-DATE-X IS NUMERIC
                       MOVE WS-HIRE-DATE-X TO WS-HIRE-DATE
                   ELSE
                       ADD 1 TO WS-ERROR-COUNT
                       STRING " [HIRE DATE:" WS-HIRE-DATE-X "]"
                       DELIMITED BY SIZE
                       INTO WS-ERROR-BUFFER
                       WITH POINTER WS-ERROR-PTR
                   END-IF

                   IF WS-ERROR-COUNT > ZERO
                       SET RECORD-INVALID TO TRUE
                       PERFORM 2100-WRITE-VALIDATION-ERROR
                   ELSE
                       SET RECORD-VALID TO TRUE
                   END-IF
           .

       2100-WRITE-VALIDATION-ERROR.
           DISPLAY WS-ERROR-BUFFER
           .

       2200-CALCULATE-AND-RELEASE.
           PERFORM 1300-CALCULATE-PAYROLL
          
           MOVE WS-TOTAL-PAY     TO SR-TOTAL-PAY
           MOVE WS-DEPT          TO SR-DEPT
           MOVE WS-EMP-ID        TO SR-EMP-ID
           MOVE WS-LASTNAME      TO SR-LASTNAME
           MOVE WS-FIRSTNAME     TO SR-FIRSTNAME
           MOVE WS-HOURS         TO SR-HOURS
           MOVE WS-RATE          TO SR-RATE
           MOVE WS-HIRE-DATE     TO SR-HIRE-DATE

           RELEASE SORT-RECORD
           .

       3000-ABORT-RUN.
                   DISPLAY "Programm execution was aborted."
                   STOP RUN
           .


       4000-OUTPUT-PROCEDURE.

           INITIALIZE WS-SORT-EOF
           PERFORM UNTIL SORT-EOF
               RETURN SORT-WORK
                   AT END
                       SET SORT-EOF TO TRUE
                   NOT AT END
                       DISPLAY SR-DEPT " " SR-EMP-ID " " SR-HOURS
      *                 PERFORM 2200-PROCESS-SORTED-RECORD
               END-RETURN
           END-PERFORM
           CONTINUE
           .

       END PROGRAM PayrollProcessing.
       