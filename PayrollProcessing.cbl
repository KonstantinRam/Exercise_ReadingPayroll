               IDENTIFICATION DIVISION. 
               PROGRAM-ID. PayrollProcessing.
      
               ENVIRONMENT DIVISION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT PAYROLL-FILE ASSIGN TO "PAYROLL.DAT"
                   ORGANIZATION IS SEQUENTIAL
      *>           Bug was in Payroll.dat format. CRLF + missing FILLER PADDING at the END.
      *>           Added LINE SEQUENTIAL version FOR ORGANIZATION IS LINE SEQUENTIAL for Windows style, but we for this project 
      *>           I follow  mainframe style.
      *>           Another bug => missing 0 in from of rates.
                   FILE STATUS IS WS-FILE-STATUS. 
               
               DATA DIVISION.
               FILE SECTION.
               FD PAYROLL-FILE.
               01  PAYROLL-RECORD-RAW    PIC X(80).

               WORKING-STORAGE SECTION. 
               01  C-WORK-HR-BASE   PIC 9(3) VALUE 40.

               01  WS-DEBUG-LEVEL        PIC 9 VALUE 1.
                   88  DEBUG-OFF         VALUE 0.
                   88  DEBUG-ON          VALUE 1.

               01  WS-INPUT-RECORD.
                   05  WS-EMP-ID-X       PIC X(8).
                   05  WS-LASTNAME-X     PIC X(20).
                   05  WS-FIRSTNAME-X    PIC X(15).
                   05  WS-DEPT-X         PIC X(3).
                   05  WS-HOURS-X        PIC X(4).
                   05  WS-HOURS-X-NUM REDEFINES WS-HOURS-X PIC 9(3)V9.
      *>           Bug was here. I have TO redifine VALUE instead OF MOVE directly.
      *>           direct move was turning 0500 into 500.0 instead of 50.0
                   05  WS-RATE-X         PIC X(5).
                   05  WS-RATE-X-NUM REDEFINES WS-RATE-X PIC 9(3)V99.
                   05  WS-HIRE-DATE-X    PIC X(6).
                   05  FILLER            PIC X(19).
 
               01  WS-VALIDATED-RECORD.
                   05  WS-EMP-ID             PIC X(8) VALUE 'BLANC'.
                   05  WS-LASTNAME           PIC X(20) VALUE 'BLANC'.
                   05  WS-FIRSTNAME          PIC X(15) VALUE 'BLANC'.
                   05  WS-DEPT               PIC X(3) VALUE SPACES.
                   05  WS-HOURS              PIC 9(3)V9 VALUE 0.
                   05  WS-RATE               PIC 9(3)V99 VALUE ZEROS.
                   05  WS-HIRE-DATE          PIC 9(6) VALUE 000000.
                   05  WS-RECORD-STATUS      PIC X VALUE 'N'.
                       88  RECORD-VALID      VALUE 'Y'.
                       88  RECORD-INVALID    VALUE 'N'.

               01  WS-RECORD-DSP.
                   05  WS-EMP-ID-DSP         PIC X(8) VALUE 'BLANC'.
                   05  WS-LASTNAME-DSP       PIC X(20) VALUE 'BLANC'.
                   05  WS-FIRSTNAME-DSP      PIC X(15) VALUE 'BLANC'.
                   05  WS-DEPT-DSP           PIC X(3) VALUE SPACES.
                   05  WS-HOURS-DSP          PIC Z9.9 VALUE 0.
                   05  WS-RATE-DSP           PIC $$$,$$9.99 VALUE 0.
                   05  WS-HIRE-DATE-DSP      PIC X(8) VALUE SPACES.

               01 WS-CALCULATED-PAY.
                   05 WS-HR-OVERTIME         PIC 9(3)V9 VALUE 0.

               01 WS-PAYROLL-DSP.
                   05 WS-HR-OVERTIME-DSP     PIC Z9.9 VALUE 0.

               01 WS-FILE-STATUS               PIC XX.
                  88 FILE-OK                   VALUE "00".
                  88 FILE-EOF                  VALUE "10".
                  88 FILE-NOT-FOUND            VALUE "35".

               01 WS-DISPLAY-LINE              PIC X(80) VALUE SPACES.
               01  WS-ERROR-ACCUMULATOR.
                   05  WS-ERROR-BUFFER         PIC X(500) VALUE SPACES.
                   05  WS-ERROR-PTR            PIC 999 VALUE 1.
                   05  WS-ERROR-COUNT          PIC 99 VALUE ZERO.

               PROCEDURE DIVISION.
                   DISPLAY "Execution started"
                   OPEN INPUT PAYROLL-FILE
                   
                   PERFORM UNTIL FILE-EOF
                       PERFORM 1000-PROCESS-FILE
                       PERFORM 1100-DISPLAY-RECORD-DEBUG-CONDITIONAL
                   END-PERFORM

                   CLOSE PAYROLL-FILE
                   DISPLAY "Execution stopped"
                   STOP RUN.

               1000-PROCESS-FILE.
                   READ PAYROLL-FILE
                       AT END
                           SET FILE-EOF TO TRUE
                       NOT AT END
      *> TODO: what if file not okay?                 
                           MOVE PAYROLL-RECORD-RAW TO WS-INPUT-RECORD
                           PERFORM 2000-VALIDATE-AND-MOVE
                   END-READ
    
                   IF NOT (FILE-OK OR FILE-EOF)
                       DISPLAY "READ ERROR: " WS-FILE-STATUS
                       PERFORM 3000-ABORT-RUN
                   END-IF
               .

               1100-DISPLAY-RECORD-DEBUG-CONDITIONAL.
                   IF DEBUG-ON
                       PERFORM 1110-DISPLAY-PAYROLL-RECORD-DEBUG
                   END-IF
               .

               1110-DISPLAY-PAYROLL-RECORD-DEBUG.
                   PERFORM 1200-CREATE-RECORD-DSP
                   PERFORM 1300-CALCULATE-PAYROLL
                   PERFORM 1400-CREATE-PAYROLL-DSP

                   DISPLAY " "
                   DISPLAY "EMP-ID: " WS-EMP-ID-DSP 
                   " [" WS-LASTNAME-DSP ", " 
                   WS-FIRSTNAME-DSP "]"
    
                   MOVE SPACES TO WS-DISPLAY-LINE
                   STRING "Dept: " WS-DEPT-DSP 
                          "  Hours: " WS-HOURS-DSP
                          "  Rate: " WS-RATE-DSP 
                          "  Hired: " WS-HIRE-DATE-DSP
                          DELIMITED BY SIZE
                          INTO WS-DISPLAY-LINE
                   DISPLAY WS-DISPLAY-LINE
    
                   IF WS-HR-OVERTIME > 0
                       
                       DISPLAY "  ** OVERTIME: "
                               WS-HR-OVERTIME " hours **"
                   END-IF
               .
               
               1200-CREATE-RECORD-DSP.
                   INITIALIZE WS-RECORD-DSP
                   MOVE WS-EMP-ID TO WS-EMP-ID-DSP
                   MOVE WS-LASTNAME TO WS-LASTNAME-DSP
                   MOVE WS-FIRSTNAME TO WS-FIRSTNAME-DSP
                   MOVE WS-DEPT TO WS-DEPT-DSP
                   MOVE WS-HOURS TO WS-HOURS-DSP
                   MOVE WS-RATE TO WS-RATE-DSP
                   STRING WS-HIRE-DATE(1:2) "/"
                          WS-HIRE-DATE(3:2) "/"
                          WS-HIRE-DATE(5:2)
                          DELIMITED BY SIZE
                          INTO WS-HIRE-DATE-DSP
                   END-STRING
               .

               1300-CALCULATE-PAYROLL.
                   INITIALIZE WS-CALCULATED-PAY
                   COMPUTE WS-HR-OVERTIME = WS-HOURS - C-WORK-HR-BASE
               .

               1400-CREATE-PAYROLL-DSP.
                   INITIALIZE WS-PAYROLL-DSP
                   MOVE WS-HR-OVERTIME TO WS-HR-OVERTIME-DSP
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
                   MOVE WS-EMP-ID-X TO WS-EMP-ID
                   MOVE WS-LASTNAME-X TO WS-LASTNAME
                   MOVE WS-FIRSTNAME-X TO WS-FIRSTNAME
                   MOVE WS-DEPT-X TO WS-DEPT
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
                       PERFORM 2000-WRITE-VALIDATION-ERROR
               .

               2000-WRITE-VALIDATION-ERROR.
                   DISPLAY WS-ERROR-BUFFER
               .

               3000-ABORT-RUN.
                   DISPLAY "Programm execution was aborted."
                   STOP RUN
               .

               END PROGRAM PayrollProcessing.
       