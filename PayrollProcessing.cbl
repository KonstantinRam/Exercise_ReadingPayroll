               IDENTIFICATION DIVISION. 
               PROGRAM-ID. PayrollProcessing.
      
               ENVIRONMENT DIVISION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT PAYROLL-FILE ASSIGN TO "PAYROLL.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL
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
                   05  WS-RATE-X         PIC X(5).
                   05  WS-HIRE-DATE-X    PIC X(6).
                   05  FILLER            PIC X(19).
 
               01  WS-VALIDATED-RECORD.
                   05  WS-EMP-ID         PIC X(8) VALUE 'BLANC'.
                   05  WS-LASTNAME       PIC X(20) VALUE 'BLANC'.
                   05  WS-FIRSTNAME      PIC X(15) VALUE 'BLANC'.
                   05  WS-DEPT           PIC X(3) VALUE SPACES.
                   05  WS-HOURS          PIC 9(3)V9 VALUE ZEROS.
                   05  WS-RATE           PIC 9(3)V99 VALUE ZEROS.
                   05  WS-HIRE-DATE      PIC 9(6) VALUE SPACES.
                   05  WS-RECORD-STATUS  PIC X VALUE 'N'.
                       88  RECORD-VALID      VALUE 'Y'.
                       88  RECORD-INVALID    VALUE 'N'.

               01 WS-FILE-STATUS               PIC XX.
                  88 FILE-OK                   VALUE "00".
                  88 FILE-EOF                  VALUE "10".
                  88 FILE-NOT-FOUND            VALUE "35".

               01 WS-DISPLAY-LINE              PIC X(80) VALUE SPACES.
               01  WS-ERROR-ACCUMULATOR.
                   05  WS-ERROR-BUFFER       PIC X(500) VALUE SPACES.
                   05  WS-ERROR-PTR          PIC 999 VALUE 1.
                   05  WS-ERROR-COUNT        PIC 99 VALUE ZERO.

               PROCEDURE DIVISION.

                   PERFORM 1000-PROCESS-FILE
                   PERFORM 1100-DISPLAY-RECORD-DEBUG-CONDITIONAL
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
                       PERFORM 1110-DISPLAY-PAYROLL-RECORD
                   END-IF
               .

               1110-DISPLAY-PAYROLL-RECORD.
                   DISPLAY " "
                   DISPLAY "EMP-ID: " WS-EMP-ID 
                   " [" WS-LASTNAME(1:15) ", " 
                   WS-FIRSTNAME(1:10) "]"
    
                   MOVE SPACES TO WS-DISPLAY-LINE
                   STRING "Dept: " WS-DEPT 
                          "  Hours: " WS-HOURS-DSP
                          "  Rate: $" WS-RATE-DSP 
                          "  Hired: " WS-HIRE-DATE(1:4) "/"
                                      WS-HIRE-DATE(5:2) "/"
                                      WS-HIRE-DATE(7:2)
                          DELIMITED BY SIZE
                          INTO WS-DISPLAY-LINE
                   DISPLAY WS-DISPLAY-LINE
    
                   IF WS-HOURS > C-WORK-HR-BASE
                       COMPUTE WS-OVERTIME = WS-HOURS - C-WORK-HR-BASE
                       DISPLAY "  ** OVERTIME: " WS-OVERTIME " hours **"
                   END-IF
               .
               
               2000-VALIDATE-AND-MOVE
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
                       MOVE WS-HOURS-X TO WS-HOURS
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
                       MOVE WS-RATE-X TO WS-RATE
                   ELSE
                       ADD 1 TO WS-ERROR-COUNT
                       STRING " [RATE:" WS-RATE-X "]"
                       DELIMITED BY SIZE
                       INTO WS-ERROR-BUFFER
                       WITH POINTER WS-ERROR-PTR
                   END-IF
    
                   IF WS-HIRE-DATE-X IS NUMERIC
                       MOVE WS-HIRE-DATE-X TO WS-HIRE-DATE
                       PERFORM VALIDATE-DATE-LOGIC
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
                   END-IF

               .

               2000-WRITE-VALIDATION-ERROR
                   DISPLAY WS-ERROR-BUFFER
               .

               3000-ABORT-RUN
                   DISPLAY "Programm execution was aborted."
                   STOP RUN
               .

               END PROGRAM PayrollProcessing.
       