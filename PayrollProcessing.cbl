               IDENTIFICATION DIVISION. 
               PROGRAM-ID. PayrollProcessing.
      
               ENVIRONMENT DIVISION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT PAYROLL-FILE ASSIGN TO "PAYROLL.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS WS-FILE-STATUS. 
.
               WORKING-STORAGE SECTION. 
               01 WS-FILE-STATUS               PIC XX.
                  88 FILE-OK                   VALUE "00".
                  88 FILE-EOF                  VALUE "10".
                  88 FILE-NOT-FOUND            VALUE "35".
                  
               PROCEDURE DIVISION.

               END PROGRAM PayrollProcessing.
       