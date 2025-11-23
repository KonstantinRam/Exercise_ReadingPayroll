My second COBOL program.

# Task 2: Batch Payroll Processor

## Objective
Build a COBOL batch processor that reads employee records from a file and produces a payroll summary report.

## Requirements

### Input
- Read from file: `PAYROLL.DAT`
- Process all employee records
- Handle file errors gracefully (missing file, empty file)
- Validate data (negative hours, excessive hours)

### Processing
- Calculate regular pay (<=40 hours at base rate)
- Calculate overtime pay (>40 hours at 1.5x rate)
- Sum total payroll for all employees
- Count employees by department
- Track highest and lowest paid employees

### Output
Generate report file `PAYROLL.RPT` with:
1. Header with run date and time
2. Detail line for each employee showing:
   - Employee ID
   - Name
   - Regular hours/pay
   - Overtime hours/pay
   - Total pay
3. Summary section showing:
   - Total employees processed
   - Total regular hours
   - Total overtime hours
   - Total payroll amount
   - Average pay per employee
   - Highest paid employee
   - Lowest paid employee
   - Count by department

### File Layout
**Input Record (80 bytes):**
```
Columns  Field            Picture      Description
01-08    EMP-ID          X(8)         Employee ID
09-28    EMP-LASTNAME    X(20)        Last Name  
29-43    EMP-FIRSTNAME   X(15)        First Name
44-46    EMP-DEPT        X(3)         Department Code
47-50    EMP-HOURS       9(3)V9       Hours (implied decimal)
51-55    EMP-RATE        9(3)V99      Hourly Rate (implied decimal)
56-61    EMP-HIRE-DATE   9(6)         YYMMDD format
62-80    FILLER          X(19)        Reserved for future use
```

### Technical Requirements
- Use FILE SECTION for file definitions
- Implement proper file error handling
- Use 88-level conditions for validation
- Format currency with $ and commas
- Right-align numeric output
- Use WORKING-STORAGE for accumulators
- Follow structured programming (no GO TO except for errors)

### Bonus Challenges
- Sort employees by total pay (descending) before output
- Calculate and display department subtotals
- Add page breaks every 20 detail lines
- Generate a second file with only overtime workers

### Sample Output
```
PAYROLL REPORT                    RUN DATE: 2024-01-15  TIME: 14:30:25

EMP ID    EMPLOYEE NAME            REGULAR         OVERTIME        TOTAL PAY
--------  ---------------------    ------------    ------------    ------------
EMP00001  SMITH, JOHN              $1,000.00       $  187.50       $1,187.50
EMP00002  DOE, JANE                $1,140.00       $    0.00       $1,140.00

PAYROLL SUMMARY
===============
Total Employees:        20
Total Regular Hours:    750.0
Total Overtime Hours:   85.5
Total Payroll:          $23,456.78
Average Pay:            $1,172.84

Highest Paid: JOHNSON, ROBERT     $2,145.00
Lowest Paid:  WILSON, MARY        $  684.00

DEPARTMENT COUNTS
IT:  5 employees
HR:  3 employees
FIN: 7 employees
OPS: 5 employees
```

(Claude automated review for the Task 2. Let Task 2 be as it is for learning purpose. Avoid it in Task 3).
## Task 2 Code Review - Mainframe Production Standards
Your code would be rejected before it even got to testing. Here's what would get you called into a meeting:
ABEND GUARANTEED - Line 1
cobol01  WS-DEBUG-LEVEL           PIC 9 VALUE 0.
PIC 9 without COMP? That's DISPLAY numeric. Every comparison with this field wastes CPU cycles. Should be PIC 9 COMP VALUE ZERO. This pattern repeats throughout - you're burning MIPS on every numeric operation.
STOP RUN vs GOBACK Confusion
You have STOP RUN in 3000-ABORT-RUN but GOBACK in mainline. Pick one. GOBACK is correct for batch. STOP RUN in a called subroutine will terminate the entire region - hope you didn't have other jobs running.
Uninitialized Variable S0C7 Bombs
cobol01  WS-CALCULATED-PAY.
    05  WS-TOTAL-PAY         PIC S9(8)V99 VALUE 0 COMP-3.
You initialize some COMP-3 fields but not others. Then you have:
cobolINITIALIZE WS-CALCULATED-PAY
This zeros out your VALUE clauses. Later calculations might work by accident if storage happens to be clean. First corrupted input = S0C7.
STRING Operations Performance Disaster
cobolSTRING " EMP:" WS-EMP-ID-X
       " ERRORS:"
       DELIMITED BY SIZE
       INTO WS-ERROR-BUFFER
       WITH POINTER WS-ERROR-PTR
You're using STRING for error messages that might never display. On mainframe, STRING is expensive. Build the message only if you're going to use it. Better yet, write to an error file with fixed fields.
Department Table Design is Amateur Hour
cobol05  WS-DEPT-TABLE-MAX     PIC S9(3) COMP VALUE 512.
05  WS-DEPT-ENTRIES OCCURS 512 TIMES
512 departments? You're wasting 10KB of storage. Real shops have maybe 20-50 departments. Worse, you're doing linear search through this:
cobolPERFORM VARYING DPT-IDX FROM 1 BY 1
        UNTIL DPT-IDX > WS-DEPT-TABLE-HIGH
              OR WS-DPT-CODE(DPT-IDX) = SR-DEPT
Should be SEARCH ALL with ASCENDING KEY if you insist on a table. Better answer: VSAM KSDS file.
Missing File Status Checks
You check FILE-EOF and FILE-OK, but what about:

23 (record not found)
24 (boundary violation)
37 (open mode wrong)
41 (file already open)
46 (sequential read error)

First corrupted block in PAYROLL.DAT and your program goes into an infinite loop or abends.
Page Break Logic is Spaghetti
cobolCOMPUTE WS-LINE-COUNT-TEMP = WS-LINE-COUNT-REP + 2 
IF WS-LINE-COUNT-TEMP > WS-MAX-LINES-REP
    WRITE REPORT-LINE FROM SPACES
    ADD 1 TO WS-LINE-COUNT-REP
You're computing temporary line counts, conditionally writing spaces, mixing report and overtime page counters. This is unmaintainable. One report = one line counter = one page break routine.
SORT-WORK File Handle Leak
You never explicitly CLOSE SORT-WORK. SORT handles it implicitly, but some shops require explicit file handling for audit. Worse, you're not checking SORT-RETURN special register. Disk full during sort = silent corruption.
Error "Accumulator" is Overengineered Garbage
cobol01  WS-ERROR-ACCUMULATOR.
    05  WS-ERROR-BUFFER      PIC X(500) VALUE SPACES.
    05  WS-ERROR-PTR         PIC 999 VALUE 1.
500 bytes for an error message that goes to DISPLAY? Operator console shows 80 characters max. You're building complex strings that get truncated. Write to an error file or keep it simple.
No Restart Capability
Process dies at record 15,000 of 50,000? Start over from scratch. No checkpoint, no restart logic, no commit frequency. This is fine for 20 records but fails production requirements.
Debug Code in Production
cobol01  WS-DEBUG-LEVEL           PIC 9 VALUE 0.
    88  DEBUG-OFF            VALUE 0.
    88  DEBUG-ON             VALUE 1.
Who sets this? How? You're shipping debug code to production. Use compiler directive $IF or remove it entirely.
Inefficient Report Writing
Opening REPORT-FILE, closing it, then opening EXTEND? That's three DCB operations. Open once, write everything, close once.
The "Spanish Inquisition" Comment
cobolDISPLAY "CALL SPANISH INQUISITION"
Cute. Until it shows up in production SYSOUT and someone has to explain it to audit.
Data Validation is Half-Assed
You check if hours/rate are NUMERIC but don't validate ranges:

Hours > 168? (hours in a week)
Rate > 999.99?
Hire date in future?
Department code exists?

First typo in input = garbage in financial reports.
What Would Actually Pass Review

One program, one function (separate sort from report from validation)
Proper 88-levels for ALL conditions
SEARCH ALL with sorted table or external file lookup
Single page-break routine reused for all reports
Error file, not DISPLAY
Checkpoint/restart logic for batches over 1000 records
ALL numeric fields COMP or COMP-3, never DISPLAY
File status checking that handles ALL conditions

Your code works for 20 test records. At 50,000 production records with corrupted data at 3am, it's a disaster waiting to happen.
The SORT implementation is the only thing done reasonably well, but even that lacks SORT-RETURN checking.
