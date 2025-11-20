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

## Success Criteria
- [ ] Correctly reads all records from input file
- [ ] Handles file not found error
- [ ] Calculates pay correctly with overtime
- [ ] Produces formatted report file
- [ ] Summary totals are accurate
- [ ] Code follows COBOL standards (no warnings)
```
