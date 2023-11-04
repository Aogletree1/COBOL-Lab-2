       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Lab2MathLab.
       AUTHOR.  AUSTIN OGLETREE.
      ****************************************************************
      *    THERE IS NO EXTERNAL INPUT FILE ONLY AN OUTPUT FILE
      *    THE PURPOSE OF THIS PROGRAM IS TO USE MATH VERBS AND THE
      *    COMPUTE STATEMENT TO SOLVE AN EQUATION.  YOU SHOULD GET
      *    THE SAME ANSWER FOR BOTH.
      *******
      * INPUTS
      *    THERE IS NO INPUT FILE
      *    THE INPUTS ARE HARD CODED IN WORKING STORAGE
      *        A = 3
      *        B = 6
      *        C = 4
      *        D = 2
      *        E = 3
      ********
      * OUTPUT 1
      *    SOLVE THE EQUATION IN THE INSTRUCTION USING MATH VERBS
      ********
      * OUTPUT 2
      *    SOLVE THE EQUATION IN THE INSTRUCTION USING A 
      *    COMPUTE STATEMENT
      ********
      *  YOUR ANSWER FOR OUTPUT 1 AND OUTPUT 2 SHOULD MATCH EXACTLY
      ********
      *    REMEMBER TO MAKE THE REQUIRED CHANGES TO THIS PROGRAM 
      *    SPECIFIED IN THE INSTRUCTIONS.
      ****************************************************************
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.

       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.

           SELECT MLAB-REPORT
               ASSIGN TO PRINTER "MLAB-REPORT.TXT".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  MLAB-REPORT
           RECORD CONTAINS 80 CHARACTERS.
       01  REPORT-RECORD       PIC X(80).
      *
       WORKING-STORAGE SECTION.
      *
       01  TEMP-STORAGE.
           05  ANS             PIC S99V999 VALUE +0.

      *    ADD ANY TEMPORAY FIELDS YOU NEED AS AN 05 UNDER TEMP-STORAGE
      *    YOU WILL NEED TO RETAIN YOUR ORIGINAL VALUES
      *    MAKE SURE IT IS THE SAME SIZE AS ANS.

       01  VARIABLE-VALUE.
           05  A               PIC S9      VALUE +3.
           05  B               PIC S9      VALUE +6.
           05  C               PIC S9      VALUE +4.
           05  D               PIC S9      VALUE +2.
           05  E               PIC S9      VALUE +3.
           05  CAL1            PIC S9      VALUE  0.
           05  CAL2            PIC S9      VALUE  0.
           05  CAL3            PIC S9      VALUE  0.
           05  CAL4            PIC S9V9(3) VALUE  0.
           05  CAL5            PIC S9V9(3) VALUE  0.
           05  CAL6            PIC S9V9(3) VALUE  0.
           

      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING  PIC 9      VALUE 1.
      *
      ********************OUTPUT AREA***************************

       01  HEADING-LINE.
           05                  PIC X(18)   VALUE SPACE.
           05                  PIC X(15)   VALUE
                                   'MATH LAB REPORT'.


       01  DETAIL-LINE.
           05                  PIC X(10)    VALUE ' '.
           05  DL-MESSAGE      PIC X(22).
           05  DL-ANS          PIC 99.999-.
      
       PROCEDURE DIVISION.
      *
       000-CONTROL-MODULE.
           PERFORM 050-HOUSEKEEPING
           PERFORM 100-CALCULATE-VERBS
           PERFORM 200-COMPUTE
           PERFORM 999-FINAL-ROUTINE
       .

       050-HOUSEKEEPING.
           OPEN OUTPUT MLAB-REPORT

           WRITE REPORT-RECORD FROM HEADING-LINE
               AFTER ADVANCING PAGE
           MOVE 3 TO PROPER-SPACING
       .

       100-CALCULATE-VERBS.
      *    SOLVE FOR ANS
      *    FOLLOW THESE COMMENTS WITH YOUR CALCULATIONS (USING THE ADD,
      *    SUBTRACT, MULTIPLY AND DIVIDE VERBS)
      *    YOU WILL NEED TO RETAIN THE ORIGINAL VALUES
      *    YOU WILL NEED TEMPORARY VARIBLES


           
           SUBTRACT B FROM A GIVING CAL1
           MULTIPLY CAL1 BY E GIVING CAL2
           MULTIPLY C BY D GIVING CAL3
           DIVIDE CAL3 INTO CAL2 GIVING CAL4
           ADD CAL4 TO E GIVING CAL5
           ADD D TO A GIVING CAL6
           DIVIDE CAL6 INTO CAL5 GIVING ANS

           





           MOVE 'ANS MATH VERBS =      ' TO DL-MESSAGE
           MOVE ANS TO DL-ANS

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 300-WRITE-A-LINE

           MOVE 0 TO ANS
           MOVE 2 TO PROPER-SPACING
        
       .

       200-COMPUTE.
      *    SOLVE FOR ANS
      *    FOLLOW THIS COMMENT WITH YOUR SINGLE COMPUTE STATEMENT

           COMPUTE ANS = (E + (E * (A - B))/(C*D))/(D+A)




           MOVE 'ANS COMPUTED =        ' TO DL-MESSAGE
           MOVE ANS TO DL-ANS

           MOVE DETAIL-LINE TO REPORT-RECORD
           PERFORM 300-WRITE-A-LINE
       .

       300-WRITE-A-LINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
       .

       999-FINAL-ROUTINE.
           CLOSE MLAB-REPORT
           STOP RUN
       .
