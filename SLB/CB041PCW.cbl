       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB041PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/03/95.
       SECURITY.      *************************************************
                      *                                               *
                      * Checa numero da primeira pagina de relatorios *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPAEMSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPAEMFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 RK-CBPAEM                PIC  9(001) COMP VALUE 5.
           05 ER-CBPAEM.
              10 FS-CBPAEM             PIC  X(002) VALUE "00".
              10 LB-CBPAEM             PIC  X(050) VALUE "CBPAEM".

       COPY CWBOXW.
       COPY CWNCOR.

       LINKAGE SECTION.

       01  PARAMETROS-CWIMPR.
           05 CWIMPR-SETS                    PIC 9(002).
              88 CWIMPR-SET-NO-OPTION                   VALUE 00.
              88 CWIMPR-SET-PAGE                        VALUE 01.
              88 CWIMPR-GET-PAGE                        VALUE 02.
              88 CWIMPR-SET-DATE                        VALUE 03.
              88 CWIMPR-SET-TIME                        VALUE 04.
              88 CWIMPR-SET-SIZE-PAGE                   VALUE 05.
              88 CWIMPR-SET-QUIET                       VALUE 06.
              88 CWIMPR-SET-PAGE-OFF                    VALUE 07.
              88 CWIMPR-SET-FOLD-OFF                    VALUE 08.
           05 CWIMPR-REPORT                  PIC X(007).
           05 CWIMPR-FORM-TYPE               PIC X(001).
              88 CWIMPR-SIZE-132                        VALUE "1".
              88 CWIMPR-SIZE-080                        VALUE "2".
              88 CWIMPR-SIZE-220                        VALUE "3".
              88 CWIMPR-END-PRINT                       VALUE "9".
           05 CWIMPR-SIZE-PAGE               PIC 9(003).
           05 CWIMPR-TITLE                   PIC X(174).
           05 CWIMPR-SUB-TITLE               PIC X(174).
           05 CWIMPR-HEADER-1                PIC X(500).
           05 CWIMPR-HEADER-2                PIC X(500).
           05 CWIMPR-HEADER-3                PIC X(500).
           05 CWIMPR-HEADER-4                PIC X(500).
           05 CWIMPR-HEADER-5                PIC X(500).
           05 CWIMPR-DETAIL                  PIC X(500).
           05 CWIMPR-TIME-REPORT             PIC X(010).
              88 CWIMPR-CLOSE                           VALUE "CLOSE".
           05 CWIMPR-SPECIAL-PAGE            PIC 9(004).
           05 CWIMPR-SPECIAL-FOLD            PIC 9(004).
           05 CWIMPR-SPECIAL-DATE-TIME       PIC 9(006).
           05 CWIMPR-PAGE-TXT                PIC X(007).
           05 CWIMPR-TASK                    PIC 9(006).
           05 CWIMPR-NOTE                    PIC X(020).
           05 CWIMPR-RESERVED                PIC X(100).

       PROCEDURE DIVISION USING PARAMETROS-CWIMPR.

       000-INICIO.

           OPEN INPUT CBPAEM

           IF   FS-CBPAEM > "09"
                GOBACK
           END-IF

           READ CBPAEM
           IF   FS-CBPAEM < "09"
           AND  CBPAEM-TESTE-PAGINA = 2
                SET CWIMPR-GET-PAGE TO TRUE
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                SET CWBOXW-OPEN     TO TRUE
                MOVE 10             TO CWBOXW-LINE
                MOVE 10             TO CWBOXW-COLUMN
                MOVE 01             TO CWBOXW-VERTICAL-LENGTH
                MOVE 25             TO CWBOXW-HORIZONTAL-LENGTH
                MOVE RED-BROWN-HIGH TO CWBOXW-COLOR-FRAME
                                       CWBOXW-COLOR-BORDER
                CALL "CWBOXW" USING PARAMETROS-CWBOXW
                DISPLAY "   N§ da 1¦ folha:       "
                        LINE 11 COLUMN 11
                ACCEPT  CWIMPR-SPECIAL-FOLD
                        LINE 11 COLUMN 30
                SET CWIMPR-SET-PAGE TO TRUE
                IF   CWIMPR-SPECIAL-PAGE = 0
                     MOVE 1 TO CWIMPR-SPECIAL-PAGE
                END-IF
                IF   CWIMPR-SPECIAL-FOLD = 0
                     MOVE 1 TO CWIMPR-SPECIAL-FOLD
                END-IF
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                SET CWBOXW-CLOSE TO TRUE
                CALL "CWBOXW" USING PARAMETROS-CWBOXW
                CANCEL "CWBOXW"
                MOVE SPACES TO CWIMPR-TIME-REPORT
           END-IF

           CLOSE CBPAEM.

       000-99-FIM. GOBACK.

       END PROGRAM CB041PCW.
