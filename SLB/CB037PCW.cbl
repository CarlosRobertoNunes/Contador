       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB037PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  27/03/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Subrotina para redirecionar arquivos         *
                      *                                               *
                      *    CB0037-OPCAO    PIC X(01).    Input        *
                      *    1 - Moedas 2 - Reservado 3 - Estruturas    *
                      *    CB0037-FILE-OLD PIC X(12).    Input        *
                      *    CB0037-FILE-NEW PIC X(50).    Output       *
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
           05 DIR                      PIC  X(038) VALUE SPACES.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 RK-CBPAEM                PIC  9(001) COMP VALUE 1.
           05 ER-CBPAEM.
              10 FS-CBPAEM             PIC  X(002) VALUE "00".
              10 LB-CBPAEM             PIC  X(050) VALUE "CBPAEM".

       LINKAGE SECTION.

       01  CB0037-OPCAO    PIC 9(01).
           88 OPCAO-OK VALUE 1 2 3.
       01  CB0037-FILE-OLD PIC X(12).
       01  CB0037-FILE-NEW PIC X(50).

       PROCEDURE DIVISION USING CB0037-OPCAO
                                CB0037-FILE-OLD
                                CB0037-FILE-NEW.


       000-INICIO.

           IF   OPCAO-OK
           AND  CB0037-FILE-OLD NOT = SPACES
                IF  VEZ = 1
                    CALL "CWFILE" USING LB-CBPAEM
                    MOVE 2           TO VEZ
                END-IF
                OPEN INPUT CBPAEM
                IF   FS-CBPAEM > "09"
                     STOP RUN
                END-IF
                MOVE CB0037-OPCAO TO RK-CBPAEM
                READ CBPAEM IGNORE LOCK
                IF   FS-CBPAEM < "10"
                     MOVE CBPAEM-REG TO DIR
                ELSE
                     EVALUATE RK-CBPAEM
                       WHEN 1 MOVE "..\COMUNS\MDS" TO DIR
                       WHEN 2 MOVE "             " TO DIR
                       WHEN 3 MOVE "..\COMUNS\LTS" TO DIR
                     END-EVALUATE
                END-IF
                CLOSE CBPAEM
                MOVE SPACES TO CB0037-FILE-NEW
                IF  DIR NOT = SPACE
                    STRING DIR DELIMITED BY SPACE
                           "\" DELIMITED BY SIZE
                           CB0037-FILE-OLD DELIMITED BY SPACE
                    INTO CB0037-FILE-NEW
                ELSE
                   MOVE CB0037-FILE-OLD TO CB0037-FILE-NEW
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CB037PCW.
