       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB046PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/11/1998.
       SECURITY.      *************************************************
                      *                                               *
                      * Emissao de termos de abertura/encerramento    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPAEMSL.
       COPY CBTEAFSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPAEMFD.
       COPY CBTEAFFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 RK-CBPAEM                 PIC  9(002) COMP VALUE 1.
           05 PAGINA-TOTAL             PIC  9(006) VALUE 0.
           05 PAGINA-INICIAL           PIC  9(006) VALUE 0.
           05 PAGINA-FINAL             PIC  9(006) VALUE 0.
           05 WORK-LINE                            VALUE SPACES.
              10 BYTE-WORK             PIC  X(001) OCCURS 132.
           05 I                        PIC  9(003) VALUE 0.
           05 XT                       PIC  9(003) VALUE 0.
           05 XI                       PIC  9(003) VALUE 0.
           05 XF                       PIC  9(003) VALUE 0.
           05 XD                       PIC  9(003) VALUE 0.
           05 XDX                      PIC  9(001) VALUE 1.
           05 ER-CBPAEM.
              10 FS-CBPAEM              PIC  X(002) VALUE "00".
              10 LB-CBPAEM              PIC  X(050) VALUE "CBPAEM".
           05 ER-CBTEAF.
              10 FS-CBTEAF              PIC  X(002) VALUE "00".
              10 LB-CBTEAF              PIC  X(050) VALUE "CBTEAF".

       COPY CWIMPR.
       COPY CWTIME.

       SCREEN SECTION.

       01  CTAC-LIT-CB046PCW.
           05 LINE 08 COLUMN 03 VALUE "Folha inicial:".
           05 LINE 10 COLUMN 03 VALUE "Folha final  :".

       01  CTAC-VAR-CB046PCW.
           05 LINE 08 COLUMN 18 PIC Z(004) USING PAGINA-INICIAL.
           05 LINE 10 COLUMN 18 PIC Z(004) USING PAGINA-FINAL.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY CTAC-LIT-CB046PCW
           ACCEPT  CTAC-VAR-CB046PCW
           COMPUTE PAGINA-TOTAL = PAGINA-FINAL - PAGINA-INICIAL + 1

           IF   PAGINA-INICIAL = 0
           OR   PAGINA-FINAL   = 0
                GOBACK
           END-IF

           IF   PAGINA-INICIAL > PAGINA-FINAL
                EXEC COBOLware Send Message
                     "P gina inicial > p gina final"
                END-EXEC
                GO TO 000-INICIO
           END-IF

           SET  CWTIME-TODAY      TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO CWTIME-DATE
           SET  CWTIME-EDIT       TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           CALL "CWFILE"       USING LB-CBPAEM
           MOVE 99                TO CWIMPR-SIZE-PAGE
           MOVE "CB046PA"         TO CWIMPR-REPORT

           OPEN INPUT CBPAEM
           MOVE 5 TO RK-CBPAEM
           READ CBPAEM INTO LB-CBTEAF
           IF   FS-CBPAEM < "10"
                OPEN INPUT CBTEAF
                PERFORM UNTIL FS-CBTEAF > "09"
                           OR CBTEAF-FIM-ABERTURA
                           OR CWIMPR-END-PRINT
                        READ CBTEAF INTO CWIMPR-DETAIL
                        IF   FS-CBTEAF < "10"
                        AND  NOT CBTEAF-FIM-ABERTURA
                             PERFORM 010-VARIAVEIS THRU 010-99-FIM
                             CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        END-IF
                END-PERFORM
                IF   NOT CWIMPR-END-PRINT
                     MOVE X"0C" TO CWIMPR-DETAIL
                     CALL "CWIMPR" USING PARAMETROS-CWIMPR
                END-IF
                PERFORM UNTIL FS-CBTEAF > "09"
                           OR CWIMPR-END-PRINT
                        READ CBTEAF INTO CWIMPR-DETAIL
                        IF   FS-CBTEAF < "10"
                        AND  NOT CBTEAF-FIM-ABERTURA
                             PERFORM 010-VARIAVEIS THRU 010-99-FIM
                             CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        END-IF
                END-PERFORM
                CLOSE CBTEAF
                CLOSE CBPAEM
                IF   NOT CWIMPR-END-PRINT
                     SET CWIMPR-CLOSE TO TRUE
                     CALL "CWIMPR" USING PARAMETROS-CWIMPR
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       010-VARIAVEIS.

           MOVE CWIMPR-DETAIL TO WORK-LINE
           IF   WORK-LINE NOT = SPACES
                MOVE 7  TO XI XF XT
                MOVE 24 TO XD
                MOVE 1  TO XDX
                PERFORM VARYING I FROM 132 BY -1
                          UNTIL I = 0
                    EVALUATE BYTE-WORK (I)
                      WHEN "#"
                           IF   XT > 1
                                SUBTRACT 1 FROM XT
                                MOVE PAGINA-TOTAL (XT: 1)
                                  TO BYTE-WORK (I)
                           END-IF
                      WHEN "@"
                           IF   XI > 1
                                SUBTRACT 1 FROM XI
                                MOVE PAGINA-INICIAL (XI: 1)
                                  TO BYTE-WORK (I)
                           END-IF
                      WHEN "%"
                           IF   XF > 1
                                SUBTRACT 1 FROM XF
                                MOVE PAGINA-FINAL(XF: 1)
                                  TO BYTE-WORK (I)
                           END-IF
                      WHEN "$"
                           PERFORM TEST AFTER
                                   UNTIL XDX NOT = 1
                                   IF   XD > 1
                                        SUBTRACT 1 FROM XD
                                        MOVE CWTIME-DATE-EDITED-LONG
                                             (XD: 1)
                                          TO BYTE-WORK (I)
                                        IF  BYTE-WORK (I) NOT = SPACE
                                            MOVE 2 TO XDX
                                        END-IF
                                   END-IF
                           END-PERFORM
                    END-EVALUATE
                    IF  BYTE-WORK (I) = "#" OR "%" OR "@" OR "$"
                        MOVE SPACE TO BYTE-WORK (I)
                    END-IF
                END-PERFORM
                MOVE WORK-LINE TO CWIMPR-DETAIL
           END-IF.

       010-99-FIM. EXIT.

       END PROGRAM CB046PCW.

