       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB040PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/03/1995.
       SECURITY.      *************************************************
                      *                                               *
                      * Relacao de contas para razao                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPLCOSL.

           SELECT CBWK40 ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBWK40-CHAVE
                  ALTERNATE RECORD KEY CBWK40-COD-RED   WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CBWK40.

           SELECT CBTX40 ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CBTX40.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPLCOFD.

       FD  CBWK40
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBWK40.

       01  CBWK40-REG.
           05 CBWK40-CHAVE.
              10 CBWK40-CONTA    COMP-3 PIC  9(015).
           05 CBWK40-COD-RED            PIC  9(005).
           05 CBWK40-DESCRICAO          PIC  X(030).

       FD  CBTX40
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBTX40.

       01  CBTX40-REG.
           05 FILLER                    PIC  X(001).
           05 CBTX40-COD-RED            PIC  9(005).
           05 CBTX40-TRACO              PIC  X(001).
           05 CBTX40-DV                 PIC  X(001).
           05 FILLER                    PIC  X(001).
           05 CBTX40-DESCRICAO          PIC  X(030).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 COD-RED-F5               PIC  9(005) VALUE 0.
           05 COD-RED-DV-CALL          PIC  X(001) VALUE SPACE.
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBWK40.
              10 FS-CBWK40             PIC  X(002) VALUE "00".
              10 LB-CBWK40             PIC  X(050) VALUE "CBWK40##.DAT".
           05 ER-CBTX40.
              10 FS-CBTX40             PIC  X(002) VALUE "00".
              10 LB-CBTX40             PIC  X(050) VALUE "CBTX40##.TXT".
           05 USUARIO-L                PIC  X(030).
           05 TASK-L                   PIC  9(006).
           05 PROGRAMA-L               PIC  X(008).
           05 CWMENU                   PIC  X(001) VALUE "?".

       COPY CWBOXF.
       COPY CWHELP.

       LINKAGE SECTION.

       01  CB040PCW-CONTA PIC 9(015) COMP-3.

       PROCEDURE DIVISION USING CB040PCW-CONTA.

       000-INICIO.


           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           EVALUATE TRUE
                    WHEN CB040PCW-CONTA = 0
                         PERFORM 110-MANUTENCAO THRU 110-99-FIM
                    WHEN CB040PCW-CONTA = 999999999999999
                         CLOSE CBWK40
                         DELETE FILE CBWK40
                    WHEN OTHER
                         MOVE CB040PCW-CONTA TO CBWK40-CONTA
                         READ CBWK40
                         IF  FS-CBWK40 > "09"
                             MOVE ZERO TO CB040PCW-CONTA
                         END-IF
           END-EVALUATE.

       100-99-FIM. EXIT.

       110-MANUTENCAO.

           MOVE 12 TO CWBOXF-LINE
           MOVE 21 TO CWBOXF-COLUMN
           MOVE SPACES TO CWBOXF-OPTION

           MOVE "CB044PCW"          TO CWBOXF-PROGRAM
           MOVE "Cod.Red Descri‡Æo" TO CWBOXF-TITLE
           MOVE  8 TO CWBOXF-STRING-1-LENGTH
           MOVE 30 TO CWBOXF-STRING-2-LENGTH
           MOVE  2 TO CWBOXF-ORDER
           MOVE 10 TO CWBOXF-VERTICAL-LENGTH
           COMPUTE CWBOXF-HORIZONTAL-LENGTH = 6
                 + CWBOXF-STRING-1-LENGTH
                 + CWBOXF-STRING-2-LENGTH.

       110-99-SHOW.

           CALL "CWBOXF" USING PARAMETROS-CWBOXF

           IF   CWBOXF-OPTION = SPACES
                GO TO 110-99-ENCERRA
           ELSE
                MOVE CWBOXF-OPTION (1: 5) TO COD-RED-F5
                MOVE COD-RED-F5           TO CBPLCO-COD-RED
                READ CBPLCO IGNORE LOCK
                        KEY IS CBPLCO-COD-RED
                IF   FS-CBPLCO < "10"
                     MOVE CBPLCO-CONTA TO CBWK40-CONTA
                     READ CBWK40
                     IF   FS-CBWK40 < "10"
                          DELETE CBWK40 RECORD
                     ELSE
                          MOVE CBPLCO-DESCRICAO TO CBWK40-DESCRICAO
                          MOVE CBPLCO-COD-RED   TO CBWK40-COD-RED
                          WRITE CBWK40-REG
                     END-IF
                     OPEN OUTPUT CBTX40
                     MOVE 0 TO CBWK40-COD-RED CWHELP-VERTICAL-LENGTH
                     START CBWK40 KEY NOT LESS CBWK40-COD-RED
                     PERFORM TEST AFTER UNTIL FS-CBWK40 > "09"
                        READ CBWK40 NEXT RECORD
                        IF   FS-CBWK40 < "10"
                             MOVE SPACES           TO CBTX40-REG
                             MOVE CBWK40-COD-RED   TO CBTX40-COD-RED
                             CALL "CB039PCW" USING CBTX40-COD-RED
                                                   CBTX40-DV
                             MOVE "-"              TO CBTX40-TRACO
                             MOVE CBWK40-DESCRICAO TO CBTX40-DESCRICAO
                             WRITE CBTX40-REG
                             IF   CWHELP-VERTICAL-LENGTH < 10
                                  ADD 1 TO CWHELP-VERTICAL-LENGTH
                             END-IF
                        END-IF
                     END-PERFORM
                     CLOSE CBTX40
                     MOVE CWBOXF-LINE   TO CWHELP-LINE
                     MOVE CWBOXF-COLUMN TO CWHELP-COLUMN
                     MOVE 30            TO CWHELP-HORIZONTAL-LENGTH
                     MOVE LB-CBTX40     TO CWHELP-FILE
                     CALL "CWHELP" USING PARAMETROS-CWHELP
                END-IF
                DELETE FILE CBTX40
                GO TO 110-99-SHOW
           END-IF.

       110-99-ENCERRA.

           CANCEL "CWBOXF"
           CANCEL "CB044PCW"
           CLOSE CBPLCO.

       110-99-FIM. EXIT.

       800-INICIAIS.

           IF   CWMENU NOT = "?"
                GO TO 800-99-FIM
           END-IF

           CALL "CWGETU" USING USUARIO-L TASK-L PROGRAMA-L CWMENU
           MOVE TASK-L (5: 2) TO LB-CBWK40 (7: 2)
                                 LB-CBTX40 (7: 2)
           OPEN I-O CBWK40
           OPEN INPUT CBPLCO.

       800-99-FIM. EXIT.

       END PROGRAM CB040PCW.
