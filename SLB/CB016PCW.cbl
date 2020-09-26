       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB016PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  13/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Balancete                                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBPLCOSL.
       COPY CBCOSASL.
       COPY CBMVMSSL.

           SELECT CBWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBWORK-CHAVE
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CBWORK.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBPLCOFD.
       COPY CBCOSAFD.
       COPY CBMVMSFD.

       FD  CBWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBWORK.

       01  CBWORK-REG.
           05 CBWORK-CHAVE.
              10 CBWORK-CONTA          COMP-3 PIC  9(015).
              10 CBWORK-AAAAMM                PIC  9(006).
              10 REDEFINES CBWORK-AAAAMM.
                 15 CBWORK-AAAA               PIC  9(004).
                 15 CBWORK-MM                 PIC  9(002).
           05 CBWORK-SALDO-INICIAL     COMP-3 PIC S9(012)V99.
           05 CBWORK-SALDO-ATUAL       COMP-3 PIC S9(012)V99.
           05 CBWORK-A-DEBITO          COMP-3 PIC  9(012)V99.
           05 CBWORK-A-CREDITO         COMP-3 PIC  9(012)V99.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG                  PIC  9(001) VALUE 0.
           05 COD-RED-CALL             PIC  9(005) VALUE 0.
           05 COD-RED-DV-CALL          PIC  X(001) VALUE SPACE.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 CLASSE                   PIC  9(001) VALUE ZERO.
           05 GR-CBWORK                PIC  9(006) VALUE ZERO.
           05 NOTACAO                  PIC  9(001) VALUE ZERO.
           05 PARAMETROS-GRFINA.
              10 OPERADOR              PIC  X(030).
              10 TASK                  PIC  X(006).
              10 PROGRAMA              PIC  X(008).
              10 CWMENU                PIC  X(001).
           05 TIT-REF.
              10            PIC  X(12) VALUE "REFERENTE A ".
              10 REFERENCIA PIC  X(14) VALUE SPACES.
           05 OBS-1.
              10 OBS-2                 PIC  X(010) VALUE SPACES.
              10 OBS-3                 PIC  X(002) VALUE SPACES.
              10 OBS-4                 PIC  X(014) VALUE SPACES.
           05 OBS-5                    PIC  X(070) VALUE SPACES.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 GRAU                     PIC  9(001) VALUE ZERO.
           05 GRAU-MINIMO              PIC  9(001) VALUE ZERO.
           05 GRAU-ANTERIOR            PIC  9(001) VALUE ZERO.
           05 B                        PIC  9(003) VALUE ZERO.
           05 I                        PIC  9(002) VALUE ZERO.
           05 I-1                      PIC  9(001) VALUE ZERO.
           05 I-X REDEFINES I-1        PIC  X(001).
           05 LD-CBPLCO         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 ER-CBPLCO.
              10 FS-CBPLCO              PIC  X(002) VALUE "00".
              10 LB-CBPLCO              PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCACC.
              10 FS-CBCACC              PIC  X(002) VALUE "00".
              10 LB-CBCACC              PIC  X(050) VALUE "CBCACC".
           05 ER-CBCOSA.
              10 FS-CBCOSA              PIC  X(002) VALUE "00".
              10 LB-CBCOSA              PIC  X(050) VALUE "CBCOSA".
           05 ER-CBWORK.
              10 FS-CBWORK             PIC  X(002) VALUE "00".
              10 LB-CBWORK             PIC  X(050) VALUE "000000-W##".
              10 REDEFINES LB-CBWORK.
                 15 LB-WORK            PIC  X(011).
                 15 FILLER             PIC  X(039).
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Conta inexistente             ".
              10 PIC X(30) VALUE "Conta impr¢pria               ".
              10 PIC X(30) VALUE "Referˆncia impr¢pria          ".
              10 PIC X(30) VALUE "Centro de custo inexistente   ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 4   PIC X(30).
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 SALDO-INICIAL            PIC S9(012)V99 VALUE 0.
           05 CREDITOS                 PIC S9(012)V99 VALUE 0.
           05 DEBITOS                  PIC S9(012)V99 VALUE 0.
           05 SALDO-ATUAL              PIC S9(012)V99 VALUE 0.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(055) VALUE
              "CONTA                      DESCRICAO                   ".
           05 FILLER                         PIC  X(054) VALUE
              "      SALDO ANTERIOR           A DEBITO          A CRE".
           05 FILLER                         PIC  X(022) VALUE
              "DITO       SALDO ATUAL".
       02  LINHA-02.
           05 CLIC-CONTA-ED                  PIC  X(026) VALUE SPACES.
           05 REDEFINES CLIC-CONTA-ED.
              10 FILLER                      PIC  X(019).
              10 CLIC-FIM-ED.
                 15 CLIC-COD-RED             PIC  Z(005).
                 15 CLIC-COD-RED-TRACO       PIC  X(001).
                 15 CLIC-COD-RED-DV          PIC  X(001).
           05 CLIC-OBS                       PIC  X(001) VALUE SPACE.
           05 CLIC-DESCRICAO                 PIC  X(030) VALUE SPACES.
           05 CLIC-SALDO-INICIAL             PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.
           05 CLIC-DEBITOS                   PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CREDITOS                  PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 CLIC-SALDO-ATUAL               PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.

       COPY CWBOXS.
       COPY CWBOXF.
       COPY CB002PCW.
       COPY CB014PCW.
       COPY CWIMPR.
       COPY CWTIME.

       SCREEN SECTION.

       01  CB0016A.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 08 COLUMN 09 PIC X(025) FROM LB-CBPLCO.
           05 LINE 10 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBPLCO LINE 08 COLUMN 32 PIC ZZZ.ZZ9 FROM LD-CBPLCO.
           05 T-GR-PRNTER LINE 10 COLUMN 32 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       01  CB0016B AUTO.
           05 LINE 19 COLUMN 03 VALUE "Mˆs de referˆncia:".
           05 LINE 19 COLUMN 22 PIC ZZ/ USING MM-REF.
           05 LINE 19 COLUMN 25 PIC 9999 USING AAAA-REF BLANK ZERO.

       01  TELA-CC.
           05 LINE 20 COLUMN 03 VALUE "Centro de custo:".
           05 LINE 20 COLUMN 20 PIC ZZZZ USING CC.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-CBPLCO > "09"
                   PERFORM TEST AFTER
                           UNTIL FS-CBPLCO NOT = "9D"
                   READ CBPLCO NEXT RECORD
                   IF FS-CBPLCO = "9D"
                      CALL "CWISAM" USING ER-CBPLCO
                   END-IF
                   END-PERFORM
                   IF   FS-CBPLCO < "10"
                        ADD  1                TO LD-CBPLCO
                        DISPLAY                T-LD-CBPLCO
                        MOVE CBPLCO-CONTA     TO CB002PCW-CONTA
                        MOVE "C"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        MOVE "E"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        IF   CB002PCW-GRAU NOT > GRAU
                        AND  CB002PCW-GRAU NOT < GRAU-MINIMO
                             MOVE CB002PCW-CONTA-ED  TO CLIC-CONTA-ED
                             MOVE CBPLCO-DESCRICAO TO CLIC-DESCRICAO
                             MOVE CBPLCO-CONTA     TO CBCOSA-CONTA
                             MOVE CC
                               TO CBCOSA-CENTRO-CUSTO
                             MOVE AAAA-REF         TO CBCOSA-AAAA
                             MOVE MM-REF           TO CBCOSA-MM
                             PERFORM TEST AFTER
                                     UNTIL FS-CBCOSA NOT = "9D"
                             READ CBCOSA
                             IF FS-CBCOSA = "9D"
                                CALL "CWISAM" USING ER-CBCOSA
                             END-IF
                             END-PERFORM
                             IF   FS-CBCOSA < "09"
                                  IF  (CLASSE = 1)
                                 AND (CB002PCW-GRAU NOT = GRAU-ANTERIOR)
                                  AND (GRAU-ANTERIOR NOT = 0)
                                       MOVE SPACES TO CWIMPR-DETAIL
                                       CALL "CWIMPR"
                                      USING PARAMETROS-CWIMPR
                                      PERFORM 910-SAI-CWIMPR
                                  END-IF
                                  IF   CB002PCW-GRAU = GRAU-MINIMO
                                       ADD CBCOSA-SALDO-INICIAL
                                        TO SALDO-INICIAL
                                       ADD CBCOSA-SALDO-ATUAL
                                        TO SALDO-ATUAL
                                       ADD CBCOSA-A-DEBITO
                                        TO DEBITOS
                                       ADD CBCOSA-A-CREDITO
                                        TO CREDITOS
                                  END-IF
                                  MOVE CBCOSA-SALDO-INICIAL
                                    TO CLIC-SALDO-INICIAL
                                  MOVE CBCOSA-SALDO-ATUAL
                                    TO CLIC-SALDO-ATUAL
                                  MOVE CBCOSA-A-DEBITO
                                    TO CLIC-DEBITOS
                                  MOVE CBCOSA-A-CREDITO
                                    TO CLIC-CREDITOS
                                  IF   CLIC-FIM-ED = SPACES
                                  AND (CBPLCO-COD-RED NOT = 0)
                                       MOVE CBPLCO-COD-RED
                                         TO CLIC-COD-RED
                                            COD-RED-CALL
                                       MOVE "-"
                                         TO CLIC-COD-RED-TRACO
                                       CALL "CB039PCW" USING
                                                       COD-RED-CALL
                                                       COD-RED-DV-CALL
                                       MOVE COD-RED-DV-CALL
                                         TO CLIC-COD-RED-DV
                                  END-IF
                                  MOVE LINHA-02         TO CWIMPR-DETAIL
                                  CALL "CWIMPR" USING PARAMETROS-CWIMPR
                                  PERFORM 910-SAI-CWIMPR
                                  MOVE CB002PCW-GRAU TO GRAU-ANTERIOR
                                  ADD  1                TO GR-PRNTER
                                  DISPLAY                T-GR-PRNTER
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCACC
           IF   FS-CBCACC = "30" OR "35"
                OPEN I-O CBCACC

           MOVE 9999 TO CBCACC-CODIGO
           START CBCACC KEY NOT GREATER CBCACC-CHAVE
           IF   FS-CBCACC < "10"
                READ CBCACC PREVIOUS RECORD IGNORE LOCK
                IF   FS-CBCACC < "10"
                     MOVE 1 TO CC-FLAG
                END-IF
           END-IF

           OPEN INPUT CBPLCO
           IF   FS-CBPLCO > "09"
                CLOSE CBPLCO CBCACC
                GOBACK
           END-IF

           OPEN INPUT CBCOSA
           IF   FS-CBCOSA > "09"
                CLOSE CBPLCO CBCOSA CBCACC
                GOBACK
           END-IF

           MOVE 1                TO CWBOXS-OPTION.

       800-INICIAIS-BOXS.

           MOVE "N"              TO CWBOXS-ERASE
           MOVE 08               TO CWBOXS-LINE
           MOVE 40               TO CWBOXS-COLUMN
           MOVE "Do"             TO CWBOXS-TITLE

           MOVE 999999999999999  TO CB002PCW-CONTA
           MOVE "E"              TO CB002PCW-FUNCAO
           CALL "CB002PCW"      USING PARAMETROS-CB002PCW

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CB002PCW-GRAU
                   MOVE I         TO I-1
                   MOVE I-X       TO CWBOXS-TEXT   (I) (2: 1)
                                     CWBOXS-CHAR   (I)
                   MOVE "§ Grau " TO CWBOXS-TEXT   (I) (3: 7)
           END-PERFORM

           CALL "CWBOXS"        USING PARAMETROS-CWBOXS

           IF   CWBOXS-OPTION = 0
                CLOSE CBPLCO CBCOSA
                GOBACK
           END-IF

           MOVE CWBOXS-OPTION TO GRAU-MINIMO

           IF   CWBOXS-OPTION < CB002PCW-GRAU
                MOVE "At‚"                TO CWBOXS-TITLE
                MOVE 55                   TO CWBOXS-COLUMN
                MOVE CB002PCW-GRAU        TO CWBOXS-OPTION
                CALL "CWBOXS"        USING PARAMETROS-CWBOXS
                IF   CWBOXS-ARROW = "<"
                     MOVE GRAU-MINIMO TO CWBOXS-OPTION
                     GO TO 800-INICIAIS-BOXS
                END-IF
                IF   CWBOXS-OPTION = 0
                     CLOSE CBPLCO CBCOSA
                     GOBACK.

           MOVE "BALANCETE" TO CWIMPR-TITLE

           IF CWBOXS-OPTION < CB002PCW-GRAU
           OR GRAU-MINIMO > 1
              IF GRAU-MINIMO < 2
                 MOVE "ATE O GRAU "              TO CWIMPR-TITLE(21: 11)
                 MOVE CWBOXS-CHAR(CWBOXS-OPTION) TO CWIMPR-TITLE(32: 1)
              ELSE
                 MOVE "DO GRAU "                  TO CWIMPR-TITLE(21: 8)
                 MOVE GRAU-MINIMO                 TO CWIMPR-TITLE(29: 1)
                 IF CWBOXS-OPTION NOT = GRAU-MINIMO
                    MOVE " AO " TO CWIMPR-TITLE(30: 6)
                    MOVE CWBOXS-CHAR(CWBOXS-OPTION)
                      TO CWIMPR-TITLE(34: 1).

           MOVE CWBOXS-OPTION      TO GRAU
           MOVE SPACES             TO CWBOXS-ITENS
           MOVE 15                 TO CWBOXS-LINE
           MOVE 40                 TO CWBOXS-COLUMN
           MOVE "Ordem"            TO CWBOXS-TITLE
           MOVE " C¢digo "         TO CWBOXS-TEXT   (1)
           MOVE " Descri‡Æo "      TO CWBOXS-TEXT   (2)
           MOVE "C"                TO CWBOXS-CHAR   (1)
           MOVE "D"                TO CWBOXS-CHAR   (2)

           IF   CWBOXS-OPTION = CB002PCW-GRAU
                MOVE " c¢digo Reduzido" TO CWBOXS-TEXT   (3)
                MOVE "R"                TO CWBOXS-CHAR   (3).

           MOVE 1                  TO CWBOXS-OPTION
           CALL "CWBOXS"        USING PARAMETROS-CWBOXS
           MOVE CWBOXS-OPTION      TO CLASSE

           MOVE 15            TO CWBOXS-LINE
           MOVE 61            TO CWBOXS-COLUMN
           MOVE "N"           TO CWBOXS-ERASE
           MOVE "Nota‡Æo"     TO CWBOXS-TITLE
           MOVE " em Reais  " TO CWBOXS-TEXT   (1)
           MOVE " Convertido" TO CWBOXS-TEXT   (2)
           MOVE " corriGido " TO CWBOXS-TEXT   (3)
           MOVE "R"           TO CWBOXS-CHAR   (1)
           MOVE "C"           TO CWBOXS-CHAR   (2)
           MOVE "G"           TO CWBOXS-CHAR   (3)
           MOVE 1             TO CWBOXS-OPTION
           CALL "CWBOXS"   USING PARAMETROS-CWBOXS
           MOVE CWBOXS-OPTION TO NOTACAO

           IF   NOTACAO = 0
                CLOSE CBPLCO CBCOSA
                GOBACK.

           DISPLAY CB0016B
           IF   CC-FLAG = 1
                DISPLAY TELA-CC
           END-IF

           PERFORM TEST AFTER UNTIL ESC
                                 OR FS-CBMVMS = "00"
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT CB0016B
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE "CB016PCW.H01"
                                LINE 16 COLUMN 22
                                HEIGHT 06 WIDTH 40
                           END-EXEC
                      END-IF
                   END-PERFORM
                   IF   NOT ESC
                        CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                        OPEN INPUT CBMVMS
                        IF   FS-CBMVMS > "09"
                             EXEC COBOLware Send Message MSG(3) END-EXEC
                        ELSE
                             MOVE AAAA-REF        TO CWTIME-DATE (1: 4)
                             MOVE MM-REF          TO CWTIME-DATE (5: 2)
                             MOVE "01"            TO CWTIME-DATE (7: 2)
                             SET  CWTIME-REVERSED TO TRUE
                             SET  CWTIME-EDIT     TO TRUE
                             CALL "CWTIME"    USING PARAMETROS-CWTIME
                             INSPECT CWTIME-MOUNTH-EDITED
                                     CONVERTING MINUSCULAS TO MAIUSCULAS
                              MOVE CWTIME-MOUNTH-EDITED TO REFERENCIA
                              MOVE TIT-REF       TO CWIMPR-TITLE(62: )
                        END-IF
                   END-IF
           END-PERFORM

           IF   CC-FLAG = 1
           AND (NOT ESC)
                MOVE "<Esc>-Abandona F5-Pesquisa"
                  TO RODAPE
                DISPLAY RODAPE LINE 23 COLUMN 03
                PERFORM TEST AFTER
                        UNTIL ESC
                           OR CC = 0
                           OR FS-CBCACC < "10"
                ACCEPT  TELA-CC
                ACCEPT  TECLA FROM ESCAPE KEY
                IF   F5
                     MOVE SPACES TO CWBOXF-OPTION
                     IF   CC NOT = 0
                          MOVE CC TO CBCACC-CODIGO
                          READ CBCACC IGNORE LOCK
                          IF   FS-CBCACC < "10"
                               MOVE CBCACC-DESCRICAO
                                 TO CWBOXF-OPTION
                          END-IF
                     END-IF
                     MOVE "CB050PCW"   TO CWBOXF-PROGRAM
                     MOVE "Centros de custo"
                                       TO CWBOXF-TITLE
                     MOVE  5 TO CWBOXF-STRING-1-LENGTH
                     MOVE 30 TO CWBOXF-STRING-2-LENGTH
                     MOVE  2 TO CWBOXF-ORDER
                     MOVE 10 TO CWBOXF-VERTICAL-LENGTH
                     COMPUTE CWBOXF-HORIZONTAL-LENGTH = 6
                           + CWBOXF-STRING-1-LENGTH
                           + CWBOXF-STRING-2-LENGTH
                     MOVE 10 TO CWBOXF-LINE
                     MOVE 21 TO CWBOXF-COLUMN
                     CALL "CWBOXF" USING PARAMETROS-CWBOXF
                     IF   CWBOXF-OPTION NOT = SPACES
                          MOVE CWBOXF-OPTION (1: 4)
                            TO CC
                         SET ENTER-KEY TO TRUE
                     END-IF
                END-IF
                IF   CC NOT = 0
                     DISPLAY TELA-CC
                     MOVE CC TO CBCACC-CODIGO
                     READ CBCACC IGNORE LOCK
                     IF   FS-CBCACC > "09"
                          EXEC COBOLware Send Message MSG(4) END-EXEC
                     ELSE
                          STRING "C/C: " CC " "
                                 CBCACC-DESCRICAO
                                 DELIMITED BY SIZE
                                 INTO OBS-5
                          DISPLAY OBS-5
                             LINE 20 COLUMN 03
                     END-IF
                ELSE
                     DISPLAY "Geral" LINE 20 COLUMN 20
                END-IF
                END-PERFORM
           END-IF

           IF   ESC
                CLOSE CBPLCO CBCOSA CBCACC
                GOBACK
           END-IF

           MOVE SPACES TO RODAPE DISPLAY RODAPE LINE 23 COLUMN 03
           MOVE LINHA-01          TO CWIMPR-HEADER-1

           IF   NOTACAO > 1
                PERFORM 810-GERA-CBWORK THRU 810-99-FIM.

           MOVE LOW-VALUES         TO CBPLCO-REG
           EVALUATE CLASSE
                WHEN 1
                     MOVE "EM ORDEM DE CODIGO CONTABIL"
                       TO CWIMPR-SUB-TITLE (1: 59)
                     MOVE "CB016PA" TO CWIMPR-REPORT
                     PERFORM TEST AFTER
                             UNTIL FS-CBPLCO NOT = "9D"
                     START CBPLCO  KEY NOT LESS CBPLCO-CHAVE
                     IF FS-CBPLCO = "9D"
                        CALL "CWISAM" USING ER-CBPLCO
                     END-IF
                     END-PERFORM
                WHEN 2
                     MOVE "EM ORDEM DE DESCRICAO"
                       TO CWIMPR-SUB-TITLE(1: 59)
                     MOVE "CB016PB" TO CWIMPR-REPORT
                     PERFORM TEST AFTER
                             UNTIL FS-CBPLCO NOT = "9D"
                     START CBPLCO  KEY NOT LESS CBPLCO-DESCRICAO
                     IF FS-CBPLCO = "9D"
                        CALL "CWISAM" USING ER-CBPLCO
                     END-IF
                     END-PERFORM
                WHEN 3
                     MOVE "EM ORDEM DE CODIGO REDUZIDO"
                       TO CWIMPR-SUB-TITLE (1: 59)
                     MOVE "CB016PC" TO CWIMPR-REPORT
                     PERFORM TEST AFTER
                             UNTIL FS-CBPLCO NOT = "9D"
                     START CBPLCO  KEY NOT LESS CBPLCO-COD-RED
                     IF FS-CBPLCO = "9D"
                        CALL "CWISAM" USING ER-CBPLCO
                     END-IF
                     END-PERFORM
                WHEN OTHER
                     CLOSE CBPLCO CBCOSA
                     GOBACK
           END-EVALUATE
           IF   NOTACAO < 2
           AND (OBS-5 NOT = SPACES)
                MOVE CWIMPR-SUB-TITLE TO OBS-5 (41: )
                MOVE OBS-5
                  TO CWIMPR-SUB-TITLE
           END-IF

           DISPLAY CB0016A
           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW".

       800-99-FIM. EXIT.

       810-GERA-CBWORK.

           MOVE "?"         TO CWMENU
           CALL "CWGETU" USING OPERADOR
                               TASK
                               PROGRAMA
                               CWMENU

           IF   TASK NOT NUMERIC
                MOVE ZEROS TO TASK
           END-IF

           MOVE TASK        TO LB-CBWORK (1: 6)

           OPEN I-O CBWORK

           DISPLAY "Gerando " LINE 20 COLUMN 3
                              LB-WORK "000000/000000..."

           PERFORM UNTIL FS-CBCOSA > "09"
                   PERFORM TEST AFTER
                           UNTIL FS-CBCOSA NOT = "9D"
                           READ CBCOSA NEXT RECORD IGNORE LOCK
                           IF  (CBCOSA-CENTRO-CUSTO NOT = CC)
                           AND FS-CBCOSA < "10"
                               MOVE "9D" TO FS-CBCOSA
                           END-IF
                   END-PERFORM
                   IF   FS-CBCOSA < "10"
                        IF   CBCOSA-AAAA = AAAA-REF
                        AND  CBCOSA-MM   = MM-REF
                             ADD     1  TO GR-CBWORK
                             DISPLAY GR-CBWORK LINE 20 COLUMN 22
                             PERFORM 820-GRAVA-CBWORK THRU 820-99-FIM
                        END-IF
                  END-IF
           END-PERFORM

           CLOSE CBCOSA
           MOVE 0 TO GR-CBWORK

           MOVE 1          TO CBMVMS-DIA
           MOVE LOW-VALUES TO CBMVMS-CHAVE
           START CBMVMS KEY NOT LESS CBMVMS-DIA-CHAVE

           PERFORM UNTIL FS-CBMVMS > "09"
                   READ CBMVMS NEXT RECORD IGNORE LOCK
                   IF   FS-CBMVMS < "10"
                        ADD     1 TO GR-CBWORK
                        DISPLAY      GR-CBWORK LINE 20 COLUMN 29
                        PERFORM 830-ACUMULA-CBWORK THRU 830-99-FIM
                   END-IF
           END-PERFORM

           CLOSE CBWORK
           MOVE  LB-CBWORK TO LB-CBCOSA
           OPEN INPUT CBCOSA.

       810-99-FIM. EXIT.

       820-GRAVA-CBWORK.

           MOVE CBCOSA-SALDO-INICIAL TO CB014PCW-VALOR
           MOVE AAAA-REF             TO CB014PCW-REFERENCIA-AAAA
           MOVE MM-REF               TO CB014PCW-REFERENCIA-MM
           MOVE 01                   TO CB014PCW-REFERENCIA-DD
           CALL "CB014PCW"           USING PARAMETROS-CB014PCW

           EVALUATE NOTACAO
               WHEN 2
                    MOVE CB014PCW-CONVERTIDO TO CBCOSA-SALDO-INICIAL
                                              CBCOSA-SALDO-ATUAL
               WHEN 3
                    MOVE CB014PCW-CORRIGIDO  TO CBCOSA-SALDO-INICIAL
                                              CBCOSA-SALDO-ATUAL
           END-EVALUATE

           IF   VEZ = 1
                MOVE 2                   TO VEZ
                MOVE CB014PCW-MOEDA-NOME TO OBS-4
                EVALUATE NOTACAO
                         WHEN 2
                              MOVE "EM" TO OBS-2
                         WHEN 3
                              MOVE "CORRIGIDO" TO OBS-2
                              MOVE "P/"        TO OBS-3
                END-EVALUATE
                MOVE OBS-1 TO CWIMPR-SUB-TITLE (60: )
                IF   OBS-5 NOT = SPACES
                     MOVE CWIMPR-SUB-TITLE TO OBS-5 (41: )
                     MOVE OBS-5            TO CWIMPR-SUB-TITLE
                END-IF
           END-IF

           MOVE 0 TO CBCOSA-A-DEBITO
                     CBCOSA-A-CREDITO

           WRITE CBWORK-REG FROM CBCOSA-REG
              IF   FS-CBWORK NOT < "10"
                   CALL "CWISAM" USING ER-CBWORK
                   STOP RUN.

       820-99-FIM. EXIT.

       830-ACUMULA-CBWORK.

           MOVE CBMVMS-VALOR TO CB014PCW-VALOR
           MOVE AAAA-REF     TO CB014PCW-REFERENCIA-AAAA
           MOVE MM-REF       TO CB014PCW-REFERENCIA-MM
           MOVE CBMVMS-DIA   TO CB014PCW-REFERENCIA-DD
           CALL "CB014PCW"  USING PARAMETROS-CB014PCW

           EVALUATE NOTACAO
                    WHEN 2
                         MOVE CB014PCW-CONVERTIDO TO CB014PCW-VALOR
                    WHEN 3
                         MOVE CB014PCW-CORRIGIDO  TO CB014PCW-VALOR
           END-EVALUATE

           MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
           READ CBPLCO IGNORE LOCK
                   KEY IS CBPLCO-COD-RED

           IF   FS-CBPLCO > "09"
                STOP RUN
           END-IF

           MOVE CBPLCO-CONTA TO CB002PCW-CONTA

           PERFORM UNTIL CB002PCW-CONTA = 0
              MOVE CB002PCW-CONTA TO CBWORK-CONTA
              MOVE AAAA-REF     TO CBWORK-AAAA
              MOVE MM-REF       TO CBWORK-MM
              READ CBWORK
              IF   FS-CBWORK > "09"
                   STOP RUN
              END-IF
              IF   CBMVMS-TIPO = "D"
                   ADD CB014PCW-VALOR TO CBWORK-SALDO-ATUAL
                                       CBWORK-A-DEBITO
              ELSE
                   SUBTRACT CB014PCW-VALOR FROM CBWORK-SALDO-ATUAL
                   ADD      CB014PCW-VALOR   TO CBWORK-A-CREDITO
              END-IF
              REWRITE CBWORK-REG
              IF   FS-CBWORK > "09"
                   STOP RUN
              END-IF
              MOVE "S"         TO CB002PCW-FUNCAO
              CALL "CB002PCW" USING PARAMETROS-CB002PCW
           END-PERFORM.

       830-99-FIM. EXIT.

       900-FINAIS.

           MOVE SPACES        TO CLIC-CONTA-ED
           MOVE "TOTAL"       TO CLIC-DESCRICAO
           MOVE SALDO-INICIAL TO CLIC-SALDO-INICIAL
           MOVE SALDO-ATUAL   TO CLIC-SALDO-ATUAL
           MOVE DEBITOS       TO CLIC-DEBITOS
           MOVE CREDITOS      TO CLIC-CREDITOS
           MOVE SPACES        TO CWIMPR-DETAIL
           CALL "CWIMPR"   USING PARAMETROS-CWIMPR
           PERFORM 910-SAI-CWIMPR
           MOVE LINHA-02      TO CWIMPR-DETAIL
           CALL "CWIMPR"   USING PARAMETROS-CWIMPR
           PERFORM 910-SAI-CWIMPR
           MOVE "CLOSE"       TO CWIMPR-TIME-REPORT
           CALL "CWIMPR"   USING PARAMETROS-CWIMPR

           CLOSE CBPLCO CBCOSA CBMVMS
           IF   NOTACAO > 1
                CANCEL "CB014PCW"
                DELETE FILE CBWORK
           END-IF

           CANCEL "CB002PCW".

       900-99-FIM. EXIT.

       910-SAI-CWIMPR.

           IF   CWIMPR-END-PRINT
                CLOSE CBPLCO CBCOSA
                      CBMVMS CBWORK
                GOBACK
           END-IF.

       910-99-FIM. EXIT.

       END PROGRAM CB016PCW.
