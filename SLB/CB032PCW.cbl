       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB032PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  10/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Transferencia a resultado de exercicio       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBCAHISL.
       COPY CBCOBASL.
       COPY CBCOMSSL.
       COPY CBCOSASL REPLACING MANUAL BY EXCLUSIVE.
       COPY CBPLCOSL.
       COPY CBMVMSSL.
       COPY CBGEINSL.

           SELECT LOTEWK ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-LOTEWK.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCAHIFD.
       COPY CBCOBAFD.
       COPY CBCOMSFD.
       COPY CBCOSAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.
       COPY CBGEINFD.

       FD  LOTEWK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTEWK.

       01  LOTEWK-REG.
           05 LOTEWK-TIPO                      PIC  X(001).
           05 LOTEWK-COD-RED                   PIC  9(005).
           05 LOTEWK-VALOR                     PIC  9(012)V99.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CC                       PIC  9(004) VALUE 0.
           05 ESTORNO                  PIC  9(001) VALUE 0.
           05 HISTORICO                PIC  9(004) VALUE ZERO.
           05 COLUNA                   PIC  9(002) VALUE ZERO.
           05 CONTA-ESPECIAL           PIC  9(001) VALUE ZERO.
           05 CONTROLE-MES             PIC  9(006) VALUE ZERO.
           05 VEZ-LANCAMENTO           PIC  9(001) VALUE 0.
           05 CONTA-RESULTADO          PIC  X(001) VALUE SPACE.
           05 OK                       PIC  X(040) VALUE
             "Geracao completada".
           05 CONTROLE-REFERENCIA.
              10 CONTROLE-AAAA         PIC  9(004).
              10 CONTROLE-MM           PIC  9(002).
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 ESPACOS                  PIC  X(068) VALUE SPACES.
           05 NDF                      PIC  X(024) VALUE
              "NAO DEFINIDIO NO FORMATO".
           05 RK-CBCOMS           COMP PIC  9(001) VALUE 1.
           05 RK-CBPAPC           COMP PIC  9(001) VALUE 1.
           05 RK-CBCOHI           COMP PIC  9(001) VALUE 1.
           05 RK-CBGEIN           COMP PIC  9(001) VALUE 1.
           05 ABRE-NUMERO              PIC  9(018) VALUE 0.
           05 ABRE-NUMERO-X
              REDEFINES ABRE-NUMERO    PIC  X(018).
           05 INDICA-DBCR              PIC  X(008) VALUE SPACES.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 DV                       PIC  X(001) VALUE SPACE.
           05 ERRO                     PIC  9(002) VALUE 0.
           05 ERROS                    PIC  9(002) VALUE 0.
           05 ERROS-GERAL              PIC  9(006) VALUE 0.
           05 DATA-CRITICA             PIC  X(006) VALUE SPACES.
           05 DIA                      PIC  9(002) VALUE 0.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 P                        PIC  9(004) VALUE 0.
           05 S                        PIC  9(004) VALUE 0.
           05 S2                       PIC  9(004) VALUE 0.
           05 LIMITE                   PIC  9(002) VALUE 0.
           05 PG                       PIC  9(002) VALUE 0.
           05 GR-CBMVMS                PIC  9(006) VALUE 0.
           05 GR-LOTEWK                PIC  9(006) VALUE 0.
           05 LD-CBCOSA                PIC  9(006) VALUE 0.
           05 LD-LOTEWK                PIC  9(006) VALUE 0.
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBCOMS.
              10 FS-CBCOMS             PIC  X(002) VALUE "00".
              10 LB-CBCOMS             PIC  X(050) VALUE "CBCOMS".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBGEIN.
              10 FS-CBGEIN             PIC  X(002) VALUE "00".
              10 LB-CBGEIN             PIC  X(050) VALUE "CBGEIN".
           05 ER-LOTEWK.
              10 FS-LOTEWK             PIC  X(002) VALUE "00".
              10 LB-LOTEWK             PIC  X(050) VALUE "LOTEWK".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.
           05 LANCAMENTO       PIC 9(007)    VALUE 0.
           05 RESULTADO        PIC 9(004)    VALUE 0.
           05 MESTRE-1         PIC 9(015)    VALUE 0.
           05 MESTRE-2         PIC 9(015)    VALUE 0.

       COPY CB002PCW.
       COPY CWBOXS.

       SCREEN SECTION.

       01  CTAC-LIT-CB0032D.
           05 LINE 07 COLUMN 03 VALUE "Hist¢rico padr∆o:".

       01  CTAC-VAR-CB0032D.
           05 LINE 07 COLUMN 21 PIC Z(004) USING CBCAHI-CODIGO.

       01  CTAC-LIT-CB0032B.
           05 LINE 08 COLUMN 03 VALUE "BAC a associar: ".
           05 LINE 08 COLUMN 19 VALUE "SÇrie :".
           05 LINE 09 COLUMN 19 VALUE "N£mero:".

       01  CTAC-VAR-CB0032B.
           05 LINE 08 COLUMN 28 PIC Z(004) USING CBCOBA-SERIE.
           05 LINE 09 COLUMN 28 PIC Z(004) USING CBCOBA-NUMERO.

       01  CB0020E.
           05 LINE 10 COLUMN 03 VALUE "Conta de resultado:".

       01  CTAC-LIT-CB0032A.
           05 LINE 12 COLUMN 03 VALUE "Informe as contas me".
           05 LINE 12 COLUMN 23 VALUE "stres de 1ß grau a z".
           05 LINE 12 COLUMN 43 VALUE "erar:".

       01  CTAC-LIT-CB0032C.
           05 LINE 17 COLUMN 10 VALUE "Lidos de".
           05 LINE 18 COLUMN 10 VALUE "gravados em".
           05 LINE 19 COLUMN 10 VALUE "Lidos de".
           05 LINE 20 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0032C.
           05 T-LD-CBCOSA LINE 17 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-CBCOSA.
           05 LINE 17 COLUMN 19 PIC X(050) FROM LB-CBCOSA.
           05 T-GR-LOTEWK LINE 18 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTEWK.
           05 LINE 18 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-LD-LOTEWK LINE 19 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTEWK.
           05 LINE 19 COLUMN 19 PIC X(050) FROM LB-LOTEWK.
           05 T-GR-CBMVMS LINE 20 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-CBMVMS.
           05 LINE 20 COLUMN 22 PIC X(050) FROM LB-CBMVMS.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           INITIALIZE CBCOSA-CHAVE
           START CBCOSA KEY NOT LESS CBCOSA-CHAVE

           PERFORM UNTIL FS-CBCOSA = "10"
                   READ CBCOSA NEXT RECORD IGNORE LOCK
                   IF   FS-CBCOSA < "10"
                        ADD 1 TO LD-CBCOSA
                        DISPLAY T-LD-CBCOSA
                        IF   CBCOSA-AAAA = AAAA-REF
                        AND  CBCOSA-MM   = MM-REF
                        AND (CBCOSA-SALDO-ATUAL NOT = 0)
                             PERFORM 110-GRAVAR-LOTEWK
                                THRU 110-99-FIM
                   END-IF
           END-PERFORM

           CLOSE LOTEWK CBCOSA
           OPEN INPUT LOTEWK

           PERFORM 130-ABRIR-CBMVMS THRU 130-99-FIM
           PERFORM UNTIL FS-LOTEWK = "10"
              READ LOTEWK
              IF   FS-LOTEWK < "10"
                   ADD 1 TO LD-LOTEWK
                   DISPLAY T-LD-LOTEWK
                   PERFORM 140-IMPORTAR THRU 140-99-FIM
              END-IF
           END-PERFORM
           CLOSE LOTEWK
           DELETE FILE LOTEWK.

       100-99-FIM. EXIT.

       110-GRAVAR-LOTEWK.

           MOVE CBCOSA-CONTA TO CB002PCW-CONTA
           MOVE "S"          TO CB002PCW-FUNCAO
           MOVE "N"          TO CONTA-RESULTADO
           PERFORM UNTIL CB002PCW-CONTA = 0
                   CALL "CB002PCW" USING PARAMETROS-CB002PCW
                   IF  (CB002PCW-CONTA = MESTRE-1)
                   OR  (CB002PCW-CONTA = MESTRE-2)
                       MOVE "S" TO CONTA-RESULTADO
                   END-IF
           END-PERFORM

           IF   CONTA-RESULTADO = "N"
                GO TO 110-99-FIM
           END-IF

           MOVE CBCOSA-CONTA TO CB002PCW-CONTA
           MOVE "C"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           MOVE "E"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW

           IF   CB002PCW-LANCAVEL = "S"
                IF   CBCOSA-SALDO-ATUAL NEGATIVE
                     MOVE "D" TO LOTEWK-TIPO
                ELSE
                     MOVE "C" TO LOTEWK-TIPO
                END-IF
                MOVE CBCOSA-CONTA       TO CBPLCO-CONTA
                READ CBPLCO IGNORE LOCK
                IF   FS-CBPLCO NOT < "10"
                     PERFORM 900-FINAIS THRU 900-99-FIM
                     GOBACK
                END-IF
                IF   CBPLCO-COD-RED = RESULTADO
                     GO TO 110-99-FIM
                END-IF
                MOVE CBPLCO-COD-RED     TO LOTEWK-COD-RED
                MOVE CBCOSA-SALDO-ATUAL TO LOTEWK-VALOR
                WRITE LOTEWK-REG
                IF   FS-LOTEWK > "09"
                     PERFORM 900-FINAIS THRU 900-99-FIM
                     GOBACK
                ELSE
                     ADD 1 TO GR-LOTEWK
                     DISPLAY T-GR-LOTEWK
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       130-ABRIR-CBMVMS.

           PERFORM 700-VERIFICA-MES THRU 700-99-FIM

           CALL "CB045PCW" USING LB-CBMVMS (1: 6)
           OPEN INPUT CBMVMS
           IF   FS-CBMVMS > "09"
                MOVE "05" TO FS-CBMVMS
                CLOSE CBMVMS
                OPEN I-O CBMVMS
                INITIALIZE CBMVMS-REG
                WRITE CBMVMS-REG
                OPEN INPUT CBCOMS
                IF   FS-CBCOMS = "30" OR "35"
                     CLOSE CBCOMS
                     OPEN OUTPUT CBCOMS
                     MOVE CBCOBA-REFERENCIA TO CBCOMS-REG
                     WRITE CBCOMS-REG
                END-IF
                CLOSE CBCOMS
                OPEN I-O CBCOMS
                READ CBCOMS IGNORE LOCK
                IF   CBCOBA-REFERENCIA > CBCOMS-REG
                     PERFORM 700-VERIFICA-MES THRU 700-99-FIM
                     MOVE CBCOMS-REG        TO CBGEIN-ANTERIOR
                     MOVE CBCOBA-REFERENCIA TO CBGEIN-ATUAL
                     OPEN OUTPUT CBGEIN
                     WRITE CBGEIN-REG
                     DISPLAY ESPACOS LINE 23 COLUMN 03
                     DISPLAY
                     "Gerando referància " LINE 23 COLUMN 3
                     CBCOBA-MM  "/" CBCOBA-AAAA " aguarde..."
                     OPEN I-O CBCOSA
                     PERFORM UNTIL FS-CBCOSA > "09"
                       READ CBCOSA NEXT RECORD
                       IF  FS-CBCOSA < "10"
                           IF   CBCOSA-AAAAMM = CBCOMS-REG
                                MOVE CBCOSA-SALDO-ATUAL
                                  TO CBCOSA-SALDO-INICIAL
                                MOVE 0 TO CBCOSA-A-DEBITO
                                          CBCOSA-A-CREDITO
                                MOVE CBCOSA-REG TO SALVA-REG
                                MOVE CBCOSA-AAAAMM
                                  TO CONTROLE-REFERENCIA
                                PERFORM TEST AFTER
                                  UNTIL CBCOBA-REFERENCIA =
                                        CONTROLE-REFERENCIA
                                      ADD 1 TO CONTROLE-MM
                                      IF   CONTROLE-MM > 12
                                           MOVE 1
                                             TO CONTROLE-MM
                                            ADD 1
                                             TO CONTROLE-AAAA
                                      END-IF
                                      MOVE SALVA-REG
                                        TO CBCOSA-REG
                                      MOVE CONTROLE-REFERENCIA
                                        TO CBCOSA-AAAAMM
                                      WRITE CBCOSA-REG
                                      IF   FS-CBCOSA > "09"
                                           CALL "CWISAM"
                                           USING ER-CBCOSA
                                      END-IF
                                END-PERFORM
                            END-IF
                       END-IF
                     END-PERFORM
                     MOVE CBCOBA-REFERENCIA TO CBCOMS-REG
                     REWRITE CBCOMS-REG
                     CLOSE CBCOSA CBCOMS CBGEIN
                     DELETE FILE CBGEIN
                     DISPLAY OK LINE 23 COLUMN 3
                END-IF
           ELSE
                CLOSE CBMVMS
                OPEN I-O CBMVMS
           END-IF.

       130-99-FIM. EXIT.

       140-IMPORTAR.

           PERFORM 700-VERIFICA-MES THRU 700-99-FIM
           MOVE    ZERO        TO CBMVMS-LANCAMENTO
                                  VEZ-LANCAMENTO
           MOVE    SPACE       TO CBMVMS-TIPO
           READ CBMVMS LOCK
           IF   FS-CBMVMS > "09"
                STOP RUN
           END-IF
           ADD  1                        TO CBMVMS-VALOR
           MOVE CBMVMS-VALOR             TO LANCAMENTO
           REWRITE CBMVMS-REG
           UNLOCK CBMVMS
           MOVE LANCAMENTO               TO CBMVMS-LANCAMENTO
           MOVE LOTEWK-TIPO              TO CBMVMS-TIPO
           MOVE CBCOBA-SERIE             TO CBMVMS-SERIE
           MOVE CBCOBA-NUMERO            TO CBMVMS-NUMERO
           MOVE DIA                      TO CBMVMS-DIA
           MOVE LOTEWK-COD-RED           TO CBMVMS-COD-RED
           MOVE 0                        TO CBMVMS-DOCTO
                                            CBMVMS-AAAAMMDD-DOCTO
                                            CBMVMS-CENTRO-CUSTO
                                            CBMVMS-HISTORICO-VARIAVEL
           MOVE HISTORICO                TO CBMVMS-HISTORICO-PADRAO
           MOVE LOTEWK-VALOR             TO CBMVMS-VALOR
           PERFORM 150-CONTROLA-SALDOS THRU 150-99-FIM

           IF   LOTEWK-TIPO = "D"
                MOVE "C"                 TO CBMVMS-TIPO
           ELSE
                MOVE "D"                 TO CBMVMS-TIPO
           END-IF

           MOVE RESULTADO                TO CBMVMS-COD-RED
           PERFORM 150-CONTROLA-SALDOS THRU 150-99-FIM.

       140-99-FIM. EXIT.

       150-CONTROLA-SALDOS.

           MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
           READ CBPLCO LOCK KEY IS CBPLCO-COD-RED

           IF   FS-CBPLCO NOT < "10"
                STOP RUN
           END-IF

           IF   CBPLCO-VIRGEM = "S"
                MOVE "N" TO CBPLCO-VIRGEM
                REWRITE CBPLCO-REG
                GO TO 150-CONTROLA-SALDOS
           END-IF

           WRITE CBMVMS-REG
           IF   FS-CBMVMS NOT < "10"
                PERFORM 900-FINAIS THRU 900-99-FIM
                STOP RUN
           ELSE
                ADD 1 TO GR-CBMVMS
                DISPLAY T-GR-CBMVMS
           END-IF
           UNLOCK CBMVMS

           UNLOCK CBPLCO
           OPEN INPUT CBCOMS
           READ CBCOMS
           PERFORM TEST AFTER UNTIL FS-CBCOSA < "09"
                   OPEN I-O CBCOSA
           END-PERFORM
           COPY CBCOSACW.

           CLOSE CBCOMS CBCOSA

           READ CBCOBA LOCK
           IF   FS-CBCOBA NOT < "10"
                STOP RUN
           END-IF

           ADD  1 TO VEZ-LANCAMENTO

           IF   VEZ-LANCAMENTO = 1
                ADD 1 TO CBCOBA-LC-EFETIVOS
           END-IF

           IF   CBMVMS-TIPO = "D"
                ADD CBMVMS-VALOR TO CBCOBA-DB-EFETIVOS
           ELSE
                ADD CBMVMS-VALOR TO CBCOBA-CR-EFETIVOS
           END-IF

           REWRITE CBCOBA-REG
           IF   FS-CBCOBA NOT < "10"
                PERFORM 900-FINAIS THRU 900-99-FIM
                STOP RUN
           END-IF
           UNLOCK CBCOBA.

       150-99-FIM. EXIT.

       COPY CB021PCW.

       800-INICIAIS.

           OPEN OUTPUT LOTEWK

           OPEN I-O CBCOBA
           IF   FS-CBCOBA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN I-O CBPLCO
           IF   FS-CBPLCO > "09"
                CALL "CWISAM" USING ER-CBPLCO
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBCAHI
           IF   FS-CBCAHI > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBCOSA
           IF   FS-CBCOSA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBCACC
           IF   FS-CBCACC > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           DISPLAY CTAC-LIT-CB0032D

           PERFORM TEST AFTER UNTIL ESC
                            OR FS-CBCAHI = "00"
              ACCEPT CTAC-VAR-CB0032D
              ACCEPT TECLA FROM ESCAPE KEY
              IF   ESC
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
              ELSE
                   READ CBCAHI IGNORE LOCK
                   IF  FS-CBCAHI = "23"
                       EXEC COBOLware Send Message
                            "Hist¢rico padr∆o inexistente"
                       END-EXEC
                   END-IF
              END-IF
           END-PERFORM

           MOVE CBCAHI-CODIGO TO HISTORICO
           DISPLAY CBCAHI-DESCRICAO LINE 7 COLUMN 26
           DISPLAY CTAC-LIT-CB0032B
           CLOSE CBCAHI
           PERFORM TEST AFTER UNTIL ESC
                            OR FS-CBCOBA = "00"
              ACCEPT CTAC-VAR-CB0032B
              ACCEPT TECLA FROM ESCAPE KEY
              IF   ESC
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
              ELSE
                   READ CBCOBA IGNORE LOCK
                   IF  FS-CBCOBA = "23"
                       EXEC COBOLware Send
                            Message "BAC inexistente"
                       END-EXEC
                   ELSE
                       MOVE CBCOBA-AAAA TO AAAA-REF
                       MOVE CBCOBA-MM   TO MM-REF
                   END-IF
              END-IF
           END-PERFORM

           DISPLAY CB0020E

           MOVE 1 TO CONTA-ESPECIAL
           PERFORM 810-PESCA-CONTA THRU 810-99-FIM
           DISPLAY CTAC-LIT-CB0032A
           MOVE 2 TO CONTA-ESPECIAL
           PERFORM 810-PESCA-CONTA THRU 810-99-FIM
           MOVE 3 TO CONTA-ESPECIAL
           PERFORM 810-PESCA-CONTA THRU 810-99-FIM
           DISPLAY CTAC-LIT-CB0032C
                   CTAC-VAR-CB0032C

           MOVE ZEROS TO DATA-CRITICA
           MOVE 31    TO DIA
           PERFORM UNTIL DATA-CRITICA NOT = ZEROS
                   MOVE DIA             TO DATA-CRITICA (1: 2)
                   MOVE MM-REF          TO DATA-CRITICA (3: 2)
                   MOVE AAAA-REF (3: 2) TO DATA-CRITICA (5: 2)
                   CALL "GRVDAT" USING DATA-CRITICA
                   IF   DATA-CRITICA = ZEROS
                        SUBTRACT 1 FROM DIA
                   END-IF
           END-PERFORM.

       800-99-FIM. EXIT.

       810-PESCA-CONTA.

           MOVE "A" TO CB002PCW-FUNCAO
           MOVE 0   TO CB002PCW-CONTA

           EVALUATE CONTA-ESPECIAL
                    WHEN 1 MOVE "N" TO CB002PCW-FORCA-DV
                           MOVE 10  TO CB002PCW-LINHA
                           MOVE 23  TO CB002PCW-COLUNA
                    WHEN 2 MOVE "S" TO CB002PCW-FORCA-DV
                           MOVE 14  TO CB002PCW-LINHA
                           MOVE  3  TO CB002PCW-COLUNA
                    WHEN 3 MOVE "S" TO CB002PCW-FORCA-DV
                           MOVE 15  TO CB002PCW-LINHA
                           MOVE  3  TO CB002PCW-COLUNA
           END-EVALUATE

           PERFORM TEST AFTER UNTIL FS-CBPLCO < "10"
                   CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                   MOVE CB002PCW-RETORNO TO TECLA
                   IF   ESC
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
                   IF   CB002PCW-CONTA NOT = 0
                        MOVE CB002PCW-CONTA TO CBPLCO-CONTA
                        READ CBPLCO IGNORE LOCK
                        IF   FS-CBPLCO < "10"
                             COMPUTE COLUNA = CB002PCW-COLUNA + 26
                             DISPLAY CBPLCO-DESCRICAO
                                    LINE CB002PCW-LINHA COLUMN COLUNA
                             IF   CB002PCW-LANCAVEL NOT = "S"
                             AND  CONTA-ESPECIAL = 1
                                  EXEC COBOLware Send Message
                                       "Conta resultado n∆o anal°tica"
                                  END-EXEC
                                  MOVE "44" TO FS-CBPLCO
                             END-IF
                             IF  (CB002PCW-GRAU NOT = 1)
                             AND (CONTA-ESPECIAL NOT = 1)
                                  EXEC COBOLware Send Message
                                       "Mestre deve ser de 1ß grau"
                                  END-EXEC
                                  MOVE "44" TO FS-CBPLCO
                             END-IF
                        ELSE
                             EXEC COBOLware Send
                                  Message "Conta n∆o existe"
                             END-EXEC
                        END-IF
                   ELSE
                        MOVE "44" TO FS-CBPLCO
                   END-IF
           END-PERFORM.

           EVALUATE CONTA-ESPECIAL
                    WHEN 1 MOVE CBPLCO-COD-RED TO RESULTADO
                    WHEN 2 MOVE CB002PCW-CONTA   TO MESTRE-1
                    WHEN 3 MOVE CB002PCW-CONTA   TO MESTRE-2
           END-EVALUATE.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOBA CBPLCO LOTEWK
                 CBMVMS CBCOSA CBCAHI CBCACC.

       900-99-FIM. EXIT.

       END PROGRAM CB032PCW.
