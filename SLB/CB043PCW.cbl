       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB043PCW INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/04/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Subrotina de Lancamentos                     *
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
       COPY CBCOHISL.
       COPY CBHIVASL REPLACING MANUAL BY AUTOMATIC.
       COPY CBPLCOSL.
       COPY CBMVMSSL REPLACING MANUAL BY AUTOMATIC.
       COPY CBGEINSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCAHIFD.
       COPY CBCOBAFD.
       COPY CBCOMSFD.
       COPY CBCOSAFD.
       COPY CBCOHIFD.
       COPY CBHIVAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.
       COPY CBGEINFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 DATA-TESTE               PIC  9(006) VALUE 0.
           05 COD-RED-F5               PIC  9(005) VALUE 0.
           05 COD-RED-CALL             PIC  9(005) VALUE 0.
           05 COD-RED-DV-CALL          PIC  X(001) VALUE SPACE.
           05 F5-ON                    PIC  9(001) VALUE 0.
           05 LB-HELP                  PIC  X(012) VALUE "CB043PCW.HXX".
           05 ESPACOS                  PIC  X(068) VALUE SPACES.
           05 SALDO-DB                 PIC S9(012)V99 VALUE 0.
           05 SALDO-CR                 PIC S9(012)V99 VALUE 0.
           05 SALDO-DB-A               PIC S9(012)V99 VALUE 0.
           05 SALDO-CR-A               PIC S9(012)V99 VALUE 0.
           05 DISPONIVEL-DB-A          PIC  9(001) VALUE 0.
           05 DISPONIVEL-CR-A          PIC  9(001) VALUE 0.
           05 RODAPE-INCLUSAO          PIC  X(040) VALUE
              "     Esc-Fun‡Æo F1-Hlp F2-Aceita F3-Ver ".
           05 VEZ                      PIC  9(001) VALUE 1.
           05 ESTORNO                  PIC  9(001) VALUE 0.
           05 CC                       PIC  9(004) VALUE 0.
           05 VALOR-A                  PIC  9(012)V99 VALUE 0.
           05 COD-RED-CR-A             PIC  9(005) VALUE 0.
           05 COD-RED-DB-A             PIC  9(005) VALUE 0.
           05 VEZ-LANCAMENTO           PIC  9(001) VALUE 0.
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 CONTROLE-REFERENCIA.
              10 CONTROLE-AAAA         PIC  9(004).
              10 CONTROLE-MM           PIC  9(002).
           05 RK-CBCOMS           COMP PIC  9(001) VALUE 1.
           05 RK-CBCOHI           COMP PIC  9(001) VALUE 1.
           05 RK-CBGEIN           COMP PIC  9(001) VALUE 1.
           05 CAMPO                    PIC  9(002) VALUE 1.
           05 OPCAO-ACESSO             PIC  9(001) VALUE 1.
           05 CONTROLE-MES             PIC  9(006) VALUE ZERO.
           05 DATA-CRITICA             PIC  X(006) VALUE ALL "0".
           05 STATUS-CD                            VALUE ZEROS.
              10 STATUS-C              PIC  X(002).
              10 STATUS-D              PIC  X(002).
           05 LINHA-BRANCA             PIC  X(070) VALUE SPACES.
           05 OK                       PIC  X(040) VALUE
             "Geracao completada".
           05 SALVA-TECLA              PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 ER-CBCOMS.
              10 FS-CBCOMS              PIC  X(002) VALUE "00".
              10 LB-CBCOMS              PIC  X(050) VALUE "CBCOMS".
           05 ER-CBCOHI.
              10 FS-CBCOHI              PIC  X(002) VALUE "00".
              10 LB-CBCOHI              PIC  X(050) VALUE "CBCOHI".
           05 ER-CBGEIN.
              10 FS-CBGEIN              PIC  X(002) VALUE "00".
              10 LB-CBGEIN              PIC  X(050) VALUE "CBGEIN".
           05 HISTORICOS-VARIAVEIS VALUE SPACES.
              10 DESCR OCCURS 24       PIC X(030).

       01  LACAMENTO-EM-MEMORIA.
           05 LANCAMENTO       PIC 9(007)    VALUE 0.
           05 DD-REF           PIC 9(002)    VALUE 0.
           05 COD-RED-DB       PIC 9(005)    VALUE 0.
           05 COD-RED-CR       PIC 9(005)    VALUE 0.
           05 DOCTO            PIC 9(008)    VALUE 0.
           05 DATA-HISTORICA   PIC 9(006)    VALUE 0.
           05 CENTRO-CUSTO     PIC 9(004)    VALUE 0.
           05 HISTORICO        PIC 9(004)    VALUE 0.
           05 HISTORICO-V      PIC 9(006)    VALUE 0.
           05 VALOR            PIC 9(012)V99 VALUE 0.

       COPY CB002PCW.
       COPY CWSEND.

       LINKAGE SECTION.

       01 PARAMETROS-CB043PCW.
           05 CB043PCW-COMANDO                PIC  X(001).
              88 INCLUSAO                                VALUE "i" "I".
              88 EXCLUSAO                                VALUE "e" "E".
           05 CB043PCW-LANCAMENTO.
              10 CB043PCW-LANCAMENTO-NUMERO   PIC  9(007).
              10 CB043PCW-LANCAMENTO-TIPO     PIC  X(001).
                 88 A-DEBITO                             VALUE "D".
                 88 A-CREDITO                            VALUE "C".
           05 CB043P-BAC.
              10 CB043PCW-SERIE-BAC           PIC  9(004).
              10 CB043PCW-NUMERO-BAC          PIC  9(004).
           05 CB043PCW-CENTRO-CUSTO           PIC  9(004).
           05 CB043PCW-CONTA                  PIC  9(015).
           05 CB043PCW-COD-RED                PIC  9(005).
           05 CB043PCW-HISTORICO              PIC  9(004).
           05 CB043PCW-HISTORICOS-VARIAVEIS.
              10 CB043PCW-DESCRICAO OCCURS 24 PIC  X(030).
           05 CB043PCW-DOCTO                  PIC  9(008).
           05 CB043PCW-AAAAMMDD-DOCTO         PIC  9(006).
           05 CB043PCW-DD-REFERENCIA          PIC  9(002).
           05 CB043PCW-VALOR                  PIC  9(012)V99.
           05 CB043PCW-RETORNO.
              10 CB043PCW-FLAG-TIPO           PIC  9(001).
              10 CB043PCW-FLAG-BAC            PIC  9(001).
              10 CB043PCW-FLAG-CENTRO-CUSTO   PIC  9(001).
              10 CB043PCW-FLAG-CONTA          PIC  9(001).
              10 CB043PCW-FLAG-COD-RED        PIC  9(001).
              10 CB043PCW-FLAG-HISTORICO      PIC  9(001).
              10 CB043PCW-FLAG-DIA            PIC  9(001).
              10 CB043PCW-FLAG-AAAAMMDD-DOCTO PIC  9(001).
              10 CB043PCW-FLAG-REFERENCIA     PIC  9(001).
              10 CB043PCW-FLAG-VALOR          PIC  9(001).
              10 CB043PCW-FLAG-CBMVMS          PIC  9(001).

       PROCEDURE DIVISION USING PARAMETROS-CB043PCW.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           IF  (NOT A-DEBITO)
           AND (NOT A-CREDITO)
                MOVE 1 TO CB043PCW-FLAG-TIPO
           END-IF

           MOVE CB043PCW-SERIE-BAC  TO CBCOBA-SERIE
           MOVE CB043PCW-NUMERO-BAC TO CBCOBA-NUMERO
           READ CBCOBA
           IF   FS-CBCOBA > "09"
                MOVE 1 TO CB043PCW-FLAG-BAC
           END-IF

           IF   CB043PCW-CENTRO-CUSTO NOT = 0
                MOVE CB043PCW-CENTRO-CUSTO TO CBCACC-CODIGO
                READ CBCACC
                IF   FS-CBCACC > "09"
                     MOVE 1 TO CB043PCW-FLAG-CENTRO-CUSTO
                END-IF
           END-IF

           EVALUATE TRUE
                WHEN (CB043PCW-CONTA NOT = 0)
                AND  (CB043PCW-COD-RED = 0)
                      MOVE CB043PCW-CONTA TO CBPLCO-CONTA
                      READ CBPLCO IGNORE LOCK
                      IF   FS-CBPLCO > "09"
                      OR   CBPLCO-COD-RED = 0
                           MOVE 1 TO CB043PCW-FLAG-CONTA
                      END-IF
                WHEN (CB043PCW-CONTA = 0)
                AND  (CB043PCW-COD-RED NOT = 0)
                      MOVE CB043PCW-COD-RED TO CBPLCO-COD-RED
                      READ CBPLCO IGNORE LOCK KEY IS CBPLCO-COD-RED
                      IF   FS-CBPLCO > "09"
                           MOVE 1 TO CB043PCW-FLAG-COD-RED
                      END-IF
                WHEN (CB043PCW-CONTA NOT = 0)
                AND  (CB043PCW-COD-RED NOT = 0)
                      MOVE CB043PCW-COD-RED TO CBPLCO-COD-RED
                      READ CBPLCO IGNORE LOCK KEY IS CBPLCO-COD-RED
                      IF   FS-CBPLCO > "09"
                      OR   CB043PCW-CONTA NOT = CBPLCO-CONTA
                           MOVE 1 TO CB043PCW-FLAG-COD-RED
                                     CB043PCW-FLAG-CONTA
                      END-IF
           END-EVALUATE

           IF   CB043PCW-HISTORICO NOT = 0
                MOVE CB043PCW-HISTORICO TO CBCAHI-CODIGO
                READ CBCAHI IGNORE LOCK
                IF   FS-CBCAHI > "09"
                     MOVE 1 TO CB043PCW-FLAG-HISTORICO
                END-IF
           ELSE
                IF   CB043PCW-HISTORICOS-VARIAVEIS = SPACES
                     MOVE 1 TO CB043PCW-FLAG-HISTORICO
                END-IF
           END-IF

           IF   CB043PCW-DOCTO NOT = 0
                MOVE CB043PCW-AAAAMMDD-DOCTO TO DATA-TESTE
                CALL "GRIDAT" USING DATA-TESTE
                CALL "GRVDAT" USING DATA-TESTE
                IF   DATA-TESTE = 0
                     MOVE 1 TO CB043PCW-FLAG-AAAAMMDD-DOCTO
                END-IF
           END-IF

           IF   CB043PCW-FLAG-BAC = 0
                MOVE CBCOBA-AAAA (3: 2)      TO DATA-TESTE (5: 2)
                MOVE CBCOBA-MM               TO DATA-TESTE (3: 2)
                MOVE CB043PCW-DD-REFERENCIA TO DATA-TESTE (1: 2)
                CALL "GRVDAT" USING DATA-TESTE
                IF   DATA-TESTE = 0
                     MOVE 1 TO CB043PCW-FLAG-REFERENCIA
                END-IF
           END-IF

           IF   CB043PCW-VALOR = 0
                MOVE 1 TO CB043PCW-FLAG-VALOR
           END-IF

           IF   CB043PCW-RETORNO = ZEROS
                PERFORM 700-VERIFICA-MES THRU 700-99-FIM
                MOVE CBCOBA-AAAA TO AAAA-REF
                MOVE CBCOBA-MM   TO MM-REF
                CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                OPEN INPUT CBMVMS
                IF   FS-CBMVMS > "09"
                     IF   EXCLUSAO
                          MOVE 1 TO CB043PCW-FLAG-VALOR
                     ELSE
                          PERFORM 103-GERA-MES THRU 103-99-FIM
                     END-IF
                ELSE
                     IF   EXCLUSAO
                          MOVE CB043PCW-LANCAMENTO-NUMERO
                            TO CBMVMS-LANCAMENTO
                          MOVE CB043PCW-LANCAMENTO-TIPO
                            TO CBMVMS-TIPO
                          READ CBMVMS
                          IF   FS-CBMVMS > "09"
                          OR  (CB043PCW-VALOR NOT = CBMVMS-VALOR)
                               MOVE 1 TO CB043PCW-FLAG-VALOR
                          END-IF
                     END-IF
                     CLOSE CBMVMS
                END-IF
           END-IF

           IF   CB043PCW-RETORNO = ZEROS
                MOVE CB043PCW-LANCAMENTO-NUMERO TO LANCAMENTO
                IF   A-DEBITO
                     MOVE CBPLCO-COD-RED TO COD-RED-DB
                     MOVE 0              TO COD-RED-CR
                ELSE
                     MOVE CBPLCO-COD-RED TO COD-RED-CR
                     MOVE 0              TO COD-RED-DB
                END-IF
                MOVE CB043PCW-DD-REFERENCIA  TO DD-REF
                MOVE CB043PCW-DOCTO          TO DOCTO
                MOVE CB043PCW-AAAAMMDD-DOCTO TO DATA-HISTORICA
                MOVE CB043PCW-CENTRO-CUSTO   TO CENTRO-CUSTO
                MOVE CB043PCW-HISTORICO      TO HISTORICO
                MOVE CB043PCW-VALOR          TO VALOR
                EVALUATE TRUE
                         WHEN INCLUSAO
                              PERFORM 101-INCLUSAO THRU 101-99-FIM
                         WHEN EXCLUSAO
                              PERFORM 102-EXCLUSAO THRU 102-99-FIM
                END-EVALUATE
           END-IF.

       100-99-FIM. EXIT.

       101-INCLUSAO.

           PERFORM 152-SALVA-HIST THRU 152-99-FIM
           MOVE    ZERO        TO CBMVMS-LANCAMENTO
           MOVE    SPACE       TO CBMVMS-TIPO
           READ CBMVMS LOCK
           IF   FS-CBMVMS > "09"
                STOP RUN
           END-IF
           ADD     1           TO CBMVMS-VALOR
           MOVE    CBMVMS-VALOR TO LANCAMENTO
           REWRITE CBMVMS-REG
           UNLOCK CBMVMS
           MOVE 0 TO VEZ-LANCAMENTO
           IF   COD-RED-DB NOT = 0
                MOVE    "D"                    TO CBMVMS-TIPO
                PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                WRITE CBMVMS-REG
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
           END-IF
           IF   COD-RED-CR NOT = 0
                MOVE    "C"                   TO CBMVMS-TIPO
                PERFORM 160-SALVA-DADOS     THRU 160-99-FIM
                PERFORM 170-CONTROLA-SALDOS THRU 170-99-FIM
                WRITE CBMVMS-REG
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
           END-IF

           MOVE    CBMVMS-LANCAMENTO TO CB043PCW-LANCAMENTO-NUMERO.

       101-99-FIM. EXIT.

       102-EXCLUSAO.

           MOVE    1                TO ESTORNO
           MOVE    SPACES           TO HISTORICOS-VARIAVEIS
           PERFORM 152-SALVA-HIST THRU 152-99-FIM
           MOVE    0                TO VEZ-LANCAMENTO
           IF   COD-RED-DB NOT = 0
                MOVE    "D"                    TO CBMVMS-TIPO
                PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                MOVE    "C"                    TO CBMVMS-TIPO
                PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                MOVE    "D"                    TO CBMVMS-TIPO
                READ CBMVMS LOCK
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
                DELETE CBMVMS RECORD
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
           END-IF
           IF   COD-RED-CR NOT = 0
                MOVE    "C"                    TO CBMVMS-TIPO
                PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                MOVE    "D"                    TO CBMVMS-TIPO
                PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                MOVE    "C"                    TO CBMVMS-TIPO
                READ CBMVMS LOCK
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
                DELETE CBMVMS RECORD
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
           END-IF
           MOVE    0                TO ESTORNO.

       102-99-FIM. EXIT.

       103-GERA-MES.

           IF   CBCOBA-LC-EFETIVOS NOT = 0
           OR   CBCOBA-CR-EFETIVOS NOT = 0
           OR   CBCOBA-DB-EFETIVOS NOT = 0
                MOVE 1 TO CB043PCW-FLAG-CBMVMS
                GO TO 103-99-FIM
           END-IF
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
           READ CBCOMS
           IF   CBCOBA-REFERENCIA > CBCOMS-REG
                PERFORM 700-VERIFICA-MES THRU 700-99-FIM
                MOVE CBCOMS-REG        TO CBGEIN-ANTERIOR
                MOVE CBCOBA-REFERENCIA TO CBGEIN-ATUAL
                OPEN OUTPUT CBGEIN
                WRITE CBGEIN-REG
                DISPLAY ESPACOS LINE 23 COLUMN 3
                DISPLAY
                "Gerando referˆncia " LINE 23 COLUMN 3
                CBCOBA-MM  "/" CBCOBA-AAAA " aguarde..."
                OPEN I-O CBCOSA
                PERFORM UNTIL FS-CBCOSA > "09"
                  PERFORM TEST AFTER
                          UNTIL FS-CBCOSA NOT = "9D"
                          READ CBCOSA NEXT RECORD
                          IF  FS-CBCOSA = "9D"
                              CALL "CWISAM"
                             USING FS-CBCOSA
                          END-IF
                  END-PERFORM
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
           END-IF.

       103-99-FIM. EXIT.

       152-SALVA-HIST.

           MOVE CB043PCW-HISTORICOS-VARIAVEIS TO HISTORICOS-VARIAVEIS
           IF   HISTORICO-V = 0
                IF   HISTORICOS-VARIAVEIS = SPACES
                     GO TO 152-99-FIM
                ELSE
                     OPEN INPUT CBCOHI
                     IF   FS-CBCOHI = "30" OR "35"
                          CLOSE CBCOHI
                          OPEN OUTPUT CBCOHI
                          MOVE 200000 TO CBCOHI-ULTIMO
                          WRITE CBCOHI-REG
                          CLOSE CBCOHI
                          GO TO 152-SALVA-HIST
                     ELSE
                          CLOSE CBCOHI
                          OPEN I-O CBCOHI
                     END-IF
                     READ CBCOHI
                     ADD  1             TO CBCOHI-ULTIMO
                     MOVE CBCOHI-ULTIMO TO HISTORICO-V
                     REWRITE CBCOHI-REG
                     CLOSE CBCOHI
               END-IF
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                   COMPUTE CBHIVA-TIPO = HISTORICO-V / 100000
                   MOVE HISTORICO-V   TO CBHIVA-CODIGO
                   MOVE I             TO CBHIVA-VARIAVEL
                   READ CBHIVA
                   IF   FS-CBHIVA < "10"
                        DELETE CBHIVA RECORD
                   END-IF
           END-PERFORM.

           MOVE 0 TO Y

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                   IF   DESCR (I) NOT = SPACES
                        COMPUTE CBHIVA-TIPO = HISTORICO-V / 100000
                        MOVE HISTORICO-V   TO CBHIVA-CODIGO
                        ADD  1             TO Y
                        MOVE Y             TO CBHIVA-VARIAVEL
                        MOVE DESCR (I)     TO CBHIVA-DESCRICAO
                        WRITE CBHIVA-REG
                   END-IF
           END-PERFORM.

       152-99-FIM. EXIT.

       160-SALVA-DADOS.

           IF   CBMVMS-TIPO = "D"
                MOVE COD-RED-DB TO CBMVMS-COD-RED
           ELSE
                MOVE COD-RED-CR TO CBMVMS-COD-RED
           END-IF

           MOVE LANCAMENTO     TO CBMVMS-LANCAMENTO
           MOVE CBCOBA-SERIE   TO CBMVMS-SERIE
           MOVE CBCOBA-NUMERO  TO CBMVMS-NUMERO
           MOVE DD-REF         TO CBMVMS-DIA
           MOVE DOCTO          TO CBMVMS-DOCTO
           CALL "GRIDAT"    USING DATA-HISTORICA
           MOVE DATA-HISTORICA TO CBMVMS-AAAAMMDD-DOCTO
           CALL "GRIDAT"    USING DATA-HISTORICA
           MOVE CENTRO-CUSTO   TO CBMVMS-CENTRO-CUSTO
           MOVE HISTORICO      TO CBMVMS-HISTORICO-PADRAO
           MOVE HISTORICO-V    TO CBMVMS-HISTORICO-VARIAVEL
           MOVE VALOR          TO CBMVMS-VALOR.

       160-99-FIM. EXIT.

       170-CONTROLA-SALDOS.

           MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
           READ CBPLCO LOCK KEY IS CBPLCO-COD-RED

           IF   FS-CBPLCO > "09"
                STOP RUN
           END-IF

           IF   CBPLCO-VIRGEM = "S"
                MOVE "N" TO CBPLCO-VIRGEM
                REWRITE CBPLCO-REG
                GO TO 170-CONTROLA-SALDOS
           END-IF

           UNLOCK CBPLCO
           OPEN INPUT CBCOMS
           READ CBCOMS IGNORE LOCK
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
                IF   INCLUSAO
                     ADD 1 TO CBCOBA-LC-EFETIVOS
                ELSE
                     IF   EXCLUSAO
                          SUBTRACT 1 FROM CBCOBA-LC-EFETIVOS
                     END-IF
                END-IF
           END-IF

           IF   INCLUSAO
                IF   CBMVMS-TIPO = "D"
                     ADD CBMVMS-VALOR TO CBCOBA-DB-EFETIVOS
                ELSE
                     ADD CBMVMS-VALOR TO CBCOBA-CR-EFETIVOS
                END-IF
           ELSE
                IF   EXCLUSAO
                     IF   CBMVMS-TIPO = "D"
                          SUBTRACT CBMVMS-VALOR FROM CBCOBA-CR-EFETIVOS
                     ELSE
                          SUBTRACT CBMVMS-VALOR FROM CBCOBA-DB-EFETIVOS
                     END-IF
                ELSE
                     IF   CBMVMS-TIPO = "D"
                          IF   ESTORNO = 1
                               SUBTRACT VALOR-A FROM CBCOBA-CR-EFETIVOS
                          ELSE
                               ADD CBMVMS-VALOR TO CBCOBA-DB-EFETIVOS
                          END-IF
                     ELSE
                          IF   ESTORNO = 1
                               SUBTRACT VALOR-A FROM CBCOBA-DB-EFETIVOS
                          ELSE
                               ADD CBMVMS-VALOR TO CBCOBA-CR-EFETIVOS
                          END-IF
                     END-IF
                END-IF
           END-IF

           REWRITE CBCOBA-REG
           IF   FS-CBCOBA NOT < "10"
                STOP RUN
           END-IF
           UNLOCK CBCOBA.

       170-99-FIM. EXIT.

       COPY CB021PCW.

       800-INICIAIS.

           MOVE ZEROS TO CB043PCW-RETORNO

           OPEN INPUT CBCAHI
                      CBCACC
                  I-O CBCOBA
                      CBHIVA
                      CBPLCO.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOBA CBCAHI CBHIVA CBPLCO CBCACC CBMVMS
           CANCEL "CB002PCW"
           CANCEL "CB039PCW".

       900-99-FIM. EXIT.

       END PROGRAM CB043PCW.
