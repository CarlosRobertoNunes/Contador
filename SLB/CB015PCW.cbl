       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB015PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  12/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Razao                                        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBCAHISL.
       COPY CBCOSASL.
       COPY CBHIVASL.
       COPY CBPLCOSL.
       COPY CBMVMSSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCAHIFD.
       COPY CBCOSAFD.
       COPY CBHIVAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 ws-OPTION                PIC  9(002) VALUE 0.
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG                  PIC  9(001) VALUE 0.
           05 COD-RED-F5               PIC  9(005) VALUE 0.
           05 F5-ON                    PIC  9(001) VALUE 0.
           05 F4-ON                    PIC  9(001) VALUE 0.
           05 COD-RED-CALL             PIC  9(005) VALUE 0.
           05 COD-RED-DV-CALL          PIC  X(001) VALUE SPACE.
           05 DIA-ANTERIOR             PIC  9(002) VALUE ZERO.
           05 TESTE-CONTA              PIC  9(001) VALUE ZERO.
           05 ZERADA                   PIC  9(001) VALUE ZERO.
              88 CONSIDERAR-ZERADA VALUE 1.
           05 QUEBRA                   PIC  9(001) VALUE ZERO.
              88 QUEBRAR-DIA VALUE 1.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 VEZ-ABERTURA             PIC  9(001) VALUE 1.
           05 SALVA-DIA-POINTER        PIC  9(002).
           05 SALVA-COD-RED-POINTER    PIC  9(005) COMP-3.
           05 SALVA-CHAVE-POINTER.
              10 SALVA-LANCAMENTO      PIC  9(007) COMP-3.
              10 SALVA-TIPO            PIC  X(001).
           05 BAC                  PIC  9(008).
           05 REDEFINES BAC.
              10 SERIE                 PIC  9(004).
              10 NUMERO                PIC  9(004).
           05 I                        PIC  9(002).
           05 X                        PIC  9(003) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 Z                        PIC  9(003) VALUE 0.
           05 TOTAL-DB          COMP-3 PIC S9(012)V99.
           05 TOTAL-CR          COMP-3 PIC S9(012)V99.
           05 TOTAL-DB-DIA      COMP-3 PIC S9(012)V99.
           05 TOTAL-CR-DIA      COMP-3 PIC S9(012)V99.
           05 SALDO             COMP-3 PIC S9(012)V99.
           05 SALDO-MES         COMP-3 PIC S9(012)V99.
           05 CONTA-A-LISTAR    COMP-3 PIC  9(015) VALUE ZERO.
           05 LD-CBPLCO         COMP-3 PIC  9(006) VALUE ZERO.
           05 LD-CBMVMS         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 NOTACAO                  PIC  9(001) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Conta inexistente             ".
              10 PIC X(30) VALUE "Conta impr¢pria               ".
              10 PIC X(30) VALUE "Referˆncia impr¢pria          ".
              10 PIC X(30) VALUE "Centro de custo inexistente   ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 4   PIC X(30).
           05 SALVA-HEADER-1                PIC X(220) VALUE SPACES.
           05 SALVA-HEADER-2                PIC X(220) VALUE SPACES.
           05 SALVA-HEADER-3                PIC X(220) VALUE SPACES.
           05 SALVA-HEADER-4                PIC X(220) VALUE SPACES.
           05 SALVA-HEADER-5                PIC X(220) VALUE SPACES.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(006) VALUE
              "RAZAO ".
           05 CLIC-OBS-1                     PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(013) VALUE
              " REFERENTE A ".
           05 CLIC-REFERENCIA                PIC  X(014) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-2                     PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-3                     PIC  X(002) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-4                     PIC  X(014) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-5                     PIC  X(039) VALUE SPACES.
       02  LINHA-02.
           05 FILLER                         PIC  X(055) VALUE
              "                                    LAN.CONTABIL     DO".
           05 FILLER                         PIC  X(054) VALUE
              "CUMENTO        BAC                                ".
           05 FILLER                         PIC  X(023) VALUE
              "                 CONTRA".
       02  LINHA-03.
           05 FILLER                         PIC  X(055) VALUE
              "HISTORICO                            NUMERO DIA   NUMER".
           05 FILLER                         PIC  X(026) VALUE
              "O    DATA    SER. NUM.  ".
           05 CC-FLAG-TXT                    PIC  X(003) VALUE "C/C".
           05 FILLER                         PIC  X(032) VALUE
              "         A DEBITO           A CR".
           05 FILLER                         PIC  X(022) VALUE
              "EDITO    PARTIDA".
       02  LINHA-04.
           05 CLIC-CONTA-ED                  PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DESCRICAO-CONTA           PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED                   PIC  Z(005) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE "-".
           05 CLIC-COD-RED-DV                PIC  X(001) VALUE SPACE.
       02  LINHA-05.
           05 CLIC-HISTORICO                 PIC  ZZZ9.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DESCR-HIST                PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-LANCAMENTO                PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DIA BLANK ZERO            PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DOCTO                     PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DDMMAAAA-DOCTO BLANK ZERO PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-BAC BLANK ZERO            PIC  9999/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CENTRO-CUSTO              PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DEBITO                    PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CREDITO                   PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CLIC-COD-RED-2                 PIC  Z(005) VALUE ZEROS.
           05 CLIC-COD-RED-2-TRACO           PIC  X(001) VALUE "-".
           05 CLIC-COD-RED-2-DV              PIC  X(001) VALUE SPACE.
       02  LINHA-06                          PIC  X(132) VALUE ALL "-".

       02  LINHA-TD.
           05 FILLER              PIC X(83) VALUE "TOTAL DB/CR DO DIA".
           05 CLIC-DEBITO-DIA     PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.
           05 FILLER              PIC  X(001) VALUE SPACE.
           05 CLIC-CREDITO-DIA    PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.

       COPY CWBOXF.
       COPY CB002PCW.
       COPY CB014PCW.
       COPY CWIMPR.
       COPY CWTIME.

       SCREEN SECTION.

       01  CB0015A AUTO.
           05 LINE 14 COLUMN 03 VALUE "Conta:".
           05 LINE 16 COLUMN 03 VALUE "Mˆs de referˆncia:".
           05 LINE 16 COLUMN 22 PIC ZZ/ USING MM-REF.
           05 LINE 16 COLUMN 25 PIC 9999 USING AAAA-REF BLANK ZERO.

       01  TELA-CC.
           05 LINE 15 COLUMN 03 VALUE "Centro de custo:".
           05 LINE 15 COLUMN 20 PIC ZZZZ USING CC.

       01  CB0015B.
           05 LINE 18 COLUMN 03 VALUE "Lidos".
           05 LINE 18 COLUMN 09 PIC X(025) FROM LB-CBPLCO.
           05 LINE 19 COLUMN 09 PIC X(025) FROM LB-CBMVMS.
           05 LINE 21 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBPLCO LINE 18 COLUMN 35 PIC ZZZ.ZZ9 FROM LD-CBPLCO.
           05 T-LD-CBMVMS LINE 19 COLUMN 35 PIC ZZZ.ZZ9 FROM LD-CBMVMS.
           05 T-GR-PRNTER LINE 21 COLUMN 35 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-CBPLCO > "09"
                   READ CBPLCO NEXT RECORD IGNORE LOCK
                   MOVE 1 TO TESTE-CONTA
                   PERFORM 105-CHECK-ZERADA THRU 105-99-FIM
                   IF   FS-CBPLCO < "10"
                   AND  TESTE-CONTA = 1
                        IF   F4-ON = 0
                             PERFORM 110-LISTAR-CONTA THRU 110-99-FIM
                        ELSE
                             MOVE CBPLCO-CONTA TO CB002PCW-CONTA
                             CALL "CB040PCW" USING CB002PCW-CONTA
                             IF   CB002PCW-CONTA NOT = 0
                                  PERFORM 110-LISTAR-CONTA
                                     THRU 110-99-FIM
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       105-CHECK-ZERADA.

           IF   CONSIDERAR-ZERADA
           OR   FS-CBPLCO > "09"
                GO TO 105-99-FIM
           END-IF

           MOVE 0                TO TESTE-CONTA
           MOVE CC               TO CBCOSA-CENTRO-CUSTO
           MOVE CBPLCO-CONTA     TO CBCOSA-CONTA
           MOVE AAAA-REF         TO CBCOSA-AAAA
           MOVE MM-REF           TO CBCOSA-MM

           READ CBCOSA IGNORE LOCK

           IF  FS-CBCOSA > "09"
               GO TO 105-99-FIM
           END-IF

           IF (CBCOSA-SALDO-ATUAL NOT = 0)
           OR (CBCOSA-A-DEBITO    NOT = 0)
           OR (CBCOSA-A-CREDITO   NOT = 0)
               MOVE 1 TO TESTE-CONTA
           END-IF.

       105-99-FIM. EXIT.

       110-LISTAR-CONTA.

           IF   VEZ-ABERTURA = 1
                MOVE 2               TO VEZ-ABERTURA
                PERFORM 111-LINHA-01-TITLE THRU 111-99-FIM
           END-IF

           ADD  1            TO LD-CBPLCO
           DISPLAY              T-LD-CBPLCO
           MOVE CBPLCO-CONTA TO CB002PCW-CONTA
                                CBCOSA-CONTA
           MOVE "C"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           MOVE "E"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW

           IF   CB002PCW-LANCAVEL = "N"
                EXIT PARAGRAPH
           END-IF

           MOVE CC               TO CBCOSA-CENTRO-CUSTO
           MOVE AAAA-REF         TO CBCOSA-AAAA
           MOVE MM-REF           TO CBCOSA-MM
           MOVE 0                TO TOTAL-DB TOTAL-DB-DIA
                                    TOTAL-CR TOTAL-CR-DIA

           READ CBCOSA IGNORE LOCK
           IF   FS-CBCOSA > "09"
                EXIT PARAGRAPH
           END-IF

            IF   VEZ NOT = 1
                 MOVE LINHA-06    TO CWIMPR-DETAIL
                 PERFORM 135-CWIMPR THRU 135-99-FIM
                 MOVE SPACES      TO CWIMPR-DETAIL
                 PERFORM 135-CWIMPR THRU 135-99-FIM
           END-IF

           MOVE SPACES           TO LINHA-05
           MOVE 0                TO SALDO
                                    CB014PCW-VALOR
                                    SALDO-MES

           MOVE CBCOSA-SALDO-INICIAL TO SALDO

           IF   NOTACAO > 1
                MOVE SALDO         TO CB014PCW-VALOR
                MOVE AAAA-REF      TO CB014PCW-REFERENCIA-AAAA
                MOVE MM-REF        TO CB014PCW-REFERENCIA-MM
                MOVE 01            TO CB014PCW-REFERENCIA-DD
                CALL "CB014PCW" USING PARAMETROS-CB014PCW
                EVALUATE NOTACAO
                         WHEN 2
                              MOVE CB014PCW-CONVERTIDO TO SALDO
                         WHEN 3
                              MOVE CB014PCW-CORRIGIDO  TO SALDO
                END-EVALUATE
                IF   VEZ = 1
                     MOVE CB014PCW-MOEDA-NOME TO CLIC-OBS-4
                     EVALUATE NOTACAO
                              WHEN 2
                                   MOVE "EM" TO CLIC-OBS-2
                              WHEN 3
                                   MOVE "CORRIGIDO" TO CLIC-OBS-2
                                   MOVE "P/" TO CLIC-OBS-3
                     END-EVALUATE
                END-IF
           END-IF

           IF   VEZ = 1
                MOVE 2        TO VEZ
                IF   F4-ON = 1
                     MOVE "DE CONTAS SELECIONADAS" TO CLIC-OBS-1
                END-IF
                PERFORM 111-LINHA-01-TITLE THRU 111-99-FIM
           END-IF

           IF   SALDO POSITIVE
                MOVE SALDO TO CLIC-DEBITO
           ELSE
                MOVE SALDO TO CLIC-CREDITO
           END-IF

           MOVE CB002PCW-CONTA-ED  TO CLIC-CONTA-ED
           MOVE CBPLCO-DESCRICAO   TO CLIC-DESCRICAO-CONTA
           MOVE CBPLCO-COD-RED     TO CLIC-COD-RED COD-RED-CALL
           CALL "CB039PCW"      USING COD-RED-CALL
                                      CLIC-COD-RED-DV
           MOVE LINHA-04           TO CWIMPR-HEADER-1
           ACCEPT CWIMPR-TIME-REPORT FROM TIME
           MOVE "SALDO ANTERIOR DO EXERCICIO"
                                 TO CLIC-DESCR-HIST
           MOVE LINHA-05         TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM
           MOVE SPACES           TO LINHA-05

           MOVE CBPLCO-COD-RED   TO CBMVMS-COD-RED
           MOVE 0                TO CBMVMS-DIA

           START CBMVMS KEY NOT < CBMVMS-COD-RED-CHAVE
           PERFORM TEST AFTER UNTIL FS-CBMVMS NOT = "9D"
                   READ CBMVMS NEXT RECORD IGNORE LOCK
                   IF  (CC NOT = 0)
                   AND (CC NOT = CBMVMS-CENTRO-CUSTO)
                   AND FS-CBMVMS < "10"
                       MOVE "9D" TO FS-CBMVMS
                   END-IF
           END-PERFORM
           PERFORM UNTIL FS-CBMVMS > "09"
                      OR CBPLCO-COD-RED NOT = CBMVMS-COD-RED
                   ADD  1            TO LD-CBMVMS
                   DISPLAY              T-LD-CBMVMS
                   MOVE CBMVMS-VALOR TO CB014PCW-VALOR
                   IF   NOTACAO > 1
                        MOVE CBMVMS-DIA  TO CB014PCW-REFERENCIA-DD
                        CALL "CB014PCW" USING PARAMETROS-CB014PCW
                   END-IF
                   EVALUATE NOTACAO
                            WHEN 2
                                 MOVE CB014PCW-CONVERTIDO
                                   TO CB014PCW-VALOR
                            WHEN 3
                                 MOVE CB014PCW-CORRIGIDO
                                   TO CB014PCW-VALOR
                   END-EVALUATE
                   IF   QUEBRAR-DIA
                   AND (CBMVMS-DIA NOT = DIA-ANTERIOR)
                        PERFORM 115-TOT-DIA THRU 115-99-FIM
                   END-IF
                   MOVE CBMVMS-LANCAMENTO     TO CLIC-LANCAMENTO
                   MOVE CBMVMS-DIA            TO CLIC-DIA DIA-ANTERIOR
                   MOVE CBMVMS-DOCTO          TO CLIC-DOCTO
                   MOVE CBMVMS-AAAAMMDD-DOCTO TO CWTIME-DATE
                   SET  CWTIME-REVERSED       TO TRUE
                   SET  CWTIME-REVERSE        TO TRUE
                   CALL "CWTIME"           USING PARAMETROS-CWTIME
                   MOVE CWTIME-DATE-FINAL     TO CLIC-DDMMAAAA-DOCTO
                   MOVE CBMVMS-SERIE          TO SERIE
                   MOVE CBMVMS-NUMERO         TO NUMERO
                   MOVE BAC                   TO CLIC-BAC
                   IF   CC NOT = 0
                        MOVE 0                   TO CLIC-CENTRO-CUSTO
                   ELSE
                        MOVE CBMVMS-CENTRO-CUSTO TO CLIC-CENTRO-CUSTO
                   END-IF
                   IF    CBMVMS-TIPO = "C"
                         MOVE CB014PCW-VALOR       TO CLIC-CREDITO
                         SUBTRACT CB014PCW-VALOR FROM SALDO
                                                      SALDO-MES
                         ADD CB014PCW-VALOR        TO TOTAL-CR
                                                      TOTAL-CR-DIA
                   END-IF
                   IF    CBMVMS-TIPO = "D"
                         MOVE CB014PCW-VALOR  TO CLIC-DEBITO
                         ADD  CB014PCW-VALOR  TO SALDO
                                               SALDO-MES
                                               TOTAL-DB
                                               TOTAL-DB-DIA
                   END-IF
                   MOVE 0              TO CLIC-COD-RED-2
                   MOVE CBMVMS-COD-RED TO SALVA-COD-RED-POINTER
                   MOVE CBMVMS-DIA     TO SALVA-DIA-POINTER
                   MOVE CBMVMS-CHAVE   TO SALVA-CHAVE-POINTER
                   IF   CBMVMS-TIPO = "D"
                        MOVE "C" TO CBMVMS-TIPO
                   ELSE
                        IF   CBMVMS-TIPO = "C"
                             MOVE "D" TO CBMVMS-TIPO
                        END-IF
                   END-IF
                   READ CBMVMS IGNORE LOCK
                           KEY IS CBMVMS-CHAVE
                   IF   FS-CBMVMS NOT = "23"
                        IF   FS-CBMVMS < "09"
                             MOVE CBMVMS-COD-RED TO CLIC-COD-RED-2
                                                    COD-RED-CALL
                             MOVE "-"            TO CLIC-COD-RED-2-TRACO
                             CALL "CB039PCW"  USING COD-RED-CALL
                                                    CLIC-COD-RED-2-DV
                        END-IF
                   END-IF
                   MOVE SALVA-COD-RED-POINTER TO CBMVMS-COD-RED
                   MOVE SALVA-DIA-POINTER     TO CBMVMS-DIA
                   START CBMVMS KEY = CBMVMS-COD-RED-CHAVE
                   PERFORM TEST AFTER UNTIL CBMVMS-CHAVE
                                          = SALVA-CHAVE-POINTER
                                         OR (FS-CBMVMS > "09")
                           PERFORM TEST AFTER
                                   UNTIL FS-CBMVMS NOT = "9D"
                                   READ CBMVMS NEXT RECORD IGNORE LOCK
                                   IF  (CC NOT = 0)
                                   AND (CC NOT = CBMVMS-CENTRO-CUSTO)
                                   AND FS-CBMVMS < "10"
                                       MOVE "9D" TO FS-CBMVMS
                                   END-IF
                           END-PERFORM
                   END-PERFORM
                   PERFORM 120-HISTORICOS THRU 120-99-FIM
                   PERFORM TEST AFTER
                           UNTIL FS-CBMVMS NOT = "9D"
                           READ CBMVMS NEXT RECORD IGNORE LOCK
                           IF  (CC NOT = 0)
                           AND (CC NOT = CBMVMS-CENTRO-CUSTO)
                           AND FS-CBMVMS < "10"
                               MOVE "9D" TO FS-CBMVMS
                           END-IF
                   END-PERFORM
           END-PERFORM

           IF   QUEBRAR-DIA
                PERFORM 115-TOT-DIA THRU 115-99-FIM
           END-IF

           MOVE "TOTAIS DB/CR"   TO CLIC-DESCR-HIST
           MOVE TOTAL-DB         TO CLIC-DEBITO
           MOVE TOTAL-CR         TO CLIC-CREDITO
           MOVE LINHA-05         TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR  THRU 135-99-FIM
           MOVE SPACES           TO LINHA-05

           MOVE "SALDO DO MES" TO CLIC-DESCR-HIST

           IF   SALDO-MES POSITIVE
                MOVE SALDO-MES TO CLIC-DEBITO
           ELSE
                MOVE SALDO-MES TO CLIC-CREDITO
           END-IF

           MOVE LINHA-05         TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM
           MOVE SPACES           TO LINHA-05

           MOVE "SALDO ATUAL DO EXERCICIO" TO CLIC-DESCR-HIST

           IF   SALDO POSITIVE
                MOVE SALDO TO CLIC-DEBITO
           ELSE
                MOVE SALDO TO CLIC-CREDITO
           END-IF

           MOVE LINHA-05         TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM
           ADD  1                TO GR-PRNTER
           DISPLAY                T-GR-PRNTER.

       110-99-FIM. EXIT.

       111-LINHA-01-TITLE.

            MOVE SPACES TO CWIMPR-TITLE
            MOVE 0      TO Y
            PERFORM VARYING X FROM 1 BY 1 UNTIL X > LENGTH OF LINHA-01
                    COMPUTE Z = X + 1
                    IF   X NOT > LENGTH OF LINHA-01
                    AND  LINHA-01 (X: 1) = SPACE
                    AND  LINHA-01 (Z: 1) = SPACE
                         CONTINUE
                    ELSE
                         ADD 1 TO Y
                         MOVE LINHA-01 (X: 1) TO CWIMPR-TITLE (Y: 1)
                    END-IF
            END-PERFORM.

       111-99-FIM. EXIT.

       115-TOT-DIA.

           IF  DIA-ANTERIOR NOT = 0
           AND ((TOTAL-DB-DIA NOT = 0)
           OR   (TOTAL-CR-DIA NOT = 0))
               MOVE TOTAL-DB-DIA     TO CLIC-DEBITO-DIA
               MOVE TOTAL-CR-DIA     TO CLIC-CREDITO-DIA
               MOVE LINHA-TD         TO CWIMPR-DETAIL
               PERFORM 135-CWIMPR THRU 135-99-FIM
           END-IF

           MOVE ALL "- "         TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM
           MOVE ZEROS TO TOTAL-DB-DIA TOTAL-CR-DIA.

       115-99-FIM. EXIT.

       120-HISTORICOS.

           IF   CBMVMS-HISTORICO-PADRAO NOT = 0
                MOVE CBMVMS-HISTORICO-PADRAO TO CLIC-HISTORICO
                                                CBCAHI-CODIGO
                READ CBCAHI IGNORE LOCK
                IF   FS-CBCAHI < "10"
                     MOVE CBCAHI-DESCRICAO TO CLIC-DESCR-HIST
                ELSE
                     MOVE "NAO CADASTRADO" TO CLIC-DESCR-HIST
                END-IF
                PERFORM 130-IMPRIME-LINHA-05 THRU 130-99-FIM
                MOVE "00"   TO FS-CBHIVA
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
                                                 OR FS-CBHIVA > "09"
                        MOVE 1             TO CBHIVA-TIPO
                        MOVE CBCAHI-CODIGO TO CBHIVA-CODIGO
                        MOVE I             TO CBHIVA-VARIAVEL
                        READ CBHIVA IGNORE LOCK
                        IF   FS-CBHIVA < "09"
                             MOVE CBHIVA-DESCRICAO TO CLIC-DESCR-HIST
                             PERFORM 130-IMPRIME-LINHA-05
                                THRU 130-99-FIM
                        END-IF
                END-PERFORM
           END-IF

           MOVE "00"   TO FS-CBHIVA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                                            OR FS-CBHIVA > "09"
                   COMPUTE CBHIVA-TIPO = CBMVMS-HISTORICO-VARIAVEL
                                       / 100000
                   MOVE CBMVMS-HISTORICO-VARIAVEL TO CBHIVA-CODIGO
                   MOVE I                         TO CBHIVA-VARIAVEL
                   READ CBHIVA IGNORE LOCK
                   IF   FS-CBHIVA < "09"
                        MOVE CBHIVA-DESCRICAO TO CLIC-DESCR-HIST
                        PERFORM 130-IMPRIME-LINHA-05 THRU 130-99-FIM
                   END-IF
           END-PERFORM.

       120-99-FIM. EXIT.

       130-IMPRIME-LINHA-05.

           MOVE LINHA-05         TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM
           MOVE SPACES           TO LINHA-05.

       130-99-FIM. EXIT.

       135-CWIMPR.

           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBCAHI CBCACC
                      CBCOSA
                      CBHIVA
                      CBPLCO
                      CBMVMS
                GOBACK
           END-IF.

       135-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCACC
           IF   FS-CBCACC = "30" OR "35"
                OPEN I-O CBCACC
           END-IF

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
                CLOSE CBCACC
           END-IF

           OPEN INPUT CBCOSA
           IF   FS-CBCOSA > "09"
                CLOSE CBCACC CBPLCO
           END-IF

           OPEN INPUT CBCAHI
           IF   FS-CBCAHI > "09"
                CLOSE CBCACC CBPLCO CBCOSA
           END-IF

           OPEN INPUT CBHIVA
           IF   FS-CBHIVA > "09"
                CLOSE CBCACC CBPLCO CBCOSA CBHIVA
           END-IF

           MOVE ALL "-"           TO CWIMPR-HEADER-2
           MOVE LINHA-02          TO CWIMPR-HEADER-3
           DISPLAY CB0015A
           IF   CC-FLAG = 1
                DISPLAY TELA-CC
           END-IF
           MOVE "A"               TO CB002PCW-FUNCAO
           MOVE 14                TO CB002PCW-LINHA
           MOVE 10                TO CB002PCW-COLUNA

           PERFORM TEST AFTER UNTIL (CB002PCW-LANCAVEL = "S"
                                 OR CB002PCW-RETORNO = 01
                                 OR CB002PCW-CONTA = 0
                                 OR ESC)
                                 AND NOT F1
                                 AND (COD-RED-CALL = 0 OR ESC)
                      MOVE 0 TO COD-RED-CALL
                      MOVE
                 "<Esc>-Abandona F1-Help F2-Todas F4-Tabela F5-Pesquisa"
                        TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      CALL "CB002PCW" USING PARAMETROS-CB002PCW
                      MOVE CB002PCW-RETORNO TO TECLA
                      MOVE "N"              TO CB002PCW-FORCA-DV
                      IF   CB002PCW-CONTA = 0
                      AND  NOT F1
                      AND  NOT F2
                      AND  NOT F4
                      AND  NOT F5
                           DISPLAY "C¢digo reduzido: "
                                   LINE 14 COLUMN 03
                                   RODAPE LINE 23 COLUMN 03
                           ACCEPT COD-RED-CALL LINE 14 COLUMN 20
                                  WITH PROMPT UPDATE
                           ACCEPT TECLA FROM ESCAPE KEY
                           DISPLAY "Conta:                         "
                                   LINE 14 COLUMN 03
                      END-IF
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      EVALUATE TRUE
                      WHEN F4
                           MOVE 0 TO CB002PCW-CONTA
                           CALL "CB040PCW" USING CB002PCW-CONTA
                           MOVE 1 TO F4-ON
                      WHEN F5
                           MOVE 0 TO CB002PCW-CONTA
                           PERFORM 810-PESQUISA-CONTA THRU 810-99-FIM
                           SET F1 TO TRUE
                      WHEN F2
                           MOVE 0 TO CB002PCW-CONTA
                      WHEN F1
                           EXEC COBOLware Help
                                FILE   "CB015PCW.H01"
                                LINE   15 COLUMN 11
                                HEIGHT 06 WIDTH  40
                           END-EXEC
                      WHEN ESC
                           CONTINUE
                      WHEN OTHER
                           IF   CB002PCW-LANCAVEL EQUAL "S"
                           OR   COD-RED-CALL NOT = 0
                                PERFORM TEST AFTER
                                        UNTIL FS-CBPLCO NOT = "9D"
                                IF   CB002PCW-CONTA NOT = 0
                                     MOVE CB002PCW-CONTA TO CBPLCO-CONTA
                                     READ CBPLCO
                                ELSE
                                     MOVE COD-RED-CALL TO CBPLCO-COD-RED
                                     READ CBPLCO KEY IS CBPLCO-COD-RED
                                     IF   FS-CBPLCO < "10"
                                          MOVE CBPLCO-CONTA
                                            TO CB002PCW-CONTA
                                          MOVE "C" TO CB002PCW-FUNCAO
                                          CALL "CB002PCW" USING
                                               PARAMETROS-CB002PCW
                                          MOVE "D" TO CB002PCW-FUNCAO
                                          CALL "CB002PCW" USING
                                               PARAMETROS-CB002PCW
                                          MOVE "A" TO CB002PCW-FUNCAO
                                          MOVE "S" TO CB002PCW-FORCA-DV
                                     END-IF
                                END-IF
                                IF   FS-CBPLCO > "09"
                                     EXEC COBOLware Send
                                          Message MSG(1)
                                     END-EXEC
                                     MOVE 99 TO COD-RED-CALL
                                ELSE
                                     DISPLAY CBPLCO-DESCRICAO
                                             LINE 14 COLUMN 49
                                END-IF
                                END-PERFORM
                           ELSE
                                IF   CB002PCW-CONTA NOT = 0
                                     EXEC COBOLware Send
                                          Message MSG(2)
                                     END-EXEC
                                END-IF
                           END-IF
                      END-EVALUATE
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
                                     EXEC COBOLware Send
                                          Message MSG(4)
                                     END-EXEC
                                ELSE
                                     STRING "C/C: " CC " "
                                            CBCACC-DESCRICAO
                                            DELIMITED BY SIZE
                                            INTO CLIC-OBS-5
                                     DISPLAY CLIC-OBS-5
                                        LINE 15 COLUMN 03
                                     MOVE SPACES TO CC-FLAG-TXT
                                END-IF
                           ELSE
                                DISPLAY "Geral" LINE 15 COLUMN 20
                           END-IF
                           END-PERFORM
                      END-IF
           END-PERFORM

           MOVE LINHA-03          TO CWIMPR-HEADER-4
           CANCEL "CB002PCW"
           CANCEL "CB014PCW"
           CANCEL "CB039PCW"
           CANCEL "CWBOXF"
           CANCEL "CB044PCW"

           IF  ESC
               CLOSE CBCOSA CBCACC
                     CBCAHI
                     CBHIVA
               IF  F4-ON = 1
                   MOVE 999999999999999 TO CB002PCW-CONTA
                   CALL "CB040PCW" USING CB002PCW-CONTA
                   CANCEL "CB040PCW"
               END-IF
               GOBACK.

           MOVE SPACES TO RODAPE DISPLAY RODAPE LINE 23 COLUMN 03
           EXEC COBOLware BOXselect NOERASE
                LINE 08 COLUMN 04
                TITLE "Nota‡Æo"
                CAPTION(1) " em ~Reais"
                CAPTION(2) " ~Convertido"
                CAPTION(3) " corri~Gido"
                OPTION 1;NOTACAO
           END-EXEC

           IF   NOTACAO = 0
                CLOSE CBPLCO CBCOSA CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           EXEC COBOLware BOXselect NOERASE
                LINE 08 COLUMN  27
                TITLE  "C/contas_zeradas_?"
                CAPTION(1) " ~Sim "
                CAPTION(2) " ~NÆo "
                OPTION     1;ZERADA
           END-EXEC

           IF   ZERADA = 0
                CLOSE CBPLCO CBCOSA CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           EXEC COBOLware BOXselect NOERASE
                LINE 08 COLUMN  54
                TITLE "Totaliza_p/dia ?"
                CAPTION(1) " ~Sim "
                CAPTION(2) " ~NÆo "
                OPTION     1;QUEBRA
           END-EXEC

           IF   QUEBRA = 0
                CLOSE CBPLCO CBCOSA CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           PERFORM TEST AFTER UNTIL ESC
                                 OR FS-CBMVMS = "00"
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03 WITH SIZE 22
                      ACCEPT CB0015A
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE   "CB015PCW.H02"
                                LINE   16 COLUMN 23
                                HEIGHT 06 WIDTH  40
                           END-EXEC
                      END-IF
                   END-PERFORM
                   IF   NOT ESC
                        CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                        OPEN INPUT CBMVMS
                        IF   FS-CBMVMS > "09"
                             EXEC COBOLware Send
                                  Message MSG(3)
                             END-EXEC
                        ELSE
                             MOVE AAAA-REF        TO CWTIME-DATE (1: 4)
                             MOVE MM-REF          TO CWTIME-DATE (5: 2)
                             MOVE "01"            TO CWTIME-DATE (7: 2)
                             SET  CWTIME-REVERSED TO TRUE
                             SET  CWTIME-EDIT     TO TRUE
                             CALL "CWTIME"    USING PARAMETROS-CWTIME
                             INSPECT CWTIME-MOUNTH-EDITED
                                     CONVERTING MINUSCULAS TO MAIUSCULAS
                            MOVE CWTIME-MOUNTH-EDITED TO CLIC-REFERENCIA
                        END-IF
                   END-IF
           END-PERFORM

           IF   ESC
                CLOSE CBPLCO CBCOSA CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           CANCEL "CB041PCW"
           DISPLAY CB0015B

           CALL   "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW"

           IF   CB002PCW-CONTA = 0
           OR   F4-ON = 1
                PERFORM 820-ORDEM THRU 820-99-FIM
           ELSE
                MOVE "DE APENAS UM CONTA"  TO CWIMPR-SUB-TITLE
                MOVE "CB015PD"             TO CWIMPR-REPORT
                MOVE    "10"               TO FS-CBPLCO
                PERFORM 110-LISTAR-CONTA THRU 110-99-FIM
           END-IF.

       800-99-FIM. EXIT.

       810-PESQUISA-CONTA.

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
                 + CWBOXF-STRING-2-LENGTH

           CALL "CWBOXF" USING PARAMETROS-CWBOXF
           CANCEL "CWBOXF"

           IF   CWBOXF-OPTION = SPACES
                MOVE ZERO TO COD-RED-F5
           ELSE
                MOVE CWBOXF-OPTION (1: 5) TO COD-RED-F5
                MOVE COD-RED-F5           TO CBPLCO-COD-RED
                READ CBPLCO IGNORE LOCK
                        KEY IS CBPLCO-COD-RED
                IF   FS-CBPLCO < "10"
                     MOVE CBPLCO-CONTA     TO CB002PCW-CONTA
                     MOVE "C"              TO CB002PCW-FUNCAO
                     CALL "CB002PCW" USING    PARAMETROS-CB002PCW
                     MOVE "D"              TO CB002PCW-FUNCAO
                     CALL "CB002PCW" USING    PARAMETROS-CB002PCW
                     MOVE "A"              TO CB002PCW-FUNCAO
                     MOVE "S"              TO CB002PCW-FORCA-DV
                     DISPLAY CBPLCO-DESCRICAO LINE 14 COLUMN 49
                END-IF
           END-IF.

       810-99-FIM. EXIT.

       820-ORDEM.

           EXEC COBOLware BOXselect NOERASE
                LINE 15 COLUMN 50
                TITLE "Ordem"
                CAPTION(1) " ~C¢digo "
                CAPTION(2) " ~Descri‡Æo "
                CAPTION(3) " c¢digo ~Reduzido "
                OPTION     1;ws-OPTION
           END-EXEC
           MOVE LOW-VALUES          TO CBPLCO-REG

           EVALUATE ws-OPTION
                WHEN 1
                     MOVE "EM ORDEM DE CODIGO CONTABIL"
                       TO CWIMPR-SUB-TITLE
                     MOVE "CB015PA"                     TO CWIMPR-REPORT
                     PERFORM TEST AFTER
                             UNTIL FS-CBPLCO NOT = "9D"
                     START CBPLCO  KEY NOT LESS CBPLCO-CHAVE
                     IF FS-CBPLCO = "9D"
                        CALL "CWISAM" USING ER-CBPLCO
                     END-IF
                     END-PERFORM
                WHEN 2
                     MOVE "EM ORDEM DE DESCRICAO" TO CWIMPR-SUB-TITLE
                     MOVE "CB015PB"               TO CWIMPR-REPORT
                     PERFORM TEST AFTER
                             UNTIL FS-CBPLCO NOT = "9D"
                     START CBPLCO  KEY NOT LESS CBPLCO-DESCRICAO
                     IF FS-CBPLCO = "9D"
                        CALL "CWISAM" USING ER-CBPLCO
                     END-IF
                     END-PERFORM
                WHEN 3
                     MOVE "EM ORDEM DE CODIGO REDUZIDO"
                       TO CWIMPR-SUB-TITLE
                     MOVE "CB015PC"                     TO CWIMPR-REPORT
                     PERFORM TEST AFTER
                             UNTIL FS-CBPLCO NOT = "9D"
                     START CBPLCO  KEY NOT LESS CBPLCO-COD-RED
                     IF FS-CBPLCO = "9D"
                        CALL "CWISAM" USING ER-CBPLCO
                     END-IF
                     END-PERFORM
                WHEN OTHER
                     CLOSE CBPLCO CBCOSA CBCAHI CBHIVA CBCACC
                     GOBACK
           END-EVALUATE.

       820-99-FIM. EXIT.

       900-FINAIS.

           MOVE LINHA-06    TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM
           MOVE SPACES      TO CWIMPR-DETAIL
           PERFORM 135-CWIMPR THRU 135-99-FIM

           CLOSE CBPLCO CBCOSA CBCAHI CBHIVA CBMVMS CBCACC
           CANCEL "CB002PCW"
           CANCEL "CB014PCW"
           CANCEL "CB039PCW"
           IF  F4-ON = 1
               MOVE 999999999999999 TO CB002PCW-CONTA
               CALL "CB040PCW" USING CB002PCW-CONTA
               CANCEL "CB040PCW".

           MOVE "CLOSE"      TO CWIMPR-TIME-REPORT
           PERFORM 135-CWIMPR THRU 135-99-FIM.

       900-99-FIM. EXIT.

       END PROGRAM CB015PCW.
