       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB024PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Importa lancamentos                          *
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
       COPY CBHIVASL.
       COPY CBPAPCSL.
       COPY CBPLCOSL.
       COPY CBFOLCSL.
       COPY CBMVMSSL.
       COPY CBGEINSL.

           SELECT LOTE-I ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOTE-I.

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
       COPY CBCOHIFD.
       COPY CBHIVAFD.
       COPY CBPAPCFD.
       COPY CBPLCOFD.
       COPY CBFOLCFD.
       COPY CBMVMSFD.
       COPY CBGEINFD.

       FD  LOTE-I
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTE-I.

       01  LOTE-I-REG.
           05 LOTE-I-BYE PIC X(001) OCCURS 300.

       FD  LOTEWK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTEWK.

       01  LOTEWK-REG.
           05 LOTEWK-TIPO                      PIC  X(001).
           05 LOTEWK-DIA                       PIC  9(002).
           05 LOTEWK-COD-RED                   PIC  9(005).
           05 LOTEWK-DOCTO                     PIC  9(008).
           05 LOTEWK-AAAAMMDD-DOCTO            PIC  9(008).
           05 LOTEWK-CENTRO-CUSTO              PIC  9(004).
           05 LOTEWK-HISTORICO-PADRAO          PIC  9(004).
           05 LOTEWK-HISTORICO-VARIAVEL        PIC  X(030).
           05 LOTEWK-VALOR                     PIC  9(012)V99.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CC                       PIC  9(004) VALUE 0.
           05 ESTORNO                  PIC  9(001) VALUE 0.
           05 CONTROLE-MES             PIC  9(006) VALUE ZERO.
           05 VEZ-LANCAMENTO           PIC  9(001) VALUE 0.
           05 MESMO-LANCAMENTO         PIC  9(001) VALUE 0.
           05 OK                       PIC  X(040) VALUE
             "Geracao completada".
           05 CONTROLE-REFERENCIA.
              10 CONTROLE-AAAA         PIC  9(004).
              10 CONTROLE-MM           PIC  9(002).
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 ESPACOS                  PIC  X(068) VALUE SPACES.
           05 NDF                      PIC  X(024) VALUE
              "NAO DEFINIDO NO FORMATO".
           05 RK-CBCOMS           COMP PIC  9(001) VALUE 1.
           05 RK-CBPAPC           COMP PIC  9(001) VALUE 1.
           05 RK-CBCOHI           COMP PIC  9(001) VALUE 1.
           05 RK-CBGEIN           COMP PIC  9(001) VALUE 1.
           05 ABRE-NUMERO              PIC  9(018) VALUE 0.
           05 ABRE-NUMERO-X
              REDEFINES ABRE-NUMERO    PIC  X(018).
           05 INDICA-DBCR              PIC  X(008) VALUE SPACES.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 MSG-NE                   PIC  X(074) VALUE SPACES.
           05 DV                       PIC  X(001) VALUE SPACE.
           05 LER                      PIC  X(001) VALUE "S".
           05 ERRO                     PIC  9(002) VALUE 0.
           05 ERROS                    PIC  9(002) VALUE 0.
           05 ERROS-GERAL              PIC  9(006) VALUE 0.
           05 MODO                     PIC  9(001) VALUE 0.
              88 EXPORTA-BAC        VALUE 1.
              88 EXPORTA-REFERENCIA VALUE 2.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 DGC                      PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 P                        PIC  9(004) VALUE 0.
           05 S                        PIC  9(004) VALUE 0.
           05 S2                       PIC  9(004) VALUE 0.
           05 LIMITE                   PIC  9(002) VALUE 0.
           05 PG                       PIC  9(002) VALUE 0.
           05 LD-LOTE-I                PIC  9(006) VALUE 0.
           05 GR-CBMVMS                PIC  9(006) VALUE 0.
           05 GR-LOTEWK                PIC  9(006) VALUE 0.
           05 LD-LOTEWK                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
              88 EFETIVAR-OK VALUE "S" "s" "N" "n".
              88 EFETIVAR    VALUE "S" "s".
           05 ER-CBFOLC.
              10 FS-CBFOLC             PIC  X(002) VALUE "00".
              10 LB-CBFOLC             PIC  X(050) VALUE "CBFOLC.DAT".
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "000000.DAT".
                 15 AAAA-REF           PIC 9(004).
                 15 MM-REF             PIC 9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
                 15 FILLER             PIC  X(044).
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 ER-CBPAPC.
              10 FS-CBPAPC             PIC  X(002) VALUE "00".
              10 LB-CBPAPC             PIC  X(050) VALUE "CBPAPC".
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
           05 ER-CBCOHI.
              10 FS-CBCOHI             PIC  X(002) VALUE "00".
              10 LB-CBCOHI             PIC  X(050) VALUE "CBCOHI".
           05 ER-CBGEIN.
              10 FS-CBGEIN             PIC  X(002) VALUE "00".
              10 LB-CBGEIN             PIC  X(050) VALUE "CBGEIN.DAT".
           05 ER-LOTE-I.
              10 FS-LOTE-I             PIC  X(002) VALUE "00".
              10 LB-LOTE-I             PIC  X(050) VALUE "LOTE-I.TXT".
           05 ER-LOTEWK.
              10 FS-LOTEWK             PIC  X(002) VALUE "00".
              10 LB-LOTEWK             PIC  X(050) VALUE "LOTEWK".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.
           05 MENSAGENS.
              10 PIC X(19) VALUE "TIPO DE LANCAMENTO ".
              10 PIC X(19) VALUE "DIA CONTABIL       ".
              10 PIC X(19) VALUE "CONTA              ".
              10 PIC X(19) VALUE "CODIGO REDUZIDO    ".
              10 PIC X(19) VALUE "CENTRO DE CUSTOS   ".
              10 PIC X(19) VALUE "NUMERO DO DOCUMENTO".
              10 PIC X(19) VALUE "DATA DO DOCUMENTO  ".
              10 PIC X(19) VALUE "HISTORICO PADRAO   ".
              10 PIC X(19) VALUE "FALTA HISTORICO    ".
              10 PIC X(19) VALUE "VALOR              ".
           05 REDEFINES MENSAGENS.
              10 MSG OCCURS 10 PIC X(19).
           05 LANCAMENTO       PIC 9(007)    VALUE 0.
           05 HISTORICO        PIC 9(004)    VALUE 0.
           05 HISTORICO-V      PIC 9(006)    VALUE 0.
       01  FUNCAO                            PIC  X(001).
       01  OBS                               PIC  X(035).

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(037) VALUE
              "REGISTRO CAMPO INVALIDO      CONTEUDO".
       02  LINHA-02.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-SEQUENCIA                 PIC  ZZZ.ZZ9.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CAMPO                     PIC  X(019) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CONTEUDO OCCURS 51        PIC  X(001).

       COPY CB002PCW.
       COPY CWBOXS.
       COPY CWIMPR.
       COPY CWTIME.

       SCREEN SECTION.

       01  CTAC-LIT-CB024PCW.
           05 LINE 08 COLUMN 03 VALUE "Nome do arquivo a importar:".

       01  CTAC-VAR-CB024PCW.
           05 LINE 08 COLUMN 31 PIC X(048) USING LB-LOTE-I.

       01  CTAC-LIT-CB0024B.
           05 LINE 12 COLUMN 03 VALUE "BAC a associar: ".
           05 LINE 12 COLUMN 19 VALUE "S‚rie :".
           05 LINE 13 COLUMN 19 VALUE "N£mero:".

       01  CTAC-VAR-CB0024B AUTO.
           05 LINE 12 COLUMN 28 PIC Z(004) USING CBCOBA-SERIE.
           05 LINE 13 COLUMN 28 PIC Z(004) USING CBCOBA-NUMERO.

       01  CB0020A AUTO.
           05 LINE 12 COLUMN 03 VALUE "Mˆs de referˆncia:".
           05 LINE 12 COLUMN 22 PIC ZZ/ USING MM-REF.
           05 LINE 12 COLUMN 25 PIC 9999 USING AAAA-REF BLANK ZERO.

       01  CTAC-LIT-CB0024C.
           05 LINE 16 COLUMN 10 VALUE "Lidos de".
           05 LINE 17 COLUMN 10 VALUE "gravados em".
           05 LINE 19 COLUMN 10 VALUE "Lidos de".
           05 LINE 20 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0024C.
           05 T-LD-LOTE-I LINE 16 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTE-I.
           05 LINE 16 COLUMN 19 PIC X(050) FROM LB-LOTE-I.
           05 T-GR-LOTEWK LINE 17 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTEWK.
           05 LINE 17 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-LD-LOTEWK LINE 19 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTEWK.
           05 LINE 19 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-GR-CBMVMS LINE 20 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-CBMVMS.
           05 LINE 20 COLUMN 22 PIC X(050) FROM LB-CBMVMS.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-LOTE-I = "10"
                   IF   LER = "S"
                        READ LOTE-I
                   ELSE
                        MOVE "S" TO LER
                   END-IF
                   IF   FS-LOTE-I < "10"
                        ADD 1 TO LD-LOTE-I
                        DISPLAY T-LD-LOTE-I
                        PERFORM 110-CRITICAR THRU 110-99-FIM
                   END-IF
           END-PERFORM

           EVALUATE TRUE
           WHEN LD-LOTE-I = 0
                EXEC COBOLware Send
                     Message "Arquivo a importar vazio"
                END-EXEC
           WHEN ERROS-GERAL NOT = 0
                EXEC COBOLware Send
                     Message
                "Arquivo a importar com erro, examinar listagem"
                END-EXEC
           WHEN OTHER
                EXEC COBOLware Send
                     Message "Arquivo bom para importa‡Æo, efetivar ?"
                     CAPTION(1) " ~Sim"
                     CAPTION(2) " ~NÆo"
                     OPTION-CHAR;RESPOSTA
                END-EXEC
                IF  EFETIVAR
                    CLOSE LOTEWK
                    OPEN INPUT LOTEWK
                    MOVE "S" TO LER
                    PERFORM 130-ABRIR-CBMVMS THRU 130-99-FIM
                    PERFORM UNTIL FS-LOTEWK = "10"
                       IF   LER = "S"
                            READ LOTEWK
                       ELSE
                            MOVE "S" TO LER
                       END-IF
                       IF  (FS-LOTEWK < "10")
                       AND (LOTEWK-TIPO NOT = SPACE)
                            ADD 1 TO LD-LOTEWK
                            DISPLAY T-LD-LOTEWK
                            PERFORM 140-IMPORTAR THRU 140-99-FIM
                       END-IF
                    END-PERFORM
                END-IF
           END-EVALUATE.

       100-99-FIM. EXIT.

       110-CRITICAR.

           INITIALIZE LOTEWK-REG
           MOVE SPACES TO INDICA-DBCR
           MOVE 0      TO ERROS

           IF   CBFOLC-I (01) NOT = 0
                MOVE CBFOLC-I (01) TO P
                MOVE CBFOLC-F (01) TO S
                MOVE LOTE-I-REG (P: S) TO INDICA-DBCR
           END-IF

           IF   INDICA-DBCR = CBFOLC-INDICA-DEBITO
                MOVE "D" TO LOTEWK-TIPO
           ELSE
                IF   INDICA-DBCR = CBFOLC-INDICA-CREDITO
                     MOVE "C" TO LOTEWK-TIPO
                ELSE
                     MOVE 1 TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                END-IF
           END-IF

           IF   CBFOLC-I (02) NOT = 0
                MOVE CBFOLC-I (02) TO P
                MOVE CBFOLC-F (02) TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                                          CWTIME-DATE (7: 2)
                MOVE MM-REF            TO CWTIME-DATE (5: 2)
                MOVE AAAA-REF          TO CWTIME-DATE (1: 4)
                MOVE ABRE-NUMERO       TO CWTIME-DATE
                SET  CWTIME-REVERSED   TO TRUE
                SET  CWTIME-VALIDATE   TO TRUE
                CALL "CWTIME"      USING PARAMETROS-CWTIME
                IF   CWTIME-DATE-FINAL = ZERO
                     MOVE    2          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE ABRE-NUMERO TO LOTEWK-DIA
                END-IF
           ELSE
                MOVE    2          TO ERRO
                PERFORM 120-ERRO THRU 120-99-FIM
           END-IF

           MOVE 0 TO ERRO

           IF   CBFOLC-I (03) NOT = 0
                MOVE CBFOLC-I (03)     TO P
                MOVE CBFOLC-F (03)     TO S
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (4: DGC)
                MOVE ABRE-NUMERO       TO CBPLCO-CONTA
                IF   ABRE-NUMERO NUMERIC
                     READ CBPLCO IGNORE LOCK
                     COMPUTE P = P + S - 1
                     MOVE LOTE-I-REG (P: 1) TO DV
                     IF   (DGC + 1) = S
                          MOVE CBPLCO-CONTA TO CB002PCW-CONTA
                          MOVE "C"          TO CB002PCW-FUNCAO
                          CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                     END-IF
                END-IF
                IF  (FS-CBPLCO NOT < "10")
                OR  ((DGC + 1) NOT = S)
                OR  (CB002PCW-DV NOT = DV)
                OR  (CBPLCO-COD-RED = 0)
                OR  (ABRE-NUMERO NOT NUMERIC)
                     MOVE    3          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE CBPLCO-COD-RED TO LOTEWK-COD-RED
                END-IF
           END-IF

           IF   CBFOLC-I (04) NOT = 0
                MOVE CBFOLC-I (04)     TO P
                MOVE CBFOLC-F (04)     TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                MOVE ABRE-NUMERO       TO CBPLCO-COD-RED
                IF   ABRE-NUMERO NUMERIC
                     READ CBPLCO IGNORE LOCK
                             KEY IS CBPLCO-COD-RED
                END-IF
                IF  (FS-CBPLCO NOT < "10")
                OR ((LOTEWK-COD-RED NOT = 0) AND
                    (LOTEWK-COD-RED NOT = CBPLCO-COD-RED))
                OR  (CBPLCO-COD-RED = 0)
                OR  (ABRE-NUMERO NOT NUMERIC)
                     MOVE    4          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE CBPLCO-COD-RED TO LOTEWK-COD-RED
                END-IF
           END-IF

           IF   LOTEWK-COD-RED = 0
           AND  ERRO = 0
                MOVE    3          TO ERRO
                PERFORM 120-ERRO THRU 120-99-FIM
           END-IF

           MOVE 0 TO ERRO

           IF   CBFOLC-I (05) NOT = 0
                MOVE CBFOLC-I (05) TO P
                MOVE CBFOLC-F (05) TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                IF   ABRE-NUMERO NOT NUMERIC
                     MOVE    5          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE ABRE-NUMERO TO LOTEWK-CENTRO-CUSTO
                                         CBCACC-CODIGO
                     IF   CBCACC-CODIGO NOT = 0
                          READ CBCACC IGNORE LOCK
                          IF   FS-CBCACC = "23"
                               MOVE    5          TO ERRO
                               PERFORM 120-ERRO THRU 120-99-FIM
                          END-IF
                     END-IF
                END-IF
           END-IF

           IF   CBFOLC-I (06) NOT = 0
                MOVE CBFOLC-I (06) TO P
                MOVE CBFOLC-F (06) TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                IF   ABRE-NUMERO NOT NUMERIC
                     MOVE    6          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE ABRE-NUMERO TO LOTEWK-DOCTO
                END-IF
           END-IF

           IF   CBFOLC-I (07) NOT = 0
                MOVE CBFOLC-I (07) TO P
                MOVE CBFOLC-F (07) TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                IF   ABRE-NUMERO NOT = 0
                AND  ABRE-NUMERO < 999999
                     MOVE LB-CBMVMS (1: 2) TO ABRE-NUMERO (11: 2)
                END-IF
                MOVE ABRE-NUMERO         TO CWTIME-DATE
                SET  CWTIME-REVERSED     TO TRUE
                SET  CWTIME-VALIDATE     TO TRUE
                CALL "CWTIME"         USING PARAMETROS-CWTIME
                IF  (ABRE-NUMERO-X NOT = ALL "0")
                AND (CWTIME-DATE-FINAL = ZERO)
                     MOVE    7          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE CWTIME-DATE-FINAL TO LOTEWK-AAAAMMDD-DOCTO
                END-IF
           END-IF

           IF   ERRO = 0
                IF  (LOTEWK-DOCTO = 0)
                AND (LOTEWK-AAAAMMDD-DOCTO NOT = 0)
                     MOVE    6          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     IF  (LOTEWK-DOCTO NOT = 0)
                     AND (LOTEWK-AAAAMMDD-DOCTO = 0)
                          MOVE    7          TO ERRO
                          PERFORM 120-ERRO THRU 120-99-FIM
                     END-IF
                END-IF
           END-IF

           MOVE 0 TO ERRO

           IF   CBFOLC-I (08) NOT = 0
                MOVE CBFOLC-I (08) TO P
                MOVE CBFOLC-F (08) TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                MOVE "00"              TO FS-CBCAHI
                IF   ABRE-NUMERO NUMERIC
                AND  ABRE-NUMERO NOT = 0
                     MOVE ABRE-NUMERO  TO CBCAHI-CODIGO
                     READ CBCAHI IGNORE LOCK
                END-IF
                IF  (ABRE-NUMERO NOT NUMERIC)
                OR  (FS-CBCAHI > "09")
                     MOVE    8          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE ABRE-NUMERO TO LOTEWK-HISTORICO-PADRAO
                END-IF
           END-IF

           IF   CBFOLC-I (10) NOT = 0
                MOVE CBFOLC-I (10)     TO P
                MOVE CBFOLC-F (10)     TO S
                COMPUTE S2 = 18 - S + 1
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (S2: S)
                IF   ABRE-NUMERO NOT NUMERIC
                OR   ABRE-NUMERO = 0
                     MOVE    10         TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     COMPUTE LOTEWK-VALOR = ABRE-NUMERO / 100
                END-IF
           END-IF

           IF   CBFOLC-I (09) NOT = 0
                MOVE CBFOLC-I (09)     TO P
                MOVE CBFOLC-F (09)     TO S
                MOVE LOTE-I-REG (P: S) TO LOTEWK-HISTORICO-VARIAVEL
                IF  (LOTEWK-HISTORICO-VARIAVEL NOT = SPACES)
                AND (LOTEWK-HISTORICO-VARIAVEL NOT = ALL "0")
                     PERFORM UNTIL LER = "N"
                        IF   LOTEWK-REG NOT = SPACES
                        AND  ERROS = 0
                             ADD 1 TO GR-LOTEWK
                             DISPLAY T-GR-LOTEWK
                             WRITE LOTEWK-REG
                             IF   FS-LOTEWK > "09"
                                  PERFORM 900-FINAIS THRU 900-99-FIM
                                  GOBACK
                             END-IF
                        END-IF
                        MOVE SPACES TO LOTEWK-REG
                        READ LOTE-I
                             AT END MOVE "N" TO LER
                             NOT AT END
                                 MOVE LOTE-I-REG (P: S)
                                   TO LOTEWK-HISTORICO-VARIAVEL
                                 MOVE SPACES TO LOTE-I-REG (P: S)
                                 IF   LOTE-I-REG NOT = SPACES
                                      MOVE "N"    TO LER
                                      MOVE LOTEWK-HISTORICO-VARIAVEL
                                        TO LOTE-I-REG (P: S)
                                      MOVE SPACES TO LOTEWK-REG
                                 ELSE
                                      MOVE LOTEWK-HISTORICO-VARIAVEL
                                        TO LOTE-I-REG (P: S)
                                 END-IF
                        END-READ
                     END-PERFORM
                END-IF
           END-IF

           IF  (LOTEWK-REG NOT = SPACES)
           AND (LOTEWK-HISTORICO-VARIAVEL = SPACES OR ALL "0")
           AND (LOTEWK-HISTORICO-PADRAO = 0)
                MOVE    9          TO ERRO
                PERFORM 120-ERRO THRU 120-99-FIM
           END-IF

           IF   LOTEWK-REG NOT = SPACES
           AND  ERROS = 0
                ADD 1 TO GR-LOTEWK
                DISPLAY T-GR-LOTEWK
                WRITE LOTEWK-REG
                IF   FS-LOTEWK > "09"
                     PERFORM 900-FINAIS THRU 900-99-FIM
                     GOBACK
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       120-ERRO.

           ADD  1               TO ERROS
                                   ERROS-GERAL
           MOVE SPACES          TO LINHA-02
           MOVE LD-LOTE-I       TO CLIC-SEQUENCIA
           MOVE MSG (ERRO)      TO CLIC-CAMPO
           MOVE "["             TO CLIC-CONTEUDO (1)
           MOVE 1               TO I
           IF   CBFOLC-I (ERRO) NOT = 0
                MOVE CBFOLC-I (ERRO) TO P
                MOVE CBFOLC-F (ERRO) TO S
                PERFORM S TIMES
                   ADD  1                 TO I
                   MOVE LOTE-I-REG (P: 1) TO CLIC-CONTEUDO (I)
                   ADD  1                 TO P
                END-PERFORM
           ELSE
                MOVE 1 TO P
                PERFORM 24 TIMES
                   ADD  1          TO I
                   MOVE NDF (P: 1) TO CLIC-CONTEUDO (I)
                   ADD  1          TO P
                END-PERFORM
           END-IF
           ADD  1               TO I
           MOVE "]"             TO CLIC-CONTEUDO (I)
           MOVE LINHA-02        TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBFOLC CBCOBA CBMVMS CBPLCO CBHIVA CBPAPC CBCAHI
                      CBCACC CBCOMS CBCOSA CBCOHI LOTE-I LOTEWK
                GOBACK
           END-IF.

       120-99-FIM. EXIT.

       130-ABRIR-CBMVMS.

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
                     DISPLAY ESPACOS LINE 23 COLUMN 3
                     DISPLAY
                     "Gerando referˆncia " LINE 23 COLUMN 3
                     CBCOBA-MM  "/" CBCOBA-AAAA " aguarde..."
                     OPEN I-O CBCOSA
                     PERFORM UNTIL FS-CBCOSA > "09"
                       READ CBCOSA NEXT RECORD IGNORE LOCK
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

           IF  (LOTEWK-DIA              NOT = CBMVMS-DIA)
           OR  (LOTEWK-DOCTO            NOT = CBMVMS-DOCTO)
           OR  (LOTEWK-AAAAMMDD-DOCTO   NOT = CBMVMS-AAAAMMDD-DOCTO)
           OR  (LOTEWK-CENTRO-CUSTO     NOT = CBMVMS-CENTRO-CUSTO)
           OR  (LOTEWK-HISTORICO-PADRAO NOT = CBMVMS-HISTORICO-PADRAO)
           OR  (LOTEWK-VALOR            NOT = CBMVMS-VALOR)
           OR  (LOTEWK-TIPO                 = CBMVMS-TIPO)
           OR  (LOTEWK-COD-RED              = CBMVMS-COD-RED)
           OR  (MESMO-LANCAMENTO            = 2)
                MOVE    ZERO        TO CBMVMS-LANCAMENTO
                                       VEZ-LANCAMENTO
                MOVE    SPACE       TO CBMVMS-TIPO
                READ CBMVMS WITH LOCK
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
                ADD  1                        TO CBMVMS-VALOR
                MOVE CBMVMS-VALOR             TO LANCAMENTO
                REWRITE CBMVMS-REG
                UNLOCK CBMVMS
                MOVE LANCAMENTO               TO CBMVMS-LANCAMENTO
                MOVE 1                        TO MESMO-LANCAMENTO
                MOVE LOTEWK-TIPO              TO CBMVMS-TIPO
                MOVE CBCOBA-SERIE             TO CBMVMS-SERIE
                MOVE CBCOBA-NUMERO            TO CBMVMS-NUMERO
                MOVE LOTEWK-DIA               TO CBMVMS-DIA
                MOVE LOTEWK-COD-RED           TO CBMVMS-COD-RED
                MOVE LOTEWK-DOCTO             TO CBMVMS-DOCTO
                MOVE LOTEWK-AAAAMMDD-DOCTO    TO CBMVMS-AAAAMMDD-DOCTO
                MOVE LOTEWK-CENTRO-CUSTO      TO CBMVMS-CENTRO-CUSTO
                MOVE LOTEWK-HISTORICO-PADRAO  TO CBMVMS-HISTORICO-PADRAO
                MOVE LOTEWK-VALOR             TO CBMVMS-VALOR
                PERFORM 160-SALVA-HIST      THRU 160-99-FIM
                PERFORM 150-CONTROLA-SALDOS THRU 150-99-FIM
           ELSE
                MOVE 2 TO MESMO-LANCAMENTO
                MOVE LOTEWK-TIPO              TO CBMVMS-TIPO
                MOVE LOTEWK-COD-RED           TO CBMVMS-COD-RED
                PERFORM 150-CONTROLA-SALDOS THRU 150-99-FIM
           END-IF.

       140-99-FIM. EXIT.

       150-CONTROLA-SALDOS.

           MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
           READ CBPLCO WITH LOCK
                KEY IS CBPLCO-COD-RED

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

           READ CBCOBA WITH LOCK

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

       160-SALVA-HIST.

           IF   LOTEWK-HISTORICO-VARIAVEL = SPACES
                GO TO 160-99-FIM
           ELSE
                OPEN INPUT CBCOHI
                IF   FS-CBCOHI = "30" OR "35"
                     CLOSE CBCOHI
                     OPEN OUTPUT CBCOHI
                     MOVE 200000 TO CBCOHI-ULTIMO
                     WRITE CBCOHI-REG
                     CLOSE CBCOHI
                     GO TO 160-SALVA-HIST
                ELSE
                     CLOSE CBCOHI
                     PERFORM TEST AFTER UNTIL FS-CBCOHI NOT = '9A'
                             OPEN I-O CBCOHI
                     END-PERFORM
                     IF   FS-CBCOHI > '09'
                          STOP RUN
                     END-IF
                END-IF
                READ CBCOHI
                ADD  1             TO CBCOHI-ULTIMO
                MOVE CBCOHI-ULTIMO TO HISTORICO-V
                                      CBMVMS-HISTORICO-VARIAVEL
                REWRITE CBCOHI-REG
                CLOSE CBCOHI
           END-IF

           MOVE 0 TO Y

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                                            OR LER = "N"
              IF   LOTEWK-HISTORICO-VARIAVEL NOT = SPACES
                   COMPUTE CBHIVA-TIPO = HISTORICO-V / 100000
                   MOVE HISTORICO-V               TO CBHIVA-CODIGO
                   ADD  1                         TO Y
                   MOVE Y                         TO CBHIVA-VARIAVEL
                   MOVE LOTEWK-HISTORICO-VARIAVEL TO CBHIVA-DESCRICAO
                   WRITE CBHIVA-REG
              END-IF
              READ LOTEWK
                   AT END
                      MOVE "N" TO LER
                   NOT AT END
                   IF   LOTEWK-TIPO NOT = SPACE
                        MOVE "N" TO LER
                   END-IF
              END-READ
           END-PERFORM.

       160-99-FIM. EXIT.

       COPY CB021PCW.

       800-INICIAIS.

           OPEN OUTPUT LOTEWK
           DISPLAY CTAC-LIT-CB024PCW
           PERFORM TEST AFTER UNTIL FS-LOTE-I = "00"
                   DISPLAY "<Esc>-Fim" LINE 23 COLUMN 03
                   CLOSE LOTE-I
                   ACCEPT CTAC-VAR-CB024PCW
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   ESC
                        GOBACK
                   END-IF
                   OPEN INPUT LOTE-I
                   IF   FS-LOTE-I > "09"
                        CLOSE LOTE-I
                        MOVE SPACES TO MSG-NE
                        STRING "NÆo existe" DELIMITED BY SIZE
                               LB-LOTE-I    DELIMITED SPACE
                           INTO MSG-NE
                        EXEC COBOLware Send Message MSG-NE END-EXEC
                   END-IF
           END-PERFORM

           MOVE "CRITICA DE IMPORTACAO" TO CWIMPR-TITLE
           MOVE "DO ARQUIVO"            TO CWIMPR-SUB-TITLE
           MOVE LINHA-01                TO CWIMPR-HEADER-1
           MOVE 2                       TO CWIMPR-FORM-TYPE
           MOVE LB-LOTE-I               TO CWIMPR-SUB-TITLE (12: )
           MOVE "CB0024A"               TO CWIMPR-REPORT

           OPEN INPUT CBFOLC
           IF   FS-CBFOLC > "09"
                GOBACK
           END-IF

           OPEN INPUT CBPAPC
           IF   FS-CBPAPC > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           ELSE
                READ CBPAPC IGNORE LOCK
                COMPUTE DGC = CBPAPC-DG1
                            + CBPAPC-DG2
                            + CBPAPC-DG3
                            + CBPAPC-DG4
                            + CBPAPC-DG5
                            + CBPAPC-DG6
                            + CBPAPC-DG7
                            + CBPAPC-DG8
                            + CBPAPC-DG9
                CLOSE CBPAPC.

           OPEN I-O CBCOBA
           IF   FS-CBCOBA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN I-O CBPLCO
           IF   FS-CBPLCO > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN I-O CBHIVA
           IF   FS-CBHIVA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBCAHI
           IF   FS-CBCAHI > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBCACC
           IF   FS-CBCACC > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           MOVE 10                     TO CWBOXS-LINE
           MOVE 04                     TO CWBOXS-COLUMN
           MOVE "Formatos dispon¡veis" TO CWBOXS-TITLE
           MOVE X"FF"                  TO CWBOXS-TITLE (9: 1)
           PERFORM TEST AFTER UNTIL CWBOXS-OPTION < 9
                   PERFORM 810-MONTA-PAGINA THRU 810-99-FIM
                   CALL "CWBOXS" USING PARAMETROS-CWBOXS
                   IF   CWBOXS-ARROW = ">"
                   AND  LIMITE = 9
                        MOVE 9 TO CWBOXS-OPTION
                   END-IF
                   IF   (CWBOXS-OPTION = 1 OR CWBOXS-ARROW = "<")
                   AND  PG > 1
                        SUBTRACT 1 FROM PG
                        MOVE PONTEIRO (PG) TO CBFOLC-FORMATO
                        START CBFOLC KEY NOT LESS CBFOLC-CHAVE
                        SUBTRACT 1 FROM PG
                        MOVE 9 TO CWBOXS-OPTION
                   END-IF
                   IF   CWBOXS-OPTION = 0
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
           END-PERFORM

           MOVE CWBOXS-TEXT   (CWBOXS-OPTION) (2: ) TO CBFOLC-CHAVE
           READ CBFOLC

           DISPLAY "Formato: " LINE 10 COLUMN 03
                   CBFOLC-FORMATO " - " CBFOLC-COMENTARIO

           DISPLAY CTAC-LIT-CB0024B

           PERFORM TEST AFTER UNTIL ESC
                            OR FS-CBCOBA = "00"
              ACCEPT CTAC-VAR-CB0024B
              ACCEPT TECLA FROM ESCAPE KEY
              IF   ESC
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
              ELSE
                   READ CBCOBA
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

           DISPLAY CTAC-LIT-CB0024C
                   CTAC-VAR-CB0024C.

       800-99-FIM. EXIT.

       810-MONTA-PAGINA.

           ADD  1      TO PG
           MOVE 0      TO LIMITE
           MOVE SPACES TO CWBOXS-ITENS

           IF   PG > 1
                ADD  1                    TO LIMITE
                MOVE " Op‡äes anteriores" TO CWBOXS-TEXT   (LIMITE)
           END-IF

           PERFORM TEST AFTER UNTIL LIMITE = 8
                       OR FS-CBFOLC > "09"
              READ CBFOLC NEXT RECORD IGNORE LOCK
              IF   FS-CBFOLC < "10"
                   ADD 1 TO LIMITE
                   IF   PONTEIRO (PG) = SPACES
                        MOVE CBFOLC-FORMATO TO PONTEIRO (PG)
                   END-IF
                   MOVE CBFOLC-FORMATO    TO CWBOXS-TEXT
                                             (LIMITE) (2: )
                   IF   CBFOLC-COMENTARIO NOT = SPACES
                        MOVE " - "             TO CWBOXS-TEXT
                                                  (LIMITE) (9: 3)
                        MOVE CBFOLC-COMENTARIO TO CWBOXS-TEXT
                                                  (LIMITE) (12: )
                   END-IF
              END-IF
           END-PERFORM

           IF   LIMITE = 8
                READ CBFOLC NEXT RECORD IGNORE LOCK
                IF   FS-CBFOLC < "10"
                     ADD  1              TO LIMITE
                     MOVE " Mais op‡äes" TO CWBOXS-TEXT   (LIMITE)
                     READ CBFOLC PREVIOUS RECORD IGNORE LOCK
                END-IF
                MOVE 9 TO CWBOXS-OPTION
           ELSE
                MOVE 1 TO CWBOXS-OPTION
           END-IF.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOLC CBCOBA CBPLCO LOTEWK
                 CBMVMS CBHIVA LOTE-I CBCAHI CBCACC

           IF   ERROS-GERAL NOT = 0
                MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
           END-IF

           DELETE FILE LOTEWK.

       900-99-FIM. EXIT.

       END PROGRAM CB024PCW.
