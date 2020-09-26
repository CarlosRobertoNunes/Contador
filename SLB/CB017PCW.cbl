       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB017PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Diario                                       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBCAHISL.
       COPY CBHIVASL.
       COPY CBPLCOSL.
       COPY CBMVMSSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCAHIFD.
       COPY CBHIVAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 OBS-5                    PIC  X(039) VALUE SPACES.
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG                  PIC  9(001) VALUE 0.
           05 DESCRICAO                PIC  X(030) VALUE SPACES.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 CR-MOVIMENTO      COMP-3 PIC  9(012)V99 VALUE 0.
           05 DB-MOVIMENTO      COMP-3 PIC  9(012)V99 VALUE 0.
           05 LANCAMENTO-A             PIC  9(007) VALUE 0.
           05 NOTACAO                  PIC  9(001) VALUE 0.
           05 COM-DESCRICAO            PIC  9(001) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 ws-OPTION                PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 HISTORICO-VARIAVEL       PIC  9(006) VALUE 0.
           05 TECLA                    PIC  9(002) VALUE 0.
              COPY CWKEYS.
           05 BAC                      PIC  9(008) VALUE 0.
           05 LD-CBMVMS         COMP-3 PIC  9(006) VALUE 0.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE 0.
           05 DATA-LANCAMENTO          PIC  9(008) VALUE 0.
           05 REDEFINES DATA-LANCAMENTO.
              10 MES-LANCAMENTO        PIC 9(006).
              10 REDEFINES MES-LANCAMENTO.
                 15 LANCAMENTO-AAAA    PIC  9(004).
                 15 LANCAMENTO-MM      PIC  9(002).
              10 LANCAMENTO-DD         PIC  9(002).
           05 INICIO                   PIC  9(008) VALUE ZEROS.
           05 REDEFINES INICIO.
              10 INICIO-SE             PIC  9(002).
              10 INICIO-AA             PIC  9(002).
              10 INICIO-MM             PIC  9(002).
              10 INICIO-DD             PIC  9(002).
           05 REDEFINES INICIO.
              10 INICIO-DIA-AC         PIC  9(002).
              10 INICIO-MES-AC         PIC  9(002).
              10 INICIO-ANO-AC         PIC  9(004).
           05 FIM                      PIC  9(008) VALUE ZEROS.
           05 REDEFINES FIM.
              10 MES-FIM               PIC 9(006).
              10 REDEFINES MES-FIM.
                 15 FIM-SE             PIC  9(002).
                 15 FIM-AA             PIC  9(002).
                 15 FIM-MM             PIC  9(002).
              10 FIM-DD                PIC  9(002).
           05 REDEFINES FIM.
              10 FIM-DIA-AC            PIC  9(002).
              10 FIM-MES-AC            PIC  9(002).
              10 FIM-ANO-AC            PIC  9(004).
           05 TESTE-INICIO             PIC  9(006).
           05 TESTE-FIM                PIC  9(006).
           05 INVERSOR                 PIC  9(008).
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC 9(004).
                 15 MM-REF             PIC 9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
              10 REDEFINES LB-CBMVMS.
                 15 LB-CB017-RED       PIC  X(010).
                 15 FILLER             PIC  X(040).
           05 MENSAGENS-DE-ERRO.
              10 MSG01 PIC X(30) VALUE "Data inicial inv lida         ".
              10 MSG02 PIC X(30) VALUE "Data final inv lida           ".
              10 MSG03 PIC X(30) VALUE "Centro de custo inexistente   ".

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(007) VALUE
              "DIARIO ".
           05 CLIC-OBS-1                     PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(004) VALUE " DE ".
           05 CLIC-INICIO                    PIC  99/99/9999.
           05 FILLER                         PIC  X(003) VALUE " A ".
           05 CLIC-FIM                       PIC  99/99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-2                     PIC  X(010) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-3                     PIC  X(002) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS-4                     PIC  X(014) VALUE SPACES.
       02  LINHA-02.
           05 FILLER                         PIC  X(055) VALUE
              "LANCT DIA    CONTA                          HISTORICO  ".
           05 FILLER                         PIC  X(038) VALUE
              "                      DOCTO    DATA   ".
           05 CC-FLAG-TXT                    PIC  X(003) VALUE "C/C".
           05 FILLER                         PIC  X(029) VALUE
              "           A DEBITO        A ".
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(005) VALUE "EDITO".
       02  LINHA-03.
           05 CLIC-LANCAMENTO                PIC  ZZZZZ9.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DIA                       PIC  99/99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CONTA-ED                  PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-HISTORICO                 PIC  X(030) VALUE SPACES.
           05 CLIC-DOCTO                     PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DDMMAAAA-DOCTO            PIC  99/99/9999 BLANK ZERO.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CENTRO-CUSTO              PIC  ZZZZ.
           05 CLIC-VALOR-DB VALUE ZERO       PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
           05 CLIC-VALOR-CR VALUE ZERO       PIC  ZZ.ZZZ.ZZZ.ZZ9,99.

       COPY CWIMPR.
       COPY CB002PCW.
       COPY CB014PCW.
       COPY CWBOXF.
       COPY CWTIME.

       SCREEN SECTION.

       01  CB0017A.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 10 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBMVMS LINE 08 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBMVMS.
           05 T-GR-PRNTER LINE 10 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       01  CB0017B AUTO.
           05 LINE 15 COLUMN 03 VALUE "Per¡odo desejado de ".
           05 LINE 15 COLUMN 23 VALUE "00/00/0000 a 00/00/0000".
           05 LINE 15 COLUMN 23 PIC 99/  BLANK ZERO USING INICIO-DIA-AC.
           05 LINE 15 COLUMN 26 PIC 99/  BLANK ZERO USING INICIO-MES-AC.
           05 LINE 15 COLUMN 29 PIC 9999 BLANK ZERO USING INICIO-ANO-AC.
           05 LINE 15 COLUMN 36 PIC 99/  BLANK ZERO USING FIM-DIA-AC.
           05 LINE 15 COLUMN 39 PIC 99/  BLANK ZERO USING FIM-MES-AC.
           05 LINE 15 COLUMN 42 PIC 9999 BLANK ZERO USING FIM-ANO-AC.

       01  TELA-CC.
           05 LINE 16 COLUMN 03 VALUE "Centro de custo:".
           05 LINE 16 COLUMN 20 PIC ZZZZ USING CC.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL MES-LANCAMENTO GREATER MES-FIM
                   MOVE LANCAMENTO-AAAA    TO AAAA-REF
                   MOVE LANCAMENTO-MM      TO MM-REF
                   CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                   OPEN INPUT CBMVMS
                   MOVE 0                  TO LD-CBMVMS
                   MOVE LOW-VALUES         TO CBMVMS-CHAVE
                   MOVE LANCAMENTO-DD      TO CBMVMS-DIA
                   START CBMVMS KEY NOT < CBMVMS-DIA-CHAVE
                   PERFORM UNTIL FS-CBMVMS > "09"
                      PERFORM TEST AFTER
                              UNTIL FS-CBMVMS NOT = "9D"
                              READ CBMVMS NEXT RECORD IGNORE LOCK
                              IF  (CC NOT = 0)
                              AND (CC NOT = CBMVMS-CENTRO-CUSTO)
                              AND FS-CBMVMS < "10"
                                 MOVE "9D" TO FS-CBMVMS
                              END-IF
                      END-PERFORM
                      IF   FS-CBMVMS < "10"
                           ADD  1                TO LD-CBMVMS
                           IF   LD-CBMVMS = 1
                                DISPLAY LB-CB017-RED LINE 08 COLUMN 14
                           END-IF
                           DISPLAY                T-LD-CBMVMS
                           MOVE CBMVMS-DIA TO LANCAMENTO-DD
                           IF   DATA-LANCAMENTO NOT > FIM
                                PERFORM 110-LISTA THRU 110-99-FIM
                           END-IF
                      END-IF
                   END-PERFORM
                   CLOSE CBMVMS
                   MOVE 1 TO LANCAMENTO-DD
                   ADD  1 TO LANCAMENTO-MM
                   IF   LANCAMENTO-MM = 13
                        MOVE 1 TO LANCAMENTO-MM
                        ADD  1 TO LANCAMENTO-AAAA
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       110-LISTA.

           ADD  1                         TO GR-PRNTER
           DISPLAY                         T-GR-PRNTER
           MOVE CBMVMS-LANCAMENTO         TO CLIC-LANCAMENTO
           COMPUTE CLIC-DIA =             MM-REF + (CBMVMS-DIA * 100)
           MOVE CBMVMS-DOCTO              TO CLIC-DOCTO
           MOVE CBMVMS-AAAAMMDD-DOCTO     TO CWTIME-DATE
           SET  CWTIME-REVERSED          TO TRUE
           SET  CWTIME-REVERSE           TO TRUE
           CALL "CWTIME"              USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL        TO CLIC-DDMMAAAA-DOCTO
           IF   CC = 0
                MOVE CBMVMS-CENTRO-CUSTO  TO CLIC-CENTRO-CUSTO
           ELSE
                MOVE 0                    TO CLIC-CENTRO-CUSTO
           END-IF
           MOVE CBMVMS-HISTORICO-VARIAVEL TO HISTORICO-VARIAVEL

           IF   NOTACAO > 1
                MOVE CBMVMS-VALOR TO CB014PCW-VALOR
                MOVE AAAA-REF     TO CB014PCW-REFERENCIA-AAAA
                MOVE MM-REF       TO CB014PCW-REFERENCIA-MM
                MOVE CBMVMS-DIA   TO CB014PCW-REFERENCIA-DD
                CALL "CB014PCW"  USING PARAMETROS-CB014PCW
                EVALUATE NOTACAO
                         WHEN 2
                              MOVE CB014PCW-CONVERTIDO TO CBMVMS-VALOR
                         WHEN 3
                              MOVE CB014PCW-CORRIGIDO  TO CBMVMS-VALOR
                END-EVALUATE
                IF   VEZ = 1
                     MOVE 2                   TO VEZ
                     MOVE CB014PCW-MOEDA-NOME TO CLIC-OBS-4
                     EVALUATE NOTACAO
                              WHEN 2
                                   MOVE "EM" TO CLIC-OBS-2
                              WHEN 3
                                   MOVE "CORRIGIDO" TO CLIC-OBS-2
                                   MOVE "P/" TO CLIC-OBS-3
                     END-EVALUATE.

           IF   VEZ = 1
                MOVE LINHA-01    TO CWIMPR-TITLE
                MOVE LINHA-02    TO CWIMPR-HEADER-1
                MOVE 2           TO VEZ
           END-IF

           MOVE CBMVMS-COD-RED  TO CBPLCO-COD-RED
           READ CBPLCO IGNORE LOCK
                   KEY IS CBPLCO-COD-RED
           IF   FS-CBPLCO > "09"
                MOVE "CONTA PERDIDA" TO CLIC-CONTA-ED
           ELSE
                IF   COM-DESCRICAO = 1
                     MOVE CBPLCO-DESCRICAO TO DESCRICAO
                END-IF
                MOVE "C"               TO CB002PCW-FUNCAO
                MOVE CBPLCO-CONTA      TO CB002PCW-CONTA
                CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                MOVE "E"               TO CB002PCW-FUNCAO
                CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                MOVE CB002PCW-CONTA-ED TO CLIC-CONTA-ED
           END-IF

           IF   CBMVMS-TIPO = "C"
                ADD  CBMVMS-VALOR    TO CR-MOVIMENTO
                MOVE CBMVMS-VALOR    TO CLIC-VALOR-CR
           ELSE
                ADD  CBMVMS-VALOR    TO DB-MOVIMENTO
                MOVE CBMVMS-VALOR    TO CLIC-VALOR-DB
           END-IF

           IF   LANCAMENTO-A = CBMVMS-LANCAMENTO
                MOVE "IDEM AO ANTERIOR" TO CLIC-HISTORICO
                PERFORM 120-IMPRIME-LINHA-03 THRU 120-99-FIM
                GO TO 110-50-DESCRICAO
           ELSE
                MOVE CBMVMS-LANCAMENTO TO LANCAMENTO-A
           END-IF

           IF   CBMVMS-HISTORICO-PADRAO NOT = 0
                MOVE CBMVMS-HISTORICO-PADRAO TO CBCAHI-CODIGO
                READ CBCAHI IGNORE LOCK
                IF   FS-CBCAHI < "10"
                     MOVE CBCAHI-DESCRICAO TO CLIC-HISTORICO
                ELSE
                     MOVE "NAO CADASTRADO" TO CLIC-HISTORICO
                END-IF
                PERFORM 120-IMPRIME-LINHA-03 THRU 120-99-FIM
                MOVE "00"   TO FS-CBHIVA
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
                                                 OR FS-CBHIVA > "09"
                        MOVE 1             TO CBHIVA-TIPO
                        MOVE CBCAHI-CODIGO TO CBHIVA-CODIGO
                        MOVE I             TO CBHIVA-VARIAVEL
                        READ CBHIVA IGNORE LOCK
                        IF   FS-CBHIVA < "09"
                             MOVE CBHIVA-DESCRICAO TO CLIC-HISTORICO
                             PERFORM 120-IMPRIME-LINHA-03
                                THRU 120-99-FIM
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
                        MOVE CBHIVA-DESCRICAO TO CLIC-HISTORICO
                        PERFORM 120-IMPRIME-LINHA-03 THRU 120-99-FIM
                   END-IF
           END-PERFORM.

       110-50-DESCRICAO.

           IF   CLIC-CONTA-ED NOT = SPACES
                MOVE LINHA-03         TO CWIMPR-DETAIL
                PERFORM 125-CWIMPR THRU 125-99-FIM
           END-IF.

       110-99-FIM. EXIT.

       120-IMPRIME-LINHA-03.

           MOVE LINHA-03         TO CWIMPR-DETAIL
           PERFORM 125-CWIMPR THRU 125-99-FIM
           MOVE SPACES           TO LINHA-03
           MOVE DESCRICAO        TO CLIC-CONTA-ED
           MOVE SPACES           TO DESCRICAO.

       120-99-FIM. EXIT.

       125-CWIMPR.

           CALL "CWIMPR" USING PARAMETROS-CWIMPR

           IF   CWIMPR-END-PRINT
                CLOSE CBCAHI
                      CBHIVA
                      CBPLCO
                      CBMVMS
                GOBACK
           END-IF.

       125-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCACC
           IF   FS-CBCACC = "30" OR 35
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
           MOVE SPACES      TO LINHA-03

           OPEN INPUT CBPLCO
           IF   FS-CBPLCO > "09"
                CLOSE CBCACC
                GOBACK
           END-IF

           OPEN INPUT CBCAHI
           IF   FS-CBHIVA > "09"
                CLOSE CBPLCO CBCACC
                GOBACK
           END-IF

           OPEN INPUT CBHIVA
           IF   FS-CBHIVA > "09"
                CLOSE CBPLCO CBCAHI CBCACC
                GOBACK
           END-IF

           DISPLAY CB0017B
           IF   CC-FLAG = 1
                DISPLAY TELA-CC
           END-IF

           PERFORM TEST AFTER UNTIL ESC
                                 OR (TESTE-INICIO NOT = ZEROS
                                    AND TESTE-FIM NOT = ZEROS)
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT CB0017B
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE   "CB017PCW.H01"
                                LINE   16 COLUMN 22
                                HEIGHT 06 WIDTH  40
                           END-EXEC
                      END-IF
                   END-PERFORM
                   IF   NOT ESC
                        MOVE INICIO        TO CLIC-INICIO
                        MOVE FIM           TO CLIC-FIM
                        MOVE INICIO (1: 2) TO TESTE-INICIO (1: 2)
                        MOVE INICIO (3: 2) TO TESTE-INICIO (3: 2)
                        MOVE INICIO (7: 2) TO TESTE-INICIO (5: 2)
                        MOVE FIM    (1: 2) TO TESTE-FIM    (1: 2)
                        MOVE FIM    (3: 2) TO TESTE-FIM    (3: 2)
                        MOVE FIM    (7: 2) TO TESTE-FIM    (5: 2)
                        CALL "GRVDAT" USING TESTE-INICIO
                        IF   TESTE-INICIO = ZEROS
                             EXEC COBOLware Send Message MSG01 END-EXEC
                        ELSE
                             CALL "GRVDAT" USING TESTE-FIM
                             IF   TESTE-FIM = ZEROS
                                  EXEC COBOLware Send
                                       Message MSG02
                                  END-EXEC
                             END-IF
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
                          EXEC COBOLware Send Message MSG03 END-EXEC
                     ELSE
                          STRING "C/C: " CC " "
                                 CBCACC-DESCRICAO
                                 DELIMITED BY SIZE
                                 INTO OBS-5
                          DISPLAY OBS-5
                             LINE 16 COLUMN 03 WITH SIZE 37
                          MOVE OBS-5 TO CWIMPR-SUB-TITLE
                          MOVE SPACES TO CC-FLAG-TXT
                     END-IF
                ELSE
                     DISPLAY "Geral" LINE 16 COLUMN 20
                END-IF
                END-PERFORM
           END-IF

           IF   ESC
                CLOSE CBPLCO CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           MOVE SPACES TO RODAPE DISPLAY RODAPE LINE 23 COLUMN 03
           CALL "GRIDAT"        USING TESTE-INICIO
           CALL "GRIDAT"        USING TESTE-FIM
           MOVE INICIO (5: 2)      TO INICIO (1: 2)
           MOVE FIM    (5: 2)      TO FIM    (1: 2)
           MOVE TESTE-INICIO       TO INICIO (3: 6)
           MOVE TESTE-FIM          TO FIM    (3: 6)
           MOVE INICIO             TO DATA-LANCAMENTO

           EXEC COBOLware BoxSelect NoErase
                LINE 08 COLUMN 54
                TITLE      "Nota‡Æo"
                CAPTION(1) " em ~Reais"
                CAPTION(2) " ~Convertido"
                CAPTION(3) " corri~Gido"
                OPTION 1;ws-OPTION
           END-EXEC
           MOVE ws-OPTION      TO NOTACAO

           IF   NOTACAO = 0
                CLOSE CBPLCO CBMVMS CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           EXEC COBOLware BoxSelect NoErase
                LINE 15 COLUMN 51
                TITLE "C/Descri‡Æo_da_conta ?"
                CAPTION(1) " ~Sim "
                CAPTION(2) " ~NÆo "
                OPTION 2;ws-OPTION
           END-EXEC
           MOVE ws-OPTION      TO COM-DESCRICAO

           IF   COM-DESCRICAO = 0
                CLOSE CBPLCO CBMVMS CBCAHI CBHIVA CBCACC
                GOBACK
           END-IF

           MOVE "CB017PA" TO CWIMPR-REPORT
           DISPLAY CB0017A
           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW".

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE CR-MOVIMENTO TO CLIC-VALOR-CR
           MOVE DB-MOVIMENTO TO CLIC-VALOR-DB
           MOVE "TOTAIS    " TO CLIC-HISTORICO
           MOVE SPACES       TO CWIMPR-DETAIL
           PERFORM 125-CWIMPR THRU 125-99-FIM
           MOVE LINHA-03     TO CWIMPR-DETAIL
           PERFORM 125-CWIMPR THRU 125-99-FIM
           CLOSE CBMVMS CBCAHI CBHIVA CBPLCO CBCACC

           IF   NOTACAO > 1
                CANCEL "CB014PCW".

           CANCEL "CB002PCW".

           MOVE "CLOSE"      TO CWIMPR-TIME-REPORT
           PERFORM 125-CWIMPR THRU 125-99-FIM.

       900-99-FIM. EXIT.

       END PROGRAM CB017PCW.
