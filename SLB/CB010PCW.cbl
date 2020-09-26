       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB010PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Listagem dos lancamentos de um BAC           *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOBASL.
       COPY CBHIVASL.
       COPY CBMVMSSL.
       COPY CBPLCOSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOBAFD.
       COPY CBHIVAFD.
       COPY CBMVMSFD.
       COPY CBPLCOFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 COD-RED-CALL             PIC  9(005) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 CR-MOVIMENTO      COMP-3 PIC  9(012)V99 VALUE 0.
           05 DB-MOVIMENTO      COMP-3 PIC  9(012)V99 VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 LANCAMENTO-A             PIC  9(007) VALUE 0.
           05 HISTORICO-VARIAVEL       PIC  9(006) VALUE 0.
           05 ERRO-BAC                 PIC  X(030) VALUE
              "BAC inexistente".
           05 ERRO-MOVIMENTO           PIC  X(030) VALUE
              "BAC sem lan‡amentos".
           05 TECLA                    PIC  9(002) VALUE 0.
              COPY CWKEYS.
           05 BAC                      PIC  9(008) VALUE 0.
           05 REFERENCIA               PIC  9(006) VALUE 0.
           05 REDEFINES REFERENCIA.
              10 MM                    PIC  9(002).
              10 AAAA                  PIC  9(004).
           05 LD-CBMVMS         COMP-3 PIC  9(006) VALUE 0.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE 0.
           05 ER-CBCOBA.
              10 FS-CBCOBA              PIC  X(002) VALUE "00".
              10 LB-CBCOBA              PIC  X(050) VALUE "CBCOBA".
           05 ER-CBMVMS.
              10 FS-CBMVMS              PIC  X(002) VALUE "00".
              10 LB-CBMVMS                          VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 ER-CBPLCO.
              10 FS-CBPLCO              PIC  X(002) VALUE "00".
              10 LB-CBPLCO              PIC  X(050) VALUE "CBPLCO".
           05 ER-CBHIVA.
              10 FS-CBHIVA              PIC  X(002) VALUE "00".
              10 LB-CBHIVA              PIC  X(050) VALUE "CBHIVA".

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(008) VALUE
              "BAC ".
           05 CLIC-BAC                       PIC  9999/9999.
           05 FILLER                         PIC  X(005) VALUE " REF.".
           05 CLIC-REFERENCIA                PIC  99/9999.
       02  LINHA-02.
           05 FILLER                         PIC  X(017) VALUE
              "LANCAMENTO DIA CO".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(003) VALUE ".RE".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(030) VALUE
              ". CONTA A DEBITO            CO".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(003) VALUE ".RE".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(010) VALUE
              ". CONTA A ".
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(039) VALUE
              "EDITO               DOCTO    DATA     C".
           05 FILLER                         PIC  X(002) VALUE "/C".
           05 FILLER                         PIC  X(022) VALUE
              " HIST            VALOR".
       02  LINHA-03.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 CLIC-LANCAMENTO                PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 CLIC-DIA                       PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 CLIC-COD-RED-DB                PIC  Z(005) VALUE ZEROS.
           05 CLIC-COD-RED-DB-TRACO          PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED-DB-DV             PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CONTA-ED-DB               PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED-CR                PIC  Z(005) VALUE ZEROS.
           05 CLIC-COD-RED-CR-TRACO          PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED-CR-DV             PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CONTA-ED-CR               PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DOCTO                     PIC  Z(008) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DDMMAAAA-DOCTO            PIC  99/99/9999 BLANK ZERO.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CENTRO-CUSTO              PIC  Z(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-HISTORICO-PADRAO          PIC  Z(004) VALUE ZEROS.
           05 CLIC-VALOR                     PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
       02  LINHA-04.
           05 FILLER                         PIC  X(095) VALUE SPACES.
           05 CLIC-H-VARIAVEL                PIC  X(030) VALUE SPACES.
       02  LINHA-05.
           05 FILLER                         PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(018) VALUE ALL "-".
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(018) VALUE ALL "-".
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(018) VALUE ALL "-".
       02  LINHA-06.
           05 FILLER                         PIC  X(029) VALUE SPACES.
           05 FILLER                         PIC  X(047) VALUE
              "PREVISTOS         EFETIVADOS          MOVIMENTO".
       02  LINHA-07.
           05 FILLER                         PIC  X(020) VALUE SPACES.
           05 FILLER                         PIC  X(018) VALUE ALL "-".
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(018) VALUE ALL "-".
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(018) VALUE ALL "-".
       02  LINHA-08.
           05 FILLER                         PIC  X(008) VALUE SPACES.
           05 FILLER                         PIC  X(022) VALUE
              "LANCAMENTOS           ".
           05 CLIC-LC-PREVISTOS              PIC  ZZZZZZZ9.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 CLIC-LC-EFETIVOS               PIC  ZZZZZZZ9.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 CLIC-LC-MOVIMENTO              PIC  ZZZZZZZ9.
       02  LINHA-09.
           05 FILLER                         PIC  X(012) VALUE SPACES.
           05 FILLER                         PIC  X(008) VALUE
              "DEBITOS ".
           05 CLIC-DB-PREVISTOS              PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DB-EFETIVOS               PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DB-MOVIMENTO              PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
       02  LINHA-10.
           05 FILLER                         PIC  X(011) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(007) VALUE
              "EDITOS ".
           05 CLIC-CR-PREVISTOS              PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CR-EFETIVOS               PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CR-MOVIMENTO              PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
       02  LINHA-11.
           05 FILLER                         PIC  X(018) VALUE SPACES.
           05 FILLER                         PIC  X(055) VALUE
              "ATENCAO: ENCONTRADA DIFERENCA ENTRE EFETIVADOS E MOVIME".
           05 FILLER                         PIC  X(003) VALUE "NTO".
       02  LINHA-12.
           05 FILLER                         PIC  X(027) VALUE SPACES.
           05 FILLER                         PIC  X(039) VALUE
              "ISSO SIGNIFICA QUE O AQUIVO MOVIMENTO, ".
           05 CLIC-LB-CBMVMS                  PIC  X(010) VALUE SPACES.
       02  LINHA-13.
           05 FILLER                         PIC  X(027) VALUE SPACES.
           05 FILLER                         PIC  X(049) VALUE
              "SOFREU AVARIAS PROVOCADAS POR ELEMENTOS ESTRANHOS".
       02  LINHA-14.
           05 FILLER                         PIC  X(027) VALUE SPACES.
           05 FILLER                         PIC  X(049) VALUE
              "AO SISTEMA,RECOMENDA-SE O RETORNO AO BACK-UP MAIS".
       02  LINHA-15.
           05 FILLER                         PIC  X(027) VALUE SPACES.
           05 FILLER                         PIC  X(039) VALUE
              "RECENTE E CONFIAVEL,OU TENTAR UM REBUIL".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(001) VALUE ".".

       COPY CB002PCW.
       COPY CWIMPR.
       COPY CWTIME.

       SCREEN SECTION.

       01  CB0010A AUTO.
           05 LINE 08 COLUMN 03 VALUE "BAC a listar: ".
           05 LINE 08 COLUMN 17 PIC ZZZZ/ USING CBCOBA-SERIE.
           05 LINE 08 COLUMN 22 PIC ZZZZ USING CBCOBA-NUMERO.
           05 LINE 10 COLUMN 03 VALUE "Lidos".
           05 LINE 10 COLUMN 09 PIC X(015) FROM LB-CBCOBA.
           05 LINE 12 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBMVMS LINE 10 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBMVMS.
           05 T-GR-PRNTER LINE 12 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-CBMVMS > "09"
              READ CBMVMS NEXT RECORD IGNORE LOCK
              IF   CBMVMS-SERIE  NOT = CBCOBA-SERIE
              OR   CBMVMS-NUMERO NOT = CBCOBA-NUMERO
                   MOVE "10" TO FS-CBMVMS
              END-IF
              IF   FS-CBMVMS < "10"
                   ADD  1                TO LD-CBMVMS
                   DISPLAY                T-LD-CBMVMS
                   PERFORM 110-LISTA THRU 110-99-FIM
              ELSE
                   IF   LINHA-03 NOT = SPACES
                        ADD  1           TO GR-PRNTER
                        DISPLAY           T-GR-PRNTER
                        MOVE LINHA-03    TO CWIMPR-DETAIL
                        CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        PERFORM 910-SAI-CWIMPR
                   END-IF
              END-IF
           END-PERFORM.

           PERFORM 120-LISTA-HISTORICO-VARIAVEL THRU 120-99-FIM.

       100-99-FIM. EXIT.

       110-LISTA.

           IF   VEZ = 1
                MOVE 2                 TO VEZ
                MOVE CBMVMS-LANCAMENTO TO LANCAMENTO-A
           ELSE
                IF   CBMVMS-LANCAMENTO NOT = LANCAMENTO-A
                     ADD  1                 TO GR-PRNTER
                     DISPLAY                   T-GR-PRNTER
                     MOVE CBMVMS-LANCAMENTO TO LANCAMENTO-A
                     MOVE LINHA-03          TO CWIMPR-DETAIL
                     MOVE SPACES            TO LINHA-03
                     CALL "CWIMPR"       USING PARAMETROS-CWIMPR
                     PERFORM 910-SAI-CWIMPR
                     PERFORM 120-LISTA-HISTORICO-VARIAVEL
                        THRU 120-99-FIM
                     MOVE ZERO TO HISTORICO-VARIAVEL.

           MOVE CBMVMS-LANCAMENTO         TO CLIC-LANCAMENTO
           MOVE CBMVMS-DIA                TO CLIC-DIA
           MOVE CBMVMS-DOCTO              TO CLIC-DOCTO
           MOVE CBMVMS-AAAAMMDD-DOCTO     TO CWTIME-DATE
           SET  CWTIME-REVERSED          TO TRUE
           SET  CWTIME-REVERSE           TO TRUE
           CALL "CWTIME"              USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL        TO CLIC-DDMMAAAA-DOCTO
           MOVE CBMVMS-CENTRO-CUSTO       TO CLIC-CENTRO-CUSTO
           MOVE CBMVMS-HISTORICO-PADRAO   TO CLIC-HISTORICO-PADRAO
           MOVE CBMVMS-HISTORICO-VARIAVEL TO HISTORICO-VARIAVEL
           MOVE CBMVMS-VALOR              TO CLIC-VALOR

           IF   CBMVMS-TIPO = "C"
                ADD  CBMVMS-VALOR    TO CR-MOVIMENTO
                MOVE CBMVMS-COD-RED  TO CLIC-COD-RED-CR
                                        CBPLCO-COD-RED
                                        COD-RED-CALL
                PERFORM TEST AFTER
                        UNTIL FS-CBPLCO NOT = "9D"
                READ CBPLCO KEY IS CBPLCO-COD-RED
                IF FS-CBPLCO = "9D"
                   CALL "CWISAM" USING ER-CBPLCO
                END-IF
                END-PERFORM
                IF   FS-CBPLCO > "09"
                     MOVE "CONTA PERDIDA" TO CLIC-CONTA-ED-CR
                ELSE
                     MOVE "C"             TO CB002PCW-FUNCAO
                     MOVE CBPLCO-CONTA    TO CB002PCW-CONTA
                     CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                     MOVE "E"             TO CB002PCW-FUNCAO
                     CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                     MOVE CB002PCW-CONTA-ED TO CLIC-CONTA-ED-CR
                     CALL "CB039PCW" USING COD-RED-CALL
                                           CLIC-COD-RED-CR-DV
                     MOVE "-"           TO CLIC-COD-RED-CR-TRACO
                END-IF
           ELSE
                ADD  CBMVMS-VALOR    TO DB-MOVIMENTO
                MOVE CBMVMS-COD-RED  TO CLIC-COD-RED-DB
                                        CBPLCO-COD-RED
                                        COD-RED-CALL
                READ CBPLCO IGNORE LOCK
                     KEY IS CBPLCO-COD-RED
                IF   FS-CBPLCO > "09"
                     MOVE "CONTA PERDIDA" TO CLIC-CONTA-ED-DB
                ELSE
                     MOVE "C"               TO CB002PCW-FUNCAO
                     MOVE CBPLCO-CONTA      TO CB002PCW-CONTA
                     CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                     MOVE "E"               TO CB002PCW-FUNCAO
                     CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                     MOVE CB002PCW-CONTA-ED TO CLIC-CONTA-ED-DB
                     CALL "CB039PCW"     USING COD-RED-CALL
                                               CLIC-COD-RED-DB-DV
                     MOVE "-"               TO CLIC-COD-RED-DB-TRACO
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       120-LISTA-HISTORICO-VARIAVEL.

           IF   HISTORICO-VARIAVEL NOT = 0
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                      COMPUTE CBHIVA-TIPO = HISTORICO-VARIAVEL / 100000
                      MOVE HISTORICO-VARIAVEL TO CBHIVA-CODIGO
                      MOVE I                  TO CBHIVA-VARIAVEL
                      READ CBHIVA IGNORE LOCK
                      IF   FS-CBHIVA < "10"
                           MOVE CBHIVA-DESCRICAO TO CLIC-H-VARIAVEL
                           MOVE LINHA-04         TO CWIMPR-DETAIL
                           CALL "CWIMPR"      USING PARAMETROS-CWIMPR
                           PERFORM 910-SAI-CWIMPR
                      ELSE
                           IF   FS-CBHIVA NOT = "23"
                                CALL "CWISAM" USING ER-CBHIVA
                           END-IF
                      END-IF
                END-PERFORM.

       120-99-FIM. EXIT.

       800-INICIAIS.

           MOVE SPACES      TO LINHA-03

           OPEN INPUT CBCOBA
           IF   FS-CBCOBA > "09"
                GOBACK.

           OPEN INPUT CBPLCO
           IF   FS-CBPLCO > "09"
                CLOSE CBCOBA
                GOBACK.

           OPEN INPUT CBHIVA
           IF   FS-CBHIVA > "09"
                CLOSE CBPLCO CBCOBA
                GOBACK.

           DISPLAY CB0010A
           PERFORM TEST AFTER UNTIL FS-CBCOBA < "09"
                                    OR TECLA = 01
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT CB0010A
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE "CB010PCW.H01"
                                LINE 09 COLUMN 22
                                HEIGHT 05
                                WIDTH 40
                           END-EXEC
                      END-IF
                   END-PERFORM
                   IF   TECLA = 1
                        CLOSE CBCOBA
                        GOBACK
                   END-IF
                   OPEN INPUT CBCOBA
                   READ CBCOBA IGNORE LOCK
                   IF FS-CBCOBA = "23"
                      EXEC COBOLware Send Message ERRO-BAC END-EXEC
                   ELSE
                      IF FS-CBCOBA < "10"
                         IF CBCOBA-LC-EFETIVOS = 0
                            EXEC COBOLware Send
                                 Message ERRO-MOVIMENTO
                            END-EXEC
                         ELSE
                           MOVE CBCOBA-MM           TO MM-REF
                           MOVE CBCOBA-AAAA         TO AAAA-REF
                           MOVE CBCOBA-LC-PREVISTOS TO CLIC-LC-PREVISTOS
                           MOVE CBCOBA-LC-EFETIVOS  TO CLIC-LC-EFETIVOS
                           MOVE CBCOBA-CR-PREVISTOS TO CLIC-CR-PREVISTOS
                           MOVE CBCOBA-CR-EFETIVOS  TO CLIC-CR-EFETIVOS
                           MOVE CBCOBA-DB-PREVISTOS TO CLIC-DB-PREVISTOS
                           MOVE CBCOBA-DB-EFETIVOS  TO CLIC-DB-EFETIVOS
                           CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                           OPEN INPUT CBMVMS
                           IF FS-CBMVMS > "09"
                              CALL "CWISAM" USING ER-CBMVMS
                              CLOSE CBCOBA CBMVMS
                              MOVE "44" TO FS-CBCOBA
                           ELSE
                             MOVE LB-CBMVMS     TO CLIC-LB-CBMVMS
                             MOVE CBCOBA-SERIE  TO CBMVMS-SERIE
                             MOVE CBCOBA-NUMERO TO CBMVMS-NUMERO
                             MOVE ZERO          TO CBMVMS-LANCAMENTO
                             MOVE SPACE         TO CBMVMS-TIPO
                             START CBMVMS
                                   KEY NOT LESS CBMVMS-BAC-CHAVE
                           END-IF
                         END-IF
                      END-IF
                   END-IF
           END-PERFORM

           MOVE CBCOBA-CHAVE   TO BAC
           MOVE BAC        TO CLIC-BAC
           MOVE CBCOBA-AAAA    TO AAAA
           MOVE CBCOBA-MM      TO MM
           MOVE REFERENCIA     TO CLIC-REFERENCIA
           MOVE "CB010PA"      TO CWIMPR-REPORT
           MOVE LINHA-01       TO CWIMPR-TITLE
           MOVE LINHA-02       TO CWIMPR-HEADER-1
           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW".

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE GR-PRNTER    TO CLIC-LC-MOVIMENTO
           MOVE CR-MOVIMENTO TO CLIC-CR-MOVIMENTO
           MOVE DB-MOVIMENTO TO CLIC-DB-MOVIMENTO

           MOVE SPACES   TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-05 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-06 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-07 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-08 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-09 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-10 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR
           MOVE LINHA-05 TO CWIMPR-DETAIL
           CALL "CWIMPR" USING PARAMETROS-CWIMPR PERFORM 910-SAI-CWIMPR

           IF  (GR-PRNTER    NOT = CBCOBA-LC-EFETIVOS)
           OR  (CR-MOVIMENTO NOT = CBCOBA-CR-EFETIVOS)
           OR  (DB-MOVIMENTO NOT = CBCOBA-DB-EFETIVOS)
                MOVE SPACES      TO CWIMPR-DETAIL
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                PERFORM 910-SAI-CWIMPR
                MOVE LINHA-11    TO CWIMPR-DETAIL
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                PERFORM 910-SAI-CWIMPR
                MOVE LINHA-12    TO CWIMPR-DETAIL
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                PERFORM 910-SAI-CWIMPR
                MOVE LINHA-13    TO CWIMPR-DETAIL
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                PERFORM 910-SAI-CWIMPR
                MOVE LINHA-14    TO CWIMPR-DETAIL
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                PERFORM 910-SAI-CWIMPR
                MOVE LINHA-15    TO CWIMPR-DETAIL
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
                PERFORM 910-SAI-CWIMPR
           END-IF

           MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           CLOSE CBCOBA CBMVMS CBHIVA CBPLCO
           CANCEL "CB039PCW"
           CANCEL "CB002PCW".

       900-99-FIM. EXIT.

       910-SAI-CWIMPR.

           IF   CWIMPR-END-PRINT
                CLOSE CBCOBA CBMVMS
                      CBPLCO CBHIVA
                GOBACK.

       910-99-FIM. EXIT.

       END PROGRAM CB010PCW.
