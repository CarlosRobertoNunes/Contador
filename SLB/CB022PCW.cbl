       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB022PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  02/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exporta lancamentos                          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBCOBASL.
       COPY CBHIVASL.
       COPY CBPAPCSL.
       COPY CBPLCOSL.
       COPY CBFOLCSL.
       COPY CBMVMSSL.

           SELECT LOTE-E ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOTE-E.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCOBAFD.
       COPY CBHIVAFD.
       COPY CBPAPCFD.
       COPY CBPLCOFD.
       COPY CBFOLCFD.
       COPY CBMVMSFD.

       FD  LOTE-E
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTE-E.

       01  LOTE-E-REG.
           05 LOTE-E-BYE PIC X(001) OCCURS 300.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 RK-CBPAPC                PIC  9(001) COMP VALUE 1.
           05 ABRE-NUMERO              PIC  9(018) VALUE 0.
           05 MSG                      PIC  X(050) VALUE SPACES.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 MODO                     PIC  9(001) VALUE 0.
              88 EXPORTA-BAC        VALUE 1.
              88 EXPORTA-REFERENCIA VALUE 2.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 DGC                      PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 P                        PIC  9(004) VALUE 0.
           05 S                        PIC  9(004) VALUE 0.
           05 S2                       PIC  9(004) VALUE 0.
           05 LIMITE                   PIC  9(002) VALUE 0.
           05 PG                       PIC  9(002) VALUE 0.
           05 CONTA-SELE               PIC  9(004) VALUE 0.
           05 CC-SELE                  PIC  9(004) VALUE 0.
           05 LD-CBMVMS                PIC  9(006) VALUE 0.
           05 GR-LOTE-E                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
           05 ER-CBFOLC.
              10 FS-CBFOLC             PIC  X(002) VALUE "00".
              10 LB-CBFOLC             PIC  X(050) VALUE "CBFOLC.DAT".
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
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBPAPC.
              10 FS-CBPAPC             PIC  X(002) VALUE "00".
              10 LB-CBPAPC             PIC  X(050) VALUE "CBPAPC".
           05 ER-LOTE-E.
              10 FS-LOTE-E             PIC  X(002) VALUE "00".
              10 LB-LOTE-E             PIC  X(050) VALUE "LOTE-E.TXT".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.

       COPY CWBOXS.
       COPY CB002PCW.

       SCREEN SECTION.

       01  CTAC-LIT-CB022PCW.
           05 LINE 08 COLUMN 03 VALUE "Nome do arquivo a ge".
           05 LINE 08 COLUMN 23 VALUE "rar :".

       01  CTAC-VAR-CB022PCW.
           05 LINE 08 COLUMN 29 PIC X(050) USING LB-LOTE-E.

       01  CTAC-LIT-CB0022B AUTO.
           05 LINE 11 COLUMN 03 VALUE "BAC a exportar: ".
           05 LINE 11 COLUMN 19 VALUE "SÇrie :".
           05 LINE 12 COLUMN 19 VALUE "N£mero:".

       01  CTAC-VAR-CB0022B AUTO.
           05 LINE 11 COLUMN 28 PIC Z(004) USING CBCOBA-SERIE.
           05 LINE 12 COLUMN 28 PIC Z(004) USING CBCOBA-NUMERO.

       01  CB020PA AUTO.
           05 LINE 13 COLUMN 03 VALUE "Màs de referància:".
           05 LINE 13 COLUMN 22 PIC ZZ/ USING MM-REF.
           05 LINE 13 COLUMN 25 PIC 9999 USING AAAA-REF BLANK ZERO.

       01  CB020PE AUTO.
           05 LINE 14 COLUMN 03 VALUE "Selecionar conta:".
           05 LINE 14 COLUMN 21 PIC ZZZZ USING CONTA-SELE.

       01  CB020PF AUTO.
           05 LINE 15 COLUMN 03 VALUE "Selecionar centro de custos:".
           05 LINE 15 COLUMN 32 PIC ZZZZ USING CC-SELE.

       01  CTAC-LIT-CB0022C.
           05 LINE 17 COLUMN 10 VALUE "Lidos de".
           05 LINE 18 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0022C.
           05 T-LD-CBMVMS LINE 17 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-CBMVMS.
           05 LINE 17 COLUMN 19 PIC X(010) FROM LB-CBMVMS.
           05 T-GR-LOTE-E LINE 18 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTE-E.
           05 LINE 18 COLUMN 22 PIC X(050) FROM LB-LOTE-E.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FS-CBMVMS = "10"
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           READ CBMVMS NEXT RECORD IGNORE LOCK

           IF   FS-CBMVMS < "10"
           AND  CBMVMS-COD-RED NOT = 0
                ADD 1 TO LD-CBMVMS
                DISPLAY T-LD-CBMVMS
                IF ((EXPORTA-BAC
                AND  CBMVMS-SERIE  = CBCOBA-SERIE
                AND  CBMVMS-NUMERO = CBCOBA-NUMERO)
                OR   EXPORTA-REFERENCIA)
                AND (CONTA-SELE = 0
                OR   CONTA-SELE = CBMVMS-COD-RED)
                AND (CC-SELE = 0
                OR   CC-SELE = CBMVMS-CENTRO-CUSTO)
                     PERFORM 110-EXPORTAR THRU 110-99-FIM
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       110-EXPORTAR.

           ADD 1 TO GR-LOTE-E
           DISPLAY T-GR-LOTE-E
           MOVE SPACES TO LOTE-E-REG

           IF   CBFOLC-I (01) NOT = 0
                MOVE CBFOLC-I (01) TO P
                MOVE CBFOLC-F (01) TO S
                IF   CBMVMS-TIPO = "D"
                     MOVE CBFOLC-INDICA-DEBITO  TO LOTE-E-REG (P: S)
                ELSE
                     MOVE CBFOLC-INDICA-CREDITO TO LOTE-E-REG (P: S)
                END-IF
           END-IF

           IF   CBFOLC-I (02) NOT = 0
                MOVE CBFOLC-I (02) TO P
                MOVE CBFOLC-F (02) TO S
                MOVE CBMVMS-DIA TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOLC-I (03) NOT = 0
                MOVE CBFOLC-I (03)  TO P
                MOVE CBFOLC-F (03)  TO S
                MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
                PERFORM TEST AFTER UNTIL FS-CBPLCO NOT = "9D"
                        READ CBPLCO KEY IS CBPLCO-COD-RED
                        IF  FS-CBPLCO = "9D"
                            CALL "CWISAM" USING FS-CBPLCO
                        END-IF
                END-PERFORM
                MOVE CBPLCO-CONTA TO ABRE-NUMERO
                MOVE ABRE-NUMERO (4: S) TO LOTE-E-REG (P: S)
                IF   (DGC + 1) = S
                     MOVE CBPLCO-CONTA TO CB002PCW-CONTA
                     MOVE "C"          TO CB002PCW-FUNCAO
                     CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                     COMPUTE P = P + S - 1
                     MOVE CB002PCW-DV    TO LOTE-E-REG (P: 1)
                END-IF
           END-IF

           IF   CBFOLC-I (04) NOT = 0
                MOVE CBFOLC-I (04)  TO P
                MOVE CBFOLC-F (04)  TO S
                MOVE CBMVMS-COD-RED TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOLC-I (05) NOT = 0
                MOVE CBFOLC-I (05)       TO P
                MOVE CBFOLC-F (05)       TO S
                MOVE CBMVMS-CENTRO-CUSTO TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOLC-I (06) NOT = 0
                MOVE CBFOLC-I (06)  TO P
                MOVE CBFOLC-F (06)  TO S
                MOVE CBMVMS-DOCTO   TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOLC-I (07) NOT = 0
                MOVE CBFOLC-I (07)       TO P
                MOVE CBFOLC-F (07)       TO S
                MOVE CBMVMS-AAAAMMDD-DOCTO TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOLC-I (08) NOT = 0
                MOVE CBFOLC-I (08)           TO P
                MOVE CBFOLC-F (08)           TO S
                MOVE CBMVMS-HISTORICO-PADRAO TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOLC-I (10) NOT = 0
                MOVE CBFOLC-I (10)           TO P
                MOVE CBFOLC-F (10)           TO S
                COMPUTE ABRE-NUMERO = CBMVMS-VALOR * 100
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF  (CBFOLC-I (09) NOT = 0)
           AND (CBMVMS-HISTORICO-VARIAVEL NOT = 0)
                MOVE CBFOLC-I (09)           TO P
                MOVE CBFOLC-F (09)           TO S
                MOVE "00"   TO FS-CBHIVA
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                                                 OR FS-CBHIVA > "09"
                        COMPUTE CBHIVA-TIPO
                              = CBMVMS-HISTORICO-VARIAVEL / 100000
                        MOVE CBMVMS-HISTORICO-VARIAVEL TO CBHIVA-CODIGO
                        MOVE I TO CBHIVA-VARIAVEL
                        READ CBHIVA NEXT RECORD IGNORE LOCK
                        IF   FS-CBHIVA < "09"
                             MOVE CBHIVA-DESCRICAO TO LOTE-E-REG (P: S)
                        END-IF
                        IF   LOTE-E-REG NOT = SPACES
                             WRITE LOTE-E-REG
                              IF   FS-LOTE-E > "09"
                                   PERFORM 900-FINAIS THRU 900-99-FIM
                                   GOBACK
                              END-IF
                        END-IF
                        MOVE SPACES TO LOTE-E-REG
                END-PERFORM
           END-IF

           IF   LOTE-E-REG NOT = SPACES
                WRITE LOTE-E-REG
                IF   FS-LOTE-E > "09"
                     PERFORM 900-FINAIS THRU 900-99-FIM
                     GOBACK
                END-IF
           END-IF.

       110-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY CTAC-LIT-CB022PCW
           PERFORM TEST AFTER UNTIL FS-LOTE-E = "00"
                   DISPLAY "<Esc>-Fim" LINE 23 COLUMN 03
                   CLOSE LOTE-E
                   ACCEPT CTAC-VAR-CB022PCW
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   ESC
                        GOBACK
                   END-IF
                   OPEN INPUT LOTE-E
                   IF   FS-LOTE-E = "00"
                        CLOSE LOTE-E
                        MOVE SPACES TO MSG
                        STRING "O arquivo "   DELIMITED BY SIZE
                                LB-LOTE-E     DELIMITED BY SPACE
                               " j† existe !" DELIMITED BY SIZE
                              INTO MSG
                        EXEC COBOLware Send
                             Message MSG
                             CAPTION(1) "~Extender"
                             CAPTION(2) "~Destruir"
                             CAPTION(3) "~Novo nome"
                             OPTION-CHAR;RESPOSTA
                        END-EXEC
                        EVALUATE TRUE
                            WHEN EXTENDER
                                 OPEN EXTEND LOTE-E
                            WHEN DESTRUIR
                                 OPEN OUTPUT LOTE-E
                            WHEN NOVO-NOME
                                 MOVE "44" TO FS-LOTE-E
                            WHEN OTHER
                                 GOBACK
                        END-EVALUATE
                   ELSE
                        CLOSE LOTE-E
                        OPEN OUTPUT LOTE-E
                   END-IF
           END-PERFORM

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
                CLOSE CBPAPC
           END-IF

           OPEN INPUT CBCOBA
           IF   FS-CBCOBA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBPLCO
           IF   FS-CBPLCO > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBHIVA
           IF   FS-CBHIVA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           MOVE 10                     TO CWBOXS-LINE
           MOVE 04                     TO CWBOXS-COLUMN
           MOVE "Formatos dispon°veis" TO CWBOXS-TITLE
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
                        PERFORM TEST AFTER UNTIL FS-CBFOLC NOT = "9D"
                                START CBFOLC KEY NOT LESS CBFOLC-CHAVE
                                IF   FS-CBFOLC = "9D"
                                     CALL "CWISAM" USING ER-CBFOLC
                                END-IF
                        END-PERFORM
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
           READ CBFOLC IGNORE LOCK

           DISPLAY "Formato: " LINE 09 COLUMN 03
                   CBFOLC-FORMATO " - " CBFOLC-COMENTARIO

           ADD  4                    TO CWBOXS-LINE
           MOVE "AU"                 TO CWBOXS-ITENS
           MOVE "Modo de exportaá∆o" TO CWBOXS-TITLE
           MOVE X"FF"                TO CWBOXS-TITLE (5: 1)
                                        CWBOXS-TITLE (8: 1)
           MOVE " Apenas 1 BAC"      TO CWBOXS-TEXT   (1)
           MOVE " Um màs inteiro"    TO CWBOXS-TEXT   (2)
           CALL "CWBOXS"          USING PARAMETROS-CWBOXS
           MOVE CWBOXS-OPTION        TO MODO

           EVALUATE CWBOXS-OPTION
                WHEN 0 PERFORM 900-FINAIS THRU 900-99-FIM
                       GOBACK
                WHEN 1 DISPLAY CTAC-LIT-CB0022B
                       PERFORM TEST AFTER UNTIL ESC
                                        OR FS-CBCOBA = "00"
                          ACCEPT CTAC-VAR-CB0022B
                          ACCEPT TECLA FROM ESCAPE KEY
                          IF   ESC
                               PERFORM 900-FINAIS THRU 900-99-FIM
                               GOBACK
                          ELSE
                               PERFORM TEST AFTER
                                       UNTIL FS-CBCOBA NOT = "9D"
                                  READ CBCOBA
                                  IF   FS-CBCOBA = "9D"
                                       CALL "CWISAM" USING ER-CBCOBA
                                  END-IF
                               END-PERFORM
                               IF  FS-CBCOBA = "23"
                                   EXEC COBOLware Send
                                        Message "BAC inexistente"
                                   END-EXEC
                               ELSE
                                   MOVE CBCOBA-AAAA TO AAAA-REF
                                   MOVE CBCOBA-MM   TO MM-REF
                                   OPEN INPUT CBMVMS
                                   IF  FS-CBMVMS > "09"
                                       EXEC COBOLware Send Message
                                            "BAC n∆o possui lanáamentos"
                                       END-EXEC
                                       MOVE "44" TO FS-CBCOBA
                                   END-IF
                               END-IF
                          END-IF
                       END-PERFORM
                WHEN 2 DISPLAY CB020PA
                       PERFORM TEST AFTER UNTIL ESC
                                        OR FS-CBMVMS = "00"
                          PERFORM TEST AFTER UNTIL NOT F1
                             MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                             DISPLAY RODAPE LINE 23 COLUMN 03
                             ACCEPT CB020PA
                             MOVE SPACES TO RODAPE
                             DISPLAY RODAPE LINE 23 COLUMN 03
                             ACCEPT TECLA FROM ESCAPE KEY
                             IF   F1
                                  EXEC COBOLware Help
                                       FILE   "CB022PCW.H01"
                                       LINE   16 COLUMN 23
                                       HEIGHT 06 WIDTH  40
                                  END-EXEC
                             END-IF
                          END-PERFORM
                          IF   NOT ESC
                               OPEN INPUT CBMVMS
                               IF   FS-CBMVMS > "09"
                                    EXEC COBOLware Send
                                         Message "Referància impr¢pria"
                                    END-EXEC
                               END-IF
                          ELSE
                               PERFORM 900-FINAIS THRU 900-99-FIM
                               GOBACK
                          END-IF
                       END-PERFORM
           END-EVALUATE

           DISPLAY CB020PE
           PERFORM TEST AFTER UNTIL CONTA-SELE = 0
                                 OR FS-CBPLCO < "10"
                   ACCEPT CB020PE
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   ESC
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
                   IF   CONTA-SELE NOT = 0
                        MOVE CONTA-SELE TO CBPLCO-COD-RED
                        READ CBPLCO IGNORE LOCK
                                KEY IS CBPLCO-COD-RED
                        IF   FS-CBPLCO < "10"
                             DISPLAY CBPLCO-DESCRICAO LINE 14 COLUMN 26
                        ELSE
                             DISPLAY "Conta n∆o existe"
                                     LINE 14 COLUMN 26
                        END-IF
                   ELSE
                        DISPLAY "Todas as contas                      "
                                LINE 14 COLUMN 21
                   END-IF
           END-PERFORM

           OPEN INPUT CBCACC

           DISPLAY CB020PF
           PERFORM TEST AFTER UNTIL CC-SELE = 0
                                 OR FS-CBCACC < "10"
                   ACCEPT CB020PF
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   ESC
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
                   IF   CC-SELE NOT = 0
                        MOVE CC-SELE TO CBCACC-CODIGO
                        READ CBCACC IGNORE LOCK
                        IF   FS-CBCACC < "10"
                             DISPLAY CBCACC-DESCRICAO LINE 15 COLUMN 37
                        ELSE
                             DISPLAY "Centro de custos n∆o existe"
                                     LINE 15 COLUMN 37
                        END-IF
                   ELSE
                        DISPLAY "Todos os centros de custos            "
                                LINE 15 COLUMN 32
                   END-IF
           END-PERFORM

           CLOSE CBCACC

           DISPLAY CTAC-LIT-CB0022C
                   CTAC-VAR-CB0022C.

       800-99-FIM. EXIT.

       810-MONTA-PAGINA.

           ADD  1      TO PG
           MOVE 0      TO LIMITE
           MOVE SPACES TO CWBOXS-ITENS

           IF   PG > 1
                ADD  1                    TO LIMITE
                MOVE " Opá‰es anteriores" TO CWBOXS-TEXT   (LIMITE)
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
                     MOVE " Mais opá‰es" TO CWBOXS-TEXT   (LIMITE)
                     READ CBFOLC PREVIOUS RECORD IGNORE LOCK
                END-IF
                MOVE 9 TO CWBOXS-OPTION
           ELSE
                MOVE 1 TO CWBOXS-OPTION
           END-IF.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOLC CBCOBA CBPLCO CBMVMS CBHIVA CBCACC LOTE-E.

       900-99-FIM. EXIT.

       END PROGRAM CB022PCW.
