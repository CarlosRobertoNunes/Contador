       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB034PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  11/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exporta saldos para planilhas eletronicas    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOSASL.
       COPY CBPLCOSL.
       COPY CBFOSDSL.

           SELECT LOTE-E ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOTE-E.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOSAFD.
       COPY CBPLCOFD.
       COPY CBFOSDFD.

       FD  LOTE-E
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTE-E.

       01  LOTE-E-REG.
           05 LOTE-E-BYE PIC X(001) OCCURS 300.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LB-HELP                  PIC  X(012) VALUE "CB034PCW.HXX".
           05 GRAU                     PIC  9(001) VALUE ZERO.
           05 GRAU-MINIMO              PIC  9(001) VALUE ZERO.
           05 PERIODO-I.
              10 AAAA-I                PIC  9(004) VALUE 0.
              10 MM-I                  PIC  9(002) VALUE 0.
           05 PERIODO-F.
              10 AAAA-F                PIC  9(004) VALUE 0.
              10 MM-F                  PIC  9(002) VALUE 0.
           05 I-1                      PIC  9(001) VALUE 0.
           05 I-X REDEFINES I-1        PIC  X(001).
           05 ABRE-NUMERO              PIC  9(018) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 MSG                      PIC  X(074) VALUE SPACES.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 0.
           05 P                        PIC  9(004) VALUE 0.
           05 S                        PIC  9(004) VALUE 0.
           05 S2                       PIC  9(004) VALUE 0.
           05 LIMITE                   PIC  9(002) VALUE 0.
           05 PG                       PIC  9(002) VALUE 0.
           05 CONTA-SELE               PIC  9(015) VALUE 0.
           05 LD-CBCOSA                PIC  9(006) VALUE 0.
           05 GR-LOTE-E                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 RESPOSTA-OK VALUE "E" "e" "D" "d" "N" "n" " ".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
              88 SAIR        VALUE " ".
           05 ER-CBFOSD.
              10 FS-CBFOSD             PIC  X(002) VALUE "00".
              10 LB-CBFOSD             PIC  X(050) VALUE "CBFOSD".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-LOTE-E.
              10 FS-LOTE-E             PIC  X(002) VALUE "00".
              10 LB-LOTE-E             PIC  X(050) VALUE "LOTE-E.TXT".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.

       COPY CB002PCW.
       COPY CWBOXS.

       SCREEN SECTION.

       01  CTAC-LIT-CB034PCW.
           05 LINE 07 COLUMN 03 VALUE "Nome do arquivo a ge".
           05 LINE 07 COLUMN 23 VALUE "rar :".

       01  CTAC-VAR-CB034PCW.
           05 LINE 07 COLUMN 29 PIC X(050) USING LB-LOTE-E.

       01  CTAC-LIT-CB0034A.
           05 LINE 09 COLUMN 03 VALUE "Per¡odo de referˆnci".
           05 LINE 09 COLUMN 23 VALUE "a de:".
           05 LINE 09 COLUMN 37 VALUE "a".
           05 LINE 09 COLUMN 31 VALUE "/".
           05 LINE 09 COLUMN 41 VALUE "/".

       01  CTAC-VAR-CB0034A AUTO.
           05 LINE 09 COLUMN 29 PIC Z(002) USING MM-I.
           05 LINE 09 COLUMN 32 PIC Z(004) USING AAAA-I.
           05 LINE 09 COLUMN 39 PIC Z(002) USING MM-F.
           05 LINE 09 COLUMN 42 PIC Z(004) USING AAAA-F.

       01  CB0034E.
           05 LINE 12 COLUMN 03 VALUE "Selecionar conta:".
           05 LINE 12 COLUMN 21 PIC ZZZZ USING CONTA-SELE.

       01  CTAC-LIT-CB0034C.
           05 LINE 10 COLUMN 10 VALUE "Lidos de".
           05 LINE 11 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0034C.
           05 T-LD-CBCOSA LINE 10 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-CBCOSA.
           05 LINE 10 COLUMN 19 PIC X(010) FROM LB-CBCOSA.
           05 T-GR-LOTE-E LINE 11 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTE-E.
           05 LINE 11 COLUMN 22 PIC X(050) FROM LB-LOTE-E.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FS-CBCOSA = "10"
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           READ CBCOSA NEXT RECORD IGNORE LOCK

           IF   FS-CBCOSA < "10"
                ADD 1 TO LD-CBCOSA
                DISPLAY T-LD-CBCOSA
                MOVE CBCOSA-CONTA    TO CB002PCW-CONTA
                MOVE "C"            TO CB002PCW-FUNCAO
                CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                MOVE "E"            TO CB002PCW-FUNCAO
                CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                IF  (CBCOSA-AAAAMM NOT < PERIODO-I)
                AND (CBCOSA-AAAAMM NOT > PERIODO-F)
                AND (CB002PCW-GRAU NOT > GRAU)
                AND (CB002PCW-GRAU NOT < GRAU-MINIMO)
                AND (CBCOSA-CONTA = CONTA-SELE OR CONTA-SELE = 0)
                     PERFORM 110-EXPORTAR THRU 110-99-FIM
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       110-EXPORTAR.

           ADD 1 TO GR-LOTE-E
           DISPLAY T-GR-LOTE-E
           MOVE SPACES TO LOTE-E-REG

           IF   CBFOSD-I (01) NOT = 0
                MOVE CBFOSD-I (01) TO P
                MOVE CBFOSD-F (01) TO S
                COMPUTE ABRE-NUMERO = CBCOSA-SALDO-INICIAL * 1000
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
                COMPUTE P = P + S - 1
                IF   CBCOSA-SALDO-INICIAL NEGATIVE
                     MOVE "-" TO LOTE-E-REG (P: 1)
                ELSE
                     MOVE "+" TO LOTE-E-REG (P: 1)
                END-IF
           END-IF

           IF   CBFOSD-I (02) NOT = 0
                MOVE CBFOSD-I (02) TO P
                MOVE CBFOSD-F (02) TO S
                COMPUTE ABRE-NUMERO = CBCOSA-SALDO-ATUAL * 1000
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
                COMPUTE P = P + S - 1
                IF   CBCOSA-SALDO-ATUAL NEGATIVE
                     MOVE "-" TO LOTE-E-REG (P: 1)
                ELSE
                     MOVE "+" TO LOTE-E-REG (P: 1)
                END-IF
           END-IF

           IF   CBFOSD-I (03) NOT = 0
                MOVE CBFOSD-I (03) TO P
                MOVE CBFOSD-F (03) TO S
                COMPUTE ABRE-NUMERO = CBCOSA-A-DEBITO * 100
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOSD-I (04) NOT = 0
                MOVE CBFOSD-I (04) TO P
                MOVE CBFOSD-F (04) TO S
                COMPUTE ABRE-NUMERO = CBCOSA-A-CREDITO * 100
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOSD-I (05) NOT = 0
                MOVE CBFOSD-I (05) TO P
                MOVE CBFOSD-F (05) TO S
                MOVE CB002PCW-CONTA-ED TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOSD-I (06) NOT = 0
                MOVE CBFOSD-I (06)  TO P
                MOVE CBFOSD-F (06)  TO S
                MOVE CBCOSA-CONTA TO CBPLCO-CONTA
                READ CBPLCO IGNORE LOCK
                MOVE CBPLCO-DESCRICAO TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOSD-I (07) NOT = 0
                MOVE CBFOSD-I (07) TO P
                MOVE CBFOSD-F (07) TO S
                MOVE CBCOSA-MM TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           IF   CBFOSD-I (08) NOT = 0
                MOVE CBFOSD-I (08) TO P
                MOVE CBFOSD-F (08) TO S
                MOVE CBCOSA-AAAA TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
           END-IF

           WRITE LOTE-E-REG
           IF   FS-LOTE-E > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF.

       110-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY CTAC-LIT-CB034PCW
           PERFORM TEST AFTER UNTIL FS-LOTE-E = "00"
                   DISPLAY "<Esc>-Fim" LINE 23 COLUMN 03
                   CLOSE LOTE-E
                   ACCEPT CTAC-VAR-CB034PCW
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
                               " j  existe !" DELIMITED BY SIZE
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

           OPEN INPUT CBFOSD
           IF   FS-CBFOSD > "09"
                GOBACK
           END-IF

           OPEN INPUT CBCOSA
           IF   FS-CBCOSA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           OPEN INPUT CBPLCO.
           IF   FS-CBPLCO > "09"
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
                        MOVE PONTEIRO (PG) TO CBFOSD-FORMATO
                        START CBFOSD KEY NOT LESS CBFOSD-CHAVE
                        SUBTRACT 1 FROM PG
                        MOVE 9 TO CWBOXS-OPTION
                   END-IF
                   IF   CWBOXS-OPTION = 0
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
           END-PERFORM

           DISPLAY "Formato: " LINE 08 COLUMN 03
                   CBFOSD-FORMATO " - " CBFOSD-COMENTARIO

           DISPLAY CTAC-LIT-CB0034A
           PERFORM TEST AFTER UNTIL AAAA-I > 1900
                                AND AAAA-F > 1900
                                AND MM-I > 00
                                AND MM-I < 13
                                AND MM-F > 00
                                AND MM-F < 13
                                AND PERIODO-F NOT < PERIODO-I
                    ACCEPT CTAC-VAR-CB0034A
                    IF AAAA-I > 1900
                    AND AAAA-F > 1900
                    AND MM-I > 00
                    AND MM-I < 13
                    AND MM-F > 00
                    AND MM-F < 13
                    AND PERIODO-F NOT < PERIODO-I
                        CONTINUE
                    ELSE
                        EXEC COBOLware Send
                             Message "Per¡odo inv lido"
                        END-EXEC
                    END-IF
           END-PERFORM

           MOVE CWBOXS-TEXT   (CWBOXS-OPTION) (2: ) TO CBFOSD-CHAVE
           READ CBFOSD

           DISPLAY CB0034E
           PERFORM TEST AFTER UNTIL CONTA-SELE = 0
                                 OR FS-CBPLCO < "10"
                   ACCEPT CB0034E
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
                             DISPLAY CBPLCO-DESCRICAO LINE 12 COLUMN 26
                             MOVE CBPLCO-CONTA TO CONTA-SELE
                        ELSE
                             DISPLAY "Conta nÆo existe"
                                     LINE 12 COLUMN 26
                        END-IF
                   ELSE
                        DISPLAY "Todas as contas                      "
                                LINE 12 COLUMN 21
                   END-IF
           END-PERFORM

           IF   CONTA-SELE NOT = 0
                MOVE 9 TO GRAU
                MOVE 1 TO GRAU-MINIMO
                GO TO 800-INICIAIS-BOXS-FIM
           END-IF

           MOVE 1                TO CWBOXS-OPTION
           MOVE SPACES           TO CWBOXS-ITENS.

       800-INICIAIS-BOXS.

           MOVE "N"              TO CWBOXS-ERASE
           MOVE 12               TO CWBOXS-LINE
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
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF

           MOVE CWBOXS-OPTION TO GRAU-MINIMO

           IF   CWBOXS-OPTION < CB002PCW-GRAU
                MOVE "At‚"              TO CWBOXS-TITLE
                MOVE 53                 TO CWBOXS-COLUMN
                MOVE CB002PCW-GRAU        TO CWBOXS-OPTION
                CALL "CWBOXS"        USING PARAMETROS-CWBOXS
                IF   CWBOXS-ARROW = "<"
                     MOVE GRAU-MINIMO TO CWBOXS-OPTION
                     GO TO 800-INICIAIS-BOXS
                END-IF
                IF   CWBOXS-OPTION = 0
                     PERFORM 900-FINAIS THRU 900-99-FIM
                     GOBACK
                END-IF
           END-IF

           MOVE CWBOXS-OPTION      TO GRAU.

       800-INICIAIS-BOXS-FIM.

           DISPLAY CTAC-LIT-CB0034C
                   CTAC-VAR-CB0034C.

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
                       OR FS-CBFOSD > "09"
              READ CBFOSD NEXT RECORD IGNORE LOCK
              IF   FS-CBFOSD < "10"
                   ADD 1 TO LIMITE
                   IF   PONTEIRO (PG) = SPACES
                        MOVE CBFOSD-FORMATO TO PONTEIRO (PG)
                   END-IF
                   MOVE CBFOSD-FORMATO    TO CWBOXS-TEXT
                                             (LIMITE) (2: )
                   IF   CBFOSD-COMENTARIO NOT = SPACES
                        MOVE " - "             TO CWBOXS-TEXT
                                                  (LIMITE) (9: 3)
                        MOVE CBFOSD-COMENTARIO TO CWBOXS-TEXT
                                                  (LIMITE) (12: )
                   END-IF
              END-IF
           END-PERFORM

           IF   LIMITE = 8
                READ CBFOSD NEXT RECORD IGNORE LOCK
                IF   FS-CBFOSD < "10"
                     ADD  1              TO LIMITE
                     MOVE " Mais op‡äes" TO CWBOXS-TEXT   (LIMITE)
                     READ CBFOSD PREVIOUS RECORD IGNORE LOCK
                END-IF
                MOVE 9 TO CWBOXS-OPTION
           ELSE
                MOVE 1 TO CWBOXS-OPTION
           END-IF.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOSD CBCOSA CBPLCO LOTE-E.

       900-99-FIM. EXIT.

       END PROGRAM CB034PCW.
