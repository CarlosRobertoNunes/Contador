       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB025PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Importa plano de contas                      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCTCRSL.
       COPY CBPAPCSL.
       COPY CBPLCOSL.
       COPY CBFOPCSL.

           SELECT LOTE-I ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOTE-I.

           SELECT LOTEWK ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-LOTEWK.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCTCRFD.
       COPY CBPAPCFD.
       COPY CBPLCOFD.
       COPY CBFOPCFD.

       FD  LOTE-I
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTE-I.

       01  LOTE-I-REG.
           05 LOTE-I-BYE PIC X(001) OCCURS 300.

       FD  LOTEWK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTEWK.

       01  LOTEWK-REG.
           05 LOTEWK-CONTA             PIC  9(015).
           05 LOTEWK-DESCRICAO         PIC  X(030).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 ESPACOS                  PIC  X(068) VALUE SPACES.
           05 NDF                      PIC  X(024) VALUE
              "NAO DEFINIDIO NO FORMATO".
           05 RK-CBPAPC           COMP PIC  9(001) VALUE 1.
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
           05 DATA-CRITICA             PIC  X(006) VALUE SPACES.
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
           05 GR-CBPLCO                PIC  9(006) VALUE 0.
           05 GR-LOTEWK                PIC  9(006) VALUE 0.
           05 LD-LOTEWK                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
              88 EFETIVAR-OK VALUE "S" "s" "N" "n".
              88 EFETIVAR    VALUE "S" "s".
           05 ER-CBFOPC.
              10 FS-CBFOPC             PIC  X(002) VALUE "00".
              10 LB-CBFOPC             PIC  X(050) VALUE "CBFOPC".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBPAPC.
              10 FS-CBPAPC             PIC  X(002) VALUE "00".
              10 LB-CBPAPC             PIC  X(050) VALUE "CBPAPC".
           05 ER-CBCTCR.
              10 FS-CBCTCR             PIC  X(002) VALUE "00".
              10 LB-CBCTCR             PIC  X(050) VALUE "CBCTCR".
           05 ER-LOTE-I.
              10 FS-LOTE-I             PIC  X(002) VALUE "00".
              10 LB-LOTE-I             PIC  X(050) VALUE "LOTE-I.TXT".
           05 ER-LOTEWK.
              10 FS-LOTEWK             PIC  X(002) VALUE "00".
              10 LB-LOTEWK             PIC  X(050) VALUE "LOTEWK".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.
           05 MENSAGENS.
              10 PIC X(19) VALUE "CONTA              ".
              10 PIC X(19) VALUE "                   ".
              10 PIC X(19) VALUE "DESCRICAO          ".
           05 REDEFINES MENSAGENS.
              10 MSG OCCURS 3  PIC X(19).

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

       SCREEN SECTION.

       01  CTAC-LIT-CB025PCW.
           05 LINE 08 COLUMN 03 VALUE "Nome do arquivo a importar:".

       01  CTAC-VAR-CB025PCW.
           05 LINE 08 COLUMN 31 PIC X(048) USING LB-LOTE-I.

       01  CTAC-LIT-CB0025C.
           05 LINE 16 COLUMN 10 VALUE "Lidos de".
           05 LINE 17 COLUMN 10 VALUE "gravados em".
           05 LINE 19 COLUMN 10 VALUE "Lidos de".
           05 LINE 20 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0025C.
           05 T-LD-LOTE-I LINE 16 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTE-I.
           05 LINE 16 COLUMN 19 PIC X(050) FROM LB-LOTE-I.
           05 T-GR-LOTEWK LINE 17 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTEWK.
           05 LINE 17 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-LD-LOTEWK LINE 19 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTEWK.
           05 LINE 19 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-GR-CBPLCO LINE 20 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-CBPLCO.
           05 LINE 20 COLUMN 22 PIC X(050) FROM LB-CBPLCO.

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
                    PERFORM UNTIL FS-LOTEWK = "10"
                       IF   LER = "S"
                            READ LOTEWK
                       ELSE
                            MOVE "S" TO LER
                       END-IF
                       IF   FS-LOTEWK > "10"
                            CALL "CWISAM" USING ER-LOTEWK
                       ELSE
                            IF   FS-LOTEWK < "10"
                                 ADD 1 TO LD-LOTEWK
                                 DISPLAY T-LD-LOTEWK
                                 PERFORM 140-IMPORTAR THRU 140-99-FIM
                            END-IF
                       END-IF
                    END-PERFORM
                END-IF
           END-EVALUATE.

       100-99-FIM. EXIT.

       110-CRITICAR.

           INITIALIZE LOTEWK-REG
           MOVE SPACES TO INDICA-DBCR
           MOVE 0      TO ERROS

           IF   CBFOPC-I (01) = 0
                MOVE    1          TO ERRO
                PERFORM 120-ERRO THRU 120-99-FIM
           ELSE
                MOVE CBFOPC-I (01)     TO P
                MOVE CBFOPC-F (01)     TO S
                MOVE 0                 TO ABRE-NUMERO
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO-X (4: DGC)
                MOVE ABRE-NUMERO       TO CBPLCO-CONTA
                IF   ABRE-NUMERO NUMERIC
                     PERFORM TEST AFTER UNTIL FS-CBPLCO NOT = "9D"
                             READ CBPLCO
                             IF  FS-CBPLCO = "9D"
                                 CALL "CWISAM" USING FS-CBPLCO
                             END-IF
                     END-PERFORM
                     COMPUTE P = P + S - 1
                     MOVE LOTE-I-REG (P: 1) TO DV
                     MOVE "E"               TO CB002PCW-FUNCAO
                     CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                     IF   (DGC + 1) = S
                     AND  CB002PCW-LANCAVEL = "S"
                          MOVE CBPLCO-CONTA  TO CB002PCW-CONTA
                          MOVE "C"          TO CB002PCW-FUNCAO
                          CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                     END-IF
                END-IF
                IF  (FS-CBPLCO < "10")
                OR  ((DGC + 1) NOT = S)
                OR  ((CB002PCW-DV NOT = DV)
                     AND (CB002PCW-LANCAVEL = "S"))
                OR  (ABRE-NUMERO NOT NUMERIC)
                OR  (ABRE-NUMERO = 0)
                     MOVE    1          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE ABRE-NUMERO TO LOTEWK-CONTA
                END-IF
           END-IF

           IF   CBFOPC-I (03) NOT = 0
                MOVE CBFOPC-I (03)      TO P
                MOVE CBFOPC-F (03)      TO S
                MOVE LOTE-I-REG (P: S) TO LOTEWK-DESCRICAO
           END-IF

           IF   LOTEWK-DESCRICAO = SPACES
                MOVE    3          TO ERRO
                PERFORM 120-ERRO THRU 120-99-FIM
           END-IF

           IF   ERROS = 0
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
           IF   CBFOPC-I (ERRO) NOT = 0
                MOVE CBFOPC-I (ERRO) TO P
                MOVE CBFOPC-F (ERRO) TO S
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
                CLOSE CBFOPC CBPLCO CBPAPC CBCTCR LOTE-I LOTEWK
                GOBACK
           END-IF.

       120-99-FIM. EXIT.

       140-IMPORTAR.

           MOVE LOTEWK-CONTA             TO CBPLCO-CONTA
                                            CB002PCW-CONTA
           MOVE LOTEWK-DESCRICAO         TO CBPLCO-DESCRICAO
           MOVE "C"                      TO CB002PCW-FUNCAO
           CALL "CB002PCW"            USING PARAMETROS-CB002PCW
           MOVE "E"                      TO CB002PCW-FUNCAO
           CALL "CB002PCW"            USING PARAMETROS-CB002PCW
           MOVE ZERO                     TO CBPLCO-COD-RED
           IF   CB002PCW-LANCAVEL = "S"
                MOVE ZERO TO CBCTCR-COD-RED
                READ CBCTCR
                IF   FS-CBCTCR = "23"
                     MOVE 0 TO CBCTCR-COD-RED
                               CBCTCR-ULTIMO
                     WRITE CBCTCR-REG
                END-IF
                READ CBCTCR NEXT RECORD IGNORE LOCK
                IF   FS-CBCTCR < "10"
                     MOVE CBCTCR-COD-RED TO CBPLCO-COD-RED
                     DELETE CBCTCR RECORD
                ELSE
                     MOVE ZERO TO CBCTCR-COD-RED
                     READ CBCTCR
                     ADD  1             TO CBCTCR-ULTIMO
                     MOVE CBCTCR-ULTIMO TO CBPLCO-COD-RED
                     REWRITE CBCTCR-REG
                END-IF
           END-IF
           MOVE "S" TO CBPLCO-VIRGEM
           ADD 1 TO GR-CBPLCO
           DISPLAY T-GR-CBPLCO
           WRITE CBPLCO-REG
           IF   FS-CBPLCO >  "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF.

       140-99-FIM. EXIT.

       800-INICIAIS.

           CALL "CWFILE" USING LB-LOTEWK
           OPEN OUTPUT LOTEWK
           DISPLAY CTAC-LIT-CB025PCW
           PERFORM TEST AFTER UNTIL FS-LOTE-I = "00"
                   DISPLAY "<Esc>-Fim" LINE 23 COLUMN 03
                   CLOSE LOTE-I
                   ACCEPT CTAC-VAR-CB025PCW
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
           MOVE "CB0025A"               TO CWIMPR-REPORT

           OPEN INPUT CBFOPC
           IF   FS-CBFOPC > "09"
                GOBACK
           END-IF

           OPEN INPUT CBPAPC
           IF   FS-CBPAPC > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           ELSE
                READ CBPAPC
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

           OPEN I-O CBPLCO
           IF   FS-CBPLCO >  "10"
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
                        MOVE PONTEIRO (PG) TO CBFOPC-FORMATO
                        START CBFOPC KEY NOT LESS CBFOPC-CHAVE
                        SUBTRACT 1 FROM PG
                        MOVE 9 TO CWBOXS-OPTION
                   END-IF
                   IF   CWBOXS-OPTION = 0
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
           END-PERFORM

           MOVE CWBOXS-TEXT   (CWBOXS-OPTION) (2: ) TO CBFOPC-CHAVE
           READ CBFOPC

           DISPLAY "Formato: " LINE 10 COLUMN 03
                   CBFOPC-FORMATO " - " CBFOPC-COMENTARIO

           OPEN I-O CBCTCR

           DISPLAY CTAC-LIT-CB0025C
                   CTAC-VAR-CB0025C.

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
                       OR FS-CBFOPC > "09"
              READ CBFOPC NEXT RECORD IGNORE LOCK
              IF   FS-CBFOPC < "10"
                   ADD 1 TO LIMITE
                   IF   PONTEIRO (PG) = SPACES
                        MOVE CBFOPC-FORMATO TO PONTEIRO (PG)
                   END-IF
                   MOVE CBFOPC-FORMATO    TO CWBOXS-TEXT
                                             (LIMITE) (2: )
                   IF   CBFOPC-COMENTARIO NOT = SPACES
                        MOVE " - "             TO CWBOXS-TEXT
                                                  (LIMITE) (9: 3)
                        MOVE CBFOPC-COMENTARIO TO CWBOXS-TEXT
                                                  (LIMITE) (12: )
                   END-IF
              END-IF
           END-PERFORM

           IF   LIMITE = 8
                READ CBFOPC NEXT RECORD IGNORE LOCK
                IF   FS-CBFOPC < "10"
                     ADD  1              TO LIMITE
                     MOVE " Mais op‡äes" TO CWBOXS-TEXT   (LIMITE)
                     READ CBFOPC PREVIOUS RECORD IGNORE LOCK
                END-IF
                MOVE 9 TO CWBOXS-OPTION
           ELSE
                MOVE 1 TO CWBOXS-OPTION
           END-IF.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOPC CBPLCO LOTEWK CBCTCR LOTE-I

           IF   ERROS-GERAL NOT = 0
                MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
           END-IF

           DELETE FILE LOTEWK.

       900-99-FIM. EXIT.

       END PROGRAM CB025PCW.
