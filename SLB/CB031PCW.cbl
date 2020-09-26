       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB031PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  10/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Importa historicos padrao                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCAHISL REPLACING AUTOMATIC BY MANUAL.
       COPY CBFOHISL.

           SELECT LOTE-I ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOTE-I.

           SELECT LOTEWK ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-LOTEWK.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCAHIFD.
       COPY CBFOHIFD.

       FD  LOTE-I
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTE-I.

       01  LOTE-I-REG.
           05 LOTE-I-BYE PIC X(001) OCCURS 300.

       FD  LOTEWK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTEWK.

       01  LOTEWK-REG.
           05 LOTEWK-CODIGO                    PIC  9(004).
           05 LOTEWK-DESCRICAO                 PIC  X(030).

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
           05 GR-CBCAHI                PIC  9(006) VALUE 0.
           05 GR-LOTEWK                PIC  9(006) VALUE 0.
           05 LD-LOTEWK                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
              88 EFETIVAR    VALUE "S" "s".
           05 ER-CBFOHI.
              10 FS-CBFOHI              PIC  X(002) VALUE "00".
              10 LB-CBFOHI              PIC  X(050) VALUE "CBFOHI".
           05 ER-CBCAHI.
              10 FS-CBCAHI              PIC  X(002) VALUE "00".
              10 LB-CBCAHI              PIC  X(050) VALUE "CBCAHI".
           05 ER-LOTE-I.
              10 FS-LOTE-I             PIC  X(002) VALUE "00".
              10 LB-LOTE-I             PIC  X(050) VALUE "LOTE-I.TXT".
           05 ER-LOTEWK.
              10 FS-LOTEWK             PIC  X(002) VALUE "00".
              10 LB-LOTEWK             PIC  X(050) VALUE "LOTEWK".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.
           05 MENSAGENS.
              10 PIC X(19) VALUE "CODIGO             ".
              10 PIC X(19) VALUE "DESCRICAO          ".
           05 REDEFINES MENSAGENS.
              10 MSG OCCURS 2  PIC X(19).

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

       COPY CWBOXS.
       COPY CWIMPR.

       SCREEN SECTION.

       01  CTAC-LIT-CB031PCW.
           05 LINE 08 COLUMN 03 VALUE "Nome do arquivo a importar:".

       01  CTAC-VAR-CB031PCW.
           05 LINE 08 COLUMN 31 PIC X(048) USING LB-LOTE-I.

       01  CTAC-LIT-CB0031C.
           05 LINE 16 COLUMN 10 VALUE "Lidos de".
           05 LINE 17 COLUMN 10 VALUE "gravados em".
           05 LINE 19 COLUMN 10 VALUE "Lidos de".
           05 LINE 20 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0031C.
           05 T-LD-LOTE-I LINE 16 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTE-I.
           05 LINE 16 COLUMN 19 PIC X(050) FROM LB-LOTE-I.
           05 T-GR-LOTEWK LINE 17 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTEWK.
           05 LINE 17 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-LD-LOTEWK LINE 19 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-LOTEWK.
           05 LINE 19 COLUMN 22 PIC X(050) FROM LB-LOTEWK.
           05 T-GR-CBCAHI LINE 20 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-CBCAHI.
           05 LINE 20 COLUMN 22 PIC X(050) FROM LB-CBCAHI.

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
                       IF   FS-LOTEWK < "10"
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

           IF   CBFOHI-I (01) = 0
                MOVE    1          TO ERRO
                PERFORM 120-ERRO THRU 120-99-FIM
           ELSE
                MOVE CBFOHI-I (01)     TO P
                MOVE CBFOHI-F (01)     TO S
                MOVE LOTE-I-REG (P: S) TO ABRE-NUMERO
                MOVE ABRE-NUMERO       TO CBCAHI-CODIGO
                IF   ABRE-NUMERO NUMERIC
                     READ CBCAHI
                END-IF
                IF  (FS-CBCAHI < "10")
                OR  (ABRE-NUMERO NOT NUMERIC)
                OR  (ABRE-NUMERO = 0)
                     MOVE    1          TO ERRO
                     PERFORM 120-ERRO THRU 120-99-FIM
                ELSE
                     MOVE ABRE-NUMERO TO LOTEWK-CODIGO
                END-IF
           END-IF

           IF   CBFOHI-I (02) NOT = 0
                MOVE CBFOHI-I (02)     TO P
                MOVE CBFOHI-F (02)     TO S
                MOVE LOTE-I-REG (P: S) TO LOTEWK-DESCRICAO
           END-IF

           IF   LOTEWK-DESCRICAO = SPACES
                MOVE    2          TO ERRO
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
           IF   CBFOHI-I (ERRO) NOT = 0
                MOVE CBFOHI-I (ERRO) TO P
                MOVE CBFOHI-F (ERRO) TO S
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
                CLOSE CBFOHI CBCAHI LOTE-I LOTEWK
                GOBACK
           END-IF.

       120-99-FIM. EXIT.

       140-IMPORTAR.

           MOVE LOTEWK-CODIGO             TO CBCAHI-CODIGO
           MOVE LOTEWK-DESCRICAO         TO CBCAHI-DESCRICAO
           WRITE CBCAHI-REG
           IF   FS-CBCAHI >  "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF.

       140-99-FIM. EXIT.

       800-INICIAIS.

           OPEN OUTPUT LOTEWK
           DISPLAY CTAC-LIT-CB031PCW
           PERFORM TEST AFTER UNTIL FS-LOTE-I = "00"
                   DISPLAY "<Esc>-Fim" LINE 23 COLUMN 03
                   CLOSE LOTE-I
                   ACCEPT CTAC-VAR-CB031PCW
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
           MOVE "CB0031A"               TO CWIMPR-REPORT

           OPEN INPUT CBFOHI
           IF   FS-CBFOHI > "09"
                CALL "CWISAM" USING ER-CBFOHI
                GOBACK
           END-IF

           CALL "CWFILE" USING LB-CBCAHI

           OPEN I-O CBCAHI
           IF   FS-CBCAHI >  "10"
                CALL "CWISAM" USING ER-CBCAHI
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
                        MOVE PONTEIRO (PG) TO CBFOHI-FORMATO
                        START CBFOHI KEY NOT LESS CBFOHI-CHAVE
                        SUBTRACT 1 FROM PG
                        MOVE 9 TO CWBOXS-OPTION
                   END-IF
                   IF   CWBOXS-OPTION = 0
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
           END-PERFORM

           MOVE CWBOXS-TEXT   (CWBOXS-OPTION) (2: ) TO CBFOHI-CHAVE
           READ CBFOHI

           DISPLAY "Formato: " LINE 10 COLUMN 03
                   CBFOHI-FORMATO " - " CBFOHI-COMENTARIO

           DISPLAY CTAC-LIT-CB0031C
                   CTAC-VAR-CB0031C.

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
                       OR FS-CBFOHI > "09"
              READ CBFOHI NEXT RECORD IGNORE LOCK
              IF   FS-CBFOHI < "10"
                   ADD 1 TO LIMITE
                   IF   PONTEIRO (PG) = SPACES
                        MOVE CBFOHI-FORMATO TO PONTEIRO (PG)
                   END-IF
                   MOVE CBFOHI-FORMATO    TO CWBOXS-TEXT
                                             (LIMITE) (2: )
                   IF   CBFOHI-COMENTARIO NOT = SPACES
                        MOVE " - "             TO CWBOXS-TEXT
                                                  (LIMITE) (9: 3)
                        MOVE CBFOHI-COMENTARIO TO CWBOXS-TEXT
                                                  (LIMITE) (12: )
                   END-IF
              END-IF
           END-PERFORM

           IF   LIMITE = 8
                READ CBFOHI NEXT RECORD IGNORE LOCK
                IF   FS-CBFOHI < "10"
                     ADD  1              TO LIMITE
                     MOVE " Mais op‡äes" TO CWBOXS-TEXT   (LIMITE)
                     READ CBFOHI PREVIOUS RECORD IGNORE LOCK
                END-IF
                MOVE 9 TO CWBOXS-OPTION
           ELSE
                MOVE 1 TO CWBOXS-OPTION
           END-IF.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOHI CBCAHI LOTEWK LOTE-I

           IF   ERROS-GERAL NOT = 0
                MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
                CALL "CWIMPR" USING PARAMETROS-CWIMPR
           END-IF

           DELETE FILE LOTEWK.

       900-99-FIM. EXIT.

       END PROGRAM CB031PCW.
