       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB028PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exporta centros de custos                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBFOCCSL.

           SELECT LOTE-E ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOTE-E.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBFOCCFD.

       FD  LOTE-E
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOTE-E.

       01  LOTE-E-REG.
           05 LOTE-E-BYE PIC X(001) OCCURS 300.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LB-HELP                  PIC  X(012) VALUE "CB028PCW.HXX".
           05 I-1                      PIC  9(001) VALUE ZERO.
           05 I-X REDEFINES I-1        PIC  X(001).
           05 ABRE-NUMERO              PIC  9(018) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 MSG                      PIC  X(050) VALUE SPACES.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 0.
           05 P                        PIC  9(004) VALUE 0.
           05 S                        PIC  9(004) VALUE 0.
           05 S2                       PIC  9(004) VALUE 0.
           05 LIMITE                   PIC  9(002) VALUE 0.
           05 PG                       PIC  9(002) VALUE 0.
           05 CONTA-SELE               PIC  9(004) VALUE 0.
           05 LD-CBCACC                 PIC  9(006) VALUE 0.
           05 GR-LOTE-E                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
           05 ER-CBFOCC.
              10 FS-CBFOCC              PIC  X(002) VALUE "00".
              10 LB-CBFOCC              PIC  X(050) VALUE "CBFOCC".
           05 ER-CBCACC.
              10 FS-CBCACC              PIC  X(002) VALUE "00".
              10 LB-CBCACC              PIC  X(050) VALUE "CBCACC".
           05 ER-LOTE-E.
              10 FS-LOTE-E             PIC  X(002) VALUE "00".
              10 LB-LOTE-E             PIC  X(050) VALUE "LOTE-E.TXT".
           05 PONTEIROS VALUE SPACES.
              10 PONTEIRO              PIC X(008) OCCURS 100.

       COPY CWBOXS.

       SCREEN SECTION.

       01  CTAC-LIT-CB028PCW.
           05 LINE 08 COLUMN 03 VALUE "Nome do arquivo a ge".
           05 LINE 08 COLUMN 23 VALUE "rar :".

       01  CTAC-VAR-CB028PCW.
           05 LINE 08 COLUMN 29 PIC X(050) USING LB-LOTE-E.

       01  CTAC-LIT-CB0028C.
           05 LINE 10 COLUMN 10 VALUE "Lidos de".
           05 LINE 11 COLUMN 10 VALUE "gravados em".

       01  CTAC-VAR-CB0028C.
           05 T-LD-CBCACC LINE 10 COLUMN 02 PIC ZZZ.ZZ9 FROM LD-CBCACC.
           05 LINE 10 COLUMN 19 PIC X(010) FROM LB-CBCACC.
           05 T-GR-LOTE-E LINE 11 COLUMN 02 PIC ZZZ.ZZ9 FROM GR-LOTE-E.
           05 LINE 11 COLUMN 22 PIC X(050) FROM LB-LOTE-E.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FS-CBCACC = "10"
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           READ CBCACC NEXT RECORD IGNORE LOCK

           IF   FS-CBCACC < "10"
                ADD 1 TO LD-CBCACC
                DISPLAY T-LD-CBCACC
                PERFORM 110-EXPORTAR THRU 110-99-FIM
           END-IF.

       100-99-FIM. EXIT.

       110-EXPORTAR.

           ADD 1 TO GR-LOTE-E
           DISPLAY T-GR-LOTE-E
           MOVE SPACES TO LOTE-E-REG

           IF   CBFOCC-I (01) NOT = 0
                MOVE CBFOCC-I (01) TO P
                MOVE CBFOCC-F (01) TO S
                MOVE CBCACC-CODIGO TO ABRE-NUMERO
                COMPUTE S2 = 18 - S + 1
                MOVE ABRE-NUMERO (S2: S) TO LOTE-E-REG (P: S)
            END-IF

           IF   CBFOCC-I (02) NOT = 0
                MOVE CBFOCC-I (02)    TO P
                MOVE CBFOCC-F (02)    TO S
                MOVE CBCACC-DESCRICAO TO LOTE-E-REG (P: S)
            END-IF

           WRITE LOTE-E-REG
           IF   FS-LOTE-E > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                GOBACK
           END-IF.

       110-99-FIM. EXIT.

       800-INICIAIS.

           DISPLAY CTAC-LIT-CB028PCW
           PERFORM TEST AFTER UNTIL FS-LOTE-E = "00"
                   DISPLAY "<Esc>-Fim" LINE 23 COLUMN 03
                   CLOSE LOTE-E
                   ACCEPT CTAC-VAR-CB028PCW
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

           OPEN INPUT CBFOCC
           IF   FS-CBFOCC > "09"
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
                        MOVE PONTEIRO (PG) TO CBFOCC-FORMATO
                        START CBFOCC KEY NOT LESS CBFOCC-CHAVE
                        SUBTRACT 1 FROM PG
                        MOVE 9 TO CWBOXS-OPTION
                   END-IF
                   IF   CWBOXS-OPTION = 0
                        PERFORM 900-FINAIS THRU 900-99-FIM
                        GOBACK
                   END-IF
           END-PERFORM

           MOVE CWBOXS-TEXT   (CWBOXS-OPTION) (2: ) TO CBFOCC-CHAVE
           READ CBFOCC

           DISPLAY "Formato: " LINE 09 COLUMN 03
                   CBFOCC-FORMATO " - " CBFOCC-COMENTARIO

           DISPLAY CTAC-LIT-CB0028C
                   CTAC-VAR-CB0028C.

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
                       OR FS-CBFOCC > "09"
              READ CBFOCC NEXT RECORD IGNORE LOCK
              IF   FS-CBFOCC < "10"
                   ADD 1 TO LIMITE
                   IF   PONTEIRO (PG) = SPACES
                        MOVE CBFOCC-FORMATO TO PONTEIRO (PG)
                   END-IF
                   MOVE CBFOCC-FORMATO    TO CWBOXS-TEXT
                                             (LIMITE) (2: )
                   IF   CBFOCC-COMENTARIO NOT = SPACES
                        MOVE " - "             TO CWBOXS-TEXT
                                                  (LIMITE) (9: 3)
                        MOVE CBFOCC-COMENTARIO TO CWBOXS-TEXT
                                                  (LIMITE) (12: )
                   END-IF
              END-IF
           END-PERFORM

           IF   LIMITE = 8
                READ CBFOCC NEXT RECORD IGNORE LOCK
                IF   FS-CBFOCC < "10"
                     ADD  1              TO LIMITE
                     MOVE " Mais op‡äes" TO CWBOXS-TEXT   (LIMITE)
                     READ CBFOCC PREVIOUS RECORD IGNORE LOCK
                END-IF
                MOVE 9 TO CWBOXS-OPTION
           ELSE
                MOVE 1 TO CWBOXS-OPTION
           END-IF.

       810-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOCC CBCACC LOTE-E.

       900-99-FIM. EXIT.

       END PROGRAM CB028PCW.
