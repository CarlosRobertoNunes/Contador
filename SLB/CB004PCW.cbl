       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB004PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/01/91.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao dos historicos padrao             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCAHISL.
       COPY CBHIVASL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCAHIFD.
       COPY CBHIVAFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 ws-OPTION                PIC  9(002) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help ".
           05 RODAPE-INCLUSAO          PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help".
           05 RODAPE-PAGINAVEL         PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help PgDn-Pr¢ximo PgUp-Anterior ".
           05 CAMPO                    PIC  9(002) VALUE 0.
           05 ANTERIOR                 PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 RE-START                 PIC  X(001) VALUE "N".
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 I                        PIC  9(002) VALUE ZERO.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Entre com os dados            ".
              10 PIC X(30) VALUE "N∆o cadastrado                ".
              10 PIC X(30) VALUE "Confirme exclus∆o             ".
              10 PIC X(30) VALUE "Hist¢rico j† cadastrado       ".
              10 PIC X(30) VALUE "C¢digo zerado                 ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 6 PIC X(30).
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 HISTORICOS-VARIAVEIS VALUE SPACES.
              10 DESCR-01 PIC X(030).
              10 DESCR-02 PIC X(030).
              10 DESCR-03 PIC X(030).
              10 DESCR-04 PIC X(030).
              10 DESCR-05 PIC X(030).
              10 DESCR-06 PIC X(030).
              10 DESCR-07 PIC X(030).
              10 DESCR-08 PIC X(030).
              10 DESCR-09 PIC X(030).
              10 DESCR-10 PIC X(030).
              10 DESCR-11 PIC X(030).
              10 DESCR-12 PIC X(030).
              10 DESCR-13 PIC X(030).
              10 DESCR-14 PIC X(030).
              10 DESCR-15 PIC X(030).
              10 DESCR-16 PIC X(030).
              10 DESCR-17 PIC X(030).
              10 DESCR-18 PIC X(030).
              10 DESCR-19 PIC X(030).
              10 DESCR-20 PIC X(030).
              10 DESCR-21 PIC X(030).
              10 DESCR-22 PIC X(030).
              10 DESCR-23 PIC X(030).
           05 REDEFINES HISTORICOS-VARIAVEIS.
              10 DESCR OCCURS 23 PIC X(030).

       COPY CWFUNC.

       SCREEN SECTION.

       01  CB004PA.
       03  CONSTANTES.
           05 LINE 08 COLUMN 03 VALUE "C¢digo   :".
           05 LINE 10 COLUMN 03 VALUE "Descriá∆o:".
       03  CB004PB.
           05 CODIGO
              LINE 08 COLUMN 14 PIC Z(004) USING CBCAHI-CODIGO.
       04  DESCRICOES.
           05 DESCRICAO
                  LINE 10 COLUMN 14 PIC X(030) USING CBCAHI-DESCRICAO.
           05 D01 LINE 11 COLUMN 14 PIC X(030) USING DESCR-01.
           05 D02 LINE 12 COLUMN 14 PIC X(030) USING DESCR-02.
           05 D03 LINE 13 COLUMN 14 PIC X(030) USING DESCR-03.
           05 D04 LINE 14 COLUMN 14 PIC X(030) USING DESCR-04.
           05 D05 LINE 15 COLUMN 14 PIC X(030) USING DESCR-05.
           05 D06 LINE 16 COLUMN 14 PIC X(030) USING DESCR-06.
           05 D07 LINE 17 COLUMN 14 PIC X(030) USING DESCR-07.
           05 D08 LINE 18 COLUMN 14 PIC X(030) USING DESCR-08.
           05 D09 LINE 19 COLUMN 14 PIC X(030) USING DESCR-09.
           05 D10 LINE 20 COLUMN 14 PIC X(030) USING DESCR-10.
           05 D11 LINE 08 COLUMN 46 PIC X(030) USING DESCR-11.
           05 D12 LINE 09 COLUMN 46 PIC X(030) USING DESCR-12.
           05 D13 LINE 10 COLUMN 46 PIC X(030) USING DESCR-13.
           05 D14 LINE 11 COLUMN 46 PIC X(030) USING DESCR-14.
           05 D15 LINE 12 COLUMN 46 PIC X(030) USING DESCR-15.
           05 D16 LINE 13 COLUMN 46 PIC X(030) USING DESCR-16.
           05 D17 LINE 14 COLUMN 46 PIC X(030) USING DESCR-17.
           05 D18 LINE 15 COLUMN 46 PIC X(030) USING DESCR-18.
           05 D19 LINE 16 COLUMN 46 PIC X(030) USING DESCR-19.
           05 D20 LINE 17 COLUMN 46 PIC X(030) USING DESCR-20.
           05 D21 LINE 18 COLUMN 46 PIC X(030) USING DESCR-21.
           05 D22 LINE 19 COLUMN 46 PIC X(030) USING DESCR-22.
           05 D23 LINE 20 COLUMN 46 PIC X(030) USING DESCR-23.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FINALIZAR
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           IF   PARAR
                STOP RUN
           ELSE
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           IF   FL-EXIT EQUAL 1
                MOVE SPACE TO FUNCAO
                EXEC COBOLware Option
                     Function FUNCAO
                END-EXEC
                MOVE ZERO  TO FL-EXIT ws-OPTION
                IF   NOT INCLUSAO
                     MOVE "S" TO RE-START
                END-IF
           END-IF

           IF   VEZ = 1
                MOVE 2 TO VEZ
                IF   NOT FINALIZAR
                     DISPLAY CB004PA
                END-IF
           END-IF

           MOVE "23" TO FS-CBCAHI

           IF   NOT FINALIZAR
                IF   INCLUSAO
                     MOVE ZERO   TO CBCAHI-CODIGO
                     MOVE SPACES TO CBCAHI-DESCRICAO
                                    HISTORICOS-VARIAVEIS
                END-IF
                MOVE SPACES TO  MENSAGEM-ERRO
                DISPLAY CB004PB
                PERFORM 140-LER-CBCAHI  THRU 140-99-FIM
                        UNTIL FS-CBCAHI < "10"
                        OR    FL-EXIT EQUAL 1
                DISPLAY LINHA-BRANCA LINE 23 COLUMN 3
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT EQUAL 1
                     MOVE    MSG (1)            TO MENSAGEM-ERRO
                     MOVE    SPACE              TO COMANDO
                     PERFORM 130-CRITICA      THRU 130-99-FIM
                             UNTIL MENSAGEM-ERRO EQUAL SPACES
                             OR    ABORTAR
                ELSE
                     IF   FL-EXIT NOT EQUAL 1
                     AND  EXCLUSAO
                          DISPLAY MSG (3) LINE 23 COLUMN 3
                          MOVE    SPACE TO COMANDO
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                     END-IF
                END-IF
           END-IF

           MOVE "00" TO FS-CBCAHI

           IF   EFETIVAR
           AND  NOT FINALIZAR
                IF   INCLUSAO
                     PERFORM 180-SALVA THRU 180-99-FIM
                     WRITE CBCAHI-REG
                ELSE
                     IF   EXCLUSAO
                          PERFORM 190-MATA THRU 190-99-FIM
                          DELETE CBCAHI RECORD
                     ELSE
                          PERFORM 190-MATA THRU 190-99-FIM
                          PERFORM 180-SALVA THRU 180-99-FIM
                          REWRITE CBCAHI-REG
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO
           MOVE    0          TO TECLA CAMPO
           PERFORM TEST AFTER UNTIL (CBCAHI-DESCRICAO NOT = SPACES
                                 AND CAMPO > 22)
                                 OR ESC
                   DISPLAY RODAPE-INCLUSAO
                           LINE 23 COLUMN 03
                   EVALUATE CAMPO
                       WHEN 00 ACCEPT DESCRICAO
                       WHEN 01 ACCEPT D01
                       WHEN 02 ACCEPT D02
                       WHEN 03 ACCEPT D03
                       WHEN 04 ACCEPT D04
                       WHEN 05 ACCEPT D05
                       WHEN 06 ACCEPT D06
                       WHEN 07 ACCEPT D07
                       WHEN 08 ACCEPT D08
                       WHEN 09 ACCEPT D09
                       WHEN 10 ACCEPT D10
                       WHEN 11 ACCEPT D11
                       WHEN 12 ACCEPT D12
                       WHEN 13 ACCEPT D13
                       WHEN 14 ACCEPT D14
                       WHEN 15 ACCEPT D15
                       WHEN 16 ACCEPT D16
                       WHEN 17 ACCEPT D17
                       WHEN 18 ACCEPT D18
                       WHEN 19 ACCEPT D19
                       WHEN 20 ACCEPT D20
                       WHEN 21 ACCEPT D21
                       WHEN 22 ACCEPT D22
                       WHEN 23 ACCEPT D23
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   EVALUATE TRUE
                       WHEN F1
                            EXEC COBOLware Help
                                 FILE   "CB004PCW.H02"
                                 LINE   11 COLUMN 14
                                 HEIGHT 8
                                 WIDTH  42
                            END-EXEC
                       WHEN CURSOR-UP
                        AND CAMPO > 0
                            SUBTRACT 1 FROM CAMPO
                       WHEN CURSOR-DOWN
                         OR ENTER-KEY
                            IF   CAMPO > 1
                                 COMPUTE ANTERIOR = CAMPO - 1
                                 IF  DESCR (CAMPO)    = SPACES
                                 AND DESCR (ANTERIOR) = SPACES
                                     MOVE 23 TO CAMPO
                                 END-IF
                            END-IF
                            ADD 1 TO CAMPO
                            IF   CBCAHI-DESCRICAO = SPACES
                                 MOVE 0 TO CAMPO
                            END-IF
                   END-EVALUATE
           END-PERFORM
           IF   ESC
                MOVE "A" TO COMANDO
           ELSE
                DISPLAY CB004PB
                MOVE    SPACE               TO COMANDO
                PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
           END-IF.

       130-99-FIM.  EXIT.

       140-LER-CBCAHI.

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           IF   MENSAGEM-ERRO EQUAL SPACES
                IF   NOT INCLUSAO
                     DISPLAY RODAPE-PAGINAVEL LINE 23 COLUMN 03
                END-IF
           ELSE
                MOVE SPACES TO MENSAGEM-ERRO
           END-IF

           EVALUATE TRUE
               WHEN RE-START = "S"
               AND  NOT INCLUSAO
                    EXEC COBOLware BoxSelect
                         TITLE "Pesquisar:"
                         LINE 08 COLUMN 10
                         TEXT(1) "~C¢digo"
                         TEXT(2) "~Descriá∆o"
                         OPTION 1;ws-OPTION
                    END-EXEC
                    IF   ws-OPTION = 0
                         MOVE 1 TO FL-EXIT
                    END-IF
                    MOVE "N"  TO RE-START
                    IF   ws-OPTION = 1
                         PERFORM TEST AFTER UNTIL NOT F1
                                 ACCEPT CODIGO
                                 PERFORM 145-TECLA THRU 145-99-FIM
                         END-PERFORM
                         START CBCAHI  KEY NOT LESS CBCAHI-CHAVE
                    ELSE
                         PERFORM TEST AFTER UNTIL NOT F1
                                 ACCEPT DESCRICAO
                                 PERFORM 145-TECLA THRU 145-99-FIM
                         END-PERFORM
                         START CBCAHI  KEY NOT LESS CBCAHI-DESCRICAO
                    END-IF
                    READ CBCAHI NEXT RECORD
                    PERFORM 200-CARREGA     THRU 200-99-FIM
                    DISPLAY CB004PB
                    EXIT PARAGRAPH
               WHEN INCLUSAO
                    DISPLAY RODAPE          LINE 23 COLUMN 03
                    MOVE 9999 TO CBCAHI-CODIGO
                    START CBCAHI KEY NOT GREATER CBCAHI-CHAVE
                    READ CBCAHI PREVIOUS RECORD IGNORE LOCK
                    ADD 1 TO CBCAHI-CODIGO
                    IF   CBCAHI-CODIGO = 0
                         ADD 1 TO CBCAHI-CODIGO
                    END-IF
                    INITIALIZE CBCAHI-DESCRICAO
                    ACCEPT CODIGO
               WHEN ws-OPTION = 1
                    DISPLAY RODAPE-PAGINAVEL LINE 23 COLUMN 03
                    ACCEPT CODIGO
               WHEN OTHER
                    ACCEPT DESCRICAO
           END-EVALUATE

           PERFORM 145-TECLA THRU 145-99-FIM

           IF  ESC
               MOVE 1 TO FL-EXIT.

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           IF   FL-EXIT NOT EQUAL 1
                IF   (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                     IF   PAGE-DOWN
                          READ CBCAHI NEXT RECORD
                     ELSE
                          READ CBCAHI PREVIOUS RECORD
                     END-IF
                     IF   FS-CBCAHI < "10"
                          PERFORM 200-CARREGA     THRU 200-99-FIM
                          DISPLAY CB004PB
                          EXIT PARAGRAPH
                     ELSE
                          MOVE "44" TO FS-CBCAHI
                     END-IF
                ELSE
                     IF   INCLUSAO
                     OR   ws-OPTION = 1
                          READ CBCAHI
                     ELSE
                          READ CBCAHI KEY IS CBCAHI-DESCRICAO
                     END-IF
                END-IF
                IF   FS-CBCAHI < "10"
                     PERFORM 200-CARREGA     THRU 200-99-FIM
                     DISPLAY CB004PB
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBCAHI
                     END-IF
                ELSE
                     IF   NOT INCLUSAO
                          MOVE MSG (2) TO MENSAGEM-ERRO
                     ELSE
                          IF   CBCAHI-CODIGO NOT = 0
                               MOVE "00" TO FS-CBCAHI
                               DISPLAY CB004PB
                          ELSE
                               MOVE "44"    TO FS-CBCAHI
                               MOVE MSG (5) TO MENSAGEM-ERRO
                          END-IF
                     END-IF
                END-IF
           ELSE
                MOVE "00"   TO FS-CBCAHI
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF.

       140-99-FIM. EXIT.

       145-TECLA.

           ACCEPT TECLA FROM ESCAPE KEY
           IF   F1
                IF   INCLUSAO
                OR   ws-OPTION = 1
                     EXEC COBOLware Help
                          FILE   "CB004PCW.H01"
                          LINE   09
                          COLUMN 14
                          HEIGHT 8
                          WIDTH  42
                     END-EXEC
                ELSE
                     EXEC COBOLware Help
                          FILE   "CB004PCW.H02"
                          LINE   11
                          COLUMN 14
                          HEIGHT 8
                          WIDTH  42
                     END-EXEC
                END-IF
           END-IF.

       145-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       180-SALVA.

           MOVE 0 TO Y

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
                   IF   DESCR (I) NOT = SPACES
                        MOVE 1             TO CBHIVA-TIPO
                        MOVE CBCAHI-CODIGO TO CBHIVA-CODIGO
                        ADD  1             TO Y
                        MOVE Y             TO CBHIVA-VARIAVEL
                        MOVE DESCR (I)     TO CBHIVA-DESCRICAO
                        WRITE CBHIVA-REG
                   END-IF
           END-PERFORM.

       180-99-FIM. EXIT.

       190-MATA.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
                   MOVE 1             TO CBHIVA-TIPO
                   MOVE CBCAHI-CODIGO TO CBHIVA-CODIGO
                   MOVE I             TO CBHIVA-VARIAVEL
                   READ CBHIVA
                   IF   FS-CBHIVA < "10"
                        DELETE CBHIVA RECORD
                   END-IF
           END-PERFORM.

       190-99-FIM. EXIT.

       200-CARREGA.

           MOVE "00"   TO FS-CBHIVA
           MOVE SPACES TO HISTORICOS-VARIAVEIS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
                                            OR FS-CBHIVA > "09"
                   MOVE 1             TO CBHIVA-TIPO
                   MOVE CBCAHI-CODIGO TO CBHIVA-CODIGO
                   MOVE I             TO CBHIVA-VARIAVEL
                   READ CBHIVA
                   IF   FS-CBHIVA < "09"
                        MOVE CBHIVA-DESCRICAO TO DESCR (I)
                   END-IF
           END-PERFORM.

       200-99-FIM. EXIT.

       800-INICIAIS.

           OPEN I-O CBCAHI
                    CBHIVA.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCAHI CBHIVA.

       900-99-FIM. EXIT.

       END PROGRAM CB004PCW.
