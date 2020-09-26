       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB011PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao dos centros de custos             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL REPLACING MANUAL BY AUTOMATIC.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 RODAPE                   PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help ".
           05 RODAPE-INCLUSAO          PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help ".
           05 RODAPE-PAGINAVEL         PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help PgDn-Pr¢ximo PgUp-Anterior ".
           05 RE-START                 PIC  X(001) VALUE "N".
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 I                        PIC  9(002) VALUE ZERO.
           05 Y                        PIC  9(002) VALUE ZERO.
           05 ws-OPTION                PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "N∆o cadastrado                ".
              10 F PIC X(30) VALUE "Confirme exclus∆o             ".
              10 F PIC X(30) VALUE "Centro de custos j† cadastrado".
              10 F PIC X(30) VALUE "C¢digo zerado                 ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 5 PIC X(30).
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".

       COPY CWFUNC.

       SCREEN SECTION.

       01  CB0011A.
           05 LINE 08 COLUMN 03 VALUE "C¢digo   :".
           05 LINE 10 COLUMN 03 VALUE "Descriá∆o:".

       03  CB0011B.
           05 CODIGO
              LINE 08 COLUMN 14 PIC Z(004) USING CBCACC-CODIGO.
           05 DESCRICAO
              LINE 10 COLUMN 14 PIC X(030) USING CBCACC-DESCRICAO.

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
                MOVE ZERO  TO FL-EXIT
                IF   NOT INCLUSAO
                     MOVE "S" TO RE-START
                END-IF
           END-IF

           IF   VEZ = 1
                MOVE 2 TO VEZ
                IF   NOT FINALIZAR
                     DISPLAY CB0011A
                END-IF
           END-IF

           MOVE "23" TO FS-CBCACC

           IF   NOT FINALIZAR
                IF   INCLUSAO
                     MOVE ZERO   TO CBCACC-CODIGO
                     MOVE SPACES TO CBCACC-DESCRICAO
                END-IF
                MOVE SPACES TO  MENSAGEM-ERRO
                DISPLAY CB0011B
                PERFORM 140-LER-CBCACC  THRU 140-99-FIM
                        UNTIL FS-CBCACC < "10"
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

           MOVE "00" TO FS-CBCACC

           IF   EFETIVAR
           AND  NOT FINALIZAR
                IF   INCLUSAO
                     WRITE CBCACC-REG
                ELSE
                     IF   EXCLUSAO
                          DELETE CBCACC RECORD
                     ELSE
                          REWRITE CBCACC-REG
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO
           MOVE    0          TO TECLA
           DISPLAY RODAPE-INCLUSAO
                   LINE 23 COLUMN 03
           MOVE 2 TO ws-OPTION
           ACCEPT DESCRICAO
           PERFORM 141-TECLA THRU 141-99-FIM

           IF   ESC
                MOVE "A" TO COMANDO
                MOVE 1   TO FL-EXIT
           ELSE
                DISPLAY CB0011B
                MOVE    SPACE               TO COMANDO
                PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
            END-IF.

       130-99-FIM.  EXIT.

       140-LER-CBCACC.

           EXEC COBOLware Send Message MENSAGEM-ERRO END-EXEC

           IF   MENSAGEM-ERRO EQUAL SPACES
                IF   NOT INCLUSAO
                     DISPLAY RODAPE-PAGINAVEL LINE 23 COLUMN 03
                END-IF
           ELSE
                MOVE SPACES TO MENSAGEM-ERRO
           END-IF

           EVALUATE TRUE
               WHEN RE-START ="S"
               AND  NOT INCLUSAO
                    EXEC COBOLware BoxSelect
                         LINE 08 COLUMN 13
                         TITLE   "Pesquisar_por:"
                         TEXT(1) " ~C¢digo "
                         TEXT(2) " ~Descriá∆o"
                         OPTION 1;ws-OPTION
                    END-EXEC
                    IF   ws-OPTION = 0
                         MOVE 1 TO FL-EXIT
                         IF   ws-OPTION = 0
                              NEXT SENTENCE
                         END-IF
                    END-IF
                    MOVE "N"  TO RE-START
                    IF   ws-OPTION = 1
                         PERFORM TEST AFTER UNTIL NOT F1
                                 ACCEPT CODIGO
                                 PERFORM 141-TECLA THRU 141-99-FIM
                         END-PERFORM
                         START CBCACC  KEY NOT LESS CBCACC-CHAVE
                    ELSE
                         PERFORM TEST AFTER UNTIL NOT F1
                                 ACCEPT DESCRICAO
                                 PERFORM 141-TECLA THRU 141-99-FIM
                         END-PERFORM
                         START CBCACC  KEY NOT LESS CBCACC-DESCRICAO
                    END-IF
                    READ CBCACC NEXT RECORD IGNORE LOCK
                    DISPLAY CB0011B
                    EXIT PARAGRAPH
               WHEN INCLUSAO
                    DISPLAY RODAPE          LINE 23 COLUMN 03
                    ACCEPT CODIGO
                    MOVE 1 TO ws-OPTION
               WHEN ws-OPTION = 1
                    DISPLAY RODAPE-PAGINAVEL LINE 23 COLUMN 03
                    ACCEPT CODIGO
               WHEN OTHER
                    ACCEPT DESCRICAO
           END-EVALUATE

           PERFORM 141-TECLA THRU 141-99-FIM

           IF   F1
                GO TO 140-LER-CBCACC
           END-IF

           IF  ESC
               MOVE 1 TO FL-EXIT
           END-IF

           EXEC COBOLware Send Message MENSAGEM-ERRO END-EXEC

           IF   FL-EXIT NOT EQUAL 1
                IF   (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                     IF   PAGE-DOWN
                          READ CBCACC NEXT RECORD IGNORE LOCK
                     ELSE
                          READ CBCACC PREVIOUS RECORD IGNORE LOCK
                     END-IF
                     IF   FS-CBCACC < "10"
                          DISPLAY CB0011B
                          GO TO 140-LER-CBCACC
                     ELSE
                          MOVE "44" TO FS-CBCACC
                     END-IF
                ELSE
                     IF   INCLUSAO
                     OR   ws-OPTION = 1
                          READ CBCACC IGNORE LOCK
                     ELSE
                          READ CBCACC IGNORE LOCK
                               KEY IS CBCACC-DESCRICAO
                     END-IF
                END-IF
                IF   FS-CBCACC < "10"
                     DISPLAY CB0011B
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBCACC
                     ELSE
                          CONTINUE
                ELSE
                     IF   NOT INCLUSAO
                          MOVE MSG (2) TO MENSAGEM-ERRO
                     ELSE
                          IF   CBCACC-CODIGO NOT = 0
                               MOVE "00" TO FS-CBCACC
                               DISPLAY CB0011B
                          ELSE
                               MOVE "44"    TO FS-CBCACC
                               MOVE MSG (5) TO MENSAGEM-ERRO
                          END-IF
                     END-IF
                END-IF
           ELSE
                MOVE "00"   TO FS-CBCACC
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF.

       140-99-FIM. EXIT.

       141-TECLA.

           ACCEPT TECLA FROM ESCAPE KEY

           IF   F1
                IF   ws-OPTION = 1
                     EXEC COBOLware Help
                          FILE "CB011PCW.H01"
                          LINE 08 COLUMN 14
                          HEIGHT 6 WIDTH 42
                     END-EXEC
                ELSE
                     EXEC COBOLware Help
                          FILE "CB011PCW.H02"
                          LINE 10 COLUMN 14
                          HEIGHT 6 WIDTH 42
                     END-EXEC
                END-IF
           END-IF.

       141-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       800-INICIAIS.

           OPEN I-O CBCACC.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCACC.

       900-99-FIM. EXIT.

       END PROGRAM CB011PCW.
