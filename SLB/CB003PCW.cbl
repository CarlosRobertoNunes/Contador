       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB003PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/12/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao do plano de contas                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPAPCSL.
       COPY CBPLCOSL.
       COPY CBCTCRSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPAPCFD.
       COPY CBPLCOFD.
       COPY CBCTCRFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 ws-OPTION                PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 CHAMADO                  PIC  9(001) VALUE 0.
           05 TRACO-DV                 PIC  X(001) VALUE SPACE.
           05 COD-RED-CALL             PIC  9(005) VALUE 0.
           05 COD-RED-DV               PIC  X(001) VALUE SPACE.
           05 CLASSE                   PIC  9(015) VALUE 0.
           05 REDEFINES CLASSE.
              10 BYTE-CLASSE           PIC  9(001) OCCURS 15.
           05 ER-CBPAPC.
              10 FS-CBPAPC             PIC  X(002) VALUE "00".
              10 LB-CBPAPC             PIC  X(050) VALUE "CBPAPC".
           05 RK-CBPAPC                PIC  9(001) COMP VALUE 1.
           05 DESCRICAO-A              PIC  X(030) VALUE SPACES.
           05 COD-RED-A                PIC  9(005) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help ".
           05 RODAPE-INCLUSAO          PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help ".
           05 RODAPE-PAGINAVEL         PIC  X(068) VALUE
              "<Esc>-Funá∆o F1-Help PgDn-Pr¢ximo PgUp-Anterior ".
           05 VEZ                      PIC  9(001) VALUE 1.
           05 LANCAVEL                 PIC  X(001) VALUE SPACE.
           05 SALVA-GRAU               PIC  9(001) VALUE ZERO.
           05 SALVA-CONTA       COMP-3 PIC  9(015) VALUE ZERO.
           05 SALVA-CONTA-PROX  COMP-3 PIC  9(015) VALUE ZERO.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "Entre com os dados            ".
              10 PIC X(30) VALUE "Conta n∆o cadastrada          ".
              10 PIC X(30) VALUE "Confirme exclus∆o             ".
              10 PIC X(30) VALUE "Conta j† cadastrada           ".
              10 PIC X(30) VALUE "Falta conta de grau superior  ".
              10 PIC X(30) VALUE "Conta possui saldo/lanáamentos".
              10 PIC X(30) VALUE "Existe conta de grau inferior ".
              10 PIC X(30) VALUE "Falta descriá∆o               ".
              10 PIC X(30) VALUE "Conta zerada                  ".
              10 PIC X(30) VALUE "Fim de arquivo                ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 10 PIC X(30).
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCTCR.
              10 FS-CBCTCR             PIC  X(002) VALUE "00".
              10 LB-CBCTCR             PIC  X(050) VALUE "CBCTCR".

       COPY CB002PCW.
       COPY CWFUNC.
       COPY CWDCNP.

       SCREEN SECTION.

       01  CB003PA.
       03  CONSTANTES.
           05 LINE 10 COLUMN 16 VALUE "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ".
           05 LINE 10 COLUMN 36 VALUE "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ".
           05 LINE 10 COLUMN 56 VALUE "ÕÕÕÕÕÕÕª".
           05 LINE 11 COLUMN 16 VALUE "∫                   ".
           05 LINE 11 COLUMN 36 VALUE "                    ".
           05 LINE 11 COLUMN 56 VALUE "       ∫".
           05 LINE 12 COLUMN 16 VALUE "∫ Conta       :     ".
           05 LINE 12 COLUMN 36 VALUE "                    ".
           05 LINE 12 COLUMN 56 VALUE "       ∫".
           05 LINE 13 COLUMN 16 VALUE "∫                   ".
           05 LINE 13 COLUMN 36 VALUE "                    ".
           05 LINE 13 COLUMN 56 VALUE "       ∫".
           05 LINE 14 COLUMN 16 VALUE "∫ C¢d.Reduzido:     ".
           05 LINE 14 COLUMN 36 VALUE "                    ".
           05 LINE 14 COLUMN 56 VALUE "       ∫".
           05 LINE 15 COLUMN 16 VALUE "∫                   ".
           05 LINE 15 COLUMN 36 VALUE "                    ".
           05 LINE 15 COLUMN 56 VALUE "       ∫".
           05 LINE 16 COLUMN 16 VALUE "∫ Descriá∆o   :     ".
           05 LINE 16 COLUMN 36 VALUE "                    ".
           05 LINE 16 COLUMN 56 VALUE "       ∫".
           05 LINE 17 COLUMN 16 VALUE "∫                   ".
           05 LINE 17 COLUMN 36 VALUE "                    ".
           05 LINE 17 COLUMN 56 VALUE "       ∫".
           05 LINE 18 COLUMN 16 VALUE "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ".
           05 LINE 18 COLUMN 36 VALUE "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ".
           05 LINE 18 COLUMN 56 VALUE "ÕÕÕÕÕÕÕº".
       03  CB003PB.
           05 LINE 14 COLUMN 32 PIC Z(005) FROM CBPLCO-COD-RED.
           05 DESCRICAO LINE 16 COLUMN 32 PIC X(030)
                        USING CBPLCO-DESCRICAO.

       01  COD-RED.
           05 LINE 14 COLUMN 32 PIC Z(005) USING CBPLCO-COD-RED.
           05 LINE 14 COLUMN 37 PIC X(001) FROM TRACO-DV.
           05 LINE 14 COLUMN 38 PIC X(001) FROM COD-RED-DV.

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
                CANCEL "CB002PCW"
                CANCEL "CB003P9W"
                GOBACK
          END-IF.

       100-PROCESSAMENTO.

           IF   FL-EXIT EQUAL 1
                MOVE SPACE TO FUNCAO
                IF   CHAMADO = 1
                     MOVE "I" TO FUNCAO
                     IF   VEZ = 2
                          MOVE "V" TO FUNCAO
                          GO TO 100-CHAMADO-FIM
                     END-IF
                END-IF
                EXEC COBOLware Option
                     Function FUNCAO
                END-EXEC
                MOVE ZERO  TO FL-EXIT OK ws-OPTION
           END-IF

           IF   VEZ = 1
                MOVE 2   TO VEZ
                MOVE 12  TO CB002PCW-LINHA
                MOVE 32  TO CB002PCW-COLUNA
                IF   NOT FINALIZAR
                     DISPLAY CB003PA
                END-IF
           END-IF

           MOVE "23" TO FS-CBPLCO.

       100-CHAMADO-FIM.

           IF   NOT FINALIZAR
                INITIALIZE CBPLCO-REG MENSAGEM-ERRO
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CBPLCO   THRU 140-99-FIM
                        TEST AFTER
                        UNTIL (FS-CBPLCO < "10"
                           AND MENSAGEM-ERRO = SPACES)
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

           MOVE "00" TO FS-CBPLCO

           IF   EFETIVAR
           AND  NOT FINALIZAR
                IF   INCLUSAO
                     IF   LANCAVEL = "S"
                          MOVE ZERO TO CBCTCR-COD-RED
                          READ CBCTCR
                          IF   FS-CBCTCR = "23"
                               MOVE 0 TO CBCTCR-COD-RED
                                         CBCTCR-ULTIMO
                               WRITE CBCTCR-REG
                          END-IF
                          READ CBCTCR NEXT RECORD
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
                     MOVE CBPLCO-CONTA    TO CBPLCO-CLASSE
                                             CB002PCW-CONTA
                     MOVE "C"             TO CB002PCW-FUNCAO
                     CALL "CB002PCW"   USING PARAMETROS-CB002PCW
                     MOVE "E"             TO CB002PCW-FUNCAO
                     CALL "CB002PCW"   USING PARAMETROS-CB002PCW
                     IF  CB002PCW-LANCAVEL = "S"
                         MOVE "S"         TO CB002PCW-FUNCAO
                         CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                         MOVE CB002PCW-CONTA TO CLASSE
                         PERFORM VARYING I FROM 15 BY -1
                                 UNTIL BYTE-CLASSE (I) NOT = 0
                                 MOVE 9 TO BYTE-CLASSE (I)
                         END-PERFORM
                         MOVE CLASSE         TO CBPLCO-CLASSE
                         IF  CHAMADO = 1
                             MOVE "V"            TO FUNCAO
                             MOVE "77"           TO CWDCNP-RETORNO
                             MOVE CBPLCO-COD-RED TO CWDCNP-CNPJ
                             CALL "CWDCNP"    USING PARAMETROS-CWDCNP
                         END-IF
                     END-IF
                     MOVE "S" TO CBPLCO-VIRGEM
                     WRITE CBPLCO-REG
                ELSE
                     IF   EXCLUSAO
                          IF   LANCAVEL = "S"
                               MOVE CBPLCO-COD-RED TO CBCTCR-COD-RED
                               MOVE ZERO           TO CBCTCR-ULTIMO
                               WRITE CBCTCR-REG
                          END-IF
                          DELETE CBPLCO RECORD
                     ELSE
                          REWRITE CBPLCO-REG
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO
           MOVE    0          TO TECLA
           PERFORM TEST AFTER
                     UNTIL CBPLCO-DESCRICAO NOT = SPACES
                        OR ESC
                           DISPLAY RODAPE-INCLUSAO
                                   LINE 23 COLUMN 03
                           ACCEPT DESCRICAO
                           ACCEPT TECLA FROM ESCAPE KEY
                           IF   F1
                                EXEC COBOLware Help
                                     FILE   "CB003PCW.H03"
                                     LINE   16
                                     COLUMN 32
                                     HEIGHT 6
                                     WIDTH  42
                                END-EXEC
                           END-IF
           END-PERFORM

           IF   ESC
                MOVE "A" TO COMANDO
                MOVE 1   TO FL-EXIT
           ELSE
                IF   CBPLCO-DESCRICAO NOT = SPACES
                     PERFORM 170-EXIBE-DADOS   THRU 170-99-FIM
                     MOVE    SPACE               TO COMANDO
                     PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                ELSE
                     MOVE 1   TO FL-EXIT
                     MOVE "A" TO COMANDO
                END-IF
           END-IF.

       130-99-FIM.  EXIT.

       140-LER-CBPLCO.

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           IF   MENSAGEM-ERRO EQUAL SPACES
                IF   NOT INCLUSAO
                     DISPLAY RODAPE-PAGINAVEL
                             LINE 23 COLUMN 03
                ELSE
                     DISPLAY RODAPE
                             LINE 23 COLUMN 03
                END-IF
           ELSE
                MOVE SPACES TO MENSAGEM-ERRO
           END-IF

           IF   INCLUSAO
                PERFORM TEST AFTER UNTIL NOT F1
                        MOVE "S"               TO CB002PCW-FORCA-DV
                        MOVE CBPLCO-CONTA      TO CB002PCW-CONTA
                        MOVE "A"               TO CB002PCW-FUNCAO
                        CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                        MOVE CB002PCW-CONTA    TO CBPLCO-CONTA
                        MOVE CB002PCW-RETORNO  TO TECLA
                        MOVE CB002PCW-LANCAVEL TO LANCAVEL
                        IF   F1
                             EXEC COBOLware Help
                                  FILE   "CB003PCW.H01"
                                  LINE   16
                                  COLUMN 32
                                  HEIGHT 6
                                  WIDTH  42
                             END-EXEC
                             GO TO 140-LER-CBPLCO
                        END-IF
                END-PERFORM
                READ CBPLCO
           ELSE
                MOVE "N" TO CB002PCW-FORCA-DV
                IF   OK = 0
                     INITIALIZE CBPLCO-REG
                     MOVE 1 TO OK
                     EXEC COBOLware BoxSelect
                          LINE    11 COLUMN  34
                          TITLE "Pesquisar:"
                          TEXT(1) " ~C¢digo "
                          TEXT(2) " c¢d.~Red."
                          TEXT(3) " ~Descriá∆o "
                          OPTION  1;ws-OPTION
                     END-EXEC
                     IF   ws-OPTION = 0
                          SET ESC TO TRUE
                          GO TO 140-1
                     END-IF
                END-IF
                EVALUATE ws-OPTION
                WHEN 1
                     PERFORM TEST AFTER UNTIL NOT F1
                        MOVE "A"             TO CB002PCW-FUNCAO
                        MOVE CBPLCO-CONTA    TO CB002PCW-CONTA
                        CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                        MOVE CB002PCW-RETORNO  TO TECLA
                        IF   F1
                             EXEC COBOLware Help
                                  FILE   "CB003PCW.H01"
                                  LINE   16
                                  COLUMN 32
                                  HEIGHT 6
                                  WIDTH  42
                             END-EXEC
                        END-IF
                     END-PERFORM
                     IF   CB002PCW-CONTA NOT = CBPLCO-CONTA
                     OR   CB002PCW-CONTA = 0
                          MOVE CB002PCW-CONTA TO CBPLCO-CONTA
                          START CBPLCO  KEY NOT < CBPLCO-CHAVE
                     END-IF
                WHEN 2
                     ACCEPT COD-RED
                     ACCEPT TECLA FROM ESCAPE KEY
                     IF   F1
                          EXEC COBOLware Help
                               FILE   "CB003PCW.H02"
                               LINE   16
                               COLUMN 32
                               HEIGHT 6
                               WIDTH  42
                          END-EXEC
                          GO TO 140-LER-CBPLCO
                     ELSE
                          IF  CBPLCO-COD-RED NOT = COD-RED-A
                              START CBPLCO KEY NOT < CBPLCO-COD-RED
                          END-IF
                     END-IF
                WHEN 3
                     ACCEPT DESCRICAO
                     ACCEPT TECLA FROM ESCAPE KEY
                     IF   F1
                          EXEC COBOLware Help
                               FILE   "CB003PCW.H03"
                               LINE   16
                               COLUMN 32
                               HEIGHT 6
                               WIDTH  42
                          END-EXEC
                          GO TO 140-LER-CBPLCO
                     ELSE
                         IF   CBPLCO-DESCRICAO NOT = DESCRICAO-A
                         OR   CBPLCO-DESCRICAO = SPACES
                         OR   ENTER-KEY
                              START CBPLCO
                                   KEY NOT < CBPLCO-DESCRICAO
                         END-IF
                END-EVALUATE
                IF   PAGE-DOWN
                     READ CBPLCO NEXT RECORD
                ELSE
                     IF   PAGE-UP
                          READ CBPLCO PREVIOUS RECORD
                     END-IF
                END-IF
                MOVE CBPLCO-COD-RED   TO COD-RED-A
                MOVE CBPLCO-DESCRICAO TO DESCRICAO-A
           END-IF.

       140-1.

           IF  ESC
               MOVE 1 TO FL-EXIT
               MOVE "00"   TO FS-CBPLCO
               MOVE SPACES TO COMANDO
                              FUNCAO
               GO TO 140-99-FIM
           END-IF

           IF   (NOT INCLUSAO)
           AND  ENTER-KEY
                EVALUATE ws-OPTION
                   WHEN 1 READ CBPLCO KEY IS CBPLCO-CHAVE
                   WHEN 2 READ CBPLCO KEY IS CBPLCO-COD-RED
                   WHEN 3 READ CBPLCO KEY IS CBPLCO-DESCRICAO
                END-EVALUATE
           END-IF

           IF   FS-CBPLCO < "10"
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                IF  NOT ENTER-KEY
                OR  CONSULTA
                    GO TO 140-LER-CBPLCO
                END-IF
                IF   INCLUSAO
                     MOVE MSG (4) TO MENSAGEM-ERRO
                     MOVE "44"    TO FS-CBPLCO
                ELSE
                     IF EXCLUSAO
                        IF   CBPLCO-VIRGEM = "N"
                             MOVE MSG (6) TO MENSAGEM-ERRO
                             MOVE "44"    TO FS-CBPLCO
                        END-IF
                        MOVE CB002PCW-CONTA TO SALVA-CONTA
                        MOVE "E"            TO CB002PCW-FUNCAO
                        CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                        MOVE CB002PCW-GRAU  TO SALVA-GRAU
                        READ CBPLCO NEXT RECORD
                        IF   FS-CBPLCO > "10"
                             MOVE SALVA-CONTA TO CB002PCW-CONTA
                                                 CBPLCO-CONTA
                             READ CBPLCO
                        ELSE
                             MOVE CBPLCO-CONTA    TO SALVA-CONTA-PROX
                                                     CB002PCW-CONTA
                             MOVE "S"             TO CB002PCW-FUNCAO
                             PERFORM TEST AFTER
                                          UNTIL CB002PCW-GRAU
                                                NOT > SALVA-GRAU
                                     CALL "CB002PCW" USING
                                                   PARAMETROS-CB002PCW
                             END-PERFORM
                             MOVE CB002PCW-CONTA    TO SALVA-CONTA-PROX
                             MOVE SALVA-CONTA     TO CBPLCO-CONTA
                                                     CB002PCW-CONTA
                             READ CBPLCO
                             IF   SALVA-CONTA = SALVA-CONTA-PROX
                                  MOVE MSG (7) TO MENSAGEM-ERRO
                                  MOVE "44"    TO FS-CBPLCO
                             END-IF
                        END-IF
                     END-IF
                END-IF
           ELSE
                IF   NOT INCLUSAO
                     MOVE MSG (2) TO MENSAGEM-ERRO
                     IF (PAGE-DOWN
                     OR  PAGE-UP)
                         MOVE MSG (10) TO MENSAGEM-ERRO
                         GO TO 140-99-FIM
                     END-IF
                ELSE
                  IF CBPLCO-CONTA NOT = 0
                     MOVE "00" TO FS-CBPLCO
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                        MOVE CB002PCW-CONTA TO SALVA-CONTA
                        MOVE "S"          TO CB002PCW-FUNCAO
                        CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                        IF   CB002PCW-CONTA NOT = 0
                             MOVE CB002PCW-CONTA TO CBPLCO-CONTA
                             READ CBPLCO
                             IF  FS-CBPLCO > "09"
                                 MOVE MSG (5) TO MENSAGEM-ERRO
                                 MOVE "44"    TO FS-CBPLCO
                             ELSE
                                 MOVE SALVA-CONTA TO CBPLCO-CONTA
                                                     CB002PCW-CONTA
                                 READ CBPLCO
                             END-IF
                        END-IF
                        MOVE SALVA-CONTA TO CBPLCO-CONTA
                                            CB002PCW-CONTA
                  ELSE
                       MOVE "44"    TO FS-CBPLCO
                       MOVE MSG (9) TO MENSAGEM-ERRO
                  END-IF
                END-IF
           END-IF.

       140-99-FIM. EXIT.

       160-CHECK-COMANDO.

           IF   NOT EXCLUSAO
                DISPLAY LINHA-BRANCA LINE 23 COLUMN 3
           END-IF

           IF   CHAMADO = 1
           AND (LANCAVEL NOT = "S")
                DISPLAY "Cadastre conta anal°tica:   <Enter>"
                        AT 2303 WITH BEEP SIZE 68
                ACCEPT COMANDO AT 2329
                MOVE "A" TO COMANDO
           ELSE
                COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           MOVE CBPLCO-CONTA TO CB002PCW-CONTA
           MOVE "C"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           IF   CBPLCO-COD-RED = 0
                MOVE SPACE TO TRACO-DV
                MOVE SPACE TO COD-RED-DV
           ELSE
                MOVE CBPLCO-COD-RED TO COD-RED-CALL
                MOVE "-"            TO TRACO-DV
                CALL "CB039PCW"  USING COD-RED-CALL COD-RED-DV
           END-IF
           MOVE CBPLCO-CONTA TO CB002PCW-CONTA
           MOVE "D"          TO CB002PCW-FUNCAO
           MOVE 12           TO CB002PCW-LINHA
           MOVE 32           TO CB002PCW-COLUNA
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           DISPLAY CB003PB
           DISPLAY COD-RED.

       170-99-FIM. EXIT.

       800-INICIAIS.

           CALL "CWDCNP"    USING PARAMETROS-CWDCNP
           IF   CWDCNP-RETORNO = "78"
                MOVE " <Esc>-Abandona  F1-Help "
                 TO RODAPE
                 MOVE " <Esc>-Abandona F1-Help "  TO RODAPE-INCLUSAO
                MOVE 1 TO CHAMADO.

           OPEN INPUT CBPAPC
           IF  FS-CBPAPC > "09"
               EXEC COBOLware Send
                    Message "Defina a estrutura das contas"
               END-EXEC
               GOBACK
           ELSE
               READ CBPAPC
               IF  FS-CBPAPC > "09"
               OR  CBPAPC-ESTRUTURA-CONTA NOT NUMERIC
               OR  CBPAPC-ESTRUTURA-CONTA = ZEROS
                   EXEC COBOLware Send
                        Message "Defina a estrutura das contas"
                   END-EXEC
                   CLOSE CBPAPC
                   GOBACK
               END-IF
           END-IF

           CLOSE CBPAPC
           OPEN I-O CBPLCO
                    CBCTCR

           INITIALIZE CBPLCO-REG.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBPLCO CBCTCR
           CANCEL "CB002PCW".

       900-99-FIM. EXIT.

       END PROGRAM CB003PCW.
