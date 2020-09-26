       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB033PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de formatos de integracao de      *
                      *                                               *
                      *  Saldos                                       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBFOSDSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBFOSDFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 APAGA-RODAPE             PIC  X(068) VALUE SPACES.
           05 RODAPE-INCLUSAO          PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Help F2-Aceita tela ".
           05 RODAPE-PAGINAVEL         PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Help PgDn-Pr¢ximo PgUp-Anterior ".
           05 VEZ                      PIC  9(001) VALUE 1.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 CAMPO                    PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 I                        PIC  9(003) VALUE 1.
           05 C                        PIC  9(003) VALUE 1.
           05 Y                        PIC  9(003) VALUE 0.
           05 ERRO                     PIC  9(001) VALUE 0.
           05 U                        PIC  9(002) VALUE 1.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "Formato nÆo cadastrado        ".
              10 F PIC X(30) VALUE "Confirme exclusÆo             ".
              10 F PIC X(30) VALUE "Formato j  cadastrado         ".
              10 F PIC X(30) VALUE "Falta formato                 ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 5  PIC X(30).
           05 ER-CBFOSD.
              10 FS-CBFOSD             PIC  X(002) VALUE "00".
              10 LB-CBFOSD             PIC  X(050) VALUE "CBFOSD".
           05 LIMITES VALUE "1818181830301818".
              10 LIMITE OCCURS 8       PIC 9(002).
           05 LETRA                    PIC  X(001) VALUE SPACE.
           05 TESTE                    PIC  X(001) VALUE SPACE.
           05 MAPA.
              10 MAPA-1               PIC X(50) VALUE SPACES.
              10 MAPA-2               PIC X(50) VALUE SPACES.
              10 MAPA-3               PIC X(50) VALUE SPACES.
              10 MAPA-4               PIC X(50) VALUE SPACES.
              10 MAPA-5               PIC X(50) VALUE SPACES.
              10 MAPA-6               PIC X(50) VALUE SPACES.

       COPY CWFUNC.

       SCREEN SECTION.

       01  CB033PA.
           05 LINE 07 COLUMN 04 VALUE "Formato   :".
           05 LINE 08 COLUMN 04 VALUE "Coment rio:".
           05 LINE 09 COLUMN 27 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 09 COLUMN 47 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 09 COLUMN 67 VALUE "ÄÄÄÄÄÄÄÄÄÄÄ¿".
           05 LINE 10 COLUMN 05 VALUE "Campo".
           05 LINE 10 COLUMN 19 VALUE "Posi‡Æo".
           05 LINE 10 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 10 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 10 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 11 COLUMN 27 VALUE "³".
           05 LINE 11 COLUMN 36 VALUE "10".
           05 LINE 11 COLUMN 46 VALUE "20".
           05 LINE 11 COLUMN 56 VALUE "30".
           05 LINE 11 COLUMN 66 VALUE "40".
           05 LINE 11 COLUMN 76 VALUE "50³".
           05 LINE 12 COLUMN 03 VALUE "1-Sld.anterior".
           05 LINE 12 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 12 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 12 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 13 COLUMN 03 VALUE "2-Sld.atual".
           05 LINE 13 COLUMN 27 VALUE "³".
           05 LINE 13 COLUMN 36 VALUE "60".
           05 LINE 13 COLUMN 46 VALUE "70".
           05 LINE 13 COLUMN 56 VALUE "80".
           05 LINE 13 COLUMN 65 VALUE " 90 ".
           05 LINE 13 COLUMN 75 VALUE "100³".
           05 LINE 14 COLUMN 03 VALUE "3-D‚bitos".
           05 LINE 14 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 14 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 14 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 15 COLUMN 03 VALUE "4-Cr‚ditos".
           05 LINE 15 COLUMN 27 VALUE "³".
           05 LINE 15 COLUMN 35 VALUE "110".
           05 LINE 15 COLUMN 45 VALUE "120".
           05 LINE 15 COLUMN 55 VALUE "130".
           05 LINE 15 COLUMN 65 VALUE "140".
           05 LINE 15 COLUMN 75 VALUE "150³".
           05 LINE 16 COLUMN 03 VALUE "5-C¢d.editado".
           05 LINE 16 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 16 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 16 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 17 COLUMN 03 VALUE "6-Descri‡Æo".
           05 LINE 17 COLUMN 27 VALUE "³".
           05 LINE 17 COLUMN 35 VALUE "160".
           05 LINE 17 COLUMN 45 VALUE "170".
           05 LINE 17 COLUMN 55 VALUE "180".
           05 LINE 17 COLUMN 65 VALUE "190".
           05 LINE 17 COLUMN 75 VALUE "200³".
           05 LINE 18 COLUMN 03 VALUE "7-Mˆs ref.".
           05 LINE 18 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 18 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 18 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 19 COLUMN 03 VALUE "8-Ano ref.".
           05 LINE 19 COLUMN 27 VALUE "³".
           05 LINE 19 COLUMN 35 VALUE "210".
           05 LINE 19 COLUMN 45 VALUE "220".
           05 LINE 19 COLUMN 55 VALUE "230".
           05 LINE 19 COLUMN 65 VALUE "240".
           05 LINE 19 COLUMN 75 VALUE "250³".
           05 LINE 20 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 20 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 20 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 21 COLUMN 27 VALUE "³".
           05 LINE 21 COLUMN 35 VALUE "260".
           05 LINE 21 COLUMN 45 VALUE "270".
           05 LINE 21 COLUMN 55 VALUE "280".
           05 LINE 21 COLUMN 65 VALUE "290".
           05 LINE 21 COLUMN 75 VALUE "300³".
           05 LINE 22 COLUMN 27 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 22 COLUMN 47 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 22 COLUMN 67 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  CB033PE AUTO.
           05 CTAC-FORMATO
              LINE 07 COLUMN 16 PIC X(008) USING CBFOSD-FORMATO.

       01  CB033PB AUTO.
           05 CTAC-COMENTARIO
              LINE 08 COLUMN 16 PIC X(050) USING CBFOSD-COMENTARIO.
           05 CTAC-I-01 LINE 12 COLUMN 18 PIC Z(003) USING CBFOSD-I(01).
           05 CTAC-F-01 LINE 12 COLUMN 22 PIC Z(003) USING CBFOSD-F(01).
           05 CTAC-I-02 LINE 13 COLUMN 18 PIC Z(003) USING CBFOSD-I(02).
           05 CTAC-F-02 LINE 13 COLUMN 22 PIC Z(003) USING CBFOSD-F(02).
           05 CTAC-I-03 LINE 14 COLUMN 18 PIC Z(003) USING CBFOSD-I(03).
           05 CTAC-F-03 LINE 14 COLUMN 22 PIC Z(003) USING CBFOSD-F(03).
           05 CTAC-I-04 LINE 15 COLUMN 18 PIC Z(003) USING CBFOSD-I(04).
           05 CTAC-F-04 LINE 15 COLUMN 22 PIC Z(003) USING CBFOSD-F(04).
           05 CTAC-I-05 LINE 16 COLUMN 18 PIC Z(003) USING CBFOSD-I(05).
           05 CTAC-F-05 LINE 16 COLUMN 22 PIC Z(003) USING CBFOSD-F(05).
           05 CTAC-I-06 LINE 17 COLUMN 18 PIC Z(003) USING CBFOSD-I(06).
           05 CTAC-F-06 LINE 17 COLUMN 22 PIC Z(003) USING CBFOSD-F(06).
           05 CTAC-I-07 LINE 18 COLUMN 18 PIC Z(003) USING CBFOSD-I(07).
           05 CTAC-F-07 LINE 18 COLUMN 22 PIC Z(003) USING CBFOSD-F(07).
           05 CTAC-I-08 LINE 19 COLUMN 18 PIC Z(003) USING CBFOSD-I(08).
           05 CTAC-F-08 LINE 19 COLUMN 22 PIC Z(003) USING CBFOSD-F(08).

       01  CB033P-MAPA.
           05 LINE 10 COLUMN 28 PIC X(50) FROM MAPA-1.
           05 LINE 12 COLUMN 28 PIC X(50) FROM MAPA-2.
           05 LINE 14 COLUMN 28 PIC X(50) FROM MAPA-3.
           05 LINE 16 COLUMN 28 PIC X(50) FROM MAPA-4.
           05 LINE 18 COLUMN 28 PIC X(50) FROM MAPA-5.
           05 LINE 20 COLUMN 28 PIC X(50) FROM MAPA-6.

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

           IF   FL-EXIT = 1
                MOVE SPACE TO FUNCAO
                EXEC COBOLware Option
                     Function FUNCAO
                END-EXEC
                MOVE ZERO  TO FL-EXIT OK
           END-IF

           IF   VEZ = 1
                MOVE 2 TO VEZ
                IF   NOT FINALIZAR
                     DISPLAY CB033PA
                END-IF
           END-IF

           MOVE "23" TO FS-CBFOSD

           IF   NOT FINALIZAR
                MOVE SPACES TO MENSAGEM-ERRO
                IF   INCLUSAO
                OR   VEZ = 2
                     MOVE 3 TO VEZ
                     INITIALIZE CBFOSD-REG
                END-IF
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CBFOSD  THRU 140-99-FIM
                        UNTIL FS-CBFOSD = "00"
                        OR    FL-EXIT = 1
                DISPLAY LINHA-BRANCA LINE 23 COLUMN 03
                IF  (INCLUSAO OR ALTERACAO)
                AND  FL-EXIT NOT = 1
                     MOVE    MSG (1)            TO MENSAGEM-ERRO
                     MOVE    SPACE              TO COMANDO
                     PERFORM 130-CRITICA      THRU 130-99-FIM
                             UNTIL MENSAGEM-ERRO = SPACES
                             OR    ABORTAR
                ELSE
                     IF   FL-EXIT NOT = 1
                     AND  EXCLUSAO
                          MOVE    SPACE TO COMANDO
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                     END-IF
                END-IF
           END-IF

           MOVE ZERO TO FS-CBFOSD

           IF   EFETIVAR
           AND  NOT FINALIZAR
                IF   INCLUSAO
                     WRITE CBFOSD-REG
                ELSE
                     IF   EXCLUSAO
                          DELETE CBFOSD RECORD
                     ELSE
                          REWRITE CBFOSD-REG
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO

           DISPLAY CB033PB

           PERFORM TEST AFTER UNTIL (F2 AND  ERRO = 0)
                                 OR ESC
                   IF   ERRO = 0
                        DISPLAY APAGA-RODAPE LINE 23 COLUMN 03
                   END-IF
                   PERFORM 175-EXIBE-MAPA THRU 175-99-FIM
                   DISPLAY RODAPE-INCLUSAO LINE 23 COLUMN 03
                   MOVE 0 TO ERRO
                   COMPUTE I = CAMPO / 2
                   IF CBFOSD-I (I) NOT = 0
                   AND I < 9
                   AND > 0
                      INSPECT MAPA CONVERTING "²" TO "°"
                      MOVE CBFOSD-I (I) TO Y
                      PERFORM CBFOSD-F (I) TIMES
                              MOVE "²"  TO MAPA (Y: 1)
                              ADD 1 TO Y
                      END-PERFORM
                      DISPLAY CB033P-MAPA
                   END-IF
                   EVALUATE CAMPO
                            WHEN 01 ACCEPT CTAC-COMENTARIO
                            WHEN 02 ACCEPT CTAC-I-01
                            WHEN 03 ACCEPT CTAC-F-01
                            WHEN 04 ACCEPT CTAC-I-02
                            WHEN 05 ACCEPT CTAC-F-02
                            WHEN 06 ACCEPT CTAC-I-03
                            WHEN 07 ACCEPT CTAC-F-03
                            WHEN 08 ACCEPT CTAC-I-04
                            WHEN 09 ACCEPT CTAC-F-04
                            WHEN 10 ACCEPT CTAC-I-05
                            WHEN 11 ACCEPT CTAC-F-05
                            WHEN 12 ACCEPT CTAC-I-06
                            WHEN 13 ACCEPT CTAC-F-06
                            WHEN 14 ACCEPT CTAC-I-07
                            WHEN 15 ACCEPT CTAC-F-07
                            WHEN 16 ACCEPT CTAC-I-08
                            WHEN 17 ACCEPT CTAC-F-08
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   F1
                        PERFORM 150-HELP THRU 150-99-FIM
                   END-IF
                   IF   I NOT = 0
                   AND  I < 9
                        IF   CBFOSD-I (I) NOT = 0
                             IF   CBFOSD-F (I) > LIMITE (I)
                                  MOVE 1 TO ERRO
                                  EXEC COBOLware Send Message
                                       "Tamanho de campo excessivo"
                                  END-EXEC
                             ELSE
                                  IF  (CBFOSD-I (I) + CBFOSD-F (I) - 1)
                                      > 300
                                      MOVE 1 TO ERRO
                                      EXEC COBOLware Send Message
                                         "Registro excede limite de 300"
                                      END-EXEC
                                  ELSE
                                      MOVE CBFOSD-I (I) TO Y
                                      MOVE I (3: 1) TO LETRA
                                      PERFORM CBFOSD-F (I) TIMES
                                              MOVE MAPA (Y: 1) TO TESTE
                                              IF  (TESTE NOT = "²")
                                              AND (TESTE NOT = "°")
                                              AND (TESTE NOT = LETRA)
                                                  MOVE 1 TO ERRO
                                              ELSE
                                                  MOVE LETRA
                                                    TO MAPA (Y: 1)
                                              END-IF
                                              ADD 1 TO Y
                                      END-PERFORM
                                      IF ERRO = 1
                                         EXEC COBOLware Send Message
                                         "Campo invadindo  rea de outro"
                                         END-EXEC
                                         DISPLAY CB033P-MAPA
                                      END-IF
                                  END-IF
                             END-IF
                        END-IF
                   END-IF
                   IF   CURSOR-DOWN
                   AND  ERRO = 0
                        ADD 1 TO CAMPO
                        IF   CAMPO = 18
                             MOVE 1 TO CAMPO
                        END-IF
                   ELSE
                        IF   CURSOR-UP
                        AND  ERRO = 0
                             SUBTRACT 1 FROM CAMPO
                             IF   CAMPO = 0
                                  MOVE 17 TO CAMPO
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM

           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
           EXEC COBOLware Send Message MENSAGEM-ERRO END-EXEC

           MOVE    SPACE               TO COMANDO
           PERFORM 160-CHECK-COMANDO THRU 160-99-FIM.

       130-99-FIM.  EXIT.

       140-LER-CBFOSD.

           DISPLAY CB033PE
           EXEC COBOLware Send Message MENSAGEM-ERRO END-EXEC

           MOVE 1 TO CAMPO
           PERFORM TEST AFTER UNTIL ENTER-KEY
                                 OR ESC
                                 OR PAGE-UP
                                 OR PAGE-DOWN
                   DISPLAY APAGA-RODAPE LINE 23 COLUMN 03
                   IF   MENSAGEM-ERRO = SPACES
                        IF   NOT INCLUSAO
                             DISPLAY RODAPE-PAGINAVEL
                                     LINE 23 COLUMN 03
                        ELSE
                             DISPLAY RODAPE-INCLUSAO
                                     LINE 23 COLUMN 03
                        END-IF
                   ELSE
                        MOVE SPACES TO MENSAGEM-ERRO
                   END-IF
                   ACCEPT CB033PE
                   ACCEPT TECLA FROM ESCAPE KEY
                   INSPECT CBFOSD-FORMATO
                           CONVERTING MINUSCULAS TO MAIUSCULAS
                   IF   F1
                        EXEC COBOLware Help
                             FILE "CB033PCW.H01"
                             LINE   08 COLUMN 20
                             HEIGHT 06 WIDTH  42
                        END-EXEC
                   END-IF
           END-PERFORM

           IF  ESC
               MOVE 1 TO FL-EXIT
           END-IF

           EXEC COBOLware Send Message MENSAGEM-ERRO END-EXEC

           IF   FL-EXIT NOT = 1
                IF  (PAGE-UP OR PAGE-DOWN)
                AND (NOT INCLUSAO)
                AND  OK = ZERO
                     MOVE 1     TO OK
                     START CBFOSD  KEY NOT < CBFOSD-CHAVE
                END-IF
                IF  (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                     IF   PAGE-DOWN
                          READ CBFOSD NEXT RECORD IGNORE LOCK
                     ELSE
                          READ CBFOSD PREVIOUS RECORD IGNORE LOCK
                     END-IF
                     IF   FS-CBFOSD < "10"
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CBFOSD
                     ELSE
                          MOVE ZERO TO OK
                          MOVE "44" TO FS-CBFOSD
                     END-IF
                ELSE
                     IF   CBFOSD-FORMATO = SPACES
                          MOVE MSG (5) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBFOSD
                     ELSE
                          READ CBFOSD
                     END-IF
                END-IF
                IF   FS-CBFOSD = "00"
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBFOSD
                     END-IF
                ELSE
                     IF   FS-CBFOSD NOT = "44"
                          IF   NOT INCLUSAO
                               MOVE MSG (2) TO MENSAGEM-ERRO
                          ELSE
                               MOVE "00" TO FS-CBFOSD
                               PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          END-IF
                     END-IF
                END-IF
           ELSE
                MOVE ZERO   TO FS-CBFOSD
                MOVE SPACES TO COMANDO
                               FUNCAO.

       140-99-FIM. EXIT.

       150-HELP.

           EVALUATE TRUE
           WHEN CAMPO = 1
                EXEC COBOLware Help
                     FILE   "CB033PCW.H02"
                     LINE   09 COLUMN 20
                     HEIGHT 06 WIDTH  42
                END-EXEC
           WHEN CAMPO < 8
                EXEC COBOLware Help
                     FILE   "CB033PCW.H03"
                     LINE   13 COLUMN 30
                     HEIGHT 06 WIDTH  42
                END-EXEC
           END-EVALUATE.

       150-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           DISPLAY CB033PB
           PERFORM 175-EXIBE-MAPA THRU 175-99-FIM.

       170-99-FIM. EXIT.

       175-EXIBE-MAPA.

           MOVE ALL "°" TO MAPA
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 8
                   IF   CBFOSD-I (C) NOT = 0
                        MOVE CBFOSD-I (C) TO Y
                        PERFORM CBFOSD-F (C) TIMES
                                MOVE C (3: 1)
                                  TO MAPA (Y: 1)
                                ADD 1 TO Y
                        END-PERFORM
                   END-IF
           END-PERFORM
           DISPLAY CB033P-MAPA.

       175-99-FIM. EXIT.

       800-INICIAIS.

           OPEN I-O CBFOSD.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOSD.

       900-99-FIM. EXIT.

       END PROGRAM CB033PCW.
