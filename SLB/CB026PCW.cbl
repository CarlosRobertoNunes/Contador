       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB026PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  10/01/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de formatos de integracao de      *
                      *                                               *
                      *  centros de custos                            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBFOCCSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBFOCCFD.

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
           05 ER-CBFOCC.
              10 FS-CBFOCC             PIC  X(002) VALUE "00".
              10 LB-CBFOCC             PIC  X(050) VALUE "CBFOCC".
           05 LIMITES VALUE "1830".
              10 LIMITE OCCURS 2 PIC 9(002).
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

       01  CB026PA.
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
           05 LINE 12 COLUMN 03 VALUE "1-C¢digo".
           05 LINE 12 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 12 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 12 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 13 COLUMN 03 VALUE "2-Descri‡Æo".
           05 LINE 13 COLUMN 27 VALUE "³".
           05 LINE 13 COLUMN 36 VALUE "60".
           05 LINE 13 COLUMN 46 VALUE "70".
           05 LINE 13 COLUMN 56 VALUE "80".
           05 LINE 13 COLUMN 65 VALUE " 90 ".
           05 LINE 13 COLUMN 75 VALUE "100³".
           05 LINE 14 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 14 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 14 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 15 COLUMN 27 VALUE "³".
           05 LINE 15 COLUMN 35 VALUE "110".
           05 LINE 15 COLUMN 45 VALUE "120".
           05 LINE 15 COLUMN 55 VALUE "130".
           05 LINE 15 COLUMN 65 VALUE "140".
           05 LINE 15 COLUMN 75 VALUE "150³".
           05 LINE 16 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 16 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 16 COLUMN 67 VALUE "°°°°°°°°°°°³".
           05 LINE 17 COLUMN 27 VALUE "³".
           05 LINE 17 COLUMN 35 VALUE "160".
           05 LINE 17 COLUMN 45 VALUE "170".
           05 LINE 17 COLUMN 55 VALUE "180".
           05 LINE 17 COLUMN 65 VALUE "190".
           05 LINE 17 COLUMN 75 VALUE "200³".
           05 LINE 18 COLUMN 27 VALUE "³°°°°°°°°°°°°°°°°°°°".
           05 LINE 18 COLUMN 47 VALUE "°°°°°°°°°°°°°°°°°°°°".
           05 LINE 18 COLUMN 67 VALUE "°°°°°°°°°°°³".
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

       01  CB026PE AUTO.
           05 CTAC-FORMATO
              LINE 07 COLUMN 16 PIC X(008) USING CBFOCC-FORMATO.

       01  CB026PB AUTO.
           05 CTAC-COMENTARIO
              LINE 08 COLUMN 16 PIC X(050) USING CBFOCC-COMENTARIO.
           05 CTAC-I-01 LINE 12 COLUMN 18 PIC Z(003) USING CBFOCC-I(01).
           05 CTAC-F-01 LINE 12 COLUMN 22 PIC Z(003) USING CBFOCC-F(01).
           05 CTAC-I-02 LINE 13 COLUMN 18 PIC Z(003) USING CBFOCC-I(02).
           05 CTAC-F-02 LINE 13 COLUMN 22 PIC Z(003) USING CBFOCC-F(02).

       01  CB026P-MAPA.
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
                     DISPLAY CB026PA
                END-IF
           END-IF

           MOVE "23" TO FS-CBFOCC

           IF   NOT FINALIZAR
                MOVE SPACES TO MENSAGEM-ERRO
                IF   INCLUSAO
                OR   VEZ = 2
                     MOVE 3 TO VEZ
                     INITIALIZE CBFOCC-REG
                END-IF
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CBFOCC  THRU 140-99-FIM
                        UNTIL FS-CBFOCC = "00"
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

           MOVE ZERO TO FS-CBFOCC

           IF   EFETIVAR
           AND  NOT FINALIZAR
                IF   INCLUSAO
                     WRITE CBFOCC-REG
                ELSE
                     IF   EXCLUSAO
                          DELETE CBFOCC RECORD
                     ELSE
                          REWRITE CBFOCC-REG
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO
           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM

           PERFORM TEST AFTER UNTIL (F2 AND  ERRO = 0)
                                 OR ESC
                   IF   ERRO = 0
                        DISPLAY APAGA-RODAPE LINE 23 COLUMN 03
                   END-IF
                   PERFORM 175-EXIBE-MAPA THRU 175-99-FIM
                   DISPLAY RODAPE-INCLUSAO LINE 23 COLUMN 03
                   MOVE 0 TO ERRO
                   COMPUTE I = CAMPO / 2
                   IF CBFOCC-I (I) NOT = 0
                   AND I < 3
                   AND > 0
                      INSPECT MAPA CONVERTING "²" TO "°"
                      MOVE CBFOCC-I (I) TO Y
                      PERFORM CBFOCC-F (I) TIMES
                              MOVE "²"  TO MAPA (Y: 1)
                              ADD 1 TO Y
                      END-PERFORM
                      DISPLAY CB026P-MAPA
                   END-IF
                   EVALUATE CAMPO
                            WHEN 01 ACCEPT CTAC-COMENTARIO
                            WHEN 02 ACCEPT CTAC-I-01
                            WHEN 03 ACCEPT CTAC-F-01
                            WHEN 04 ACCEPT CTAC-I-02
                            WHEN 05 ACCEPT CTAC-F-02
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   F1
                        PERFORM 150-HELP THRU 150-99-FIM
                   END-IF
                   IF   I NOT = 0
                   AND  I < 10
                        IF   CBFOCC-I (I) NOT = 0
                             IF   CBFOCC-F (I) > LIMITE (I)
                                  MOVE 1 TO ERRO
                                  EXEC COBOLware Send
                                   Message "Tamanho de campo excessivo"
                                  END-EXEC
                             ELSE
                                  IF  (CBFOCC-I (I) + CBFOCC-F (I) - 1)
                                      > 300
                                      MOVE 1 TO ERRO
                                      EXEC COBOLware Send
                                       Message
                                         "Registro excede limite de 300"
                                      END-EXEC
                                  ELSE
                                      MOVE CBFOCC-I (I) TO Y
                                      MOVE I (3: 1) TO LETRA
                                      PERFORM CBFOCC-F (I) TIMES
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
                                         EXEC COBOLware Send
                                              Message
                                         "Campo invadindo  rea de outro"
                                         END-EXEC
                                      END-IF
                                      DISPLAY CB026P-MAPA
                                  END-IF
                             END-IF
                        END-IF
                   END-IF
                   IF   CURSOR-DOWN
                   AND  ERRO = 0
                        ADD 1 TO CAMPO
                        IF   CAMPO = 6
                             MOVE 1 TO CAMPO
                        END-IF
                   ELSE
                        IF   CURSOR-UP
                        AND  ERRO = 0
                             SUBTRACT 1 FROM CAMPO
                             IF   CAMPO = 0
                                  MOVE 5 TO CAMPO
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM

           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           PERFORM 160-CHECK-COMANDO THRU 160-99-FIM.

       130-99-FIM.  EXIT.

       140-LER-CBFOCC.

           DISPLAY CB026PE
           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

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
                   ACCEPT CB026PE
                   ACCEPT TECLA FROM ESCAPE KEY
                   INSPECT CBFOCC-FORMATO
                           CONVERTING MINUSCULAS TO MAIUSCULAS
                   IF   F1
                        EXEC COBOLware Help
                             FILE   "CB026PCW.H01"
                             LINE   08 COLUMN 20
                             HEIGHT 06 WIDTH  42
                        END-EXEC
                   END-IF
           END-PERFORM

           IF  ESC
               MOVE 1 TO FL-EXIT
           END-IF

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           IF   FL-EXIT NOT = 1
                IF  (PAGE-UP OR PAGE-DOWN)
                AND (NOT INCLUSAO)
                AND  OK = ZERO
                     MOVE 1     TO OK
                     PERFORM TEST AFTER
                             UNTIL FS-CBFOCC NOT = "9D"
                     START CBFOCC  KEY NOT < CBFOCC-CHAVE
                     IF FS-CBFOCC = "9D"
                        CALL "CWISAM" USING ER-CBFOCC
                     END-IF
                     END-PERFORM
                END-IF
                IF  (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                     IF   PAGE-DOWN
                          READ CBFOCC NEXT RECORD IGNORE LOCK
                     ELSE
                          READ CBFOCC PREVIOUS RECORD IGNORE LOCK
                     END-IF
                     IF   FS-CBFOCC < "10"
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CBFOCC
                     ELSE
                          MOVE ZERO TO OK
                          MOVE "44" TO FS-CBFOCC
                     END-IF
                ELSE
                     IF   CBFOCC-FORMATO = SPACES
                          MOVE MSG (5) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBFOCC
                     ELSE
                          READ CBFOCC
                     END-IF
                END-IF
                IF   FS-CBFOCC = "00"
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBFOCC
                     END-IF
                ELSE
                     IF   FS-CBFOCC NOT = "44"
                          IF   NOT INCLUSAO
                               MOVE MSG (2) TO MENSAGEM-ERRO
                          ELSE
                               MOVE "00" TO FS-CBFOCC
                               PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          END-IF
                     END-IF
                END-IF
           ELSE
                MOVE ZERO   TO FS-CBFOCC
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF.

       140-99-FIM. EXIT.

       150-HELP.

           EVALUATE TRUE
               WHEN CAMPO = 1
                    EXEC COBOLware Help
                         FILE   "CB026PCW.H02"
                         LINE   09
                         COLUMN 20
                         HEIGHT 06
                         WIDTH  42
                    END-EXEC
               WHEN CAMPO < 8
                    EXEC COBOLware Help
                         FILE   "CB026PCW.H03"
                         LINE   13
                         COLUMN 30
                         HEIGHT 06
                         WIDTH  42
                    END-EXEC
           END-EVALUATE.

       150-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           DISPLAY CB026PB
           PERFORM 175-EXIBE-MAPA THRU 175-99-FIM.

       170-99-FIM. EXIT.

       175-EXIBE-MAPA.

           MOVE ALL "°" TO MAPA
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 2
                   IF   CBFOCC-I (C) NOT = 0
                        MOVE CBFOCC-I (C) TO Y
                        PERFORM CBFOCC-F (C) TIMES
                                MOVE C (3: 1)
                                  TO MAPA (Y: 1)
                                ADD 1 TO Y
                        END-PERFORM
                   END-IF
           END-PERFORM
           DISPLAY CB026P-MAPA.

       175-99-FIM. EXIT.

       800-INICIAIS.

           OPEN I-O CBFOCC.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBFOCC.

       900-99-FIM. EXIT.

       END PROGRAM CB026PCW.
