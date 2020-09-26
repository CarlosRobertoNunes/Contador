       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB007PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de BACs                           *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOBASL.
       COPY CBMVMSSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOBAFD.
       COPY CBMVMSFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 MSG-ACESSO               PIC  X(068) VALUE
              "Verificando acesso...".
           05 APAGA-RODAPE             PIC  X(068) VALUE SPACES.
           05 RODAPE-INCLUSAO          PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Help".
           05 RODAPE-PAGINAVEL         PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Help PgDn-Pr¢ximo PgUp-Anterior ".
           05 PRIMEIRO                 PIC  9(001) VALUE 3.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 LD-CBCOBA                 PIC  9(006) VALUE 0.
           05 OK                       PIC  9(001) VALUE ZERO.
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 HELP-FILE.
              10                       PIC  X(010) VALUE "CB007PCW.H".
              10 CAMPO                 PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 1.
           05 U                        PIC  9(002) VALUE 1.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Entre com os dados            ".
              10 F PIC X(30) VALUE "BAC nÆo cadastrado            ".
              10 F PIC X(30) VALUE "Confirme exclusÆo             ".
              10 F PIC X(30) VALUE "BAC j  cadastrado             ".
              10 F PIC X(30) VALUE "Mˆs de referˆncia inv lido    ".
              10 F PIC X(30) VALUE "Ano de referˆncia inv lido    ".
              10 F PIC X(30) VALUE "Falta S‚rie                   ".
              10 F PIC X(30) VALUE "Falta n£mero                  ".
              10 F PIC X(30) VALUE "BAC possui lan‡amentos        ".
              10 F PIC X(30) VALUE "Referˆncia inacess¡vel        ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 10 PIC X(30).
           05 ER-CBCOBA.
              10 FS-CBCOBA              PIC  X(002) VALUE "00".
              10 LB-CBCOBA              PIC  X(050) VALUE "CBCOBA".
           05 ER-CBMVMS.
              10 FS-CBMVMS              PIC  X(002) VALUE "00".
              10 LB-CBMVMS                          VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 OPCOES-DE-HELP.
              10 PIC X(8) VALUE "12200642".
              10 PIC X(8) VALUE "13200642".
              10 PIC X(8) VALUE "12150642".
              10 PIC X(8) VALUE "13150642".
              10 PIC X(8) VALUE "12350642".
              10 PIC X(8) VALUE "13350642".
              10 PIC X(8) VALUE "14350642".
           05 REDEFINES OPCOES-DE-HELP.
              10 OCCURS 7.
                 15 HELP-LIN PIC 99.
                 15 HELP-COL PIC 99.
                 15 HELP-VER PIC 99.
                 15 HELP-HOR PIC 99.

       COPY CWFUNC.

       SCREEN SECTION.

       01  CB007PA.
           05 LINE 08 COLUMN 07 VALUE "BAC".
           05 LINE 16 COLUMN 07 VALUE "Referˆncia".
           05 LINE 10 COLUMN 10 VALUE "S‚rie :".
           05 LINE 11 COLUMN 10 VALUE "N£mero:".
           05 LINE 18 COLUMN 10 VALUE "Mˆs:".
           05 LINE 19 COLUMN 10 VALUE "Ano:".
           05 LINE 08 COLUMN 38 VALUE "PrevisÆo de lan‡amentos".
           05 LINE 10 COLUMN 41 VALUE "Quantidade:".
           05 LINE 11 COLUMN 41 VALUE "A d‚bito  :".
           05 LINE 12 COLUMN 41 VALUE "A cr‚dito :".
           05 LINE 16 COLUMN 38 VALUE "Lan‡amentos j  efetivados".
           05 LINE 18 COLUMN 41 VALUE "Quantidade:".
           05 LINE 19 COLUMN 41 VALUE "A d‚bito  :".
           05 LINE 20 COLUMN 41 VALUE "A cr‚dito :".

       01  CB007PE AUTO.
           05 C1 LINE 10 COLUMN 18 PIC Z(004) USING CBCOBA-SERIE.
           05 C2 LINE 11 COLUMN 18 PIC Z(004) USING CBCOBA-NUMERO.

       01  CB007PB AUTO.
       03  CB007PC.
           05 C3 LINE 18 COLUMN 15 PIC Z(002) USING CBCOBA-MM.
           05 C4 LINE 19 COLUMN 15 PIC 9(004) USING CBCOBA-AAAA
                                                    BLANK ZERO.
       03  CB007PD.
           05 C5 LINE 10 COLUMN 53 PIC Z(008) USING CBCOBA-LC-PREVISTOS.
           05 C6 LINE 11 COLUMN 53 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                   USING CBCOBA-DB-PREVISTOS BLANK ZERO.
           05 C7 LINE 12 COLUMN 53 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                   USING CBCOBA-CR-PREVISTOS BLANK ZERO.

           05 LINE 18 COLUMN 53 PIC Z(008) FROM CBCOBA-LC-EFETIVOS.
           05 LINE 19 COLUMN 53 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                FROM CBCOBA-DB-EFETIVOS BLANK ZERO.
           05 LINE 20 COLUMN 53 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                FROM CBCOBA-CR-EFETIVOS BLANK ZERO.

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
                     DISPLAY CB007PA
                END-IF
           END-IF

           MOVE "23" TO FS-CBCOBA

           IF   NOT FINALIZAR
                MOVE SPACES TO MENSAGEM-ERRO
                IF   INCLUSAO
                OR   VEZ = 2
                     MOVE 3 TO VEZ
                     INITIALIZE CBCOBA-REG
                END-IF
                PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                PERFORM 140-LER-CBCOBA  THRU 140-99-FIM
                        UNTIL FS-CBCOBA = "00"
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
                          DISPLAY MSG (3) LINE 23 COLUMN 3
                          MOVE    SPACE TO COMANDO
                          PERFORM 160-CHECK-COMANDO THRU 160-99-FIM
                     END-IF
                END-IF
           END-IF

           MOVE ZERO TO FS-CBCOBA

           IF   EFETIVAR
           AND  NOT FINALIZAR
                IF   INCLUSAO
                     WRITE CBCOBA-REG
                ELSE
                     IF   EXCLUSAO
                          DELETE CBCOBA RECORD
                     ELSE
                          REWRITE CBCOBA-REG
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       130-CRITICA.

           MOVE    SPACES     TO MENSAGEM-ERRO

           IF   0 = CBCOBA-LC-EFETIVOS
           AND      CBCOBA-DB-EFETIVOS
           AND      CBCOBA-CR-EFETIVOS
                MOVE 3 TO PRIMEIRO
           ELSE
                MOVE 5 TO PRIMEIRO
           END-IF

           MOVE PRIMEIRO TO CAMPO
           DISPLAY CB007PB

           PERFORM TEST AFTER UNTIL CAMPO > 7
                                 OR ESC
                   DISPLAY RODAPE-INCLUSAO LINE 23 COLUMN 03
                   EVALUATE CAMPO
                        WHEN 3  ACCEPT C3
                        WHEN 4  ACCEPT C4
                        WHEN 5  ACCEPT C5
                        WHEN 6  ACCEPT C6
                        WHEN 7  ACCEPT C7
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   F1
                        PERFORM 150-HELP THRU 150-99-FIM
                   END-IF
                   IF   CURSOR-DOWN
                        ADD 1 TO CAMPO
                   ELSE
                        IF   CURSOR-UP
                             SUBTRACT 1 FROM CAMPO
                             IF   CAMPO = (PRIMEIRO - 1)
                                  MOVE 7 TO CAMPO
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM

           IF   CBCOBA-MM < 1 OR > 12
                MOVE MSG (5) TO MENSAGEM-ERRO
           ELSE
                IF   CBCOBA-AAAA < 1900
                     MOVE MSG (6) TO MENSAGEM-ERRO
                END-IF
           END-IF

           IF  (0 = CBCOBA-LC-EFETIVOS
           AND      CBCOBA-DB-EFETIVOS
           AND      CBCOBA-CR-EFETIVOS)
           AND      MENSAGEM-ERRO = SPACES
                    MOVE CBCOBA-AAAA TO AAAA-REF
                    MOVE CBCOBA-MM   TO MM-REF
                    CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                    OPEN INPUT CBMVMS
                    IF   FS-CBMVMS < "10"
                         CLOSE CBMVMS
                    ELSE
                         MOVE CBCOBA-REG TO SALVA-REG
                         MOVE 0          TO LD-CBCOBA
                         DISPLAY MSG-ACESSO
                                 LINE 23 COLUMN 03
                         INITIALIZE CBCOBA-REG
                         MOVE AAAA-REF TO CBCOBA-AAAA
                         MOVE MM-REF   TO CBCOBA-MM
                         PERFORM TEST AFTER
                                      UNTIL FS-CBCOBA NOT = "9D"
                                  START CBCOBA
                                        KEY NOT < CBCOBA-REFERENCIA
                                  IF FS-CBCOBA = "9D"
                                      CALL "CWISAM" USING ER-CBCOBA
                                  END-IF
                         END-PERFORM
                         PERFORM TEST AFTER
                                 UNTIL FS-CBCOBA > "09"
                                    OR MENSAGEM-ERRO NOT = SPACES
                                 PERFORM TEST AFTER
                                         UNTIL FS-CBCOBA NOT = "9D"
                                      READ CBCOBA NEXT RECORD
                                      IF FS-CBCOBA = "9D"
                                         CALL "CWISAM" USING ER-CBCOBA
                                      END-IF
                                 END-PERFORM
                                 IF   FS-CBCOBA < "10"
                                      ADD 1 TO LD-CBCOBA
                                      DISPLAY LD-CBCOBA
                                              LINE 23 COLUMN 24
                                      IF  CBCOBA-AAAA = AAAA-REF
                                      AND CBCOBA-MM   = MM-REF
                                          IF CBCOBA-LC-EFETIVOS NOT = 0
                                             MOVE MSG(10)
                                               TO MENSAGEM-ERRO
                                          END-IF
                                      ELSE
                                           MOVE "10" TO FS-CBCOBA
                                     END-IF
                                 END-IF
                         END-PERFORM
                         PERFORM TEST AFTER
                                      UNTIL FS-CBCOBA NOT = "9D"
                                 MOVE SALVA-REG  TO CBCOBA-REG
                                 START CBCOBA KEY NOT < CBCOBA-CHAVE
                                  IF FS-CBCOBA = "9D"
                                      CALL "CWISAM" USING ER-CBCOBA
                                   END-IF
                         END-PERFORM
                         PERFORM TEST AFTER
                                      UNTIL FS-CBCOBA NOT = "9D"
                                 MOVE SALVA-REG  TO CBCOBA-REG
                                 READ CBCOBA LOCK
                                  IF FS-CBCOBA = "9D"
                                      CALL "CWISAM" USING ER-CBCOBA
                                  END-IF
                         END-PERFORM
                         MOVE SALVA-REG  TO CBCOBA-REG
                    END-IF
           END-IF

           PERFORM 170-EXIBE-DADOS THRU 170-99-FIM

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           MOVE    SPACE               TO COMANDO
           PERFORM 160-CHECK-COMANDO THRU 160-99-FIM.

       130-99-FIM.  EXIT.

       140-LER-CBCOBA.

           DISPLAY CB007PE
           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           MOVE 1 TO CAMPO
           PERFORM TEST AFTER UNTIL CAMPO > 2
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
                   EVALUATE CAMPO
                        WHEN 1 ACCEPT C1
                        WHEN 2 ACCEPT C2
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF   F1
                        PERFORM 150-HELP THRU 150-99-FIM
                   END-IF
                   IF   CURSOR-UP
                   AND  CAMPO = 2
                        SUBTRACT 1 FROM CAMPO
                        IF   CAMPO = 0
                             MOVE 2 TO CAMPO
                        END-IF
                   ELSE
                   IF   CURSOR-DOWN
                        ADD  1 TO CAMPO
                   END-IF
           END-PERFORM

           IF  ESC
               MOVE 1 TO FL-EXIT
           END-IF

           EXEC COBOLware Send
                Message MENSAGEM-ERRO
           END-EXEC

           IF   FL-EXIT NOT = 1
                IF   INCLUSAO
                     MOVE 0 TO CBCOBA-AAAA
                               CBCOBA-MM
                               CBCOBA-LC-PREVISTOS
                               CBCOBA-DB-PREVISTOS
                               CBCOBA-CR-PREVISTOS
                               CBCOBA-LC-EFETIVOS
                               CBCOBA-DB-EFETIVOS
                               CBCOBA-CR-EFETIVOS
                END-IF
                IF  (PAGE-UP OR PAGE-DOWN)
                AND (NOT INCLUSAO)
                AND  OK = ZERO
                     MOVE 1     TO OK
                     START CBCOBA KEY NOT < CBCOBA-CHAVE
                END-IF
                IF  (PAGE-UP OR PAGE-DOWN)
                AND  NOT INCLUSAO
                     IF   PAGE-DOWN
                          READ CBCOBA NEXT RECORD IGNORE LOCK
                     ELSE
                          READ CBCOBA PREVIOUS RECORD IGNORE LOCK
                     END-IF
                     IF   FS-CBCOBA < "10"
                          PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          GO TO 140-LER-CBCOBA
                     ELSE
                          MOVE ZERO TO OK
                          MOVE "44" TO FS-CBCOBA
                     END-IF
                ELSE
                     IF   CBCOBA-SERIE = 0
                          MOVE MSG (7) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBCOBA
                     ELSE
                          IF   CBCOBA-NUMERO = 0
                               MOVE MSG (8) TO MENSAGEM-ERRO
                               MOVE "44" TO FS-CBCOBA
                          ELSE
                               UNLOCK CBCOBA
                               READ CBCOBA LOCK
                          END-IF
                     END-IF
                END-IF
                IF   FS-CBCOBA = "00"
                     PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                     IF   INCLUSAO
                          MOVE MSG (4) TO MENSAGEM-ERRO
                          MOVE "44" TO FS-CBCOBA
                     ELSE
                          IF   EXCLUSAO
                          AND (0 NOT = CBCOBA-LC-EFETIVOS
                                    OR CBCOBA-DB-EFETIVOS
                                    OR CBCOBA-CR-EFETIVOS)
                               MOVE MSG (9) TO MENSAGEM-ERRO
                               MOVE "44" TO FS-CBCOBA
                          END-IF
                     END-IF
                ELSE
                     IF   FS-CBCOBA NOT = "44"
                          IF   NOT INCLUSAO
                               MOVE MSG (2) TO MENSAGEM-ERRO
                          ELSE
                               MOVE "00" TO FS-CBCOBA
                               PERFORM 170-EXIBE-DADOS THRU 170-99-FIM
                          END-IF
                     END-IF
                END-IF
           ELSE
                MOVE ZERO   TO FS-CBCOBA
                MOVE SPACES TO COMANDO
                               FUNCAO
           END-IF.

       140-99-FIM. EXIT.

       150-HELP.

           EXEC COBOLware Help
                FILE    HELP-FILE
                LINE    HELP-LIN(CAMPO)
                COLUMN  HELP-COL(CAMPO)
                HEIGHT  HELP-VER(CAMPO)
                WIDTH   HELP-HOR(CAMPO)
           END-EXEC.

       150-99-FIM. EXIT.

       160-CHECK-COMANDO.

           COPY CWEFAB.

       160-99-FIM. EXIT.

       170-EXIBE-DADOS.

           DISPLAY CB007PB.

       170-99-FIM. EXIT.

       800-INICIAIS.

           OPEN I-O CBCOBA.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOBA.

       900-99-FIM. EXIT.

       END PROGRAM CB007PCW.
