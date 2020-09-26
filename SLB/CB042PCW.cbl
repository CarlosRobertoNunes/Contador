       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB042PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/03/1995.
       SECURITY.      *************************************************
                      *                                               *
                      * Elimina lancamentos do mes mais antigo        *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCAHISL REPLACING AUTOMATIC BY MANUAL.
       COPY CBCOBASL REPLACING MANUAL BY EXCLUSIVE.
       COPY CBCOSASL REPLACING MANUAL BY EXCLUSIVE.
       COPY CBHIVASL REPLACING AUTOMATIC BY MANUAL.
       COPY CBPLCOSL.
       COPY CBMVMSSL REPLACING MANUAL BY EXCLUSIVE.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCAHIFD.
       COPY CBCOBAFD.
       COPY CBCOSAFD.
       COPY CBHIVAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 VEZ-ACC                  PIC  9(006) VALUE 0.
           05 VARIAVEL                 PIC  9(002) VALUE 0.
           05 HOJE                     PIC  9(008) VALUE 0.
           05 LD-CBCOSA                PIC  9(006) VALUE 0.
           05 LD-CBCOBA                PIC  9(006) VALUE 0.
           05 LD-CBMVMS                PIC  9(006) VALUE 0.
           05 LD-CBHIVA                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
           05 LANCAMENTO               PIC  9(006) VALUE 0.
           05 MAIS-ANTIGO              PIC  9(006) VALUE 999999.
           05 MAIS-NOVO                PIC  9(006) VALUE 0.
           05 PRIMEIRO.
              10 AAAA-PRIMEIRO         PIC  9(004) VALUE 0.
              10 MM-PRIMEIRO           PIC  9(002) VALUE 0.
           05 ULTIMO.
              10 AAAA-ULTIMO           PIC  9(004) VALUE 0.
              10 MM-ULTIMO             PIC  9(002) VALUE 0.
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAAMM.
                    20 AAAA-REF        PIC  9(004).
                    20 MM-REF          PIC  9(002).
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.

       COPY CWTIME.

       SCREEN SECTION.

       01  TELA.
           03 LINE 08 COLUMN 27 PIC 9(02) USING MM-ULTIMO.
           03 LINE 08 COLUMN 30 PIC 9(04) USING AAAA-ULTIMO.

       01  TELA2.
           03 LINE 09 COLUMN 03 VALUE "Hist¢rico de implantaá∆o:".
           03 LINE 09 COLUMN 29 PIC Z(04) USING CBCAHI-CODIGO.
           03 LINE 09 COLUMN 34 PIC X(30) FROM  CBCAHI-DESCRICAO.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           DISPLAY "Atená∆o:"                     LINE 11 COLUMN 03
           "Os pr¢ximos passos de processamento s¢ ser∆o revers°veis "
           LINE 13 COLUMN 03 "retornando back-up,"
           "deseja continuar ? S/<N>:"
           LINE 14 COLUMN 03
           ACCEPT RESPOSTA LINE 14 COLUMN 29
           IF   RESPOSTA = "S" OR "s"
                DISPLAY "000000 lidos de " LINE 16 COLUMN 03 LB-CBCOSA
                DISPLAY "000000 lidos de " LINE 17 COLUMN 03 LB-CBCOBA
                DISPLAY "000000 lidos de " LINE 18 COLUMN 03 LB-CBMVMS
                DISPLAY "000000 lidos de " LINE 19 COLUMN 03 LB-CBHIVA
                MOVE LOW-VALUES TO CBCOSA-REG
                START CBCOSA KEY NOT LESS CBCOSA-CHAVE
                PERFORM TEST AFTER UNTIL FS-CBCOSA > "09"
                        READ CBCOSA NEXT RECORD
                        IF   FS-CBCOSA < "10"
                             ADD 1 TO LD-CBCOSA
                             DISPLAY LD-CBCOSA LINE 16 COLUMN 3
                             IF  (CBCOSA-AAAAMM NOT < MAIS-ANTIGO)
                             AND (CBCOSA-AAAAMM NOT > ULTIMO)
                                  DELETE CBCOSA RECORD
                             END-IF
                        END-IF
                END-PERFORM
                MOVE LOW-VALUES TO CBCOBA-REG
                START CBCOBA KEY NOT LESS CBCOBA-CHAVE
                PERFORM TEST AFTER
                        UNTIL FS-CBCOBA > "09"
                        READ CBCOBA NEXT RECORD
                        IF  (CBCOBA-REFERENCIA NOT < MAIS-ANTIGO)
                        AND (CBCOBA-REFERENCIA NOT > ULTIMO)
                        AND  FS-CBCOBA < "10"
                             ADD 1 TO LD-CBCOBA
                             DISPLAY LD-CBCOBA LINE 17 COLUMN 3
                             DELETE CBCOBA RECORD
                        END-IF
                END-PERFORM
                PERFORM 105-EXCLUI-CBHIVA THRU 105-99-FIM
                DELETE FILE CBMVMS
                PERFORM UNTIL AAAAMM = ULTIMO
                        ADD 1 TO MM-REF
                        IF   MM-REF = 13
                             MOVE 1 TO MM-REF
                             ADD  1 TO AAAA-REF
                        END-IF
                        PERFORM 105-EXCLUI-CBHIVA THRU 105-99-FIM
                        DELETE FILE CBMVMS
                END-PERFORM
                IF   ULTIMO NOT = MAIS-NOVO
                     CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                     OPEN I-O CBMVMS
                     MOVE ULTIMO TO PRIMEIRO
                     INITIALIZE CBCOBA-REG
                     ADD 1 TO MM-PRIMEIRO
                     IF   MM-PRIMEIRO = 13
                          MOVE 1 TO MM-PRIMEIRO
                          ADD  1 TO AAAA-PRIMEIRO
                     END-IF
                     MOVE AAAA-ULTIMO TO CBCOBA-AAAA CBCOBA-SERIE
                     MOVE MM-ULTIMO   TO CBCOBA-MM   CBCOBA-NUMERO
                     WRITE CBCOBA-REG
                     MOVE AAAA-ULTIMO TO CBCOBA-AAAA
                     MOVE MM-ULTIMO   TO CBCOBA-MM
                     READ  CBCOBA
                     MOVE LOW-VALUES TO CBCOSA-REG
                     MOVE 0          TO LD-CBCOSA
                     START CBCOSA KEY NOT LESS CBCOSA-CHAVE
                     PERFORM TEST AFTER UNTIL FS-CBCOSA > "09"
                             READ CBCOSA NEXT RECORD
                             IF   FS-CBCOSA < "10"
                                  ADD 1 TO LD-CBCOSA
                                  DISPLAY LD-CBCOSA LINE 16 COLUMN 3
                                  IF   CBCOSA-AAAAMM = PRIMEIRO
                                       PERFORM 110-RECRIA-CBCOSA
                                          THRU 110-99-FIM
                                  END-IF
                             END-IF
                     END-PERFORM
                     REWRITE CBCOBA-REG
                     CLOSE CBMVMS
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       105-EXCLUI-CBHIVA.

           DISPLAY "000000 lidos de " LINE 18 COLUMN 03 LB-CBMVMS
           MOVE 0 TO LD-CBMVMS
           CALL "CB045PCW" USING LB-CBMVMS (1: 6)
           OPEN INPUT CBMVMS
           PERFORM UNTIL FS-CBMVMS > "09"
                   PERFORM TEST AFTER UNTIL FS-CBMVMS NOT = "9D"
                           READ CBMVMS NEXT RECORD
                   END-PERFORM
                   IF   FS-CBMVMS < "10"
                   AND  CBMVMS-HISTORICO-VARIAVEL NOT = 0
                        ADD 1 TO LD-CBMVMS
                        DISPLAY LD-CBMVMS LINE 18 COLUMN 3
                        PERFORM VARYING VARIAVEL FROM 1 BY 1
                                        UNTIL VARIAVEL > 24
                                COMPUTE CBHIVA-TIPO
                                      = CBMVMS-HISTORICO-VARIAVEL
                                      / 100000
                                MOVE CBMVMS-HISTORICO-VARIAVEL
                                                   TO CBHIVA-CODIGO
                                MOVE VARIAVEL      TO CBHIVA-VARIAVEL
                                READ CBHIVA IGNORE LOCK
                                IF   FS-CBHIVA < "10"
                                     ADD 1 TO LD-CBHIVA
                                     DISPLAY LD-CBHIVA LINE 19 COLUMN 3
                                     DELETE CBHIVA RECORD
                                ELSE
                                     MOVE "25" TO FS-CBHIVA
                                END-IF
                        END-PERFORM
                   END-IF
           END-PERFORM
           CLOSE CBMVMS.

       105-99-FIM. EXIT.

       110-RECRIA-CBCOSA.

           READ CBPLCO IGNORE LOCK

           IF   FS-CBPLCO = "23"
                MOVE 99999 TO CBPLCO-COD-RED
           END-IF

           INITIALIZE CBMVMS-REG
           MOVE ULTIMO               TO CBCOSA-AAAAMM
           MOVE CBCOSA-SALDO-INICIAL TO CBCOSA-SALDO-ATUAL
                                        CBMVMS-VALOR
           MOVE 0                    TO CBCOSA-SALDO-INICIAL
                                        CBCOSA-A-DEBITO
                                        CBCOSA-A-CREDITO

           IF   CBCOSA-SALDO-ATUAL POSITIVE
                MOVE CBCOSA-SALDO-ATUAL TO CBCOSA-A-DEBITO
                MOVE "D"                TO CBMVMS-TIPO
           ELSE
                MOVE CBCOSA-SALDO-ATUAL TO CBCOSA-A-CREDITO
                MOVE "C"                TO CBMVMS-TIPO
           END-IF

           WRITE CBCOSA-REG

           IF   CBMVMS-VALOR = 0
           OR   CBPLCO-COD-RED = 0
                GO TO 110-99-FIM
           END-IF

           IF   CBMVMS-TIPO = "D"
                ADD CBMVMS-VALOR TO CBCOBA-DB-PREVISTOS
                                    CBCOBA-DB-EFETIVOS
           ELSE
                ADD CBMVMS-VALOR TO CBCOBA-CR-PREVISTOS
                                    CBCOBA-CR-EFETIVOS
           END-IF

           ADD  1           TO LANCAMENTO
           MOVE LANCAMENTO  TO CBMVMS-LANCAMENTO
                               CBCOBA-LC-PREVISTOS
                               CBCOBA-LC-EFETIVOS
                               CBMVMS-DOCTO

           MOVE HOJE           TO CBMVMS-AAAAMMDD-DOCTO
           MOVE AAAA-ULTIMO    TO CBMVMS-SERIE
           MOVE MM-ULTIMO      TO CBMVMS-NUMERO
           MOVE 1              TO CBMVMS-DIA
           MOVE CBPLCO-COD-RED TO CBMVMS-COD-RED
           MOVE CBCAHI-CODIGO  TO CBMVMS-HISTORICO-PADRAO
           WRITE CBMVMS-REG
           INITIALIZE CBMVMS-REG
           READ CBMVMS
           MOVE LANCAMENTO  TO CBMVMS-VALOR
           IF   FS-CBMVMS < "09"
                REWRITE CBMVMS-REG
           ELSE
                WRITE CBMVMS-REG
           END-IF.

       110-99-FIM. EXIT.

       800-INICIAIS.

           SET  CWTIME-REVERSED          TO TRUE
           SET  CWTIME-TODAY             TO TRUE
           CALL "CWTIME"              USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL        TO HOJE

           OPEN I-O CBCOBA
           IF   FS-CBCOBA NOT < "10"
                GOBACK
           END-IF

           OPEN I-O CBCOSA
           IF   FS-CBCOSA > "09"
                CLOSE CBCOBA
                GOBACK
           END-IF

           OPEN INPUT CBCAHI
           IF   FS-CBCAHI > "09"
                CLOSE CBCOBA CBCOSA
                GOBACK
           END-IF

           OPEN INPUT CBPLCO
           IF   FS-CBPLCO NOT < "10"
                CLOSE CBCOBA CBCOSA CBCAHI
                GOBACK
           END-IF

           DISPLAY "Eliminar de 00/0000 atÇ 99/9999"
                   LINE 08 COLUMN 03
           DISPLAY "000000 lidos de " LINE 16 COLUMN 03 LB-CBCOSA
           PERFORM TEST AFTER UNTIL FS-CBCOSA > "09"
                   READ CBCOSA NEXT RECORD IGNORE LOCK
                   IF   FS-CBCOSA < "10"
                        ADD 1 TO LD-CBCOSA
                        DISPLAY LD-CBCOSA LINE 16 COLUMN 3
                   END-IF
                   IF   CBCOSA-AAAAMM < MAIS-ANTIGO
                   AND  FS-CBCOSA < "10"
                        MOVE CBCOSA-AAAAMM TO MAIS-ANTIGO
                        MOVE CBCOSA-MM     TO MM-REF
                        MOVE CBCOSA-AAAA   TO AAAA-REF
                        DISPLAY CBCOSA-MM LINE 08 COLUMN 15
                                "/" CBCOSA-AAAA
                   END-IF
                   IF   CBCOSA-AAAAMM > MAIS-NOVO
                   AND  FS-CBCOSA < "10"
                        MOVE CBCOSA-AAAAMM TO MAIS-NOVO
                        DISPLAY CBCOSA-MM LINE 08 COLUMN 27
                                "/" CBCOSA-AAAA
                   END-IF
           END-PERFORM

           MOVE ZERO      TO LD-CBCOSA
           MOVE MAIS-NOVO TO ULTIMO
           DISPLAY TELA

           PERFORM TEST AFTER UNTIL (ULTIMO NOT = 0)
                                AND (ULTIMO NOT > MAIS-NOVO)
                                AND (ULTIMO NOT < MAIS-ANTIGO)
                                AND (MM-ULTIMO NOT < 01)
                                AND (MM-ULTIMO NOT > 12)
                    ADD 1 TO VEZ-ACC
                    IF   VEZ-ACC > 1
                         EXEC COBOLware Send
                              Message "Referància final impr¢pria"
                         END-EXEC
                    END-IF
                    ACCEPT TELA
                    ACCEPT TECLA FROM ESCAPE KEY
                    IF   ESC
                         CLOSE CBCOSA CBCOBA CBCAHI CBPLCO
                         GOBACK
                    END-IF
           END-PERFORM.

           INITIALIZE CBCAHI-REG
           DISPLAY TELA2
           PERFORM TEST AFTER UNTIL FS-CBCAHI < "09"
                    ACCEPT TELA2
                    ACCEPT TECLA FROM ESCAPE KEY
                    IF   ESC
                         CLOSE CBCOSA CBCOBA CBCAHI CBPLCO
                         GOBACK
                    END-IF
                    READ CBCAHI IGNORE LOCK
                    IF   FS-CBCAHI > "09"
                         MOVE "N∆o cadastrado" TO CBCAHI-DESCRICAO
                    END-IF
                    DISPLAY TELA2
           END-PERFORM.

           OPEN I-O CBHIVA
           IF   FS-CBHIVA > "09"
                CLOSE CBCOSA CBCOBA CBCAHI CBPLCO
           END-IF.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOSA CBCOBA CBCAHI CBPLCO CBHIVA.

       900-99-FIM. EXIT.

       END PROGRAM CB042PCW.
