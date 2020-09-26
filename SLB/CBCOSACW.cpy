
           MOVE CBMVMS-CENTRO-CUSTO TO CC
           PERFORM TEST AFTER UNTIL CBCOSA-CENTRO-CUSTO = 0
           MOVE CBPLCO-CONTA TO CB002PCW-CONTA
           PERFORM UNTIL CB002PCW-CONTA = 0
              MOVE ZERO              TO CONTROLE-MES
              MOVE CBCOBA-REFERENCIA TO CONTROLE-REFERENCIA
              PERFORM TEST AFTER UNTIL CONTROLE-REFERENCIA = CBCOMS-REG
                      ADD  1 TO CONTROLE-MES
                      IF   CONTROLE-MES > 1
                           ADD 1 TO CONTROLE-MM
                           IF   CONTROLE-MM > 12
                                MOVE 1 TO CONTROLE-MM
                                ADD  1 TO CONTROLE-AAAA
                           END-IF
                      END-IF
                      MOVE CC                  TO CBCOSA-CENTRO-CUSTO
                      MOVE CB002PCW-CONTA      TO CBCOSA-CONTA
                      MOVE CONTROLE-REFERENCIA TO CBCOSA-AAAAMM
                      READ CBCOSA IGNORE LOCK
                      IF   FS-CBCOSA = "23"
                           INITIALIZE                  CBCOSA-REG
                           MOVE CC
                             TO CBCOSA-CENTRO-CUSTO
                           MOVE CB002PCW-CONTA      TO CBCOSA-CONTA
                           MOVE CONTROLE-REFERENCIA TO CBCOSA-AAAAMM
                           WRITE CBCOSA-REG
                           IF   FS-CBCOSA > "09"
                                STOP RUN
                           END-IF
                           MOVE CB002PCW-CONTA      TO CB002PCW-CONTA
                           MOVE CONTROLE-REFERENCIA TO CBCOSA-AAAAMM
                           READ CBCOSA IGNORE LOCK
                      END-IF
                      IF   FS-CBCOSA > "09"
                           STOP RUN
                      END-IF
                      IF   CBMVMS-TIPO = "D"
                           ADD CBMVMS-VALOR TO CBCOSA-SALDO-ATUAL
                           IF   CONTROLE-MES = 1
                                IF   ESTORNO = 0
                                     ADD CBMVMS-VALOR
                                      TO CBCOSA-A-DEBITO
                                ELSE
                                     SUBTRACT CBMVMS-VALOR
                                         FROM CBCOSA-A-CREDITO
                                END-IF
                           END-IF
                           IF   CONTROLE-MES > 1
                                ADD CBMVMS-VALOR TO CBCOSA-SALDO-INICIAL
                           END-IF
                      ELSE
                           SUBTRACT CBMVMS-VALOR FROM CBCOSA-SALDO-ATUAL
                           IF   CONTROLE-MES = 1
                                IF   ESTORNO = 0
                                     ADD CBMVMS-VALOR
                                      TO CBCOSA-A-CREDITO
                                ELSE
                                     SUBTRACT CBMVMS-VALOR
                                         FROM CBCOSA-A-DEBITO
                                END-IF
                           END-IF
                           IF   CONTROLE-MES > 1
                                SUBTRACT CBMVMS-VALOR
                                    FROM CBCOSA-SALDO-INICIAL
                           END-IF
                      END-IF
                      REWRITE CBCOSA-REG
                      IF   FS-CBCOSA > "09"
                           STOP RUN
                      END-IF
              END-PERFORM
           MOVE "S"         TO CB002PCW-FUNCAO
           CALL "CB002PCW" USING PARAMETROS-CB002PCW
           END-PERFORM
           MOVE 0 TO CC
           END-PERFORM.

