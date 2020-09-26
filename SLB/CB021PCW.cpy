       700-VERIFICA-MES.

           OPEN INPUT CBGEIN
           IF   FS-CBGEIN = "9A" *> Arquivo travado
                EXEC COBOLware Send
                     Message "Sistema gerando novo mes"
                     CAPTION(1) "~Repetir"
                END-EXEC
                GO 700-VERIFICA-MES
           END-IF
           IF   FS-CBGEIN = "00"
                READ CBGEIN
                DISPLAY "Aguarde...Eliminando saldos incompletos:"
                        AT 2303 WITH SIZE 68
                OPEN I-O CBCOSA
                PERFORM UNTIL FS-CBCOSA > "09"
                  READ CBCOSA NEXT RECORD
                  IF   FS-CBCOSA < "10"
                       DISPLAY CBCOSA-CONTA AT 2346
                       IF   CBCOSA-AAAAMM > CBGEIN-ANTERIOR
                            DELETE CBCOSA RECORD
                       END-IF
                  END-IF
                END-PERFORM
                CLOSE CBCOSA
                MOVE CBGEIN-ATUAL TO LB-CBMVMS (1: 6)
                DELETE FILE CBMVMS
                OPEN OUTPUT CBCOMS
                MOVE CBGEIN-ANTERIOR TO CBCOMS-REG
                WRITE CBCOMS-REG
                CLOSE CBCOMS CBGEIN
                DELETE FILE CBGEIN
                DISPLAY SPACES AT 2303 WITH SIZE 68
                GO 700-VERIFICA-MES
           END-IF.

       700-99-FIM. EXIT.
