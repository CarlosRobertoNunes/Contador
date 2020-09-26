       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB009PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  16/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Listagem do controle de BACs                 *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOBASL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOBAFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 ws-OPTION                PIC  9(002) VALUE 0.
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 CHAVE                    PIC  9(008) VALUE ZERO.
           05 REFERENCIA               PIC  9(006) VALUE ZERO.
           05 REDEFINES REFERENCIA.
              10 MM                    PIC  9(002).
              10 AAAA                  PIC  9(004).
           05 LD-CBCOBA         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 INICIO-X                 PIC  X(006) VALUE SPACES.
           05 FIM-X                    PIC  X(006) VALUE SPACES.
           05 INICIO                   PIC  9(008) VALUE ZEROS.
           05 REDEFINES INICIO.
              10 INICIO-SE             PIC  9(002).
              10 INICIO-AA             PIC  9(002).
              10 INICIO-MM             PIC  9(002).
              10 INICIO-DD             PIC  9(002).
           05 REDEFINES INICIO.
              10 INICIO-DIA-AC         PIC  9(002).
              10 INICIO-MES-AC         PIC  9(002).
              10 INICIO-ANO-AC         PIC  9(004).
           05 FIM                      PIC  9(008) VALUE ZEROS.
           05 REDEFINES FIM.
              10 MES-FIM               PIC  9(006).
              10 REDEFINES MES-FIM.
                 15 FIM-SE             PIC  9(002).
                 15 FIM-AA             PIC  9(002).
                 15 FIM-MM             PIC  9(002).
              10 FIM-DD                PIC  9(002).
           05 REDEFINES FIM.
              10 FIM-DIA-AC            PIC  9(002).
              10 FIM-MES-AC            PIC  9(002).
              10 FIM-ANO-AC            PIC  9(004).
           05 TESTE-INICIO             PIC  9(006).
           05 TESTE-FIM                PIC  9(006).
           05 MENSAGENS-DE-ERRO.
              10 MSG01 PIC X(30) VALUE "Referˆncia inicial inv lida   ".
              10 MSG02 PIC X(30) VALUE "Referˆncia final inv lida     ".

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(025) VALUE
              "CONSISTENCIA DOS BACS".
       02  LINHA-01A.
           05 FILLER                         PIC  X(004) VALUE " DE ".
           05 CLIC-INICIO                    PIC  99/9999.
           05 FILLER                         PIC  X(003) VALUE " A ".
           05 CLIC-FIM                       PIC  99/9999.
       02  LINHA-02.
           05 FILLER                         PIC  X(052) VALUE
              "BAC  REFERENCIA         LANCAMENTOS           A ".
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(025) VALUE
              "EDITO            A DEBITO".
       02  LINHA-03.
           05 CLIC-CHAVE                     PIC  9999/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-REFERENCIA                PIC  99/9999.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-OBS                       PIC  X(013) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-LC                        PIC  ZZZZZZZ9-.
           05 CLIC-CR                        PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DB                        PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.

       COPY CWIMPR.

       SCREEN SECTION.

       01  CB009PA.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 08 COLUMN 09 PIC X(015) FROM LB-CBCOBA.
           05 LINE 10 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBCOBA LINE 08 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBCOBA.
           05 T-GR-PRNTER LINE 10 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       01  CB009PB AUTO.
           05 LINE 18 COLUMN 03 VALUE "Per¡odo desejado de ".
           05 LINE 18 COLUMN 23 VALUE "00/0000 a 00/0000".
           05 LINE 18 COLUMN 23 PIC 99/  BLANK ZERO USING INICIO-MES-AC.
           05 LINE 18 COLUMN 26 PIC 9999 BLANK ZERO USING INICIO-ANO-AC.
           05 LINE 18 COLUMN 33 PIC 99/  BLANK ZERO USING FIM-MES-AC.
           05 LINE 18 COLUMN 36 PIC 9999 BLANK ZERO USING FIM-ANO-AC.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-CBCOBA > "09"
              READ CBCOBA NEXT RECORD IGNORE LOCK
              IF   FS-CBCOBA < "10"
                   ADD  1                TO LD-CBCOBA
                   DISPLAY                T-LD-CBCOBA
                   IF  (ws-OPTION = 2
                   OR   (CBCOBA-LC-PREVISTOS NOT = CBCOBA-LC-EFETIVOS)
                   OR   (CBCOBA-CR-PREVISTOS NOT = CBCOBA-CR-EFETIVOS)
                   OR   (CBCOBA-DB-PREVISTOS NOT = CBCOBA-DB-EFETIVOS))
                   AND ((CBCOBA-REFERENCIA   NOT < INICIO-X)
                   AND  (CBCOBA-REFERENCIA   NOT > FIM-X))
                        PERFORM 110-LISTA THRU 110-99-FIM
                   END-IF
              END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       110-LISTA.

           ADD  1                   TO GR-PRNTER
           DISPLAY                    T-GR-PRNTER
           MOVE CBCOBA-CHAVE        TO CHAVE
           MOVE CHAVE               TO CLIC-CHAVE
           MOVE CBCOBA-AAAA         TO AAAA
           MOVE CBCOBA-MM           TO MM
           MOVE REFERENCIA          TO CLIC-REFERENCIA
           MOVE CBCOBA-LC-PREVISTOS TO CLIC-LC
           MOVE CBCOBA-CR-PREVISTOS TO CLIC-CR
           MOVE CBCOBA-DB-PREVISTOS TO CLIC-DB

           IF   (CBCOBA-LC-PREVISTOS = CBCOBA-LC-EFETIVOS)
           AND  (CBCOBA-CR-PREVISTOS = CBCOBA-CR-EFETIVOS)
           AND  (CBCOBA-DB-PREVISTOS = CBCOBA-DB-EFETIVOS)
                MOVE "     FECHADO" TO CLIC-OBS
                MOVE LINHA-03       TO CWIMPR-DETAIL
                CALL "CWIMPR"    USING PARAMETROS-CWIMPR
                IF   CWIMPR-END-PRINT
                     CLOSE CBCOBA
                     GOBACK
                END-IF
                GO TO 110-99-FIM
           ELSE
                MOVE "   PREVISTOS" TO CLIC-OBS
           END-IF

           IF   VEZ = 1
                MOVE 2 TO VEZ
           ELSE
                IF   CWIMPR-DETAIL NOT = SPACES
                     MOVE SPACES      TO CWIMPR-DETAIL
                     CALL "CWIMPR" USING PARAMETROS-CWIMPR
                     IF   CWIMPR-END-PRINT
                          CLOSE CBCOBA
                          GOBACK
                     END-IF.

           MOVE LINHA-03           TO CWIMPR-DETAIL
           CALL "CWIMPR"        USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBCOBA
                GOBACK
           END-IF
           MOVE SPACES             TO LINHA-03
           MOVE CBCOBA-LC-EFETIVOS TO CLIC-LC
           MOVE CBCOBA-CR-EFETIVOS TO CLIC-CR
           MOVE CBCOBA-DB-EFETIVOS TO CLIC-DB
           MOVE "    EFETIVOS"     TO CLIC-OBS
           MOVE LINHA-03           TO CWIMPR-DETAIL
           CALL "CWIMPR"        USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBCOBA
                GOBACK
           END-IF
           COMPUTE CLIC-LC = CBCOBA-LC-EFETIVOS - CBCOBA-LC-PREVISTOS
           COMPUTE CLIC-CR = CBCOBA-CR-EFETIVOS - CBCOBA-CR-PREVISTOS
           COMPUTE CLIC-DB = CBCOBA-DB-EFETIVOS - CBCOBA-DB-PREVISTOS
           MOVE "DIFERENCA(S)"     TO CLIC-OBS
           MOVE LINHA-03           TO CWIMPR-DETAIL
           CALL "CWIMPR"        USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBCOBA
                GOBACK
           END-IF
           MOVE SPACES             TO CWIMPR-DETAIL
           CALL "CWIMPR"        USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBCOBA
                GOBACK
           END-IF.

       110-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCOBA
           IF   FS-CBCOBA > "09"
                GOBACK.

           EXEC COBOLware BOXSelect NOERASE
                TITLE "Op‡äes"
                LINE 12 COLUMN 04
                CAPTION(1) " ~Abertos "
                CAPTION(2) " ~Todos"
                OPTION 1;ws-OPTION
           END-EXEC

           IF   ws-OPTION = 0
                CLOSE CBCOBA
                GOBACK
           END-IF

           DISPLAY CB009PB
           PERFORM TEST AFTER UNTIL TECLA = 01
                                 OR (TESTE-INICIO NOT = ZEROS
                                    AND TESTE-FIM NOT = ZEROS)
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      MOVE 01 TO INICIO-DIA-AC
                                 FIM-DIA-AC
                      ACCEPT CB009PB
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE             "CB009PCW.H01"
                                LINE 16 COLUMN 22
                                HEIGHT 02
                                WIDTH 40
                           END-EXEC
                      END-IF
                   END-PERFORM
                   IF   TECLA NOT = 01
                        IF   INICIO = 0
                        AND  FIM = 0
                             MOVE LOW-VALUES  TO INICIO-X
                             MOVE HIGH-VALUES TO FIM-X
                        ELSE
                             MOVE INICIO        TO CLIC-INICIO
                             MOVE FIM           TO CLIC-FIM
                             MOVE INICIO (1: 2) TO TESTE-INICIO (1: 2)
                             MOVE INICIO (3: 2) TO TESTE-INICIO (3: 2)
                             MOVE INICIO (7: 2) TO TESTE-INICIO (5: 2)
                             MOVE FIM    (1: 2) TO TESTE-FIM    (1: 2)
                             MOVE FIM    (3: 2) TO TESTE-FIM    (3: 2)
                             MOVE FIM    (7: 2) TO TESTE-FIM    (5: 2)
                             CALL "GRVDAT" USING TESTE-INICIO
                             IF   TESTE-INICIO = ZEROS
                                  EXEC COBOLware Send
                                       Message MSG01
                                  END-EXEC
                             ELSE
                                  CALL "GRVDAT" USING TESTE-FIM
                                  IF   TESTE-FIM = ZEROS
                                       EXEC COBOLware Send
                                            Message MSG02
                                       END-EXEC
                                  END-IF
                             END-IF
                             MOVE INICIO (5: 4) TO INICIO-X
                             MOVE INICIO (3: 2) TO INICIO-X (5: 2)
                             MOVE FIM    (5: 4) TO FIM-X
                             MOVE FIM    (3: 2) TO FIM-X    (5: 2)
                        END-IF
                   END-IF
           END-PERFORM

           IF   TECLA = 01
                CLOSE CBCOBA
                GOBACK.

           DISPLAY CB009PA
           MOVE "CB009PA"         TO CWIMPR-REPORT
           MOVE LINHA-01          TO CWIMPR-SUB-TITLE
           MOVE LINHA-02          TO CWIMPR-HEADER-1
           MOVE 2                 TO CWIMPR-FORM-TYPE

           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW"

           IF   ws-OPTION = 1
                MOVE "ABERTOS" TO CWIMPR-TITLE (27: )
           END-IF.

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           CLOSE CBCOBA.

       900-99-FIM. EXIT.

       END PROGRAM CB009PCW.
