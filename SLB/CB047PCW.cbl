       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB047PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  26/12/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de parametros                     *
                      *                                               *
                      *  estrutura de contas(graus) e formula de      *
                      *  calcwlo do DV                                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPAEMSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPAEMFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 CHAMA-EDITOR.
              10 EDITOR   PIC X(050) VALUE "EDIT ".
              10 TEXTO    PIC X(050) VALUE SPACES.
           05 ws-OPTION   PIC 9(002) VALUE 0.
           05 CAMPO       PIC 9(002) VALUE 0.
           05 DIR-MOEDAS  PIC X(038) VALUE "..\COMUNS\MDS".
           05 DIR-HELP    PIC X(038) VALUE "             ".
           05 DIR-LAYOUT  PIC X(038) VALUE "..\COMUNS\LTS".
           05 TXT-TERMOS  PIC X(050) VALUE "TERMOS.TXT".
           05 MINUSCULAS  PIC X(026) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS  PIC X(026) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 RESULTADO   PIC 9(002) COMP-5 VALUE 0.
           05 FUNCAO      PIC 9(002) COMP-5 VALUE 35.
           05 CMDLINE     PIC X(051) VALUE SPACES.
           05 PARAMETRO.
              10 SIZE-BINARIO          PIC  9(002) COMP-5 VALUE 0.
              10 BINARIO.
                 15 BYTE-BINARIO       PIC  X(001) OCCURS 50.
           05 CONFIRMA                 PIC  X(001) VALUE SPACE.
           05 SALVA-OPCAO              PIC  9(002) VALUE 1.
           05 DGC                      PIC  9(002) VALUE 0.
           05 G                        PIC  9(002) VALUE 0.
           05 COL-V                    PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) COMP VALUE 1.
           05 ERRO                     PIC  9(002) COMP VALUE 1.
           05 DGS                      PIC  9(002) COMP VALUE 0.
           05 RK-CBPAEM                PIC  9(002) COMP VALUE 1.
           05 ER-CBPAEM.
              10 FS-CBPAEM             PIC  X(002) VALUE "00".
              10 LB-CBPAEM             PIC  X(050) VALUE "CBPAEM".
           05 TECLA                    PIC  9(002)      VALUE ZERO.
              COPY CWKEYS.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-5 VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-5 VALUE 0.
              10 CARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER     PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH        PIC  9(004) COMP-5 VALUE 2000.
              10 STRING-START         PIC  9(004) COMP-5 VALUE 1.
       COPY CWBOXS.
       COPY CWNCOR.

       SCREEN SECTION.

       01  CB047PD.
           03 background-color cyan foreground-color blue high.
           05 LINE 10 COLUMN 12 VALUE "ÉÍDiret¢riosÍÍÍÍÍÍÍÍ".
           05 LINE 10 COLUMN 32 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 10 COLUMN 52 VALUE "ÍÍÍÍÍÍÍÍÍÍÍ»".
           05 LINE 11 COLUMN 12 VALUE "º                   ".
           05 LINE 11 COLUMN 32 VALUE "                    ".
           05 LINE 11 COLUMN 52 VALUE "           º".
           05 LINE 12 COLUMN 12 VALUE "º Moedas    : ".
           05 LINE 12 COLUMN 62 VALUE " º".
           05 LINE 13 COLUMN 12 VALUE "º                   ".
           05 LINE 13 COLUMN 32 VALUE "                    ".
           05 LINE 13 COLUMN 52 VALUE "           º".
           05 LINE 14 COLUMN 12 VALUE "º Help      : ".
           05 LINE 14 COLUMN 62 VALUE " º".
           05 LINE 15 COLUMN 12 VALUE "º                   ".
           05 LINE 15 COLUMN 32 VALUE "                    ".
           05 LINE 15 COLUMN 52 VALUE "           º".
           05 LINE 16 COLUMN 12 VALUE "º Estruturas: ".
           05 LINE 16 COLUMN 62 VALUE " º".
           05 LINE 17 COLUMN 12 VALUE "º                   ".
           05 LINE 17 COLUMN 32 VALUE "                    ".
           05 LINE 17 COLUMN 52 VALUE "           º".
           05 LINE 18 COLUMN 12 VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 18 COLUMN 32 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 18 COLUMN 52 VALUE "ÍÍÍÍÍÍÍÍÍÍÍ¼".
           05 TELA-MOEDAS LINE 12 COLUMN 26 PIC X(036) USING DIR-MOEDAS.
           05 TELA-HELP   LINE 14 COLUMN 26 PIC X(036) USING DIR-HELP.
           05 TELA-LAYOUT LINE 16 COLUMN 26 PIC X(036) USING DIR-LAYOUT.

       01  CB047PE.
           03 background-color white foreground-color green.
           05 LINE 10 COLUMN 12 VALUE "ÉÍTermos de abertura".
           05 LINE 10 COLUMN 32 VALUE "/encerramentoÍÍÍÍÍÍÍ".
           05 LINE 10 COLUMN 52 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05 LINE 11 COLUMN 12 VALUE "º                   ".
           05 LINE 11 COLUMN 32 VALUE "                    ".
           05 LINE 11 COLUMN 52 VALUE "                      º".
           05 LINE 12 COLUMN 12 VALUE "º B sico : ".
           05 LINE 12 COLUMN 62 VALUE "            º".
           05 LINE 13 COLUMN 12 VALUE "º                   ".
           05 LINE 13 COLUMN 32 VALUE "                    ".
           05 LINE 13 COLUMN 52 VALUE "                      º".
           05 LINE 14 COLUMN 12 VALUE "º Editor : ".
           05 LINE 14 COLUMN 62 VALUE "            º".
           05 LINE 15 COLUMN 12 VALUE "º                   ".
           05 LINE 15 COLUMN 32 VALUE "                    ".
           05 LINE 15 COLUMN 52 VALUE "                      º".
           05 LINE 16 COLUMN 12 VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 16 COLUMN 32 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 16 COLUMN 52 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".
           05 TELA-TERMOS
              LINE 12 COLUMN 23 PIC X(50) USING TXT-TERMOS.
           05 TELA-EDITOR
              LINE 14 COLUMN 23 PIC X(50) USING EDITOR.

       PROCEDURE DIVISION.

       000-INICIO.

           OPEN INPUT CBPAEM

           IF   FS-CBPAEM = "30" OR "35"
                CLOSE CBPAEM
                OPEN OUTPUT CBPAEM
                WRITE CBPAEM-REG FROM DIR-MOEDAS
                IF  FS-CBPAEM > "09"
                    CLOSE CBPAEM
                    GOBACK
                ELSE
                    MOVE 2 TO RK-CBPAEM WRITE CBPAEM-REG FROM DIR-HELP
                    MOVE 3 TO RK-CBPAEM WRITE CBPAEM-REG FROM DIR-LAYOUT
                    MOVE 4 TO RK-CBPAEM WRITE CBPAEM-REG FROM "02"
                    MOVE 5 TO RK-CBPAEM WRITE CBPAEM-REG FROM TXT-TERMOS
                    MOVE 6 TO RK-CBPAEM WRITE CBPAEM-REG FROM EDITOR
                    MOVE 1 TO RK-CBPAEM
                END-IF
           END-IF

           CLOSE CBPAEM
           OPEN I-O CBPAEM

           IF  FS-CBPAEM > "09"
               CALL "CWISAM" USING ER-CBPAEM
               CLOSE CBPAEM
               GOBACK
           END-IF

           READ CBPAEM

           IF  FS-CBPAEM > "09"
               CLOSE CBPAEM
               GOBACK
           END-IF

           PERFORM TEST AFTER UNTIL WS-OPTION > 5
                                 OR WS-OPTION = 0
                   EXEC COBOLware BoxSelect
                        TITLE " Op‡äes"
                        LINE 09 COLUMN 03
                        CAPTION(1)   " ~Estrutura de contas "
                        CAPTION(2)   " ~C lculo do dv"
                        CAPTION(3)   " ~Diret¢rios"
                        CAPTION(4)   " ~P ginas dos relat¢rios"
                        CAPTION(5)   " ~Termos de abertura/encerramento"
                        CAPTION(6)   " ~Menu geral"
                        CAPTION(7)   " ~Finalizar"
                        COLOR-FRAME  BROWN-BROWN-HIGH
                        COLOR-BORDER BROWN-BROWN-HIGH
                        OPTION       SALVA-OPCAO;WS-OPTION
                   END-EXEC
                   IF   WS-OPTION NOT = 0
                        MOVE  WS-OPTION TO SALVA-OPCAO
                   END-IF
                   MOVE "N" TO CONFIRMA
                   EVALUATE WS-OPTION
                            WHEN 1 CALL   "CB001PCW" USING "1"
                                   CANCEL "CB001PCW"
                            WHEN 2 CALL   "CB001PCW" USING "2"
                                   CANCEL "CB001PCW"
                            WHEN 3 PERFORM 300-DIR     THRU 300-99-FIM
                            WHEN 4 PERFORM 400-PAGINAS THRU 400-99-FIM
                            WHEN 5 PERFORM 500-TEXTOS  THRU 500-99-FIM
                   END-EVALUATE
           END-PERFORM

           CLOSE CBPAEM

           IF   WS-OPTION = 7
                STOP RUN
           END-IF

           GOBACK.

       300-DIR.

           DISPLAY CB047PD

           PERFORM VARYING RK-CBPAEM FROM 1 BY 1 UNTIL RK-CBPAEM > 3
                   READ CBPAEM
                   IF   FS-CBPAEM < "10"
                        EVALUATE RK-CBPAEM
                          WHEN 1 MOVE CBPAEM-REG TO DIR-MOEDAS
                                 DISPLAY TELA-MOEDAS
                          WHEN 2 MOVE CBPAEM-REG TO DIR-HELP
                                 DISPLAY TELA-HELP
                          WHEN 3 MOVE CBPAEM-REG TO DIR-LAYOUT
                                 DISPLAY TELA-LAYOUT
                        END-EVALUATE
                   ELSE
                        EVALUATE RK-CBPAEM
                          WHEN 1 WRITE CBPAEM-REG FROM DIR-MOEDAS
                          WHEN 2 WRITE CBPAEM-REG FROM DIR-HELP
                          WHEN 3 WRITE CBPAEM-REG FROM DIR-LAYOUT
                        END-EVALUATE
                   END-IF
           END-PERFORM

           ACCEPT  CB047PD
           PERFORM VARYING RK-CBPAEM FROM 1 BY 1 UNTIL RK-CBPAEM > 3
                   READ CBPAEM
                   EVALUATE RK-CBPAEM
                          WHEN 1 REWRITE CBPAEM-REG FROM DIR-MOEDAS
                          WHEN 2 REWRITE CBPAEM-REG FROM DIR-HELP
                          WHEN 3 REWRITE CBPAEM-REG FROM DIR-LAYOUT
                   END-EVALUATE
                   IF   FS-CBPAEM > "09"
                        CLOSE CBPAEM
                        GOBACK
                   END-IF
           END-PERFORM.
           MOVE 1 TO RK-CBPAEM
           READ CBPAEM.

       300-99-FIM. EXIT.

       400-PAGINAS.

           MOVE 4                       TO RK-CBPAEM
           READ CBPAEM IGNORE LOCK
           IF   FS-CBPAEM < "09"
                MOVE CBPAEM-TESTE-PAGINA TO WS-OPTION
           ELSE
                MOVE SPACES     TO CBPAEM-REG
                MOVE 1          TO WS-OPTION
                                   CBPAEM-TESTE-PAGINA
                WRITE CBPAEM-REG
                READ CBPAEM
           END-IF
           EXEC COBOLware BoxSelect NoErase
                LINE 18 COLUMN 40
                TITLE "N§ de p ginas dos relat¢rios"
                CAPTION(1)   " ~Come‡ar sempre de 1 "
                CAPTION(2)   " ~Pedir p gina inicial"
                COLOR-FRAME  CYAN-BROWN-HIGH
                COLOR-BORDER CYAN-BROWN-HIGH
                OPTION (WS-OPTION)
           END-EXEC
           IF   WS-OPTION NOT = 0
                MOVE WS-OPTION TO CBPAEM-TESTE-PAGINA
                REWRITE CBPAEM-REG
           END-IF
           MOVE 4 TO WS-OPTION
           MOVE 1 TO RK-CBPAEM
           READ CBPAEM.

       400-99-FIM. EXIT.

       500-TEXTOS.

           DISPLAY CB047PE

           PERFORM VARYING RK-CBPAEM FROM 5 BY 1 UNTIL RK-CBPAEM > 6
                   READ CBPAEM IGNORE LOCK
                   IF   FS-CBPAEM < "10"
                        EVALUATE RK-CBPAEM
                          WHEN 5 MOVE CBPAEM-REG TO TXT-TERMOS
                                 DISPLAY TELA-TERMOS
                          WHEN 6 MOVE CBPAEM-REG TO EDITOR
                                 DISPLAY TELA-EDITOR
                        END-EVALUATE
                   ELSE
                        EVALUATE RK-CBPAEM
                          WHEN 5 WRITE CBPAEM-REG FROM TXT-TERMOS
                          WHEN 6 WRITE CBPAEM-REG FROM EDITOR
                        END-EVALUATE
                   END-IF
           END-PERFORM

           MOVE 1 TO CAMPO
           PERFORM UNTIL CAMPO > 2
                   IF  CAMPO < 2
                       DISPLAY "(F2-Editar)" LINE 16 COLUMN 14
                   ELSE
                       DISPLAY "ÍÍÍÍÍÍÍÍÍÍÍ" LINE 16 COLUMN 14
                   END-IF
                   EVALUATE CAMPO
                       WHEN 1 ACCEPT TELA-TERMOS
                       WHEN 2 ACCEPT TELA-EDITOR
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   EVALUATE TRUE
                       WHEN ESC
                            MOVE 3 TO CAMPO
                       WHEN F2 AND CAMPO < 2
                            MOVE TXT-TERMOS   TO TEXTO
                            DISPLAY CHAMA-EDITOR UPON COMMAND-LINE
                            CALL "CBL_READ_SCR_CHATTRS"
                                           USING SCREEN-POSITION
                                                 CARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
                            CALL X"91" USING RESULTADO FUNCAO PARAMETRO
                            CALL "CBL_WRITE_SCR_CHATTRS"
                                           USING SCREEN-POSITION
                                                 CARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                  STRING-LENGTH
                       WHEN (ENTER-KEY OR CURSOR-DOWN)
                            ADD 1 TO CAMPO
                       WHEN CURSOR-UP AND CAMPO > 1
                            SUBTRACT 1    FROM CAMPO
                   END-EVALUATE
           END-PERFORM
           PERFORM VARYING RK-CBPAEM FROM 5 BY 1 UNTIL RK-CBPAEM > 6
                   READ CBPAEM
                   EVALUATE RK-CBPAEM
                          WHEN 5 REWRITE CBPAEM-REG FROM TXT-TERMOS
                          WHEN 6 REWRITE CBPAEM-REG FROM EDITOR
                   END-EVALUATE
                   IF   FS-CBPAEM > "09"
                        CLOSE CBPAEM
                        GOBACK
                   END-IF
           END-PERFORM
           MOVE 1 TO RK-CBPAEM
           READ CBPAEM.

       500-99-FIM. EXIT.

       END PROGRAM CB047PCW.
