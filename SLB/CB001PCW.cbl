       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB001PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  26/12/1990.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de parametros                     *
                      *                                               *
                      *  Dados da Empresas/estrutura de contas(graus) *
                      *  diretorios, opcao de paginacao               *
                      *  e formula de calculo do DV                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPAPCSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPAPCFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 SUG         PIC 9(002) VALUE ZEROS.
           05 SUG-M       PIC 9(002) VALUE ZEROS.
           05 SUG-D       PIC 9(002) VALUE ZEROS.
           05 CAMPO       PIC 9(002) VALUE ZEROS.
           05 MINUSCULAS  PIC X(026) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS  PIC X(026) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 RESULTADO   PIC 9(002) COMP-5 VALUE ZEROS.
           05 CMDLINE     PIC X(051) VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 F PIC X(30) VALUE "Exedeu o limite de 15 d¡gitos ".
              10 F PIC X(30) VALUE "Defini‡Æo inconsistente       ".
              10 F PIC X(30) VALUE "Falta definir estrutura       ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 3 PIC X(30).
           05 ESPACOS                  PIC  X(068) VALUE SPACES.
           05 RODAPE                   PIC  X(068) VALUE
              "<Esc>-Abandona F1-Help F2-Aceita f¢rmula F3-SugestÆo".
           05 CONFIRMA                 PIC  X(001) VALUE SPACE.
           05 SALVA-OPCAO              PIC  9(002) VALUE 1.
           05 DGC                      PIC  9(002) VALUE ZEROS.
           05 G                        PIC  9(002) VALUE ZEROS.
           05 COL-V                    PIC  9(002) VALUE ZEROS.
           05 I                        PIC  9(002) COMP VALUE 1.
           05 ERRO                     PIC  9(002) COMP VALUE 1.
           05 DGS                      PIC  9(002) COMP VALUE ZEROS.
           05 RK-CBPAPC                PIC  9(002) COMP VALUE 1.
           05 ER-CBPAPC.
              10 FS-CBPAPC             PIC  X(002) VALUE "00".
              10 LB-CBPAPC             PIC  X(050) VALUE "CBPAPC".
           05 TECLA                    PIC  9(002)      VALUE ZERO.
              COPY CWKEYS.
           05 HELP-FILE                PIC  X(050) VALUE SPACES.
           05 OPCOES-DE-HELP.
              10 PIC X(8) VALUE "05121744".
              10 PIC X(8) VALUE "05151744".
              10 PIC X(8) VALUE "05181744".
              10 PIC X(8) VALUE "05211744".
              10 PIC X(8) VALUE "05241744".
              10 PIC X(8) VALUE "05271744".
              10 PIC X(8) VALUE "05301744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "05331744".
              10 PIC X(8) VALUE "16180642".
              10 PIC X(8) VALUE "10511025".
           05 REDEFINES OPCOES-DE-HELP.
              10 OCCURS 17.
                 15 HELP-LIN PIC 99.
                 15 HELP-COL PIC 99.
                 15 HELP-VER PIC 99.
                 15 HELP-HOR PIC 99.
       COPY CWNCOR.

       LINKAGE SECTION.

       01  OPCAO                      PIC 9(001).

       SCREEN SECTION.

       01  CB001PA.
           03 background-color red foreground-color white.
           05 LINE 08 COLUMN 17 VALUE "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05 LINE 09 COLUMN 17 VALUE "º Estrutura de contas º".
           05 LINE 10 COLUMN 17 VALUE "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹".
           05 LINE 11 COLUMN 17 VALUE "º 1§ grau     d¡gitos º".
           05 LINE 12 COLUMN 17 VALUE "º 2§                  º".
           05 LINE 13 COLUMN 17 VALUE "º 3§                  º".
           05 LINE 14 COLUMN 17 VALUE "º 4§                  º".
           05 LINE 15 COLUMN 17 VALUE "º 5§                  º".
           05 LINE 16 COLUMN 17 VALUE "º 6§                  º".
           05 LINE 17 COLUMN 17 VALUE "º 7§                  º".
           05 LINE 18 COLUMN 17 VALUE "º 8§                  º".
           05 LINE 19 COLUMN 17 VALUE "º 9§                  º".
           05 LINE 20 COLUMN 17 VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".
           05 DG1 LINE 11 COLUMN 28 PIC Z(002) USING CBPAPC-DG1.
           05 DG2 LINE 12 COLUMN 28 PIC Z(002) USING CBPAPC-DG2.
           05 DG3 LINE 13 COLUMN 28 PIC Z(002) USING CBPAPC-DG3.
           05 DG4 LINE 14 COLUMN 28 PIC Z(002) USING CBPAPC-DG4.
           05 DG5 LINE 15 COLUMN 28 PIC Z(002) USING CBPAPC-DG5.
           05 DG6 LINE 16 COLUMN 28 PIC Z(002) USING CBPAPC-DG6.
           05 DG7 LINE 17 COLUMN 28 PIC Z(002) USING CBPAPC-DG7.
           05 DG8 LINE 18 COLUMN 28 PIC Z(002) USING CBPAPC-DG8.
           05 DG9 LINE 19 COLUMN 28 PIC Z(002) USING CBPAPC-DG9.

       01  CB001PC.
           03 background-color green foreground-color white.
           05 LINE 07 COLUMN 05 VALUE "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 07 COLUMN 25 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 07 COLUMN 45 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05 LINE 08 COLUMN 05 VALUE "º C lculo do DV da co".
           05 LINE 08 COLUMN 25 VALUE "onta                ".
           05 LINE 08 COLUMN 45 VALUE "             º".
           05 LINE 09 COLUMN 05 VALUE "º                   ".
           05 LINE 09 COLUMN 25 VALUE "                    ".
           05 LINE 09 COLUMN 45 VALUE "             º".
           05 LINE 10 COLUMN 05 VALUE "º                   ".
           05 LINE 10 COLUMN 25 VALUE "                    ".
           05 LINE 10 COLUMN 45 VALUE "             º".
           05 LINE 11 COLUMN 05 VALUE "º D(i)              ".
           05 LINE 11 COLUMN 25 VALUE "                    ".
           05 LINE 11 COLUMN 45 VALUE "             º".
           05 LINE 12 COLUMN 05 VALUE "º   x               ".
           05 LINE 12 COLUMN 25 VALUE "                    ".
           05 LINE 12 COLUMN 45 VALUE "             º".
           05 LINE 13 COLUMN 05 VALUE "º X(i)              ".
           05 LINE 13 COLUMN 25 VALUE "                    ".
           05 LINE 13 COLUMN 45 VALUE "             º".
           05 LINE 14 COLUMN 05 VALUE "º                   ".
           05 LINE 14 COLUMN 25 VALUE "                    ".
           05 LINE 14 COLUMN 45 VALUE "             º".
           05 LINE 15 COLUMN 05 VALUE "º Somat¢rio         ".
           05 LINE 15 COLUMN 25 VALUE "                    ".
           05 LINE 15 COLUMN 45 VALUE "             º".
           05 LINE 16 COLUMN 05 VALUE "º D(i).X(i)³        ".
           05 LINE 16 COLUMN 25 VALUE "                    ".
           05 LINE 16 COLUMN 45 VALUE "             º".
           05 LINE 17 COLUMN 05 VALUE "º ÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ".
           05 LINE 17 COLUMN 25 VALUE "ÄÄ  ->    D¡gito =  ".
           05 LINE 17 COLUMN 45 VALUE "  - Resto    º".
           05 LINE 18 COLUMN 05 VALUE "º          ³ Quocien".
           05 LINE 18 COLUMN 25 VALUE "te                  ".
           05 LINE 18 COLUMN 45 VALUE "             º".
           05 LINE 19 COLUMN 05 VALUE "º          ³        ".
           05 LINE 19 COLUMN 25 VALUE "          Se D¡gito ".
           05 LINE 19 COLUMN 45 VALUE "             º".
           05 LINE 20 COLUMN 05 VALUE "º    Resto ³        ".
           05 LINE 20 COLUMN 25 VALUE "          tornar D¡g".
           05 LINE 20 COLUMN 45 VALUE "ito =        º".
           05 LINE 21 COLUMN 05 VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 21 COLUMN 25 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 21 COLUMN 45 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".
           05 MODULO-1 LINE 16 COLUMN 18 PIC Z(002)
                                         USING CBPAPC-MODULO-1.
           05 MODULO-2 LINE 17 COLUMN 44 PIC ZZ FROM CBPAPC-MODULO-1.
           05 OPERADOR LINE 19 COLUMN 45 PIC X(001)
                                         FROM CBPAPC-OPERADOR.
           05 MODULO-3 LINE 19 COLUMN 47 PIC Z(001)
                                         FROM CBPAPC-MODULO-3.
           05 DV       LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-01.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-02.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-03.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-04.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-05.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-06.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-07.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-08.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-09.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-10.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 13 COLUMN 39 PIC Z(002) USING CBPAPC-M10.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-11.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 13 COLUMN 39 PIC Z(002) USING CBPAPC-M10.
           05 LINE 13 COLUMN 42 PIC Z(002) USING CBPAPC-M11.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-12.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 13 COLUMN 39 PIC Z(002) USING CBPAPC-M10.
           05 LINE 13 COLUMN 42 PIC Z(002) USING CBPAPC-M11.
           05 LINE 13 COLUMN 45 PIC Z(002) USING CBPAPC-M12.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-13.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 13 COLUMN 39 PIC Z(002) USING CBPAPC-M10.
           05 LINE 13 COLUMN 42 PIC Z(002) USING CBPAPC-M11.
           05 LINE 13 COLUMN 45 PIC Z(002) USING CBPAPC-M12.
           05 LINE 13 COLUMN 48 PIC Z(002) USING CBPAPC-M13.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-14.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 13 COLUMN 39 PIC Z(002) USING CBPAPC-M10.
           05 LINE 13 COLUMN 42 PIC Z(002) USING CBPAPC-M11.
           05 LINE 13 COLUMN 45 PIC Z(002) USING CBPAPC-M12.
           05 LINE 13 COLUMN 48 PIC Z(002) USING CBPAPC-M13.
           05 LINE 13 COLUMN 51 PIC Z(002) USING CBPAPC-M14.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       01  CB001PD-15.
           05 LINE 13 COLUMN 12 PIC Z(002) USING CBPAPC-M01.
           05 LINE 13 COLUMN 15 PIC Z(002) USING CBPAPC-M02.
           05 LINE 13 COLUMN 18 PIC Z(002) USING CBPAPC-M03.
           05 LINE 13 COLUMN 21 PIC Z(002) USING CBPAPC-M04.
           05 LINE 13 COLUMN 24 PIC Z(002) USING CBPAPC-M05.
           05 LINE 13 COLUMN 27 PIC Z(002) USING CBPAPC-M06.
           05 LINE 13 COLUMN 30 PIC Z(002) USING CBPAPC-M07.
           05 LINE 13 COLUMN 33 PIC Z(002) USING CBPAPC-M08.
           05 LINE 13 COLUMN 36 PIC Z(002) USING CBPAPC-M09.
           05 LINE 13 COLUMN 39 PIC Z(002) USING CBPAPC-M10.
           05 LINE 13 COLUMN 42 PIC Z(002) USING CBPAPC-M11.
           05 LINE 13 COLUMN 45 PIC Z(002) USING CBPAPC-M12.
           05 LINE 13 COLUMN 48 PIC Z(002) USING CBPAPC-M13.
           05 LINE 13 COLUMN 51 PIC Z(002) USING CBPAPC-M14.
           05 LINE 13 COLUMN 54 PIC Z(002) USING CBPAPC-M15.
           05 LINE 16 COLUMN 18 PIC Z(002) USING CBPAPC-MODULO-1.
           05 LINE 20 COLUMN 51 PIC X(001) USING CBPAPC-DV.

       PROCEDURE DIVISION USING OPCAO.

       000-INICIO.

           OPEN INPUT CBPAPC

           IF   FS-CBPAPC = "30" OR "35"
                CLOSE CBPAPC
                OPEN OUTPUT CBPAPC
                MOVE ZEROS  TO CBPAPC-REG
                MOVE SPACES TO CBPAPC-DV
                WRITE CBPAPC-REG
                IF  FS-CBPAPC > "09"
                    CLOSE CBPAPC
                    GOBACK
                END-IF
           END-IF

           CLOSE CBPAPC
           OPEN I-O CBPAPC

           IF  FS-CBPAPC > "09"
               CLOSE CBPAPC
               GOBACK
           END-IF

           READ CBPAPC

           IF  FS-CBPAPC > "09"
               CLOSE CBPAPC
               GOBACK
           END-IF

           EVALUATE OPCAO
                    WHEN 1 PERFORM 100-CONTAS  THRU 100-99-FIM
                    WHEN 2 PERFORM 200-DIGITO  THRU 200-99-FIM
           END-EVALUATE

           CLOSE CBPAPC
           GOBACK.

       100-CONTAS.

           DISPLAY CB001PA
                   ESPACOS
                   LINE 23 COLUMN 03
                   "<Esc>-Abandona defini‡Æo <Enter>-Aceita estrutura"
                   LINE 23 COLUMN 03

           IF   CBPAPC-ESTRUTURA-CONTA = ZEROS
                MOVE 1 TO G
                PERFORM UNTIL CONFIRMA = "O"
                   PERFORM TEST AFTER UNTIL ERRO = 0
                      PERFORM TEST AFTER UNTIL NOT F1
                         EVALUATE G
                                  WHEN 1 ACCEPT DG1
                                  WHEN 2 ACCEPT DG2
                                  WHEN 3 ACCEPT DG3
                                  WHEN 4 ACCEPT DG4
                                  WHEN 5 ACCEPT DG5
                                  WHEN 6 ACCEPT DG6
                                  WHEN 7 ACCEPT DG7
                                  WHEN 8 ACCEPT DG8
                                  WHEN 9 ACCEPT DG9
                         END-EVALUATE
                         ACCEPT TECLA FROM ESCAPE KEY
                         IF   F1
                              CLOSE CBPAPC
                              EXEC COBOLware Help
                                   FILE   "CB001PCW.H18"
                                   LINE   10
                                   COLUMN 31
                                   LINES  06
                                   WIDTH  42
                              END-EXEC
                              OPEN I-O CBPAPC
                         END-IF
                      END-PERFORM
                      IF   CURSOR-UP
                           IF   G > 1
                                SUBTRACT 1 FROM G
                           END-IF
                           MOVE 1 TO ERRO
                      ELSE
                           IF   ESC
                                MOVE ZEROS TO ERRO
                                              CBPAPC-ESTRUTURA-CONTA
                                MOVE "O"   TO CONFIRMA
                           ELSE
                                IF  CBPAPC-DG (G) NOT = 0
                                AND G < 9
                                    MOVE 1 TO ERRO
                                    ADD 1 TO G
                                ELSE
                                    MOVE 0 TO DGS
                                    PERFORM VARYING I FROM 1 BY 1
                                            UNTIL (I > 9)
                                               OR (ERRO = 2)
                                            IF  I > G
                                            AND CBPAPC-DG (I) NOT = 0
                                                EXEC COBOLware Send
                                                     Message MSG(2)
                                                END-EXEC
                                                MOVE 2 TO ERRO
                                            END-IF
                                            ADD CBPAPC-DG (I) TO DGS
                                    END-PERFORM
                                    IF   DGS < 16
                                    OR   ERRO = 2
                                         IF   ERRO NOT = 2
                                              MOVE 0 TO ERRO
                                         ELSE
                                              MOVE 1 TO ERRO
                                         END-IF
                                    ELSE
                                         EXEC COBOLware Send
                                              Message MSG(1)
                                         END-EXEC
                                    END-IF
                                END-IF
                           END-IF
                      END-IF
                   END-PERFORM
                   IF  CBPAPC-ESTRUTURA-CONTA NOT = ZEROS
                       EXEC COBOLware Send
                         Message "Confirmar defini‡Æo definitivamente ?"
                             Option 2
                            Caption(1) "   ~Ok___"
                            Caption(2) "~Cancelar"
                            OPTION-CHAR;CONFIRMA
                       END-EXEC
                       IF   CONFIRMA = "O"
                            REWRITE CBPAPC-REG
                            IF  FS-CBPAPC > "09"
                                CLOSE CBPAPC
                                GOBACK
                            END-IF
                       END-IF
                   END-IF
                END-PERFORM
           ELSE
                DISPLAY ESPACOS LINE 23 COLUMN 03
                DISPLAY
                "Precione qualquer tecla para terminar a consulta "
                       LINE 23 COLUMN 03
                ACCEPT CONFIRMA LINE 23 COLUMN 52
                       WITH AUTO-SKIP SECURE
           END-IF

           DISPLAY ESPACOS LINE 23 COLUMN 03.

       100-99-FIM. EXIT.

       200-DIGITO.

           IF   CBPAPC-ESTRUTURA-CONTA = ZEROS
                EXEC COBOLware Send
                     Message MSG(3)
                END-EXEC
                EXIT PARAGRAPH
           END-IF


           COMPUTE DGC = CBPAPC-DG1
                       + CBPAPC-DG2
                       + CBPAPC-DG3
                       + CBPAPC-DG4
                       + CBPAPC-DG5
                       + CBPAPC-DG6
                       + CBPAPC-DG7
                       + CBPAPC-DG8
                       + CBPAPC-DG9
           COMPUTE SUG = DGC + 1
           MOVE ">" TO CBPAPC-OPERADOR
           MOVE 9   TO CBPAPC-MODULO-3
           DISPLAY CB001PC
                   RODAPE LINE 23 COLUMN 3
           PERFORM 210-JANELA-DIGITO THRU 210-99-FIM
           IF DGC > 00 DISPLAY CB001PD-01 END-IF
           IF DGC > 01 DISPLAY CB001PD-02 END-IF
           IF DGC > 02 DISPLAY CB001PD-03 END-IF
           IF DGC > 03 DISPLAY CB001PD-04 END-IF
           IF DGC > 04 DISPLAY CB001PD-05 END-IF
           IF DGC > 05 DISPLAY CB001PD-06 END-IF
           IF DGC > 06 DISPLAY CB001PD-07 END-IF
           IF DGC > 07 DISPLAY CB001PD-08 END-IF
           IF DGC > 08 DISPLAY CB001PD-09 END-IF
           IF DGC > 09 DISPLAY CB001PD-10 END-IF
           IF DGC > 10 DISPLAY CB001PD-11 END-IF
           IF DGC > 11 DISPLAY CB001PD-12 END-IF
           IF DGC > 12 DISPLAY CB001PD-13 END-IF
           IF DGC > 13 DISPLAY CB001PD-14 END-IF
           IF DGC > 14 DISPLAY CB001PD-15 END-IF

           PERFORM TEST AFTER UNTIL (CONFIRMA = "S" OR "s") OR ESC
                PERFORM TEST AFTER
                        UNTIL ERRO = 0
                   EVALUATE DGC
                     WHEN  1 ACCEPT CB001PD-01
                     WHEN  2 ACCEPT CB001PD-02
                     WHEN  3 ACCEPT CB001PD-03
                     WHEN  4 ACCEPT CB001PD-04
                     WHEN  5 ACCEPT CB001PD-05
                     WHEN  6 ACCEPT CB001PD-06
                     WHEN  7 ACCEPT CB001PD-07
                     WHEN  8 ACCEPT CB001PD-08
                     WHEN  9 ACCEPT CB001PD-09
                     WHEN 10 ACCEPT CB001PD-10
                     WHEN 11 ACCEPT CB001PD-11
                     WHEN 12 ACCEPT CB001PD-12
                     WHEN 13 ACCEPT CB001PD-13
                     WHEN 14 ACCEPT CB001PD-14
                     WHEN 15 ACCEPT CB001PD-15
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   MOVE CBPAPC-MODULO-1 TO CBPAPC-MODULO-2
                   DISPLAY MODULO-2
                   INSPECT CBPAPC-DV
                           CONVERTING MINUSCULAS TO MAIUSCULAS
                   IF   F3
                        MOVE 0 TO SUG-D
                        PERFORM VARYING SUG-M
                                   FROM SUG
                                     BY -1
                                  UNTIL SUG-M = 1
                                ADD  1     TO SUG-D
                                MOVE SUG-M TO CBPAPC-M (SUG-D)
                        END-PERFORM
                        MOVE 11 TO CBPAPC-MODULO-1
                                   CBPAPC-MODULO-2
                        MOVE ">" TO CBPAPC-OPERADOR
                        MOVE 9   TO CBPAPC-MODULO-3
                        MOVE "X" TO CBPAPC-DV
                        DISPLAY CB001PC
                        DISPLAY RODAPE LINE 23 COLUMN 3
                        PERFORM 210-JANELA-DIGITO THRU 210-99-FIM
                        MOVE 1 TO ERRO
                        EXIT PERFORM CYCLE
                   END-IF
                   IF   ESC
                        READ CBPAPC
                        DISPLAY CB001PC
                        PERFORM 210-JANELA-DIGITO THRU 210-99-FIM
                        EXIT PERFORM
                   END-IF
                   IF   F1
                        MOVE "CB001PCW.Hxx" TO HELP-FILE
                        MOVE G              TO HELP-FILE (11: 2)
                        IF   G < 16
                             MOVE "01" TO HELP-FILE (11: 2)
                        END-IF
                        CLOSE CBPAPC
                        OPEN I-O CBPAPC
      *                 EXEC COBOLware Help
      *                      FILE              HELP-FILE
      *                      LINE              HELP-LIN(G)
      *                      COLUMN            HELP-COL(G)
      *                      VERTICAL-LENGTH   HELP-VER(G)
      *                      HORIZONTAL-LENGTH HELP-HOR(G)
      *                 END-EXEC
                   ELSE
                        IF   F2
                             MOVE ZEROS TO ERRO
                             MOVE "S"   TO CONFIRMA
                             MOVE 1 TO ERRO
                        END-IF
                   END-IF
                END-PERFORM
                EXEC COBOLware Send
                     Message "Confirmar defini‡Æo ?"
                     Option 2
                     Caption(1) "   ~Ok___"
                     Caption(2) "~Cancelar"
                     OPTION-CHAR;CONFIRMA
                END-EXEC
                IF   CONFIRMA = "S" OR "s"
                     REWRITE CBPAPC-REG
                     IF  FS-CBPAPC > "09"
                         CLOSE CBPAPC
                         GOBACK
                     END-IF
                ELSE
                     READ CBPAPC
                END-IF
           END-PERFORM

           DISPLAY ESPACOS LINE 23 COLUMN 03.

       200-99-FIM. EXIT.

       210-JANELA-DIGITO.

           DISPLAY "Ú" LINE 09 COLUMN 11
                   "³" LINE 10 COLUMN 11
                   "³" LINE 11 COLUMN 11
                   "À" LINE 12 COLUMN 11
           MOVE 0  TO G
           MOVE 12 TO COL-V
           PERFORM DGC TIMES
                   ADD 1 TO G
                   DISPLAY "ÄÄÄ" LINE 09 COLUMN COL-V
                           "   " LINE 10 COLUMN COL-V
                           G     LINE 11 COLUMN COL-V
                   ADD 2 TO COL-V
                   DISPLAY "³"   LINE 11 COLUMN COL-V
                   SUBTRACT 2 FROM COL-V
                   DISPLAY "ÄÄÁ" LINE 12 COLUMN COL-V
                   ADD 3 TO COL-V
           END-PERFORM
           SUBTRACT 1 FROM COL-V
           DISPLAY "¿" LINE 09 COLUMN COL-V
                   "³" LINE 10 COLUMN COL-V
                   "³" LINE 11 COLUMN COL-V
                   "Ù" LINE 12 COLUMN COL-V.

       210-99-FIM. EXIT.

       END PROGRAM CB001PCW.
