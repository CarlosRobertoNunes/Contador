       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB013PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  10/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *   Atualizacao de moedas                       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOMDSL.
       COPY CBTAMDSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOMDFD.
       COPY CBTAMDFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 DATA-TESTE               PIC  9(006) VALUE 0.
           05 NOPATH-CBTAMD VALUE "CBMO99.DAT".
              15 FILLER                PIC  X(004).
              15 MOEDA-A-ATUALIZAR     PIC  9(002).
              15 FILLER                PIC  X(006).
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 RODAPE-FULL              PIC  X(068) VALUE
              "<Esc>-Outro F1-Help PgDn-Pr¢ximo PgUp-Anterior".
           05 RODAPE-KEY               PIC  X(068) VALUE
            "<Esc>-Fim F1-Help <Enter>-Muda PgDn-Pr¢ximo PgUp-Anterior".
           05 Nada                     PIC  X(001) VALUE SPACE.
           05 VIDEO                    PIC  X(020) VALUE SPACES.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 APAGADA                  PIC  9(002) VALUE ZEROS.
           05 FILLER                               VALUE ZEROS.
              10 PRIMEIRO OCCURS 5     PIC  9(002).
           05 CORRESPONDENCIAS                     VALUE ZEROS.
              10 CORRE OCCURS 15       PIC  9(002).
           05 PAGINA                   PIC  9(001) VALUE ZERO.
           05 ULTIMA                   PIC  9(002) VALUE ZERO.
           05 I                        PIC  9(003) VALUE ZERO.
           05 TIPO                     PIC  X(001) VALUE SPACE.
              88 DIARIA                            VALUE "D".
              88 MENSAL                            VALUE "M".
           05 Y                        PIC  9(003) VALUE ZERO.
           05 ER-CBCOMD.
              10 FS-CBCOMD              PIC  X(002) VALUE "00".
              10 LB-CBCOMD              PIC  X(050) VALUE "CBCOMD.DAT".
           05 ER-CBTAMD.
              10 FS-CBTAMD              PIC  X(002) VALUE "00".
              10 LB-CBTAMD              PIC  X(050) VALUE "CBTAMD.DAT".
           05 NOMES-MOEDAS                         VALUE SPACES.
              10 NOME-MOEDA OCCURS 99  PIC  X(014).
              10 TIPO-MOEDA OCCURS 99  PIC  X(001).
           05 CAMPO                    PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 DD                       PIC  9(002) VALUE ZERO.
           05 MES-REF                  PIC  9(006) VALUE ZERO.
           05 FILLER REDEFINES MES-REF.
              10 MM-REF                PIC  9(002).
                 88 MES-OK                         VALUE 01 THRU 12.
              10 AAAA-REF              PIC  9(004).
           05 DIAS-SEMANA                          VALUE SPACES.
              10 DIA OCCURS 31         PIC  X(007).
           05 MOEDAS-DO-MES                        VALUE ZEROS.
              10 MOEDA OCCURS 31       PIC  9(006)V9(004).
           05 CONTROLE-VALORES.
              10 ZERADO PIC 9 OCCURS 32.
           05 MOEDA-T                  PIC  9(006)V9(004) VALUE 0.
           05 T-LIN                    PIC  9(002) VALUE 0.
           05 T-COL                    PIC  9(002) VALUE 0.
           05 MAIOR-DIA                PIC  9(002) VALUE 0.
           05 DIA-EXT                  PIC  X(002) VALUE SPACES.
           05 DIA-WEEK                 PIC  X(007) VALUE SPACES.
           05 POSICOES PIC X(124) VALUE
              "100311031203130314031503160317031803190310291129122913291
      -       "429152916291729182919291055115512551355145515551655175518
      -       "5519552055".
           05 REDEFINES POSICOES.
              10 POSICAO OCCURS 31.
                 15 P-LIN PIC 99.
                 15 P-COL PIC 99.

       COPY CWTIME.
       COPY CWBOXS.

       SCREEN SECTION.

       01  CB018A.
           05 LINE 08 COLUMN 06 VALUE "Mˆs de referˆncia:".
           05 LINE 08 COLUMN 25 PIC   99/9999
              USING MES-REF AUTO.

       01  CB018B.
       03  DIA-NORMAL.
           05 LINE 1 COLUMN 01 PIC   X(003)        FROM  DIA-EXT.
           05 LINE 1 COLUMN 04 PIC   X(008)        FROM  DIA-WEEK.
           05 LINE 1 COLUMN 12 PIC   ZZZ.ZZ9,9999 BLANK ZERO
           USING MOEDA-T.

       01  CB018C.
       03  DIA-REVERSO REVERSE-VIDEO.
           05 LINE 1 COLUMN 01 PIC   X(003)        FROM  DIA-EXT.
           05 LINE 1 COLUMN 04 PIC   X(008)        FROM  DIA-WEEK.
           05 LINE 1 COLUMN 12 PIC   ZZZ.ZZ9,9999 BLANK ZERO
           USING MOEDA-T.

       01  CB018E.
           05 LINE 08 COLUMN 25 PIC   99/9999
              FROM MES-REF AUTO.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM VARYING CAMPO FROM 1 BY 1 UNTIL CAMPO > 31
                   SUBTRACT 1 FROM P-LIN (CAMPO)
                                   P-COL (CAMPO)
                   ADD 1 TO P-LIN (CAMPO)
                            P-COL (CAMPO)
           END-PERFORM
           MOVE 0      TO APAGADA
           MOVE SPACES TO NOMES-MOEDAS

           OPEN I-O CBCOMD

           MOVE 1 TO CBCOMD-MOEDA
           READ  CBCOMD
           IF    FS-CBCOMD = "23"
                 MOVE 1             TO CBCOMD-MOEDA
                 MOVE "Criar moeda" TO CBCOMD-NOME-MOEDA
                 MOVE 1             TO CBCOMD-ULTIMA
                                       ULTIMA
                 MOVE SPACE         TO CBCOMD-TIPO
                 WRITE CBCOMD-REG
                 IF   FS-CBCOMD > "09"
                      CLOSE CBCOMD
                      GOBACK
                 END-IF
           ELSE
                 IF    FS-CBCOMD < "10"
                       MOVE CBCOMD-ULTIMA TO ULTIMA
                 ELSE
                       CLOSE CBCOMD
                       GOBACK
                  END-IF
           END-IF

           MOVE 0 TO CBCOMD-MOEDA

           START CBCOMD KEY NOT < CBCOMD-MOEDA

           PERFORM 810-LER-CBCOMD THRU 810-99-FIM
                   UNTIL FS-CBCOMD >  "09"
           CLOSE CBCOMD

           MOVE 0 TO MOEDA-A-ATUALIZAR

           PERFORM 820-SELECIONA-MOEDA THRU 820-99-FIM

           IF   MOEDA-A-ATUALIZAR = 0
                GOBACK
           END-IF

           IF   MOEDA-A-ATUALIZAR NOT = 1
                MOVE "Renomear?" TO CWBOXS-TITLE
                MOVE 2           TO CWBOXS-OPTION
                MOVE SPACES      TO CWBOXS-ITENS
                MOVE " Sim"      TO CWBOXS-TEXT   (1)
                MOVE "S"         TO CWBOXS-CHAR   (1)
                MOVE " NÆo"      TO CWBOXS-TEXT   (2)
                MOVE "N"         TO CWBOXS-CHAR   (2)
                CALL "CWBOXS" USING PARAMETROS-CWBOXS
                IF   CWBOXS-OPTION = 1
                     OPEN I-O CBCOMD
                     PERFORM TEST AFTER UNTIL NOT F1
                           ACCEPT NOME-MOEDA (MOEDA-A-ATUALIZAR)
                                  LINE 08 COLUMN 40 WITH UPDATE
                           ACCEPT TECLA FROM ESCAPE KEY
                           IF   F1
                                EXEC COBOLware Help
                                     FILE   "CB013PCW.H01"
                                     LINE 10 COLUMN 35
                                     HEIGHT 03 WIDTH  42
                                END-EXEC
                           END-IF
                     END-PERFORM
                     IF   NOT ESC
                          MOVE MOEDA-A-ATUALIZAR TO CBCOMD-MOEDA
                          PERFORM TEST AFTER
                                  UNTIL FS-CBCOMD NOT = "9D"
                                  READ CBCOMD
                                  IF   FS-CBCOMD = "9D"
                                       CALL "CWISAM" USING ER-CBCOMD
                                  END-IF
                          END-PERFORM
                          IF   NOME-MOEDA (MOEDA-A-ATUALIZAR) = SPACES
                               MOVE "Excluir?" TO CWBOXS-TITLE
                               MOVE 2          TO CWBOXS-OPTION
                               CALL "CWBOXS" USING PARAMETROS-CWBOXS
                               IF   CWBOXS-OPTION = 1
                                    MOVE "E" TO CBCOMD-TIPO
                                    IF APAGADA = 0
                                       MOVE MOEDA-A-ATUALIZAR TO APAGADA
                                    END-IF
                                    REWRITE CBCOMD-REG
                                    DELETE FILE CBTAMD
                               END-IF
                          ELSE
                               MOVE NOME-MOEDA (MOEDA-A-ATUALIZAR)
                                 TO CBCOMD-NOME-MOEDA
                               REWRITE CBCOMD-REG
                          END-IF
                     END-IF
                     CLOSE CBCOMD
                     GO TO 000-INICIO
                END-IF
           ELSE
                OPEN I-O CBCOMD
                ADD 1 TO ULTIMA
                IF   APAGADA NOT = 0
                     MOVE APAGADA TO ULTIMA
                     MOVE SPACES  TO NOME-MOEDA (ULTIMA)
                END-IF
                DISPLAY LINHA-BRANCA           LINE 23 COLUMN 03
                        "Nome da nova moeda: " LINE 23 COLUMN 03
                PERFORM TEST AFTER UNTIL NOT F1
                        ACCEPT  NOME-MOEDA (ULTIMA)
                                WITH UPDATE LINE 23 COLUMN 23
                        ACCEPT  TECLA FROM ESCAPE KEY
                        IF   F1
                             EXEC COBOLware Help
                                  FILE   "CB013PCW.H02"
                                  LINE   16 COLUMN 23
                                  HEIGHT 06 WIDTH  42
                             END-EXEC
                        END-IF
                END-PERFORM
                IF  ESC
                    MOVE SPACES TO NOME-MOEDA (ULTIMA)
                END-IF
                IF  NOME-MOEDA (ULTIMA) NOT = SPACES
                    MOVE "Varia‡Æo"  TO CWBOXS-TITLE
                    MOVE SPACES      TO CWBOXS-ITENS
                    MOVE " Di ria"   TO CWBOXS-TEXT   (1)
                    MOVE "D"         TO CWBOXS-CHAR   (1)
                    MOVE " Mensal"   TO CWBOXS-TEXT   (2)
                    MOVE "M"         TO CWBOXS-CHAR   (2)
                    CALL "CWBOXS" USING PARAMETROS-CWBOXS
                    MOVE CWBOXS-OPTION-CHAR TO TIPO
                END-IF
                IF  NOME-MOEDA (ULTIMA) = SPACES
                OR  (NOT DIARIA AND NOT MENSAL)
                    GOBACK
                END-IF
                IF   APAGADA NOT = 0
                     MOVE APAGADA TO CBCOMD-MOEDA
                     READ CBCOMD
                ELSE
                     MOVE ULTIMA TO CBCOMD-MOEDA
                END-IF
                MOVE 0                   TO CBCOMD-ULTIMA
                MOVE NOME-MOEDA (ULTIMA) TO CBCOMD-NOME-MOEDA
                MOVE TIPO                TO CBCOMD-TIPO
                IF   APAGADA = 0
                     WRITE CBCOMD-REG
                ELSE
                     REWRITE CBCOMD-REG
                END-IF
                IF   FS-CBCOMD > "09"
                     CLOSE CBCOMD
                     GOBACK
                END-IF
                IF   APAGADA = 0
                     MOVE 1 TO CBCOMD-MOEDA
                     READ CBCOMD
                     IF    FS-CBCOMD < "10"
                           MOVE ULTIMA TO CBCOMD-ULTIMA
                           REWRITE CBCOMD-REG
                     ELSE
                           CLOSE CBCOMD
                           GOBACK
                     END-IF
                END-IF
                CLOSE CBCOMD
                DISPLAY "Moeda: "            LINE 08 COLUMN 33
                         NOME-MOEDA (ULTIMA) LINE 08 COLUMN 40
                MOVE ULTIMA TO MOEDA-A-ATUALIZAR.

           OPEN I-O CBTAMD

           IF   FS-CBTAMD = "00"
                ACCEPT CBTAMD-DATA-ACCEPT FROM DATE
                IF   CBTAMD-DATA-ACCEPT > 850000
                     MOVE 19          TO CBTAMD-AA
                ELSE
                     MOVE 20          TO CBTAMD-AA
                END-IF
                MOVE CBTAMD-MM   TO MM-REF
                MOVE CBTAMD-AAAA TO AAAA-REF
                DISPLAY CB018A
                PERFORM UNTIL ESC
                  PERFORM TEST AFTER UNTIL ESC
                                       OR MES-OK
                    IF   NOT PAGE-UP
                    AND  NOT PAGE-DOWN
                         IF   VEZ NOT = 1
                              DISPLAY LINHA-BRANCA    LINE 23 COLUMN 03
                                      RODAPE-KEY LINE 23 COLUMN 03
                              PERFORM TEST AFTER UNTIL NOT F1
                              ACCEPT CB018A
                              ACCEPT TECLA FROM ESCAPE KEY
                              IF   F1
                                   EXEC COBOLware Help
                                        FILE   "CB013PCW.H03"
                                        LINE 08 COLUMN 25
                                        HEIGHT 03 WIDTH 42
                                  END-EXEC
                              END-IF
                              END-PERFORM
                              DISPLAY LINHA-BRANCA LINE 23 COLUMN 03
                         END-IF
                    END-IF
                    IF   PAGE-DOWN
                         ADD 1 TO MM-REF
                         IF  MM-REF > 12
                             MOVE 1 TO MM-REF
                             ADD 1  TO AAAA-REF
                         END-IF
                         DISPLAY CB018E
                    ELSE
                         IF   PAGE-UP
                              SUBTRACT 1 FROM MM-REF
                              IF  MM-REF = ZERO
                              OR  MM-REF > 12
                                  MOVE     12   TO MM-REF
                                  SUBTRACT  1 FROM AAAA-REF
                              END-IF
                              DISPLAY CB018E
                         END-IF
                    END-IF
                    IF  (MES-OK
                    AND  NOT ESC)
                         SET ENTER-KEY TO TRUE
                         MOVE ZEROS    TO MOEDAS-DO-MES
                         MOVE 31       TO MAIOR-DIA
                         PERFORM VARYING DD FROM 31 BY -1
                                         UNTIL DD = 0
                                            OR (MENSAL AND DD < 1)
                           IF   MENSAL
                                MOVE 1         TO DD
                           END-IF
                           MOVE DD     TO CBTAMD-DD
                           MOVE MM-REF TO CBTAMD-MM
                           MOVE AAAA-REF TO CBTAMD-AAAA
                           READ CBTAMD
                           IF   FS-CBTAMD < "10"
                                MOVE CBTAMD-MOEDA TO MOEDA (DD)
                           END-IF
                           SET  CWTIME-REVERSED   TO TRUE
                           SET  CWTIME-WEEK       TO TRUE
                           MOVE CBTAMD-CHAVE       TO CWTIME-DATE
                           CALL "CWTIME"       USING PARAMETROS-CWTIME
                           MOVE CWTIME-WEEK-CHAR  TO DIA (DD)
                           IF   MENSAL
                                MOVE "Valor: "    TO DIA (DD)
                           END-IF
                           MOVE CBTAMD-DATA-ACCEPT TO DATA-TESTE
                           CALL "GRIDAT"       USING DATA-TESTE
                           MOVE MOEDA (DD)     TO MOEDA-T
                           MOVE P-LIN (DD)     TO T-LIN
                           MOVE P-COL (DD)     TO T-COL
                           MOVE DIA   (DD)     TO DIA-WEEK
                           MOVE DD             TO DIA-EXT
                           IF   DD = 31
                                CALL "GRVDAT" USING DATA-TESTE
                                IF   DATA-TESTE = ZEROS
                                     MOVE 30     TO MAIOR-DIA
                                     MOVE SPACES TO DIA (31)
                                                    DIA-EXT
                                                    DIA-WEEK
                                END-IF
                           END-IF
                           IF   DD = 30
                                CALL "GRVDAT" USING DATA-TESTE
                                IF   DATA-TESTE = ZEROS
                                     MOVE 29     TO MAIOR-DIA
                                     MOVE SPACES TO DIA (30)
                                                    DIA-EXT
                                                    DIA-WEEK
                                END-IF
                           END-IF
                           IF   DD = 29
                                CALL "GRVDAT" USING DATA-TESTE
                                IF   DATA-TESTE = ZEROS
                                     MOVE 28     TO MAIOR-DIA
                                     MOVE SPACES TO DIA (29)
                                                    DIA-EXT
                                                    DIA-WEEK
                                END-IF
                           END-IF
                         DISPLAY DIA-NORMAL LINE T-LIN COLUMN T-COL
                         END-PERFORM
                         MOVE 1       TO CAMPO
                         MOVE ALL "1" TO CONTROLE-VALORES
                         PERFORM VARYING DD FROM 1 BY 1
                                 UNTIL DD > 31
                                 IF   MOEDA (DD) NOT = 0
                                      MOVE 0 TO ZERADO (DD)
                                 END-IF
                         END-PERFORM
                         DISPLAY LINHA-BRANCA     LINE 23 COLUMN 03
                                 RODAPE-FULL LINE 23 COLUMN 03
                         IF   VEZ NOT = 1
                              PERFORM 010-CAMPO-A-CAMPO THRU 010-99-FIM
                                      TEST AFTER UNTIL ESC
                                                       OR PAGE-UP
                                                       OR PAGE-DOWN
                         END-IF
                         MOVE 2 TO VEZ
                         DISPLAY LINHA-BRANCA LINE 23 COLUMN 03
                         IF  ESC
                             SET ENTER-KEY TO TRUE
                         END-IF
                         PERFORM VARYING DD FROM 1 BY 1
                                 UNTIL DD > 31
                                    OR (MENSAL AND DD > 1)
                           MOVE DD       TO CBTAMD-DD
                           MOVE MM-REF   TO CBTAMD-MM
                           MOVE AAAA-REF TO CBTAMD-AAAA
                           READ CBTAMD
                           IF   FS-CBTAMD = "00"
                                IF   CBTAMD-MOEDA NOT = MOEDA (DD)
                                     MOVE MOEDA (DD) TO CBTAMD-MOEDA
                                     REWRITE CBTAMD-REG
                                END-IF
                           ELSE
                                IF   MOEDA (DD) NOT = ZERO
                                     MOVE MOEDA (DD) TO CBTAMD-MOEDA
                                     WRITE CBTAMD-REG
                                END-IF
                           END-IF
                    END-IF
                  END-PERFORM
                END-PERFORM
           END-IF

           CLOSE CBTAMD.

       000-99-FIM.

           GOBACK.

       010-CAMPO-A-CAMPO.

           IF   MENSAL
                MOVE 1 TO CAMPO
           END-IF

           MOVE MOEDA (CAMPO) TO MOEDA-T
           MOVE P-LIN (CAMPO) TO T-LIN
           MOVE P-COL (CAMPO) TO T-COL
           MOVE DIA   (CAMPO) TO DIA-WEEK
           MOVE CAMPO         TO DIA-EXT
           DISPLAY DIA-REVERSO LINE T-LIN COLUMN T-COL
           ACCEPT  DIA-REVERSO LINE T-LIN COLUMN T-COL
           MOVE MOEDA-T TO MOEDA (CAMPO)
           DISPLAY DIA-NORMAL  LINE T-LIN COLUMN T-COL

           IF   MOEDA  (CAMPO) = 0
           AND  ZERADO (CAMPO) = 0
                NEXT SENTENCE
           ELSE
                ACCEPT TECLA FROM ESCAPE KEY
                IF   F1
                     EXEC COBOLware Help
                          FILE "CB013PCW.H04"
                          LINE 12 COLUMN 15
                          HEIGHT 08 WIDTH 42
                     END-EXEC
                ELSE
                IF   DIARIA
                     IF   ENTER-KEY
                     OR   CURSOR-DOWN
                          ADD 1 TO CAMPO
                          IF   CAMPO > MAIOR-DIA
                               MOVE 1 TO CAMPO
                          END-IF
                     ELSE
                     IF   CURSOR-UP
                          SUBTRACT 1 FROM CAMPO
                          IF   CAMPO = 0
                               MOVE MAIOR-DIA TO CAMPO
                          END-IF
                     END-IF.

       010-99-FIM. EXIT.

       810-LER-CBCOMD.

           PERFORM TEST AFTER
                   UNTIL FS-CBCOMD NOT = "9D"
                   READ CBCOMD NEXT RECORD
                   IF   FS-CBCOMD = "9D"
                        CALL "CWISAM" USING ER-CBCOMD
                   END-IF
           END-PERFORM

           IF   FS-CBCOMD EQUAL "00"
                IF   CBCOMD-TIPO = "E"
                     IF   APAGADA = 0
                          MOVE CBCOMD-MOEDA TO APAGADA
                     END-IF
                ELSE
                     MOVE CBCOMD-NOME-MOEDA TO NOME-MOEDA (CBCOMD-MOEDA)
                     MOVE CBCOMD-TIPO       TO TIPO-MOEDA (CBCOMD-MOEDA)
                END-IF
           END-IF.

       810-99-FIM. EXIT.

       820-SELECIONA-MOEDA.

           MOVE "Moedas" TO CWBOXS-TITLE
           MOVE 7        TO CWBOXS-LINE
           MOVE 5        TO CWBOXS-COLUMN
           MOVE 1        TO I
           MOVE ZERO     TO CWBOXS-OPTION

           PERFORM UNTIL I > 99 OR CWBOXS-OPTION NOT = 0
                   ADD  1      TO PAGINA
                   MOVE SPACES TO CWBOXS-ITENS
                   MOVE ZEROS  TO CORRESPONDENCIAS
                   MOVE 0      TO Y
                   PERFORM VARYING I FROM I BY 1 UNTIL I > 99
                                                    OR Y = 15
                      IF   NOME-MOEDA (I) NOT = SPACE
                           IF   PRIMEIRO (PAGINA) = 0
                           AND  Y                 = 0
                                MOVE I TO PRIMEIRO (PAGINA)
                           END-IF
                           ADD  1             TO Y
                           MOVE I             TO CORRE         (Y)
                           MOVE NOME-MOEDA(I) TO CWBOXS-TEXT   (Y) (2:)
                      END-IF
                   END-PERFORM
                   MOVE SPACE       TO CWBOXS-ARROW
                   CALL "CWBOXS" USING PARAMETROS-CWBOXS
                   IF   CWBOXS-ARROW = "<"
                        IF   PAGINA > 1
                             SUBTRACT 14              FROM CWBOXS-COLUMN
                             SUBTRACT 1               FROM PAGINA
                             MOVE     PRIMEIRO (PAGINA) TO I
                             SUBTRACT 1               FROM PAGINA
                        ELSE
                             MOVE 1    TO I
                             MOVE ZERO TO PAGINA
                   ELSE
                        ADD  14 TO CWBOXS-COLUMN
                        IF   CWBOXS-OPTION NOT = 0
                             MOVE CORRE (CWBOXS-OPTION)
                               TO MOEDA-A-ATUALIZAR
                             DISPLAY "Moeda: " LINE 08 COLUMN 33
                                      NOME-MOEDA (MOEDA-A-ATUALIZAR)
                                               LINE 08 COLUMN 40
                             MOVE TIPO-MOEDA (MOEDA-A-ATUALIZAR)
                               TO TIPO
                        END-IF
                   END-IF
           END-PERFORM.

           IF   CWBOXS-ARROW = ">"
           AND  I > 99
           AND  CWBOXS-OPTION = 0
                GO TO 820-SELECIONA-MOEDA
           END-IF.

       820-99-FIM. EXIT.

       END PROGRAM CB013PCW.
