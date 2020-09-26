       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB019PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Balanco em 2 colunas                         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBCOSASL.
       COPY CBPLCOSL.

           SELECT CBWORI ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CBWORI.

           SELECT CBWORP ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CBWORP.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCOSAFD.
       COPY CBPLCOFD.

       FD  CBWORI
           VALUE OF FILE-ID IS LB-CBWORI.

       01  CBWORI-REG                         PIC  X(066).

       FD  CBWORP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBWORP.

       01  CBWORP-REG                         PIC  X(066).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG                  PIC  9(001) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 BRANCOS                  PIC  X(001) VALUE SPACE.
           05 INFERIOR                 PIC  X(001) VALUE SPACE.
           05 GRAU-PRIMARIO            PIC  9(001) VALUE ZERO.
           05 PARAMETROS-GRFINA.
              10 OPERADOR              PIC  X(030).
              10 TASK                  PIC  X(006).
              10 PROGRAMA              PIC  X(008).
              10 CWMENU                PIC  X(001).
           05 TESTE-PARIDADE           PIC  9(001) VALUE ZERO.
              88 PAR                               VALUE 0 2 4 6 8.
              88 IMPAR                             VALUE 1 3 5 7 9.
           05 CONTA                    PIC  9(015) VALUE ZERO.
           05 REDEFINES CONTA.
              10 DGC OCCURS 15         PIC  9(001).
           05 DESCRICAO.
              10 DES OCCURS 30         PIC  X(001).
           05 SALDOS-GRAUS VALUE ZEROS.
              10 SALDO OCCURS 9        PIC S9(012)V99.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 GRAU-A                   PIC  9(001) VALUE ZERO.
           05 GRAU                     PIC  9(001) VALUE ZERO.
           05 GRAU-MINIMO              PIC  9(001) VALUE ZERO.
           05 GRAU-ANTERIOR            PIC  9(001) VALUE ZERO.
           05 D                 COMP-3 PIC  9(002) VALUE ZERO.
           05 Y                 COMP-3 PIC  9(002) VALUE ZERO.
           05 I                 COMP-3 PIC  9(002) VALUE ZERO.
           05 I-1                      PIC  9(001) VALUE ZERO.
           05 I-X REDEFINES I-1        PIC  X(001).
           05 LD-CBPLCO         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-CBWORI         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-CBWORP         COMP-3 PIC  9(006) VALUE ZERO.
           05 LD-CBWORI         COMP-3 PIC  9(006) VALUE ZERO.
           05 LD-CBWORP         COMP-3 PIC  9(006) VALUE ZERO.
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBWORI.
              10 FS-CBWORI             PIC  X(002) VALUE "00".
              10 LB-CBWORI             PIC  X(050) VALUE "000000-$$1".
           05 ER-CBWORP.
              10 FS-CBWORP             PIC  X(002) VALUE "00".
              10 LB-CBWORP             PIC  X(050) VALUE "000000-$$2".
           05 AAAA-REF                 PIC  9(004).
           05 MM-REF                   PIC  9(002).
              88 MM-REF-OK VALUE 1 THRU 12.
              88 DEZEMBRO  VALUE 12.
           05 MSG01 PIC X(30) VALUE "Referˆncia impr¢pria".
           05 MSG02 PIC X(30) VALUE "Centro de custo inexistente".
           05 TIT-REF.
              10            PIC  X(10) VALUE "EXERCICIO ".
              10 TIT-OBS    PIC  X(05) VALUE " ATE ".
              10 REFERENCIA PIC  X(14) VALUE SPACES.
              10            PIC  X(01) VALUE SPACES.
              10 OBS-5      PIC  X(39) VALUE SPACES.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(037) VALUE SPACES.
           05 CLIC-SALDO                     PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-PERC                      PIC  ZZ9,999-.
           05 FILLER REDEFINES CLIC-PERC.
              10 FILLER                      PIC  X(003).
              10 PARTE-1B                    PIC  X(002).
              10 PARTE-2B                    PIC  X(001).
              10 PARTE-3B                    PIC  X(001).
              10 PARTE-SINAL                 PIC  X(001).
           05 CLIC-TIT-PERC                  PIC  X(001) VALUE "%".
       02  REDEFINES LINHA-01.
           05 BYTE-D OCCURS 66               PIC  X(001).

       COPY CWTIME.
       COPY CWBOXS.
       COPY CWBOXF.
       COPY CB002PCW.
       COPY CWIMPR.

       SCREEN SECTION.

       01  CB0019A.
           05 LINE 11 COLUMN 03 VALUE "Lidos".
           05 LINE 11 COLUMN 09 PIC X(025) FROM LB-CBPLCO.
           05 LINE 12 COLUMN 09 PIC X(025) FROM LB-CBWORI.
           05 LINE 13 COLUMN 09 PIC X(025) FROM LB-CBWORP.
           05 LINE 15 COLUMN 03 VALUE "Grav.".
           05 LINE 15 COLUMN 09 PIC X(025) FROM LB-CBWORI.
           05 LINE 16 COLUMN 09 PIC X(025) FROM LB-CBWORP.
           05 LINE 17 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBPLCO LINE 11 COLUMN 30 PIC ZZZ.ZZ9 FROM LD-CBPLCO.
           05 T-LD-CBWORI LINE 12 COLUMN 30 PIC ZZZ.ZZ9 FROM LD-CBWORI.
           05 T-LD-CBWORP LINE 13 COLUMN 30 PIC ZZZ.ZZ9 FROM LD-CBWORP.
           05 T-GR-CBWORI LINE 15 COLUMN 30 PIC ZZZ.ZZ9 FROM GR-CBWORI.
           05 T-GR-CBWORP LINE 16 COLUMN 30 PIC ZZZ.ZZ9 FROM GR-CBWORP.
           05 T-GR-PRNTER LINE 17 COLUMN 30 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       01  CB0019B AUTO.
           05 LINE 08 COLUMN 03 VALUE "Mˆs de referˆncia:".
           05 LINE 08 COLUMN 22 PIC ZZ/ USING MM-REF.
           05 LINE 08 COLUMN 25 PIC 9999 USING AAAA-REF BLANK ZERO.

       01  TELA-CC.
           05 LINE 09 COLUMN 03 VALUE "Centro de custo:".
           05 LINE 09 COLUMN 20 PIC ZZZZ USING CC.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           OPEN OUTPUT CBWORI
                       CBWORP

           PERFORM UNTIL FS-CBPLCO > "09"
                   READ CBPLCO NEXT RECORD IGNORE LOCK
                   MOVE CC           TO CBCOSA-CENTRO-CUSTO
                   MOVE CBPLCO-CONTA TO CBCOSA-CONTA
                   MOVE AAAA-REF     TO CBCOSA-AAAA
                   MOVE MM-REF       TO CBCOSA-MM
                   READ CBCOSA IGNORE LOCK
                   MOVE "N"          TO INFERIOR
                   IF   CBCOSA-SALDO-ATUAL = 0
                   AND  FS-CBCOSA < "10"
                        PERFORM 110-CHECK-INFERIOR THRU 110-99-FIM
                   END-IF
                   IF   FS-CBPLCO < "10"
                   AND  FS-CBCOSA < "10"
                   AND (CBCOSA-SALDO-ATUAL NOT = 0 OR INFERIOR = "S")
                        ADD  1                 TO LD-CBPLCO
                        DISPLAY                 T-LD-CBPLCO
                        MOVE SPACES            TO LINHA-01
                        MOVE CBPLCO-CONTA       TO CB002PCW-CONTA
                        MOVE "C"               TO CB002PCW-FUNCAO
                        CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                        MOVE "E"               TO CB002PCW-FUNCAO
                        CALL "CB002PCW"     USING PARAMETROS-CB002PCW
                        MOVE CBCOSA-SALDO-ATUAL TO SALDO (CB002PCW-GRAU)
                        COMPUTE GRAU-A = CB002PCW-GRAU - 1
                        IF   GRAU-A = 0
                             MOVE 1 TO GRAU-A
                        END-IF
                        IF   CB002PCW-GRAU NOT > GRAU
                        AND  CB002PCW-GRAU NOT < GRAU-MINIMO
                             MOVE CBPLCO-DESCRICAO    TO DESCRICAO
                             MOVE SALDO (CB002PCW-GRAU) TO CLIC-SALDO
                             IF   SALDO (GRAU-A) = 0
                                  MOVE 0 TO CLIC-PERC
                             ELSE
                                  COMPUTE CLIC-PERC ROUNDED =
                                          SALDO (CB002PCW-GRAU) /
                                          SALDO (GRAU-A)      *
                                          100
                             END-IF
                             MOVE "%" TO CLIC-TIT-PERC
                             IF   PARTE-3B EQUAL "0"
                                  MOVE SPACE TO PARTE-3B
                                  IF   PARTE-2B EQUAL "0"
                                       MOVE SPACE TO PARTE-2B
                                       IF   PARTE-1B EQUAL ",0"
                                            MOVE SPACE TO PARTE-1B
                                       END-IF
                                  END-IF
                             END-IF
                             MOVE "S" TO BRANCOS
                             PERFORM UNTIL BRANCOS = "N"
                                     MOVE "N" TO BRANCOS
                                     PERFORM VARYING I
                                                FROM 61 BY 1
                                               UNTIL I > 66
                                             COMPUTE Y = I + 1
                                             IF   Y < 67
                                             AND  BYTE-D (Y) NOT = " "
                                             AND  BYTE-D (I)     = " "
                                                  MOVE BYTE-D (Y)
                                                    TO BYTE-D (I)
                                                  MOVE "S" TO BRANCOS
                                                  MOVE " " TO BYTE-D (Y)
                                             END-IF
                                     END-PERFORM
                             END-PERFORM
                             IF   CB002PCW-GRAU = 1
                                  MOVE ZERO         TO TESTE-PARIDADE
                                  MOVE CBPLCO-CONTA TO CONTA
                                  PERFORM VARYING I FROM 15 BY -1
                                          UNTIL DGC (I) NOT = 0
                                  END-PERFORM
                                  MOVE DGC (I) TO TESTE-PARIDADE
                             END-IF
                             IF  (CB002PCW-GRAU NOT = GRAU-ANTERIOR)
                                 IF   IMPAR
                                      IF   GR-CBWORI NOT = 0
                                           WRITE CBWORI-REG FROM SPACES
                                           IF   CB002PCW-GRAU = 1
                                                WRITE CBWORI-REG
                                                 FROM ALL "-"
                                                WRITE CBWORI-REG
                                                 FROM SPACES
                                           END-IF
                                      END-IF
                                 ELSE
                                      IF   GR-CBWORP NOT = 0
                                           WRITE CBWORP-REG FROM SPACES
                                           IF   CB002PCW-GRAU = 1
                                                WRITE CBWORP-REG
                                                 FROM ALL "-"
                                                WRITE CBWORP-REG
                                                 FROM SPACES
                                           END-IF
                                      END-IF
                                 END-IF
                             END-IF
                             PERFORM VARYING Y FROM 30 BY -1
                                               UNTIL DES (Y) NOT = SPACE
                                                  OR Y =1
                             END-PERFORM
                             MOVE     0   TO D
                             COMPUTE I = CB002PCW-GRAU + 1
                             PERFORM VARYING I FROM I BY 1
                                     UNTIL BYTE-D (I) NOT = SPACE
                                     IF   D < Y
                                          ADD  1       TO D
                                          MOVE DES (D) TO BYTE-D (I)
                                     ELSE
                                          MOVE "."     TO BYTE-D (I)
                                     END-IF
                             END-PERFORM
                             IF   IMPAR
                                  WRITE CBWORI-REG FROM LINHA-01
                                  ADD  1             TO GR-CBWORI
                                  DISPLAY             T-GR-CBWORI
                             ELSE
                                  WRITE CBWORP-REG FROM LINHA-01
                                  ADD  1             TO GR-CBWORP
                                  DISPLAY             T-GR-CBWORP
                             END-IF
                             MOVE CB002PCW-GRAU      TO GRAU-ANTERIOR
                        END-IF
                   END-IF
           END-PERFORM.

           CLOSE CBWORI CBWORP
           OPEN INPUT CBWORI CBWORP

           PERFORM UNTIL "09" < FS-CBWORI AND FS-CBWORP
                   MOVE SPACES TO CWIMPR-DETAIL
                   IF   FS-CBWORI < "10"
                        READ CBWORI
                        IF   FS-CBWORI = "00"
                             IF   CBWORI-REG NOT = SPACES
                             AND  CBWORI-REG NOT = ALL "-"
                                  ADD  1             TO LD-CBWORI
                                                        GR-PRNTER
                                  DISPLAY             T-LD-CBWORI
                             END-IF
                             MOVE CBWORI-REG TO CWIMPR-DETAIL (1: 66)
                        END-IF
                   END-IF
                   IF   FS-CBWORP < "10"
                        READ CBWORP
                        IF   FS-CBWORP = "00"
                             IF   CBWORP-REG NOT = SPACES
                             AND  CBWORP-REG NOT = ALL "-"
                                  ADD  1             TO LD-CBWORP
                                                        GR-PRNTER
                                  DISPLAY             T-LD-CBWORP
                             END-IF
                             MOVE CBWORP-REG TO CWIMPR-DETAIL (67: 66)
                        END-IF
                   END-IF
                   IF   FS-CBWORI < "10"
                   OR   FS-CBWORP < "10"
                        PERFORM 120-CWIMPR THRU 120-99-FIM
                        DISPLAY T-GR-PRNTER
                   END-IF
           END-PERFORM

           CLOSE CBWORI CBWORP
           DELETE FILE CBWORI
           DELETE FILE CBWORP.

       100-99-FIM. EXIT.

       110-CHECK-INFERIOR.

            MOVE CBPLCO-CONTA     TO CB002PCW-CONTA
            MOVE "C"              TO CB002PCW-FUNCAO
            CALL "CB002PCW"      USING PARAMETROS-CB002PCW
            MOVE "E"              TO CB002PCW-FUNCAO
            CALL "CB002PCW"      USING PARAMETROS-CB002PCW

            IF   CB002PCW-LANCAVEL = "N"
                 MOVE CB002PCW-GRAU TO GRAU-PRIMARIO
                 PERFORM TEST AFTER
                  UNTIL INFERIOR = "S"
                     OR (CB002PCW-GRAU  = GRAU-PRIMARIO
                        AND  (AAAA-REF  = CBCOSA-AAAA)
                        AND  (MM-REF    = CBCOSA-MM))
                     OR FS-CBCOSA > "09"
                        PERFORM TEST AFTER
                                UNTIL FS-CBCOSA NOT = "9D"
                        READ CBCOSA NEXT RECORD
                        IF FS-CBCOSA = "9D"
                           CALL "CWISAM" USING ER-CBCOSA
                        END-IF
                        END-PERFORM
                        MOVE CBCOSA-CONTA     TO CB002PCW-CONTA
                        MOVE "C"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        MOVE "E"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        IF  ((CB002PCW-LANCAVEL     = "S")
                        AND  (CBCOSA-SALDO-ATUAL NOT = 0)
                        AND  (AAAA-REF              = CBCOSA-AAAA)
                        AND  (MM-REF                = CBCOSA-MM))
                        AND  FS-CBCOSA < "10"
                             MOVE "S" TO INFERIOR
                        END-IF
                 END-PERFORM
                 MOVE CC           TO CBCOSA-CENTRO-CUSTO
                 MOVE CBPLCO-CONTA TO CBCOSA-CONTA
                 MOVE AAAA-REF     TO CBCOSA-AAAA
                 MOVE MM-REF       TO CBCOSA-MM
                 READ CBCOSA IGNORE LOCK
           END-IF.

       110-99-FIM. EXIT.

       120-CWIMPR.

           CALL "CWIMPR" USING PARAMETROS-CWIMPR

           IF   CWIMPR-END-PRINT
                CLOSE CBCOSA
                      CBPLCO
                      CBWORI
                      CBWORP
                DELETE FILE CBWORI
                DELETE FILE CBWORP
                GOBACK
           END-IF.

       120-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCACC
           IF   FS-CBCACC = "30" OR "35"
                OPEN I-O CBCACC
           END-IF

           MOVE 9999 TO CBCACC-CODIGO
           START CBCACC KEY NOT GREATER CBCACC-CHAVE
           IF   FS-CBCACC < "10"
                READ CBCACC PREVIOUS RECORD IGNORE LOCK
                IF   FS-CBCACC < "10"
                     MOVE 1 TO CC-FLAG
                END-IF
           END-IF

           OPEN INPUT CBPLCO
           IF   FS-CBPLCO > "09"
                CLOSE CBCACC
                GOBACK
           END-IF

           OPEN INPUT CBCOSA
           IF   FS-CBCOSA > "09"
                CLOSE CBPLCO CBCACC
                GOBACK
           END-IF

           MOVE "?"         TO CWMENU
           CALL "CWGETU" USING OPERADOR
                               TASK
                               PROGRAMA
                               CWMENU

           IF   TASK NOT NUMERIC
                MOVE ZEROS TO TASK
           END-IF

           MOVE TASK        TO LB-CBWORI (1: 6)
                               LB-CBWORP (1: 6)

           MOVE 1                TO CWBOXS-OPTION.

       800-INICIAIS-BOXS.

           MOVE "N"              TO CWBOXS-ERASE
           MOVE 08               TO CWBOXS-LINE
           MOVE 40               TO CWBOXS-COLUMN
           MOVE "Do"             TO CWBOXS-TITLE

           MOVE 999999999999999  TO CB002PCW-CONTA
           MOVE "E"              TO CB002PCW-FUNCAO
           CALL "CB002PCW"      USING PARAMETROS-CB002PCW

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CB002PCW-GRAU
                   MOVE I         TO I-1
                   MOVE I-X       TO CWBOXS-TEXT   (I) (2: 1)
                                     CWBOXS-CHAR   (I)
                   MOVE "§ Grau " TO CWBOXS-TEXT   (I) (3: 7)
           END-PERFORM

           CALL "CWBOXS"        USING PARAMETROS-CWBOXS

           IF   CWBOXS-OPTION = 0
                CLOSE CBPLCO CBCACC
                GOBACK
           END-IF

           MOVE CWBOXS-OPTION TO GRAU-MINIMO

           IF   CWBOXS-OPTION < CB002PCW-GRAU
                MOVE "At‚"              TO CWBOXS-TITLE
                MOVE 53                 TO CWBOXS-COLUMN
                MOVE CB002PCW-GRAU        TO CWBOXS-OPTION
                CALL "CWBOXS"        USING PARAMETROS-CWBOXS
                IF   CWBOXS-ARROW = "<"
                     MOVE GRAU-MINIMO TO CWBOXS-OPTION
                     GO TO 800-INICIAIS-BOXS
                END-IF
                IF   CWBOXS-OPTION = 0
                     CLOSE CBPLCO CBCACC
                     GOBACK
                END-IF
           END-IF

           MOVE "BALANCO" TO CWIMPR-TITLE

           IF CWBOXS-OPTION < CB002PCW-GRAU
           OR GRAU-MINIMO > 1
              IF GRAU-MINIMO < 2
                 MOVE "ATE O GRAU "              TO CWIMPR-TITLE(19: 11)
                 MOVE CWBOXS-CHAR (CWBOXS-OPTION) TO CWIMPR-TITLE(30: 1)
              ELSE
                 MOVE "DO GRAU "                  TO CWIMPR-TITLE(19: 8)
                 MOVE GRAU-MINIMO                 TO CWIMPR-TITLE(27: 1)
                 IF CWBOXS-OPTION NOT = GRAU-MINIMO
                    MOVE " AO "  TO CWIMPR-TITLE (28: 6)
                    MOVE CWBOXS-CHAR (CWBOXS-OPTION)
                      TO CWIMPR-TITLE (32: 1)
                 END-IF
              END-IF
           END-IF

           MOVE CWBOXS-OPTION      TO GRAU
           DISPLAY CB0019B
           IF   CC-FLAG = 1
                DISPLAY TELA-CC
           END-IF

           PERFORM TEST AFTER UNTIL ESC
                                 OR (MM-REF-OK AND AAAA-REF > 1899)
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT CB0019B
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE   "CB019PCW.H01"
                                LINE   08 COLUMN 22
                                HEIGHT 06 WIDTH  40
                           END-EXEC
                      END-IF
                   END-PERFORM
                   IF   NOT ESC
                        IF   NOT MM-REF-OK
                        OR   AAAA-REF < 1900
                             EXEC COBOLware Send Message MSG01 END-EXEC
                        ELSE
                             IF   NOT DEZEMBRO
                                  MOVE AAAA-REF    TO CWTIME-DATE (1: 4)
                                  MOVE MM-REF      TO CWTIME-DATE (5: 2)
                                  MOVE "01"        TO CWTIME-DATE (7: 2)
                                  SET  CWTIME-REVERSED TO TRUE
                                  SET  CWTIME-EDIT     TO TRUE
                                  CALL "CWTIME" USING PARAMETROS-CWTIME
                                  INSPECT CWTIME-MOUNTH-EDITED
                                     CONVERTING MINUSCULAS TO MAIUSCULAS
                                  MOVE CWTIME-MOUNTH-EDITED
                                    TO REFERENCIA
                             ELSE
                                  MOVE AAAA-REF TO REFERENCIA
                                  MOVE SPACES   TO TIT-OBS
                             END-IF
                             MOVE TIT-REF       TO CWIMPR-SUB-TITLE
                        END-IF
                        MOVE "CB019PA"     TO CWIMPR-REPORT
                        CALL "CB041PCW" USING PARAMETROS-CWIMPR
                        CANCEL "CB041PCW"
                   END-IF
           END-PERFORM

           IF   CC-FLAG = 1
           AND (NOT ESC)
                MOVE "<Esc>-Abandona F5-Pesquisa"
                  TO RODAPE
                DISPLAY RODAPE LINE 23 COLUMN 03
                PERFORM TEST AFTER
                        UNTIL ESC
                           OR CC = 0
                           OR FS-CBCACC < "10"
                ACCEPT  TELA-CC
                ACCEPT  TECLA FROM ESCAPE KEY
                IF   F5
                     MOVE SPACES TO CWBOXF-OPTION
                     IF   CC NOT = 0
                          MOVE CC TO CBCACC-CODIGO
                          READ CBCACC IGNORE LOCK
                          IF   FS-CBCACC < "10"
                               MOVE CBCACC-DESCRICAO
                                 TO CWBOXF-OPTION
                          END-IF
                     END-IF
                     MOVE "CB050PCW"   TO CWBOXF-PROGRAM
                     MOVE "Centros de custo"
                                       TO CWBOXF-TITLE
                     MOVE  5 TO CWBOXF-STRING-1-LENGTH
                     MOVE 30 TO CWBOXF-STRING-2-LENGTH
                     MOVE  2 TO CWBOXF-ORDER
                     MOVE 10 TO CWBOXF-VERTICAL-LENGTH
                     COMPUTE CWBOXF-HORIZONTAL-LENGTH = 6
                           + CWBOXF-STRING-1-LENGTH
                           + CWBOXF-STRING-2-LENGTH
                     MOVE 10 TO CWBOXF-LINE
                     MOVE 21 TO CWBOXF-COLUMN
                     CALL "CWBOXF" USING PARAMETROS-CWBOXF
                     IF   CWBOXF-OPTION NOT = SPACES
                          MOVE CWBOXF-OPTION (1: 4)
                            TO CC
                         SET ENTER-KEY TO TRUE
                     END-IF
                END-IF
                IF   CC NOT = 0
                     DISPLAY TELA-CC
                     MOVE CC TO CBCACC-CODIGO
                     READ CBCACC IGNORE LOCK
                     IF   FS-CBCACC > "09"
                          EXEC COBOLware Send Message MSG02 END-EXEC
                     ELSE
                          STRING "C/C: " CC " "
                                 CBCACC-DESCRICAO
                                 DELIMITED BY SIZE
                                 INTO OBS-5
                          DISPLAY OBS-5
                             LINE 09 COLUMN 03 WITH SIZE 37
                          MOVE TIT-REF        TO CWIMPR-SUB-TITLE
                     END-IF
                ELSE
                     DISPLAY "Geral" LINE 09 COLUMN 20
                END-IF
                END-PERFORM
           END-IF

           IF   ESC
                CLOSE CBPLCO CBCOSA CBCACC
                GOBACK
           END-IF

           MOVE SPACES TO RODAPE DISPLAY RODAPE LINE 23 COLUMN 03
           DISPLAY CB0019A.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBPLCO CBCACC
           CANCEL "CB002PCW"

           MOVE "CLOSE"      TO CWIMPR-TIME-REPORT
           PERFORM 120-CWIMPR THRU 120-99-FIM
           CANCEL "CB046PCW".

       900-99-FIM. EXIT.

       END PROGRAM CB019PCW.
