       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB018PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Balanco                                      *
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

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCOSAFD.
       COPY CBPLCOFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG                  PIC  9(001) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 INFERIOR                 PIC  X(001) VALUE SPACE.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 GRAU-PRIMARIO            PIC  9(001) VALUE ZERO.
           05 GRAU                     PIC  9(001) VALUE ZERO.
           05 GRAU-MINIMO              PIC  9(001) VALUE ZERO.
           05 GRAU-ANTERIOR            PIC  9(001) VALUE ZERO.
           05 I                 COMP-3 PIC  9(002) VALUE ZERO.
           05 I-1                      PIC  9(001) VALUE ZERO.
           05 I-X REDEFINES I-1        PIC  X(001).
           05 LD-CBPLCO         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 F8                       PIC  X(001) VALUE "N".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 AAAA-REF                 PIC  9(004).
           05 MM-REF                   PIC  9(002).
              88 MM-REF-OK VALUE 1 THRU 12.
              88 DEZEMBRO  VALUE 12.
           05 MSG01 PIC X(30) VALUE "Referˆncia impr¢pria".
           05 MSG02 PIC X(30) VALUE "Centro de custo inexistente".
           05 TIT-REF.
      *       10            PIC  X(10) VALUE "EXERCICIO ".
              10 TIT-OBS    PIC  X(05) VALUE " ATE ".
              10 REFERENCIA PIC  X(14) VALUE SPACES.
              10            PIC  X(01) VALUE SPACES.
              10 OBS-5      PIC  X(39) VALUE SPACES.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(031) VALUE
              "CONTA                       DES".
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(045) VALUE
              "ICAO                                    SALDO".
       02  LINHA-02.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CODIGO                    PIC  X(026) VALUE SPACES.
           05 CLIC-OBS                       PIC  X(002) VALUE SPACES.
           05 CLIC-DESCRICAO                 PIC  X(030) VALUE SPACES.
           05 FILLER                         PIC  X(002) VALUE SPACES.
           05 CLIC-SALDO                     PIC  ZZZ.ZZZ.ZZZ.ZZ9,99-.

       COPY CWTIME.
       COPY CWBOXS.
       COPY CWBOXF.
       COPY CB002PCW.
       COPY CWIMPR.

       SCREEN SECTION.

       01  CB0018A.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 08 COLUMN 09 PIC X(025) FROM LB-CBPLCO.
           05 LINE 10 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBPLCO LINE 08 COLUMN 35 PIC ZZZ.ZZ9 FROM LD-CBPLCO.
           05 T-GR-PRNTER LINE 10 COLUMN 35 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       01  CB0018B AUTO.
           05 LINE 12 COLUMN 03 VALUE "Mˆs de referˆncia:".
           05 LINE 12 COLUMN 22 PIC ZZ/ USING MM-REF.
           05 LINE 12 COLUMN 25 PIC 9999 USING AAAA-REF BLANK ZERO.

       01  TELA-CC.
           05 LINE 13 COLUMN 03 VALUE "Centro de custo:".
           05 LINE 13 COLUMN 20 PIC ZZZZ USING CC.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

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
                        PERFORM 120-CHECK-INFERIOR THRU 120-99-FIM
                   END-IF
                   IF   FS-CBPLCO < "10"
                   AND  FS-CBCOSA < "10"
                   AND (CBCOSA-SALDO-ATUAL NOT = 0 OR INFERIOR = "S")
                        ADD  1                TO LD-CBPLCO
                        DISPLAY                T-LD-CBPLCO
                        MOVE CBPLCO-CONTA      TO CB002PCW-CONTA
                        MOVE "C"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"    USING PARAMETROS-CB002PCW
                        MOVE "E"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        IF   CB002PCW-GRAU NOT > GRAU
                        AND  CB002PCW-GRAU NOT < GRAU-MINIMO
                             MOVE CB002PCW-CONTA-ED  TO CLIC-CODIGO
                             MOVE CBPLCO-DESCRICAO TO CLIC-DESCRICAO
                             MOVE CBCOSA-SALDO-ATUAL TO CLIC-SALDO
                             IF  (CB002PCW-GRAU NOT = GRAU-ANTERIOR)
                             AND (GRAU-ANTERIOR NOT = 0)
                                  MOVE SPACES           TO CWIMPR-DETAIL
                                  PERFORM 125-CWIMPR THRU 125-99-FIM
                             END-IF
                             MOVE SPACE TO CLIC-OBS
                             IF   CB002PCW-GRAU = 1
                                  ACCEPT CWIMPR-TIME-REPORT FROM TIME
                             END-IF
                             MOVE LINHA-02         TO CWIMPR-DETAIL
                             PERFORM 125-CWIMPR THRU 125-99-FIM
                             MOVE CB002PCW-GRAU      TO GRAU-ANTERIOR
                             ADD  1                TO GR-PRNTER
                             DISPLAY                T-GR-PRNTER
                        END-IF
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       120-CHECK-INFERIOR.

            MOVE CBPLCO-CONTA     TO CB002PCW-CONTA
            MOVE "C"              TO CB002PCW-FUNCAO
            CALL "CB002PCW"      USING PARAMETROS-CB002PCW
            MOVE "E"              TO CB002PCW-FUNCAO
            CALL "CB002PCW"      USING PARAMETROS-CB002PCW

            IF   CB002PCW-LANCAVEL = "N"
                 MOVE CB002PCW-GRAU TO GRAU-PRIMARIO
                 PERFORM TEST AFTER
                  UNTIL INFERIOR = "S"
                     OR (CB002PCW-GRAU    = GRAU-PRIMARIO
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
                        IF  ((CB002PCW-LANCAVEL        = "S")
                        AND  (CBCOSA-SALDO-ATUAL NOT = 0)
                        AND  (AAAA-REF               = CBCOSA-AAAA)
                        AND  (MM-REF                 = CBCOSA-MM))
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

       120-99-FIM. EXIT.

       125-CWIMPR.

           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           IF   CWIMPR-END-PRINT
                CLOSE CBCOSA
                      CBPLCO CBCACC
                GOBACK
           END-IF.

       125-99-FIM. EXIT.

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
                   MOVE I-X       TO CWBOXS-TEXT (I) (2: 1)
                                     CWBOXS-CHAR (I)
                   MOVE "§ Grau " TO CWBOXS-TEXT (I) (3: 7)
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
                MOVE CB002PCW-GRAU      TO CWBOXS-OPTION
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
                 MOVE "ATE O GRAU " TO CWIMPR-TITLE(19: 11)
                 MOVE CWBOXS-CHAR (CWBOXS-OPTION) TO CWIMPR-TITLE(30: 1)
              ELSE
                 MOVE "DO GRAU "                  TO CWIMPR-TITLE(19: 8)
                 MOVE GRAU-MINIMO                 TO CWIMPR-TITLE(27: 1)
                 IF CWBOXS-OPTION NOT = GRAU-MINIMO
                    MOVE " AO "
                      TO CWIMPR-TITLE (28: 6)
                    MOVE CWBOXS-CHAR (CWBOXS-OPTION)
                      TO CWIMPR-TITLE (32: 1)
                 END-IF
              END-IF
           END-IF

           MOVE CWBOXS-OPTION      TO GRAU
           DISPLAY CB0018B
           IF   CC-FLAG = 1
                DISPLAY TELA-CC
           END-IF

           PERFORM TEST AFTER UNTIL ESC
                                 OR (MM-REF-OK AND AAAA-REF > 1899)
                   PERFORM TEST AFTER UNTIL NOT F1
                      MOVE "<Esc>-Abandona F1-Help" TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT CB0018B
                      MOVE SPACES TO RODAPE
                      DISPLAY RODAPE LINE 23 COLUMN 03
                      ACCEPT TECLA FROM ESCAPE KEY
                      IF   F1
                           EXEC COBOLware Help
                                FILE   "CB018PCW.H01"
                                LINE   12 COLUMN 22
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
                             MOVE TIT-REF        TO CWIMPR-SUB-TITLE
                        END-IF
                        MOVE "CB018PA"         TO CWIMPR-REPORT
                        MOVE LINHA-01          TO CWIMPR-HEADER-1
                        MOVE 2                 TO CWIMPR-FORM-TYPE
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
                             LINE 13 COLUMN 03
                          MOVE TIT-REF        TO CWIMPR-SUB-TITLE
                     END-IF
                ELSE
                     DISPLAY "Geral" LINE 13 COLUMN 20
                END-IF
                END-PERFORM
            END-IF

           IF   ESC
                CLOSE CBPLCO CBCOSA CBCACC
                GOBACK
           END-IF

           MOVE SPACES TO RODAPE DISPLAY RODAPE LINE 23 COLUMN 03
           DISPLAY CB0018A.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBPLCO CBCACC
           CANCEL "CB002PCW".

           MOVE "CLOSE"      TO CWIMPR-TIME-REPORT
           PERFORM 125-CWIMPR THRU 125-99-FIM
           CANCEL "CB046PCW".

       900-99-FIM. EXIT.

       END PROGRAM CB018PCW.
