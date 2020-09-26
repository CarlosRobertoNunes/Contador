       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB038PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/07/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Consulta saldos                              *
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

       01  AREAS-DE-TRABALHO.
           05 OBS-5                    PIC  X(039) VALUE SPACES.
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG                  PIC  9(001) VALUE 0.
           05 RODAPE                   PIC  X(068) VALUE
              "<Esc> Page-Up Page-Down ".
           05 CAMPO                    PIC  9(001) VALUE 1.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 ER-CBCACC.
              10 FS-CBCACC              PIC  X(002) VALUE "00".
              10 LB-CBCACC              PIC  X(050) VALUE "CBCACC".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".

       COPY CB002PCW.
       COPY CWBOXF.

       SCREEN SECTION.

       01  CTAC-LIT-CB038PCW.
           05 LINE 08 COLUMN 03 VALUE "Refer늧cia:   /".
           05 LINE 10 COLUMN 03 VALUE "Conta     :".
           05 LINE 12 COLUMN 03 VALUE "Descri눯o :".
           05 LINE 14 COLUMN 03 VALUE "Saldo anterior".
           05 LINE 16 COLUMN 03 VALUE "Total a d괷ito".
           05 LINE 16 COLUMN 38 VALUE "(+)".
           05 LINE 18 COLUMN 03 VALUE "Total a cr괺ito".
           05 LINE 18 COLUMN 38 VALUE "(-)".
           05 LINE 19 COLUMN 03 VALUE "컴컴컴컴컴컴컴컴컴컴".
           05 LINE 19 COLUMN 23 VALUE "컴컴컴컴컴컴컴".
           05 LINE 20 COLUMN 03 VALUE "Saldo".

       01  CTAC-VAR-CB038PCW.
           05 T01 LINE 08 COLUMN 15 PIC Z(002) USING CBCOSA-MM.
           05 T02 LINE 08 COLUMN 18 PIC Z(004) USING CBCOSA-AAAA.
      *    05 T03 LINE 10 COLUMN 15 PIC X(026) USING CBCOSA-CONTA.
           05 LINE 12 COLUMN 15 PIC X(030) FROM CBPLCO-DESCRICAO.
           05 LINE 14 COLUMN 19 PIC ZZZ.ZZZ.ZZZ.ZZ9,99+
              FROM CBCOSA-SALDO-INICIAL.
           05 LINE 16 COLUMN 19 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
              FROM CBCOSA-A-DEBITO.
           05 LINE 18 COLUMN 19 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
              FROM CBCOSA-A-CREDITO.
           05 LINE 20 COLUMN 19 PIC ZZZ.ZZZ.ZZZ.ZZ9,99+
              FROM CBCOSA-SALDO-ATUAL.
           05 LINE 20 COLUMN 50 PIC 9(004) FROM CBCOSA-CENTRO-CUSTO.

       01  TELA-CC.
           05 LINE 08 COLUMN 30 VALUE "C/C:".
           05 LINE 08 COLUMN 35 PIC Z(004) USING CC.

       PROCEDURE DIVISION.

       000-INICIO.

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

           MOVE ALL X"FF"      TO CBCOSA-REG
           MOVE 10             TO CB002PCW-LINHA
           MOVE 15             TO CB002PCW-COLUNA

           DISPLAY CTAC-LIT-CB038PCW

           IF  CC-FLAG = 1
               PERFORM 011-CC THRU 011-99-FIM
           END-IF

           MOVE  CC TO CBCOSA-CENTRO-CUSTO
           START CBCOSA KEY NOT > CBCOSA-CHAVE

           READ CBCOSA PREVIOUS RECORD IGNORE LOCK
           IF   CBCOSA-CENTRO-CUSTO NOT = CC
                MOVE "10" TO FS-CBCOSA
           END-IF

           PERFORM 010-EXIBIR THRU 010-99-FIM

           PERFORM TEST AFTER UNTIL ESC
                   DISPLAY RODAPE
                           LINE 23 COLUMN 03
                   EVALUATE CAMPO
                     WHEN 1 ACCEPT T01
                     WHEN 2 ACCEPT T02
                     WHEN 3 MOVE CBCOSA-CONTA   TO CB002PCW-CONTA
                            MOVE "A"            TO CB002PCW-FUNCAO
                            CALL "CB002PCW"  USING PARAMETROS-CB002PCW
                            MOVE CB002PCW-CONTA TO CBCOSA-CONTA
                   END-EVALUATE
                   ACCEPT TECLA FROM ESCAPE KEY
                   EVALUATE TRUE
                      WHEN CURSOR-UP
                           IF   CAMPO = 0
                                MOVE 3 TO CAMPO
                           ELSE
                                SUBTRACT 1 FROM CAMPO
                           END-IF
                      WHEN CURSOR-DOWN
                           IF   CAMPO = 3
                                MOVE 1 TO CAMPO
                           ELSE
                                ADD  1 TO CAMPO
                           END-IF
                      WHEN PAGE-UP
                           READ CBCOSA PREVIOUS RECORD IGNORE LOCK
                           IF   FS-CBCOSA > "09"
                           OR  (CBCOSA-CENTRO-CUSTO NOT = CC)
                                MOVE "10" TO FS-CBCOSA
                                CALL "CWISAM" USING ER-CBCOSA
                                READ CBCOSA NEXT RECORD IGNORE LOCK
                           END-IF
                      WHEN PAGE-DOWN
                           READ CBCOSA NEXT RECORD IGNORE LOCK
                           IF   FS-CBCOSA > "09"
                           OR  (CBCOSA-CENTRO-CUSTO NOT = CC)
                                MOVE "10" TO FS-CBCOSA
                                CALL "CWISAM" USING ER-CBCOSA
                                READ CBCOSA PREVIOUS RECORD IGNORE LOCK
                           END-IF
                      WHEN OTHER
                           MOVE CC TO CBCOSA-CENTRO-CUSTO
                           START CBCOSA KEY NOT < CBCOSA-CHAVE
                           READ CBCOSA PREVIOUS RECORD IGNORE LOCK
                           IF   CBCOSA-CENTRO-CUSTO NOT = CC
                                MOVE "10" TO FS-CBCOSA
                           END-IF
                   END-EVALUATE
                   PERFORM 010-EXIBIR THRU 010-99-FIM
           END-PERFORM

           CLOSE CBPLCO
                 CBCOSA CBCACC.

       000-99-FIM. GOBACK.

       010-EXIBIR.

           MOVE CBCOSA-CONTA TO CBPLCO-CONTA
           READ CBPLCO IGNORE LOCK
           MOVE CBCOSA-CONTA   TO CB002PCW-CONTA
           MOVE "C"            TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           MOVE "E"            TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           MOVE "D"            TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           DISPLAY CTAC-VAR-CB038PCW.

       010-99-FIM. EXIT.

       011-CC.

           DISPLAY TELA-CC
           MOVE "<Esc>-Abandona F5-Pesquisa"   TO RODAPE
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
                     EXEC COBOLware Send
                          Message "Centro de custo inexistente"
                     END-EXEC
                ELSE
                     STRING CC " "
                            CBCACC-DESCRICAO
                            DELIMITED BY SIZE
                            INTO OBS-5
                     DISPLAY OBS-5
                        LINE 08 COLUMN 35
                END-IF
           ELSE
                DISPLAY SPACES LINE 08 COLUMN 30 WITH SIZE 20
           END-IF

           MOVE "<Esc> Page-Up Page-Down " TO RODAPE
           DISPLAY RODAPE LINE 23 COLUMN 03.

       011-99-FIM. EXIT.

       END PROGRAM CB038PCW.

