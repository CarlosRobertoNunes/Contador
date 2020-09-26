       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB005PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Listagem do plano de contas                  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPLCOSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPLCOFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 LB-HELP                  PIC  X(012) VALUE "CB005PCW.HXX".
           05 GRAU                     PIC  9(001) VALUE ZERO.
           05 COD-RED-CALL             PIC  9(005) VALUE ZERO.
           05 GRAU-MINIMO              PIC  9(001) VALUE ZERO.
           05 GRAU-ANTERIOR            PIC  9(001) VALUE ZERO.
           05 I                 COMP-3 PIC  9(002) VALUE ZERO.
           05 I-1                      PIC  9(001) VALUE ZERO.
           05 I-X REDEFINES I-1        PIC  X(001).
           05 LD-CBPLCO          COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 ER-CBPLCO.
              10 FS-CBPLCO              PIC  X(002) VALUE "00".
              10 LB-CBPLCO              PIC  X(050) VALUE "CBPLCO".

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(002) VALUE "CO".
           05 FILLER                         PIC  X(001) VALUE "D".
           05 FILLER                         PIC  X(035) VALUE
              ".RED CODIGO CONTABIL            DES".
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(004) VALUE "ICAO".
       02  LINHA-02.
           05 CLIC-COD-RED                   PIC  Z(005) VALUE ZEROS.
           05 CLIC-COD-RED-TRACO             PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED-DV                PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-CODIGO                    PIC  X(026) VALUE SPACES.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DESCRICAO                 PIC  X(030) VALUE SPACES.

       COPY CWBOXS.
       COPY CB002PCW.
       COPY CWIMPR.

       SCREEN SECTION.

       01  CB005PA.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 08 COLUMN 09 PIC X(015) FROM LB-CBPLCO.
           05 LINE 10 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBPLCO LINE 08 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBPLCO.
           05 T-GR-PRNTER LINE 10 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

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
                   IF   FS-CBPLCO < "10"
                        ADD  1                TO LD-CBPLCO
                        DISPLAY                T-LD-CBPLCO
                        MOVE CBPLCO-CONTA     TO CB002PCW-CONTA
                        MOVE "C"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        MOVE "E"              TO CB002PCW-FUNCAO
                        CALL "CB002PCW"      USING PARAMETROS-CB002PCW
                        IF   CB002PCW-GRAU NOT > GRAU
                        AND  CB002PCW-GRAU NOT < GRAU-MINIMO
                             MOVE CBPLCO-COD-RED    TO CLIC-COD-RED
                             MOVE CB002PCW-CONTA-ED TO CLIC-CODIGO
                             MOVE CBPLCO-DESCRICAO  TO CLIC-DESCRICAO
                             IF  (CWBOXS-OPTION = 1 OR 4)
                             AND (CB002PCW-GRAU NOT = GRAU-ANTERIOR)
                             AND (GRAU-ANTERIOR NOT = 0)
                                  MOVE SPACES           TO CWIMPR-DETAIL
                                  CALL "CWIMPR" USING PARAMETROS-CWIMPR
                                  IF   CWIMPR-END-PRINT
                                       CLOSE CBPLCO
                                       CANCEL "CB039PCW"
                                       CANCEL "CB002PCW"
                                       GOBACK
                                  END-IF
                             END-IF
                             IF   CBPLCO-COD-RED NOT = 0
                                  MOVE CBPLCO-COD-RED
                                                   TO COD-RED-CALL
                                  MOVE "-"         TO CLIC-COD-RED-TRACO
                                  CALL "CB039PCW" USING COD-RED-CALL
                                                        CLIC-COD-RED-DV
                             ELSE
                                  MOVE SPACES TO CLIC-COD-RED-TRACO
                                                 CLIC-COD-RED-DV
                             END-IF
                             MOVE LINHA-02         TO CWIMPR-DETAIL
                             CALL "CWIMPR" USING PARAMETROS-CWIMPR
                             MOVE CB002PCW-GRAU    TO GRAU-ANTERIOR
                             ADD  1                TO GR-PRNTER
                             DISPLAY                T-GR-PRNTER
                             IF   CWIMPR-END-PRINT
                                  CLOSE CBPLCO
                                  CANCEL "CB039PCW"
                                  CANCEL "CB002PCW"
                                  GOBACK
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBPLCO
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
                CLOSE CBPLCO
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
                     CLOSE CBPLCO
                     GOBACK
                END-IF
           END-IF

           MOVE "PLANO DE CONTAS" TO CWIMPR-TITLE

           IF CWBOXS-OPTION < CB002PCW-GRAU
           OR GRAU-MINIMO > 1
              IF GRAU-MINIMO < 2
                 MOVE "ATE O GRAU "              TO CWIMPR-TITLE(17: 11)
                 MOVE CWBOXS-CHAR (CWBOXS-OPTION) TO CWIMPR-TITLE(28: 1)
              ELSE
                 MOVE "DO GRAU "                  TO CWIMPR-TITLE(17: 8)
                 MOVE GRAU-MINIMO                 TO CWIMPR-TITLE(25: 1)
                 IF CWBOXS-OPTION NOT = GRAU-MINIMO
                    MOVE " AO "
                      TO CWIMPR-TITLE (26: 6)
                    MOVE CWBOXS-CHAR (CWBOXS-OPTION)
                      TO CWIMPR-TITLE (30: 1)
                 END-IF
              END-IF
           END-IF

           MOVE CWBOXS-OPTION      TO GRAU
           MOVE SPACES             TO CWBOXS-ITENS
           MOVE 12                 TO CWBOXS-LINE
           MOVE 04                 TO CWBOXS-COLUMN
           MOVE "Ordem"            TO CWBOXS-TITLE
           MOVE " ~C¢digo "        TO CWBOXS-TEXT   (1)
           MOVE " ~Descri‡Æo "     TO CWBOXS-TEXT   (2)

           IF   CWBOXS-OPTION = CB002PCW-GRAU
                MOVE " c¢digo ~Reduzido"  TO CWBOXS-TEXT   (3)
           END-IF

           MOVE " c~Lasse/descri‡Æo" TO CWBOXS-TEXT   (4)

           MOVE 1                  TO CWBOXS-OPTION
           CALL "CWBOXS"        USING PARAMETROS-CWBOXS
           MOVE LOW-VALUES         TO CBPLCO-REG

           EVALUATE CWBOXS-OPTION
                WHEN 1
                     MOVE "EM ORDEM DE CODIGO CONTABIL"
                       TO CWIMPR-SUB-TITLE
                     MOVE "CB005PA" TO CWIMPR-REPORT
                     START CBPLCO  KEY NOT LESS CBPLCO-CHAVE
                WHEN 2
                     MOVE "EM ORDEM DE DESCRICAO" TO CWIMPR-SUB-TITLE
                     MOVE "CB005PB"               TO CWIMPR-REPORT
                     START CBPLCO  KEY NOT LESS CBPLCO-DESCRICAO
                WHEN 3
                     MOVE "EM ORDEM DE CODIGO REDUZIDO"
                       TO CWIMPR-SUB-TITLE
                     MOVE "CB005PC" TO CWIMPR-REPORT
                     START CBPLCO  KEY NOT LESS CBPLCO-COD-RED
                WHEN 4
                     MOVE "EM ORDEM DE CLASSE/DESCRICAO"
                       TO CWIMPR-SUB-TITLE
                     MOVE "CB005PD" TO CWIMPR-REPORT
                     START CBPLCO  KEY NOT LESS CBPLCO-CLASSE-DESCRICAO
                WHEN OTHER
                     CLOSE CBPLCO
                     GOBACK
           END-EVALUATE

           DISPLAY CB005PA
           MOVE LINHA-01          TO CWIMPR-HEADER-1
           MOVE 2                 TO CWIMPR-FORM-TYPE
           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW".

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           CLOSE CBPLCO
           CANCEL "CB002PCW"
           CANCEL "CB039PCW".

       900-99-FIM. EXIT.

       END PROGRAM CB005PCW.
