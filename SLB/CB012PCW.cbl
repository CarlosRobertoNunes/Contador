       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB012PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Listagem dos centros de custos               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 ws-OPTION                 PIC  9(002) VALUE ZERO.
           05 LD-CBCACC          COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER          COMP-3 PIC  9(006) VALUE ZERO.
           05 ER-CBCACC.
              10 FS-CBCACC              PIC  X(002) VALUE "00".
              10 LB-CBCACC              PIC  X(050) VALUE "CBCACC".

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(010) VALUE
              "CODIGO DES".
           05 FILLER                         PIC  X(002) VALUE "CR".
           05 FILLER                         PIC  X(004) VALUE "ICAO".
       02  LINHA-02.
           05 FILLER                         PIC  X(003) VALUE SPACES.
           05 CLIC-CODIGO BLANK ZERO         PIC  9(004) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DESCRICAO                 PIC  X(030) VALUE SPACES.

       COPY CWIMPR.

       SCREEN SECTION.

       01  CB0012A.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 08 COLUMN 09 PIC X(015) FROM LB-CBCACC.
           05 LINE 10 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBCACC LINE 08 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBCACC.
           05 T-GR-PRNTER LINE 10 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-CBCACC > "09"
                   READ CBCACC NEXT RECORD IGNORE LOCK
                   IF   FS-CBCACC < "10"
                        ADD  1                TO LD-CBCACC
                        DISPLAY                T-LD-CBCACC
                        MOVE CBCACC-CODIGO    TO CLIC-CODIGO
                        MOVE CBCACC-DESCRICAO TO CLIC-DESCRICAO
                        MOVE LINHA-02         TO CWIMPR-DETAIL
                        CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        ADD  1                TO GR-PRNTER
                        DISPLAY                T-GR-PRNTER
                        IF   CWIMPR-END-PRINT
                             CLOSE CBCACC
                             GOBACK
                        END-IF
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCACC

           EXEC COBOLware BoxSelect
                NOERASE
                TITLE "Ordem"
                LINE 10 COLUMN 40
                CAPTION(1) "~C¢digo"
                CAPTION(2) "~Descri‡Æo"
                OPTION 1;ws-OPTION
           END-EXEC
           MOVE LOW-VALUES    TO CBCACC-REG

           IF ws-OPTION = 1
              MOVE "EM ORDEM DE CODIGO" TO CWIMPR-SUB-TITLE
              MOVE "CB012PA"            TO CWIMPR-REPORT
              START CBCACC  KEY NOT LESS CBCACC-CHAVE
           ELSE
              IF ws-OPTION = 2
                 MOVE "EM ORDEM DE DESCRICAO" TO CWIMPR-SUB-TITLE
                 MOVE "CB012PB"            TO CWIMPR-REPORT
                 START CBCACC  KEY NOT LESS CBCACC-DESCRICAO
              ELSE
                 CLOSE CBCACC
                 GOBACK
              END-IF
           END-IF

           DISPLAY CB0012A
           MOVE "CENTROS DE CUSTOS" TO CWIMPR-TITLE
           MOVE LINHA-01            TO CWIMPR-HEADER-1
           MOVE 2                   TO CWIMPR-FORM-TYPE
           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW".

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           CLOSE CBCACC.

       900-99-FIM. EXIT.

       END PROGRAM CB012PCW.
