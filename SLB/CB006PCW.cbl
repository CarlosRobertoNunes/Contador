       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB006PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Listagem dos historicos padrao               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCAHISL.
       COPY CBHIVASL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCAHIFD.
       COPY CBHIVAFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 LB-HELP                  PIC  X(012) VALUE "CB006PCW.HXX".
           05 ws-OPTION                PIC  9(002) VALUE ZERO.
           05 I                 COMP-3 PIC  9(002) VALUE ZERO.
           05 LD-CBHIVA         COMP-3 PIC  9(006) VALUE ZERO.
           05 LD-CBCAHI         COMP-3 PIC  9(006) VALUE ZERO.
           05 GR-PRNTER         COMP-3 PIC  9(006) VALUE ZERO.
           05 ER-CBCAHI.
              10 FS-CBCAHI              PIC  X(002) VALUE "00".
              10 LB-CBCAHI              PIC  X(050) VALUE "CBCAHI".
           05 ER-CBHIVA.
              10 FS-CBHIVA              PIC  X(002) VALUE "00".
              10 LB-CBHIVA              PIC  X(050) VALUE "CBHIVA".

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

       01  CB006PA.
           05 LINE 08 COLUMN 03 VALUE "Lidos".
           05 LINE 08 COLUMN 09 PIC X(015) FROM LB-CBCAHI.
           05 LINE 09 COLUMN 09 PIC X(015) FROM LB-CBHIVA.
           05 LINE 11 COLUMN 03 VALUE "Impressos".
           05 T-LD-CBCAHI  LINE 08 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBCAHI.
           05 T-LD-CBHIVA  LINE 09 COLUMN 25 PIC ZZZ.ZZ9 FROM LD-CBHIVA.
           05 T-GR-PRNTER LINE 11 COLUMN 25 PIC ZZZ.ZZ9 FROM GR-PRNTER.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL FS-CBCAHI > "09"
                   READ CBCAHI NEXT RECORD IGNORE LOCK
                   IF   FS-CBCAHI < "10"
                        ADD  1                TO LD-CBCAHI
                        DISPLAY                T-LD-CBCAHI
                        MOVE CBCAHI-CODIGO    TO CLIC-CODIGO
                        MOVE CBCAHI-DESCRICAO TO CLIC-DESCRICAO
                        MOVE LINHA-02         TO CWIMPR-DETAIL
                        CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        IF   CWIMPR-END-PRINT
                             CLOSE CBCAHI CBHIVA
                             GOBACK
                        END-IF
                        MOVE ZERO             TO CLIC-CODIGO
                        PERFORM 110-COMPLEMENTOS THRU 110-99-FIM
                        MOVE SPACES           TO CWIMPR-DETAIL
                        CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        ADD  1                TO GR-PRNTER
                        DISPLAY                T-GR-PRNTER
                        IF   CWIMPR-END-PRINT
                             CLOSE CBCAHI CBHIVA
                             GOBACK
                        END-IF
                   END-IF
           END-PERFORM.

       100-99-FIM. EXIT.

       110-COMPLEMENTOS.

           MOVE "00"   TO FS-CBHIVA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 23
                                            OR FS-CBHIVA > "09"
                   MOVE 1             TO CBHIVA-TIPO
                   MOVE CBCAHI-CODIGO TO CBHIVA-CODIGO
                   MOVE I             TO CBHIVA-VARIAVEL
                   READ CBHIVA IGNORE LOCK
                   IF   FS-CBHIVA < "09"
                        ADD  1                TO LD-CBHIVA
                        DISPLAY                T-LD-CBHIVA
                        MOVE CBHIVA-DESCRICAO TO CLIC-DESCRICAO
                        MOVE LINHA-02         TO CWIMPR-DETAIL
                        CALL "CWIMPR" USING PARAMETROS-CWIMPR
                        IF   CWIMPR-END-PRINT
                             CLOSE CBCAHI CBHIVA
                             GOBACK
                        END-IF
                   END-IF
           END-PERFORM.

       110-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCAHI
                      CBHIVA

           EXEC COBOLware BoxSelect
                NOERASE
                TITLE "Ordem"
                LINE 10 COLUMN 40
                CAPTION(1) "~C¢digo"
                CAPTION(2) "~Descri‡Æo"
                OPTION 1;ws-OPTION
           END-EXEC

           MOVE LOW-VALUES    TO CBCAHI-REG

           IF ws-OPTION = 1
              MOVE "EM ORDEM DE CODIGO" TO CWIMPR-SUB-TITLE
              MOVE "CB006PA"            TO CWIMPR-REPORT
              START CBCAHI  KEY NOT LESS CBCAHI-CHAVE
           ELSE
              IF ws-OPTION = 2
                 MOVE "EM ORDEM DE DESCRICAO" TO CWIMPR-SUB-TITLE
                 MOVE "CB006PB"               TO CWIMPR-REPORT
                 START CBCAHI  KEY NOT LESS CBCAHI-DESCRICAO
              ELSE
                 CLOSE CBCAHI CBHIVA
                 GOBACK
              END-IF
           END-IF

           DISPLAY CB006PA
           MOVE "HISTORICOS PADRAO" TO CWIMPR-TITLE
           MOVE LINHA-01            TO CWIMPR-HEADER-1
           MOVE 2                   TO CWIMPR-FORM-TYPE
           CALL "CB041PCW" USING PARAMETROS-CWIMPR
           CANCEL "CB041PCW".

       800-99-FIM. EXIT.

       900-FINAIS.

           MOVE "CLOSE"     TO CWIMPR-TIME-REPORT
           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           CLOSE CBCAHI CBHIVA.

       900-99-FIM. EXIT.

       END PROGRAM CB006PCW.
