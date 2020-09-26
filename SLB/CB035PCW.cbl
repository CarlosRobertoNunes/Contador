       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB035PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  20/03/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Consulta lancamentos de um BAC               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCAHISL.
       COPY CBCOBASL.
       COPY CBHIVASL.
       COPY CBPLCOSL.
       COPY CBMVMSSL.

           SELECT CBWORK ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CBWORK.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCAHIFD.
       COPY CBCOBAFD.
       COPY CBHIVAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.

       FD  CBWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBWORK.

       01  CBWORK-REG PIC X(80).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LER                  PIC  X(001) VALUE "S".
           05 ACEITA-BAC           PIC  X(001) VALUE "S".
           05 BAC.
              10 BAC-SERIE  COMP-3 PIC  9(004).
              10 BAC-NUMERO COMP-3 PIC  9(004).
           05 LANCAMENTO    COMP-3 PIC  9(007) VALUE 0.
           05 ER-CBCOBA.
              10 FS-CBCOBA         PIC  X(002) VALUE "00".
              10 LB-CBCOBA         PIC  X(050) VALUE "CBCOBA".
           05 ER-CBCAHI.
              10 FS-CBCAHI         PIC  X(002) VALUE "00".
              10 LB-CBCAHI         PIC  X(050) VALUE "CBCAHI".
           05 ER-CBHIVA.
              10 FS-CBHIVA         PIC  X(002) VALUE "00".
              10 LB-CBHIVA         PIC  X(050) VALUE "CBHIVA".
           05 ER-CBPLCO.
              10 FS-CBPLCO         PIC  X(002) VALUE "00".
              10 LB-CBPLCO         PIC  X(050) VALUE "CBPLCO".
           05 ER-CBMVMS.
              10 FS-CBMVMS         PIC  X(002) VALUE "00".
              10 LB-CBMVMS                     VALUE "CBMV000000".
                 15 FILLER         PIC  X(044).
                 15 AAAA-REF       PIC  9(004).
                 15 MM-REF         PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 ER-CBWORK.
              10 FS-CBWORK         PIC  X(002) VALUE "00".
              10 LB-CBWORK         PIC  X(050) VALUE "CBWORK".
           05 TECLA                PIC  9(002) VALUE 0.
              COPY CWKEYS.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 CLIC-LANCAMENTO                PIC  ZZZZ.ZZZ.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED-DB BLANK ZERO     PIC  9(005) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-COD-RED-CR BLANK ZERO     PIC  9(005) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-DIA BLANK ZERO            PIC  9(002) VALUE ZEROS.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-VALOR BLANK ZERO          PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 CLIC-HISTORICO                 PIC  X(031) VALUE SPACES.

       COPY CWBOXW.
       COPY CWNCOR.

       SCREEN SECTION.

       01  CTAC-LIT-CB035PCW HIGH.
       02  FOREGROUND-COLOR BROWN
           BACKGROUND-COLOR MAGENTA.
           05 LINE 08 COLUMN 04 VALUE "BAC:".
           05 LINE 08 COLUMN 19 VALUE "Referˆncia".
           05 LINE 09 COLUMN 06 VALUE "Lan‡to".
           05 LINE 09 COLUMN 16 VALUE "DB".
           05 LINE 09 COLUMN 22 VALUE "CR".
           05 LINE 09 COLUMN 25 VALUE "Dia".
           05 LINE 09 COLUMN 41 VALUE "Valor".
           05 LINE 09 COLUMN 47 VALUE "Hist¢rico".

       01  CTAC-VAR-CB035PCW AUTO HIGH.
       02  FOREGROUND-COLOR BROWN
           BACKGROUND-COLOR MAGENTA.
           05 LINE 08 COLUMN 09 PIC Z(004)/ USING CBCOBA-SERIE.
           05 LINE 08 COLUMN 14 PIC Z(004) USING CBCOBA-NUMERO.
           05 LINE 08 COLUMN 30 PIC 99/ FROM  CBCOBA-MM.
           05 LINE 08 COLUMN 33 PIC 9999 FROM  CBCOBA-AAAA.

       01  APAGA-JANELA HIGH.
       02  FOREGROUND-COLOR BROWN
           BACKGROUND-COLOR MAGENTA.
           05 LINE 07 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 08 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 09 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 10 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 11 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 12 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 13 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 14 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 15 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 16 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 17 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 18 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 19 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 20 COLUMN 03 PIC X(76) FROM SPACES.
           05 LINE 21 COLUMN 03 PIC X(76) FROM SPACES.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           MOVE CBCOBA-SERIE   TO CBMVMS-SERIE
           MOVE CBCOBA-NUMERO  TO CBMVMS-NUMERO
           MOVE LOW-VALUES    TO CBMVMS-CHAVE
           MOVE CBCOBA-SERIE   TO BAC-SERIE
           MOVE CBCOBA-NUMERO  TO BAC-NUMERO
           MOVE SPACES         TO LINHA-01

           START CBMVMS KEY NOT LESS CBMVMS-BAC-CHAVE

           PERFORM TEST AFTER UNTIL FS-CBMVMS > "09"
                                 OR BAC NOT = CBMVMS-BAC
                   IF   LER = "S"
                        READ CBMVMS NEXT RECORD IGNORE LOCK
                   END-IF
                   IF   BAC = CBMVMS-BAC
                        IF  LANCAMENTO = 0
                        OR  CBMVMS-LANCAMENTO = LANCAMENTO
                            MOVE CBMVMS-LANCAMENTO TO LANCAMENTO
                            MOVE CBMVMS-LANCAMENTO TO CLIC-LANCAMENTO
                            IF   CBMVMS-TIPO = "D"
                                 MOVE CBMVMS-COD-RED TO CLIC-COD-RED-DB
                            ELSE
                                 MOVE CBMVMS-COD-RED TO CLIC-COD-RED-CR
                            END-IF
                            MOVE CBMVMS-DIA   TO CLIC-DIA
                            MOVE CBMVMS-VALOR TO CLIC-VALOR
                            PERFORM 110-PROCESSA-HISTORICO THRU
                                    110-99-FIM
                            MOVE "S"               TO LER
                        ELSE
                            IF   LINHA-01 NOT = SPACES
                                 WRITE CBWORK-REG FROM LINHA-01
                            END-IF
                            MOVE SPACES TO LINHA-01
                            MOVE CBMVMS-LANCAMENTO TO LANCAMENTO
                            MOVE "N"               TO LER
                        END-IF
                   END-IF
           END-PERFORM

           IF   LINHA-01 NOT = SPACES
                WRITE CBWORK-REG FROM LINHA-01
           END-IF

           WRITE CBWORK-REG FROM SPACES
           WRITE CBWORK-REG FROM SPACES
           MOVE " Fim do BAC 0000/0000" TO CBWORK-REG
           MOVE CBCOBA-NUMERO TO CBWORK-REG (13: 4)
           MOVE CBCOBA-SERIE  TO CBWORK-REG (18: 4)
           WRITE CBWORK-REG

           PERFORM 10 TIMES
                   WRITE CBWORK-REG FROM SPACES
           END-PERFORM

           CLOSE CBWORK
           EXEC COBOLware Help
                FILE   LB-CBWORK
                TYPE   8
                LINE   11 COLUMN 4
                HEIGHT 10 WIDTH  74
                COLOR-FRAME  MAGENTA-BROWN-HIGH
                COLOR-BORDER MAGENTA-BROWN-HIGH
                COLOR-SHADE  MAGENTA-BROWN-HIGH
           END-EXEC
           DELETE FILE CBWORK.

       100-99-FIM. EXIT.

       110-PROCESSA-HISTORICO.

           EVALUATE TRUE
              WHEN CBMVMS-HISTORICO-PADRAO NOT = 0
                   MOVE CBMVMS-HISTORICO-PADRAO TO CBCAHI-CODIGO
                   READ CBCAHI IGNORE LOCK
                   IF   FS-CBCAHI < "10"
                        MOVE CBCAHI-DESCRICAO TO CLIC-HISTORICO
                   ELSE
                        MOVE "NÆo cadastrado" TO CLIC-HISTORICO
                   END-IF
              WHEN CBMVMS-HISTORICO-PADRAO = 0
               AND (CBMVMS-HISTORICO-VARIAVEL NOT = 0)
                   COMPUTE CBHIVA-TIPO = CBMVMS-HISTORICO-VARIAVEL
                                       / 100000
                   MOVE CBMVMS-HISTORICO-VARIAVEL TO CBHIVA-CODIGO
                   MOVE 1                         TO CBHIVA-VARIAVEL
                   READ CBHIVA IGNORE LOCK
                   IF   FS-CBHIVA < "09"
                        MOVE CBHIVA-DESCRICAO TO CLIC-HISTORICO
                   ELSE
                        MOVE "NÆo cadastrado" TO CLIC-HISTORICO
                   END-IF
           END-EVALUATE.

       110-99-FIM. EXIT.

       800-INICIAIS.

           OPEN OUTPUT CBWORK
           IF   FS-CBWORK > "09"
                GOBACK
           END-IF

           OPEN INPUT CBCOBA
           IF   FS-CBCOBA > "09"
                GOBACK
           END-IF

           ACCEPT CBCOBA-CHAVE FROM COMMAND-LINE
           IF   CBCOBA-CHAVE NOT NUMERIC
                MOVE ZEROS TO CBCOBA-CHAVE
           ELSE
                MOVE "N"         TO ACEITA-BAC
                MOVE 6           TO CWBOXW-LINE
                MOVE 2           TO CWBOXW-COLUMN
                MOVE 15          TO CWBOXW-VERTICAL-LENGTH
                MOVE 76          TO CWBOXW-HORIZONTAL-LENGTH
                MOVE MAGENTA-BROWN-HIGH
                                 TO CWBOXW-COLOR-FRAME
                                    CWBOXW-COLOR-BORDER
                MOVE "OPEN"      TO CWBOXW-FUNCTION
                CALL "CWBOXW" USING PARAMETROS-CWBOXW
                DISPLAY APAGA-JANELA
           END-IF

           DISPLAY CTAC-LIT-CB035PCW
                   CTAC-VAR-CB035PCW

           PERFORM TEST AFTER UNTIL FS-CBCOBA < "10"
                                 OR ESC
                   IF  ACEITA-BAC = "S"
                       ACCEPT CTAC-VAR-CB035PCW
                       ACCEPT TECLA FROM ESCAPE KEY
                   END-IF
                   READ CBCOBA IGNORE LOCK
                   IF  FS-CBCOBA > "10"
                   AND (NOT ESC)
                       EXEC COBOLware Send
                            Message "BAC inexistente"
                       END-EXEC
                       IF  ACEITA-BAC = "N"
                           SET ESC TO TRUE
                       END-IF
                   END-IF
           END-PERFORM

           IF  ESC
               PERFORM 900-FINAIS THRU 900-99-FIM
               GOBACK
           END-IF

           MOVE CBCOBA-AAAA TO AAAA-REF
           MOVE CBCOBA-MM   TO MM-REF
           DISPLAY CTAC-VAR-CB035PCW

           OPEN INPUT CBCAHI
                IF FS-CBCAHI > "09"
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
                END-IF

           OPEN INPUT CBHIVA
                IF FS-CBHIVA > "09"
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
                END-IF

           OPEN INPUT CBPLCO
                IF FS-CBPLCO > "09"
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
                END-IF

           MOVE CBCOBA-AAAA TO AAAA-REF
           MOVE CBCOBA-MM   TO MM-REF

           CALL "CB045PCW" USING LB-CBMVMS (1: 6)
           OPEN INPUT CBMVMS
                IF FS-CBMVMS > "09"
                   PERFORM 900-FINAIS THRU 900-99-FIM
                   GOBACK
                END-IF.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOBA
                 CBCAHI
                 CBHIVA
                 CBPLCO
                 CBMVMS

           IF   ACEITA-BAC = "N"
                MOVE "CLOSE"      TO CWBOXW-FUNCTION
                CALL "CWBOXW" USING PARAMETROS-CWBOXW
           END-IF.

       900-99-FIM. EXIT.

       END PROGRAM CB035PCW.
