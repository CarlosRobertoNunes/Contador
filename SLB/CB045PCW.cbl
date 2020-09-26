       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB045PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/08/1998.
       SECURITY.      *************************************************
                      *                                               *
                      *  Adapta movimento ao formato de data AAAAMMDD *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBMVMSSL.

           SELECT CBWORK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS SEQUENTIAL
                  RECORD  KEY   IS CBWORK-CHAVE
                  ALTERNATE RECORD KEY CBWORK-BAC-CHAVE
                                =  CBWORK-BAC
                                   CBWORK-CHAVE
                  ALTERNATE RECORD KEY CBWORK-DIA-CHAVE
                                =  CBWORK-DIA
                                   CBWORK-CHAVE
                  ALTERNATE RECORD KEY CBWORK-COD-RED-CHAVE
                                =  CBWORK-COD-RED
                                   CBWORK-DIA          WITH DUPLICATES
                  ALTERNATE RECORD KEY CBWORK-DOCTO-CHAVE
                                =  CBWORK-DOCTO
                                   CBWORK-CHAVE
                  ALTERNATE RECORD KEY CBWORK-AAMMDD-DOCTO-CHAVE
                                =  CBWORK-AAMMDD-DOCTO
                                   CBWORK-CHAVE
                  ALTERNATE RECORD KEY CBWORK-CENTRO-CUSTO-CHAVE
                                =  CBWORK-CENTRO-CUSTO
                                   CBWORK-DIA          WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CBWORK.

       DATA DIVISION.
       FILE SECTION.

       COPY CBMVMSFD.

      ******************************************************************
      *           Contabilidade Movimento contabil mensal              *
      ******************************************************************

       FD  CBWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBWORK.

       01  CBWORK-REG.
           05 CBWORK-CHAVE.
              10 CBWORK-LANCAMENTO      COMP-3 PIC  9(007).
              10 CBWORK-TIPO                   PIC  X(001).
           05 CBWORK-BAC.
              10 CBWORK-SERIE           COMP-3 PIC  9(004).
              10 CBWORK-NUMERO          COMP-3 PIC  9(004).
           05 CBWORK-DIA                       PIC  9(002).
           05 CBWORK-COD-RED            COMP-3 PIC  9(005).
           05 CBWORK-DOCTO              COMP-3 PIC  9(008).
           05 CBWORK-AAMMDD-DOCTO       COMP-3 PIC  9(006).
           05 CBWORK-CENTRO-CUSTO       COMP-3 PIC  9(004).
           05 CBWORK-HISTORICO-PADRAO   COMP-3 PIC  9(004).
           05 CBWORK-HISTORICO-VARIAVEL COMP-3 PIC  9(006).
           05 CBWORK-VALOR              COMP-3 PIC  9(012)V99.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ANO-8                    PIC  9(008) VALUE 0.
           05 LD-CBWORK                PIC  9(008) VALUE 0.
           05 ER-IDX.
              10 LB-CB017I             PIC  X(050) VALUE "000000.idx".
              10 LB-CBWORKI            PIC  X(050) VALUE "000000$$.idx".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS             PIC  X(050) VALUE "CBMV000000".
           05 ER-CBWORK.
              10 FS-CBWORK             PIC  X(002) VALUE "00".
              10 LB-CBWORK             PIC  X(050) VALUE "000000$$".

       COPY CWBOXW.

       LINKAGE SECTION.

       01  REFERENCIA PIC 9(06).

       SCREEN SECTION.

       01  CTAC-LIT-CB045PCW.
           05 LINE 14 COLUMN 08 VALUE "            Evitando".
           05 LINE 14 COLUMN 28 VALUE " o bug do ano 2000 n".
           05 LINE 14 COLUMN 48 VALUE "o movimento       .D".
           05 LINE 14 COLUMN 68 VALUE "AT... ".

       01  CTAC-VAR-CB045PCW.
           05 T-LD-CBWORK
              LINE 14 COLUMN 09 PIC ZZ.ZZZ.ZZ9 FROM LD-CBWORK.
           05 T-REFERENCIA LINE 14 COLUMN 60 PIC 9(006) FROM REFERENCIA.

       PROCEDURE DIVISION USING REFERENCIA.

       000-INICIO.

           MOVE REFERENCIA (1: 2) TO ANO-8 (1: 2)
           MOVE REFERENCIA        TO LB-CBMVMS  (5: 6)
                                     LB-CB017I  (1: 6)
                                     LB-CBWORK  (1: 6)
                                     LB-CBWORKI (1: 6)
           OPEN INPUT CBMVMS
           IF   FS-CBMVMS = "39"
                CALL "CWNAME" USING LB-CBMVMS LB-CBWORK
                CALL "CWNAME" USING LB-CB017I LB-CBWORKI
                OPEN INPUT CBWORK
                IF   FS-CBWORK < "10"
                     OPEN OUTPUT CBMVMS
                     IF   FS-CBMVMS > "09"
                          GOBACK
                     END-IF
                     MOVE 13 TO CWBOXW-LINE
                     MOVE 07 TO CWBOXW-COLUMN
                     MOVE 01 TO CWBOXW-VERTICAL-LENGTH
                     MOVE 66 TO CWBOXW-HORIZONTAL-LENGTH
                     SET CWBOXW-OPEN TO TRUE
                     CALL "CWBOXW" USING PARAMETROS-CWBOXW
                     DISPLAY CTAC-LIT-CB045PCW
                             CTAC-VAR-CB045PCW
                     PERFORM UNTIL FS-CBWORK > "09"
                             READ CBWORK
                             IF  FS-CBWORK < "10"
                                 MOVE CBWORK-LANCAMENTO
                                 TO   CBMVMS-LANCAMENTO
                                 MOVE CBWORK-TIPO
                                 TO   CBMVMS-TIPO
                                 MOVE CBWORK-SERIE
                                 TO   CBMVMS-SERIE
                                 MOVE CBWORK-NUMERO
                                 TO   CBMVMS-NUMERO
                                 MOVE CBWORK-DIA
                                 TO   CBMVMS-DIA
                                 MOVE CBWORK-COD-RED
                                 TO   CBMVMS-COD-RED
                                 MOVE CBWORK-DOCTO
                                 TO   CBMVMS-DOCTO
                                 MOVE CBWORK-AAMMDD-DOCTO
                                 TO   CBMVMS-AAAAMMDD-DOCTO
                                 MOVE CBWORK-CENTRO-CUSTO
                                 TO   CBMVMS-CENTRO-CUSTO
                                 MOVE CBWORK-HISTORICO-PADRAO
                                 TO   CBMVMS-HISTORICO-PADRAO
                                 MOVE CBWORK-HISTORICO-VARIAVEL
                                 TO   CBMVMS-HISTORICO-VARIAVEL
                                 MOVE CBWORK-VALOR
                                 TO   CBMVMS-VALOR
                                 IF   CBMVMS-AAAAMMDD-DOCTO NOT = 0
                                      ADD ANO-8 TO CBMVMS-AAAAMMDD-DOCTO
                                 END-IF
                                 WRITE CBMVMS-REG
                                 ADD 1 TO LD-CBWORK
                                 DISPLAY T-LD-CBWORK
                             END-IF
                     END-PERFORM
                     CLOSE CBWORK
                     SET CWBOXW-CLOSE TO TRUE
                     CALL "CWBOXW" USING PARAMETROS-CWBOXW
                     DELETE FILE CBWORK
                END-IF
           END-IF
           CLOSE CBMVMS.

       000-99-FIM. GOBACK.

       END PROGRAM CB045PCW.
