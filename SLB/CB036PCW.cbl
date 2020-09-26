       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB036PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/03/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Recompoe controle de saldos pelos movimentos *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOBASL REPLACING MANUAL BY EXCLUSIVE.
       COPY CBCOMSSL.
       COPY CBCOSASL REPLACING MANUAL BY EXCLUSIVE.
       COPY CBPLCOSL REPLACING MANUAL BY EXCLUSIVE.
       COPY CBMVMSSL REPLACING MANUAL BY EXCLUSIVE.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOBAFD.
       COPY CBCOMSFD.
       COPY CBCOSAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CC                       PIC  9(004) VALUE 0.
           05 ESTORNO                  PIC  9(001) VALUE 0.
           05 MAIS-ANTIGO              PIC  9(006) VALUE 999999.
           05 LB-HELP                  PIC  X(012) VALUE "CB036PCW.HXX".
           05 RK-CBCOMS           COMP PIC  9(001) VALUE 1.
           05 LD-CBCOBA                PIC  9(006) VALUE 0.
           05 LD-CBMVMS                PIC  9(006) VALUE 0.
           05 CONTROLE-MES             PIC  9(006) VALUE ZERO.
           05 VEZ-LANCAMENTO           PIC  9(001) VALUE 0.
           05 OK                       PIC  X(040) VALUE
             "Geracao completada".
           05 CONTROLE-REFERENCIA.
              10 CONTROLE-AAAA         PIC  9(004).
              10 CONTROLE-MM           PIC  9(002).
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 ESPACOS                  PIC  X(068) VALUE SPACES.
           05 NDF                      PIC  X(024) VALUE
              "NAO DEFINIDIO NO FORMATO".
           05 ABRE-NUMERO              PIC  9(018) VALUE 0.
           05 ABRE-NUMERO-X
              REDEFINES ABRE-NUMERO    PIC  X(018).
           05 INDICA-DBCR              PIC  X(008) VALUE SPACES.
           05 RODAPE                   PIC  X(068) VALUE SPACES.
           05 DV                       PIC  X(001) VALUE SPACE.
           05 LER                      PIC  X(001) VALUE "S".
           05 TECLA                    PIC  9(002) VALUE 0. COPY CWKEYS.
           05 DGC                      PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 P                        PIC  9(004) VALUE 0.
           05 S                        PIC  9(004) VALUE 0.
           05 S2                       PIC  9(004) VALUE 0.
           05 LIMITE                   PIC  9(002) VALUE 0.
           05 PG                       PIC  9(002) VALUE 0.
           05 LD-LOTE-I                PIC  9(006) VALUE 0.
           05 GR-CBMVMS                PIC  9(006) VALUE 0.
           05 GR-LOTEWK                PIC  9(006) VALUE 0.
           05 LD-LOTEWK                PIC  9(006) VALUE 0.
           05 RESPOSTA                 PIC  X(001) VALUE "N".
              88 RESPOSTA-OK VALUE "E" "e" "D" "d" "N" "n".
              88 EXTENDER    VALUE "E" "e".
              88 DESTRUIR    VALUE "D" "d".
              88 NOVO-NOME   VALUE "N" "n".
              88 EFETIVAR-OK VALUE "S" "s" "N" "n".
              88 EFETIVAR    VALUE "S" "s".
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV999999".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBCOMS.
              10 FS-CBCOMS             PIC  X(002) VALUE "00".
              10 LB-CBCOMS             PIC  X(050) VALUE "CBCOMS".
           05 LANCAMENTO               PIC  9(007) VALUE 0.

       COPY CB002PCW.

       SCREEN SECTION.

       01  CTAC-LIT-CB036PCW.
           05 LINE 08 COLUMN 11 VALUE "Controle de saldos REMOVIDO".
           05 LINE 10 COLUMN 11 VALUE "Zerando controle de BAC".
           05 LINE 12 COLUMN 11 VALUE "Contabilizando lan‡amentos".

       01  CTAC-VAR-CB036PCW.
           05 LINE 08 COLUMN 39 PIC X(040) FROM LB-CBCOSA.
           05 TL-LD-CBCOBA LINE 10 COLUMN 03 PIC ZZZ.ZZ9 FROM LD-CBCOBA.
           05 LINE 10 COLUMN 39 PIC X(040) FROM LB-CBCOBA.
           05 TL-LD-CBMVMS LINE 12 COLUMN 03 PIC ZZZ.ZZ9 FROM LD-CBMVMS.
           05 TL-LB-CBMVMS LINE 12 COLUMN 39 PIC X(040) FROM LB-CBMVMS.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   VARYING AAAA-REF FROM AAAA-REF BY 1
                           UNTIL AAAA-REF > CBCOMS-AAAA
                     AFTER MM-REF FROM 1 BY 1
                            UNTIL MM-REF > 12
                               OR (MM-REF > CBCOMS-MM
                              AND AAAA-REF = CBCOMS-AAAA)
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           DISPLAY TL-LD-CBMVMS
                   TL-LB-CBMVMS

           CALL "CB045PCW" USING LB-CBMVMS (1: 6)
           OPEN INPUT CBMVMS
           IF   FS-CBMVMS = "00"
                MOVE 0 TO LD-CBMVMS
                          LANCAMENTO
                CLOSE CBMVMS
                PERFORM 130-ABRIR-MES THRU 130-99-FIM
                PERFORM TEST AFTER UNTIL FS-CBMVMS > "09"
                  READ CBMVMS NEXT RECORD
                   NOT AT END
                       IF   CBMVMS-LANCAMENTO NOT = 0
                            ADD 1 TO LD-CBMVMS
                            DISPLAY TL-LD-CBMVMS
                            PERFORM 150-CONTROLA-SALDOS THRU 150-99-FIM
                       END-IF
                  END-READ
                END-PERFORM
                CLOSE CBMVMS
           END-IF.

       100-99-FIM. EXIT.

       130-ABRIR-MES.

           MOVE AAAA-REF TO CBCOBA-AAAA
           MOVE MM-REF   TO CBCOBA-MM
           CALL "CB045PCW" USING LB-CBMVMS (1: 6)
           OPEN I-O CBMVMS
           DISPLAY ESPACOS LINE 23 COLUMN 03
           DISPLAY "Gerando referˆncia " LINE 23 COLUMN 03
           CBCOBA-MM  "/" CBCOBA-AAAA "..."
           PERFORM UNTIL FS-CBCOSA > "09"
             READ CBCOSA NEXT RECORD
             IF  FS-CBCOSA < "10"
                 MOVE CBCOSA-SALDO-ATUAL   TO CBCOSA-SALDO-INICIAL
                 MOVE 0                    TO CBCOSA-A-DEBITO
                                              CBCOSA-A-CREDITO
                 MOVE CBCOSA-REG           TO SALVA-REG
                 MOVE CBCOSA-AAAAMM        TO CONTROLE-REFERENCIA
                 PERFORM TEST AFTER
                   UNTIL CBCOBA-REFERENCIA = CONTROLE-REFERENCIA
                       ADD 1 TO CONTROLE-MM
                       IF   CONTROLE-MM > 12
                            MOVE 1   TO CONTROLE-MM
                             ADD 1   TO CONTROLE-AAAA
                       END-IF
                       MOVE SALVA-REG            TO CBCOSA-REG
                       MOVE CONTROLE-REFERENCIA  TO CBCOSA-AAAAMM
                       READ CBCOSA
                       WRITE CBCOSA-REG
                 END-PERFORM
             END-IF
           END-PERFORM
           DISPLAY OK LINE 23 COLUMN 3.

       130-99-FIM. EXIT.

       150-CONTROLA-SALDOS.

           MOVE CBMVMS-SERIE  TO CBCOBA-SERIE
           MOVE CBMVMS-NUMERO TO CBCOBA-NUMERO

           READ CBCOBA
           IF   FS-CBCOBA > "09"
                STOP RUN
           END-IF

           IF   CBMVMS-LANCAMENTO NOT = LANCAMENTO
                MOVE 0                 TO VEZ-LANCAMENTO
                MOVE CBMVMS-LANCAMENTO TO LANCAMENTO
           END-IF

           MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
           READ CBPLCO KEY IS CBPLCO-COD-RED

           IF   FS-CBPLCO > "09"
                STOP RUN
           END-IF

           COPY CBCOSACW.
           ADD  1 TO VEZ-LANCAMENTO

           IF   VEZ-LANCAMENTO = 1
                ADD 1 TO CBCOBA-LC-EFETIVOS
           END-IF

           IF   CBMVMS-TIPO = "D"
                ADD CBMVMS-VALOR TO CBCOBA-DB-EFETIVOS
           ELSE
                ADD CBMVMS-VALOR TO CBCOBA-CR-EFETIVOS
           END-IF

           REWRITE CBCOBA-REG
           IF   FS-CBCOBA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
                STOP RUN
           END-IF.

       150-99-FIM. EXIT.

       800-INICIAIS.

           OPEN INPUT CBCOSA
           IF   FS-CBCOSA > "09"
                GOBACK
           END-IF
           PERFORM TEST AFTER UNTIL FS-CBCOSA > "09"
                   READ CBCOSA NEXT RECORD IGNORE LOCK
                   IF   CBCOSA-AAAAMM < MAIS-ANTIGO
                   AND  FS-CBCOSA < "10"
                        MOVE CBCOSA-AAAAMM TO MAIS-ANTIGO
                        MOVE CBCOSA-MM     TO MM-REF
                        MOVE CBCOSA-AAAA   TO AAAA-REF
                   END-IF
           END-PERFORM
           CLOSE CBCOSA
           OPEN I-O CBCOMS
                    CBPLCO
                    CBCOBA

           IF   FS-CBCOMS > "09"
           OR   FS-CBPLCO > "09"
           OR   FS-CBCOBA > "09"
                PERFORM 900-FINAIS THRU 900-99-FIM
           END-IF

           READ CBCOMS

           DELETE FILE CBCOSA
           OPEN I-O    CBCOSA

           DISPLAY CTAC-LIT-CB036PCW
           DISPLAY CTAC-VAR-CB036PCW
           PERFORM TEST AFTER UNTIL FS-CBCOBA > "09"
                   READ CBCOBA NEXT RECORD
                        NOT AT END
                            ADD 1 TO LD-CBCOBA
                            DISPLAY TL-LD-CBCOBA
                            MOVE 0 TO CBCOBA-LC-EFETIVOS
                                      CBCOBA-CR-EFETIVOS
                                      CBCOBA-DB-EFETIVOS
                           REWRITE CBCOBA-REG
                   END-READ
           END-PERFORM.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOSA CBCOMS CBPLCO CBCOBA.

       900-99-FIM. EXIT.

       END PROGRAM CB036PCW.
