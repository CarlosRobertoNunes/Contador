       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB008PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  13/01/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de Lancamentos                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCACCSL.
       COPY CBCAHISL.
       COPY CBCOBASL.
       COPY CBCOMSSL.
       COPY CBCOSASL REPLACING MANUAL    BY EXCLUSIVE.
       COPY CBCOHISL.
       COPY CBHIVASL REPLACING AUTOMATIC BY MANUAL.
       COPY CBPLCOSL.
       COPY CBMVMSSL.
       COPY CBGEINSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCACCFD.
       COPY CBCAHIFD.
       COPY CBCOBAFD.
       COPY CBCOMSFD.
       COPY CBCOSAFD.
       COPY CBCOHIFD.
       COPY CBHIVAFD.
       COPY CBPLCOFD.
       COPY CBMVMSFD.
       COPY CBGEINFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 CC                       PIC  9(004) VALUE 0.
           05 CC-FLAG-TXT              PIC  X(003) VALUE SPACES.
           05 COD-RED-F5               PIC  9(005) VALUE 0.
           05 COD-RED-CALL             PIC  9(005) VALUE 0.
           05 COD-RED-DV-CALL          PIC  X(001) VALUE SPACE.
      *    05 CWBOXF-F5                PIC  9(001) VALUE 0.
           05 DXX                      PIC  9(002) VALUE 0.
           05 F5-ON                    PIC  9(001) VALUE 0.
           05 SALDO-DB                 PIC S9(012)V99 VALUE 0.
           05 SALDO-CR                 PIC S9(012)V99 VALUE 0.
           05 SALDO-DB-A               PIC S9(012)V99 VALUE 0.
           05 SALDO-CR-A               PIC S9(012)V99 VALUE 0.
           05 DISPONIVEL-DB-A          PIC  9(001) VALUE 0.
           05 DISPONIVEL-CR-A          PIC  9(001) VALUE 0.
           05 RODAPE-INCLUSAO          PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Hlp F3-Ver ".
           05 RODAPE-C7                PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Hlp F3-Ver F5-Lista hist¢ricos".
           05 RODAPE-C6                PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Hlp F3-Ver F5-Lista Centros de Custos".
           05 RODAPE-CONSULTA          PIC  X(068) VALUE
              "<Esc>-Fun‡Æo F1-Hlp F3-Ver Pg Up/Down ".
           05 VEZ                      PIC  9(001) VALUE 1.
           05 ESTORNO                  PIC  9(001) VALUE 0.
           05 VALOR-A                  PIC  9(012)V99 VALUE 0.
           05 COD-RED-CR-A             PIC  9(005) VALUE 0.
           05 COD-RED-DB-A             PIC  9(005) VALUE 0.
           05 VEZ-LANCAMENTO           PIC  9(001) VALUE 0.
           05 LANCAMENTO-ANTERIOR      PIC  9(007) VALUE 0.
           05 SALVA-REG                PIC  X(999) VALUE SPACES.
           05 CONTROLE-REFERENCIA.
              10 CONTROLE-AAAA         PIC  9(004).
              10 CONTROLE-MM           PIC  9(002).
           05 RK-CBCOMS            COMP PIC  9(001) VALUE 1.
           05 RK-CBGEIN            COMP PIC  9(001) VALUE 1.
           05 RK-CBCOHI            COMP PIC  9(001) VALUE 1.
           05 HELP-FILE.
              10                       PIC  X(010) VALUE "CB008PCW.H".
              10 CAMPO                 PIC  9(002) VALUE ZERO.
           05 OPCAO-ACESSO             PIC  9(001) VALUE 1.
           05 CONTROLE-MES             PIC  9(006) VALUE ZERO.
           05 DATA-CRITICA             PIC  X(006) VALUE ALL "0".
           05 STATUS-CD                            VALUE ZEROS.
              10 STATUS-C              PIC  X(002).
              10 STATUS-D              PIC  X(002).
           05 LINHA-BRANCA             PIC  X(068) VALUE SPACES.
           05 OK                       PIC  X(068) VALUE
             "Geracao completada".
           05 TECLA-C7                 PIC  9(002) VALUE ZERO.
           05 SALVA-TECLA2             PIC  9(002) VALUE ZERO.
           05 SALVA-TECLA              PIC  9(002) VALUE ZERO.
           05 TECLA                    PIC  9(002) VALUE ZERO.
              COPY CWKEYS.
           05 I                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 FL-EXIT                  PIC  9(001) VALUE 1.
           05 MENSAGEM-ERRO            PIC  X(030) VALUE SPACES.
              88 SEM-ERRO                          VALUE SPACES.
           05 MENSAGENS-DE-ERRO.
              10 PIC X(30) VALUE "N§ do BAC inv lido            ".
              10 PIC X(30) VALUE "BAC nÆo registrado            ".
              10 PIC X(30) VALUE "Referˆncia inv lida           ".
              10 PIC X(30) VALUE "Lan‡amento nÆo existe         ".
              10 PIC X(30) VALUE "Dia de referˆncia inv lido    ".
              10 PIC X(30) VALUE "Falta cta d‚bito e/ou cr‚dito ".
              10 PIC X(30) VALUE "Conta a d‚bito nÆo existe     ".
              10 PIC X(30) VALUE "Conta a cr‚dito nÆo existe    ".
              10 PIC X(30) VALUE "Data do documento inv lida    ".
              10 PIC X(30) VALUE "Hist¢rico padrÆo inexistente  ".
              10 PIC X(30) VALUE "Lan‡amento sem valor          ".
              10 PIC X(30) VALUE "Sem hist¢rico padrÆo/vari vel ".
              10 PIC X(30) VALUE "Conta d‚bito = conta cr‚dito  ".
              10 PIC X(30) VALUE "Confirme exclusÆo             ".
              10 PIC X(30) VALUE "Centro de custos sem registro ".
              10 PIC X(30) VALUE "Codigo reduzido inconsistente ".
           05 FILLER REDEFINES MENSAGENS-DE-ERRO.
              10 MSG OCCURS 16 PIC X(30).
           05 ER-CBCOBA.
              10 FS-CBCOBA             PIC  X(002) VALUE "00".
              10 LB-CBCOBA             PIC  X(050) VALUE "CBCOBA".
           05 ER-CBCAHI.
              10 FS-CBCAHI             PIC  X(002) VALUE "00".
              10 LB-CBCAHI             PIC  X(050) VALUE "CBCAHI".
           05 ER-CBCACC.
              10 FS-CBCACC             PIC  X(002) VALUE "00".
              10 LB-CBCACC             PIC  X(050) VALUE "CBCACC".
           05 ER-CBHIVA.
              10 FS-CBHIVA             PIC  X(002) VALUE "00".
              10 LB-CBHIVA             PIC  X(050) VALUE "CBHIVA".
           05 ER-CBPLCO.
              10 FS-CBPLCO             PIC  X(002) VALUE "00".
              10 LB-CBPLCO             PIC  X(050) VALUE "CBPLCO".
           05 ER-CBCOSA.
              10 FS-CBCOSA             PIC  X(002) VALUE "00".
              10 LB-CBCOSA             PIC  X(050) VALUE "CBCOSA".
           05 ER-CBMVMS.
              10 FS-CBMVMS             PIC  X(002) VALUE "00".
              10 LB-CBMVMS                         VALUE "CBMV000000".
                 15 FILLER             PIC  X(044).
                 15 AAAA-REF           PIC  9(004).
                 15 MM-REF             PIC  9(002).
                    88 MM-REF-OK VALUE 1 THRU 12.
           05 ER-CBCOMS.
              10 FS-CBCOMS             PIC  X(002) VALUE "00".
              10 LB-CBCOMS             PIC  X(050) VALUE "CBCOMS".
           05 ER-CBGEIN.
              10 FS-CBGEIN              PIC  X(002) VALUE "00".
              10 LB-CBGEIN              PIC  X(050) VALUE "CBGEIN".
           05 ER-CBCOHI.
              10 FS-CBCOHI              PIC  X(002) VALUE "00".
              10 LB-CBCOHI              PIC  X(050) VALUE "CBCOHI".
           05 HISTORICOS-VARIAVEIS VALUE SPACES.
              10 DESCR-01 PIC X(030).
              10 DESCR-02 PIC X(030).
              10 DESCR-03 PIC X(030).
              10 DESCR-04 PIC X(030).
              10 DESCR-05 PIC X(030).
              10 DESCR-06 PIC X(030).
              10 DESCR-07 PIC X(030).
              10 DESCR-08 PIC X(030).
              10 DESCR-09 PIC X(030).
              10 DESCR-10 PIC X(030).
              10 DESCR-11 PIC X(030).
              10 DESCR-12 PIC X(030).
              10 DESCR-13 PIC X(030).
              10 DESCR-14 PIC X(030).
              10 DESCR-15 PIC X(030).
              10 DESCR-16 PIC X(030).
              10 DESCR-17 PIC X(030).
              10 DESCR-18 PIC X(030).
              10 DESCR-19 PIC X(030).
              10 DESCR-20 PIC X(030).
              10 DESCR-21 PIC X(030).
              10 DESCR-22 PIC X(030).
              10 DESCR-23 PIC X(030).
              10 DESCR-24 PIC X(030).
           05 REDEFINES HISTORICOS-VARIAVEIS.
              10 DESCR OCCURS 24 PIC X(030).
           05 FUNCTION-B7      COMP-5 PIC  9(002) VALUE ZERO.
           05 PARAMETER-B7.
              10 SIZE-TELA     COMP-5 PIC  9(004) VALUE 2000.
              10 START-SCREEN  COMP-5 PIC  9(004) VALUE 1.
              10 START-BUFFER  COMP-5 PIC  9(004) VALUE 1.
           05 BUFFER-B7               PIC  X(2000) VALUE SPACES.
           05 OPCOES-DE-HELP.
              10 PIC X(8) VALUE "10350342".
              10 PIC X(8) VALUE "14230942".
              10 PIC X(8) VALUE "14350942".
              10 PIC X(8) VALUE "14030642".
              10 PIC X(8) VALUE "14120642".
              10 PIC X(8) VALUE "14210642".
              10 PIC X(8) VALUE "14260642".
              10 PIC X(8) VALUE "09351042".
              10 PIC X(8) VALUE "11101142".
              10 PIC X(8) VALUE "10350642".
              10 PIC X(8) VALUE "10060642".
              10 PIC X(8) VALUE "10260942".
           05 REDEFINES OPCOES-DE-HELP.
              10 OCCURS 12.
                 15 HELP-LIN PIC 99.
                 15 HELP-COL PIC 99.
                 15 HELP-VER PIC 99.
                 15 HELP-HOR PIC 99.

       01  LACAMENTO-EM-MEMORIA.
           05 LANCAMENTO       PIC 9(007)    VALUE 0.
           05 DD-REF           PIC 9(002)    VALUE 0.
           05 COD-RED-DB       PIC 9(005)    VALUE 0.
           05 COD-RED-CR       PIC 9(005)    VALUE 0.
           05 COD-RED-DB-DV    PIC X(001)    VALUE SPACE.
           05 COD-RED-CR-DV    PIC X(001)    VALUE SPACE.
           05 DOCTO            PIC 9(008)    VALUE 0.
           05 DATA-HISTORICA   PIC 9(008)    VALUE 0.
           05 CENTRO-CUSTO     PIC 9(004)    VALUE 0.
           05 HISTORICO        PIC 9(004)    VALUE 0.
           05 HISTORICO-V      PIC 9(006)    VALUE 0.
           05 VALOR            PIC 9(012)V99 VALUE 0.

           05 DESCRICAO-DB     PIC X(030)    VALUE SPACES.
           05 DESCRICAO-CR     PIC X(030)    VALUE SPACES.
           05 DESCRICAO-PADRAO PIC X(030)    VALUE SPACES.
           05 CONTA-ED-DB      PIC X(026)    VALUE SPACES.
           05 CONTA-ED-CR      PIC X(026)    VALUE SPACES.

       COPY CB002PCW.
       COPY CWBOXW.
       COPY CWBOXF.
       COPY CWNCOR.
       COPY CWDCNP.
       COPY CWTIME.
       COPY CWFUNC.

       SCREEN SECTION.

       01  CB008PA.
           05 LINE 07 COLUMN 13 VALUE "³".
           05 LINE 07 COLUMN 57 VALUE "³".
           05 LINE 08 COLUMN 03 VALUE "BAC".
           05 LINE 08 COLUMN 13 VALUE "³".
           05 LINE 08 COLUMN 19 VALUE "Previstos".
           05 LINE 08 COLUMN 40 VALUE "Digitados".
           05 LINE 08 COLUMN 57 VALUE "³".
           05 LINE 08 COLUMN 59 VALUE "Referˆncia: 00/0000".
           05 LINE 09 COLUMN 13 VALUE "³".
           05 LINE 09 COLUMN 34 VALUE "D".
           05 LINE 09 COLUMN 55 VALUE "D".
           05 LINE 09 COLUMN 57 VALUE "³".
           05 LINE 09 COLUMN 59 VALUE "Lan‡amento:".
           05 LINE 10 COLUMN 13 VALUE "³".
           05 LINE 10 COLUMN 34 VALUE "C".
           05 LINE 10 COLUMN 55 VALUE "C".
           05 LINE 10 COLUMN 57 VALUE "³".
           05 LINE 10 COLUMN 59 VALUE "Dia       :".
           05 LINE 11 COLUMN 02 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ".
           05 LINE 11 COLUMN 22 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄ".
           05 LINE 11 COLUMN 42 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄ".
           05 LINE 11 COLUMN 62 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 12 COLUMN 03 VALUE
              "Conta a d‚bito:      -  F4-Nova F5-?".
           05 LINE 12 COLUMN 40 VALUE "³".
           05 LINE 12 COLUMN 42 VALUE
              "Conta a cr‚dito:      -  F4-Nova F5-?".
           05 LINE 13 COLUMN 40 VALUE "³".
           05 LINE 14 COLUMN 40 VALUE "³".
           05 LINE 15 COLUMN 40 VALUE "³".
           05 LINE 16 COLUMN 40 VALUE "³".
           05 LINE 17 COLUMN 02 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 17 COLUMN 22 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄ".
           05 LINE 17 COLUMN 42 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 17 COLUMN 62 VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05 LINE 18 COLUMN 03 VALUE "Documento".
           05 LINE 18 COLUMN 15 VALUE "Data".
           05 LINE 18 COLUMN 24 PIC X(3) FROM CC-FLAG-TXT.
           05 LINE 18 COLUMN 28 VALUE "Hist¢rico padrÆo".
           05 LINE 18 COLUMN 74 VALUE "Valor".

       01  CB008PB.
           05 LINE 08 COLUMN 07 VALUE "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ His".
           05 LINE 08 COLUMN 27 VALUE "t¢rico complementar ".
           05 LINE 08 COLUMN 47 VALUE "vari vel ÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 08 COLUMN 67 VALUE "ÍÍÍÍÍÍ»".
           05 LINE 09 COLUMN 07 VALUE "º                   ".
           05 LINE 09 COLUMN 27 VALUE "             º      ".
           05 LINE 09 COLUMN 47 VALUE "                    ".
           05 LINE 09 COLUMN 67 VALUE "      º".
           05 LINE 10 COLUMN 07 VALUE "º                   ".
           05 LINE 10 COLUMN 27 VALUE "             º      ".
           05 LINE 10 COLUMN 47 VALUE "                    ".
           05 LINE 10 COLUMN 67 VALUE "      º".
           05 LINE 11 COLUMN 07 VALUE "º                   ".
           05 LINE 11 COLUMN 27 VALUE "             º      ".
           05 LINE 11 COLUMN 47 VALUE "                    ".
           05 LINE 11 COLUMN 67 VALUE "      º".
           05 LINE 12 COLUMN 07 VALUE "º                   ".
           05 LINE 12 COLUMN 27 VALUE "             º      ".
           05 LINE 12 COLUMN 47 VALUE "                    ".
           05 LINE 12 COLUMN 67 VALUE "      º".
           05 LINE 13 COLUMN 07 VALUE "º                   ".
           05 LINE 13 COLUMN 27 VALUE "             º      ".
           05 LINE 13 COLUMN 47 VALUE "                    ".
           05 LINE 13 COLUMN 67 VALUE "      º".
           05 LINE 14 COLUMN 07 VALUE "º                   ".
           05 LINE 14 COLUMN 27 VALUE "             º      ".
           05 LINE 14 COLUMN 47 VALUE "                    ".
           05 LINE 14 COLUMN 67 VALUE "      º".
           05 LINE 15 COLUMN 07 VALUE "º                   ".
           05 LINE 15 COLUMN 27 VALUE "             º      ".
           05 LINE 15 COLUMN 47 VALUE "                    ".
           05 LINE 15 COLUMN 67 VALUE "      º".
           05 LINE 16 COLUMN 07 VALUE "º                   ".
           05 LINE 16 COLUMN 27 VALUE "             º      ".
           05 LINE 16 COLUMN 47 VALUE "                    ".
           05 LINE 16 COLUMN 67 VALUE "      º".
           05 LINE 17 COLUMN 07 VALUE "º                   ".
           05 LINE 17 COLUMN 27 VALUE "             º      ".
           05 LINE 17 COLUMN 47 VALUE "                    ".
           05 LINE 17 COLUMN 67 VALUE "      º".
           05 LINE 18 COLUMN 07 VALUE "º                   ".
           05 LINE 18 COLUMN 27 VALUE "             º      ".
           05 LINE 18 COLUMN 47 VALUE "                    ".
           05 LINE 18 COLUMN 67 VALUE "      º".
           05 LINE 19 COLUMN 07 VALUE "º                   ".
           05 LINE 19 COLUMN 27 VALUE "             º      ".
           05 LINE 19 COLUMN 47 VALUE "                    ".
           05 LINE 19 COLUMN 67 VALUE "      º".
           05 LINE 20 COLUMN 07 VALUE "º                   ".
           05 LINE 20 COLUMN 27 VALUE "             º      ".
           05 LINE 20 COLUMN 47 VALUE "                    ".
           05 LINE 20 COLUMN 67 VALUE "      º".
           05 LINE 21 COLUMN 07 VALUE "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 21 COLUMN 27 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍ".
           05 LINE 21 COLUMN 47 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05 LINE 21 COLUMN 67 VALUE "ÍÍÍÍÍÍ¼".
           05 D01 LINE 09 COLUMN 09 PIC X(030) USING DESCR-01.
           05 D02 LINE 10 COLUMN 09 PIC X(030) USING DESCR-02.
           05 D03 LINE 11 COLUMN 09 PIC X(030) USING DESCR-03.
           05 D04 LINE 12 COLUMN 09 PIC X(030) USING DESCR-04.
           05 D05 LINE 13 COLUMN 09 PIC X(030) USING DESCR-05.
           05 D06 LINE 14 COLUMN 09 PIC X(030) USING DESCR-06.
           05 D07 LINE 15 COLUMN 09 PIC X(030) USING DESCR-07.
           05 D08 LINE 16 COLUMN 09 PIC X(030) USING DESCR-08.
           05 D09 LINE 17 COLUMN 09 PIC X(030) USING DESCR-09.
           05 D10 LINE 18 COLUMN 09 PIC X(030) USING DESCR-10.
           05 D11 LINE 19 COLUMN 09 PIC X(030) USING DESCR-11.
           05 D12 LINE 20 COLUMN 09 PIC X(030) USING DESCR-12.
           05 D13 LINE 09 COLUMN 42 PIC X(030) USING DESCR-13.
           05 D14 LINE 10 COLUMN 42 PIC X(030) USING DESCR-14.
           05 D15 LINE 11 COLUMN 42 PIC X(030) USING DESCR-15.
           05 D16 LINE 12 COLUMN 42 PIC X(030) USING DESCR-16.
           05 D17 LINE 13 COLUMN 42 PIC X(030) USING DESCR-17.
           05 D18 LINE 14 COLUMN 42 PIC X(030) USING DESCR-18.
           05 D19 LINE 15 COLUMN 42 PIC X(030) USING DESCR-19.
           05 D20 LINE 16 COLUMN 42 PIC X(030) USING DESCR-20.
           05 D21 LINE 17 COLUMN 42 PIC X(030) USING DESCR-21.
           05 D22 LINE 18 COLUMN 42 PIC X(030) USING DESCR-22.
           05 D23 LINE 19 COLUMN 42 PIC X(030) USING DESCR-23.
           05 D24 LINE 20 COLUMN 42 PIC X(030) USING DESCR-24.

       01  CHAVE-BAC.
           05 LINE 10 COLUMN 03 PIC Z(004)/ USING CBCOBA-SERIE.
           05 LINE 10 COLUMN 08 PIC Z(004) USING CBCOBA-NUMERO.

       01  DADOS-BAC.
           05 LINE 08 COLUMN 29 PIC ZZZZZ9 FROM CBCOBA-LC-PREVISTOS.
           05 LINE 08 COLUMN 50 PIC ZZZZZ9 FROM CBCOBA-LC-EFETIVOS.
           05 LINE 09 COLUMN 15 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                    FROM CBCOBA-DB-PREVISTOS.
           05 LINE 09 COLUMN 36 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                    FROM CBCOBA-DB-EFETIVOS.
           05 LINE 10 COLUMN 15 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                    FROM CBCOBA-CR-PREVISTOS.
           05 LINE 10 COLUMN 36 PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                    FROM CBCOBA-CR-EFETIVOS.

       01  CHAVE-LANCAMENTO AUTO.
       03  CHAVE-LANCAMENTO-INCLUSAO.
           05 LINE 08 COLUMN 71 PIC 9(002) USING MM-REF.
           05 LINE 08 COLUMN 74 PIC 9(004) USING AAAA-REF.
       03  C0.
           05 LINE 09 COLUMN 71 PIC Z(007) USING LANCAMENTO.

       01  DADOS-LANCAMENTO AUTO.
           05 C1 LINE 10 COLUMN 71 PIC Z(002)   USING DD-REF.
           05 APAGA-C2.
              06 LINE 13 COLUMN 03 PIC X(036)   FROM SPACES.
              06 LINE 14 COLUMN 03 PIC X(036)   FROM SPACES.
              06 LINE 15 COLUMN 03 PIC X(036)   FROM SPACES.
              06 LINE 16 COLUMN 03 PIC X(036)   FROM SPACES.
           05 C2.
              06 C2-R.
                 07 LINE 12 COLUMN 19 PIC Z(005)   USING COD-RED-DB.
                 07 LINE 12 COLUMN 25 PIC X        USING COD-RED-DB-DV.
              06 LINE 13 COLUMN 03 PIC X(028)   FROM CONTA-ED-DB.
              06 LINE 14 COLUMN 03 PIC X(032)   FROM DESCRICAO-DB.
              06 LINE 15 COLUMN 03 VALUE "Saldo 00/0000:".
              06 LINE 15 COLUMN 09 PIC 99/  FROM MM-REF.
              06 LINE 15 COLUMN 12 PIC 9999 FROM AAAA-REF.
              06 LINE 15 COLUMN 18 PIC BBZZZ.ZZZ.ZZZ.ZZ9,99+
                                                FROM SALDO-DB-A.
              06 LINE 16 COLUMN 03 VALUE "Saldo 00/0000: ".
              06 LINE 16 COLUMN 09 PIC 99/  FROM CBCOSA-MM.
              06 LINE 16 COLUMN 12 PIC 9999 FROM CBCOSA-AAAA.
              06 LINE 16 COLUMN 18 PIC BBZZZ.ZZZ.ZZZ.ZZ9,99+
                                                FROM SALDO-DB.
           05 APAGA-C3.
              06 LINE 13 COLUMN 42 PIC X(037)   FROM SPACES.
              06 LINE 14 COLUMN 42 PIC X(037)   FROM SPACES.
              06 LINE 15 COLUMN 42 PIC X(037)   FROM SPACES.
              06 LINE 16 COLUMN 42 PIC X(037)   FROM SPACES.
           05 C3.
              06 C3-R.
                 07 LINE 12 COLUMN 59 PIC Z(005)   USING COD-RED-CR.
                 07 LINE 12 COLUMN 65 PIC X(001)   USING COD-RED-CR-DV.
              06 LINE 13 COLUMN 42 PIC X(028)   FROM CONTA-ED-CR.
              06 LINE 14 COLUMN 42 PIC X(032)   FROM DESCRICAO-CR.
              06 LINE 15 COLUMN 42 VALUE "Saldo 00/0000: ".
              06 LINE 15 COLUMN 48 PIC 99/  FROM MM-REF.
              06 LINE 15 COLUMN 51 PIC 9999 FROM AAAA-REF.
              06 LINE 15 COLUMN 58 PIC BBZZZ.ZZZ.ZZZ.ZZ9,99+
                                                FROM SALDO-CR-A.
              06 LINE 16 COLUMN 42 VALUE "Saldo 00/0000: ".
              06 LINE 16 COLUMN 48 PIC 99/  FROM CBCOSA-MM.
              06 LINE 16 COLUMN 51 PIC 9999 FROM CBCOSA-AAAA.
              06 LINE 16 COLUMN 58 PIC BBZZZ.ZZZ.ZZZ.ZZ9,99+
                                                FROM SALDO-CR.
           05 C4 LINE 20 COLUMN 03 PIC Z(008)   USING DOCTO.
           05 C5 LINE 20 COLUMN 12 PIC 99/99/9999 USING DATA-HISTORICA.
           05 C6 LINE 20 COLUMN 23 PIC Z(004)   USING CENTRO-CUSTO.
           05 C7.
              06 LINE 20 COLUMN 28 PIC Z(004)   USING HISTORICO.
              06 LINE 20 COLUMN 33 PIC X(030)   FROM DESCRICAO-PADRAO.
           05 C8 LINE 20 COLUMN 63 PIC Z.ZZZ.ZZZ.ZZ9,99 USING VALOR.

       01  APAGA-C1-C2-15.
           06 LINE 15 COLUMN 03 PIC X(36) FROM SPACES.
           06 LINE 15 COLUMN 42 PIC X(37) FROM SPACES.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   UNTIL FINALIZAR
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM.

           IF   PARAR
                STOP RUN
           ELSE
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           CLOSE CBMVMS

           IF   FL-EXIT = 1
                MOVE SPACE TO FUNCAO
                EXEC COBOLware Option
                     Function FUNCAO
                END-EXEC
                MOVE ZERO  TO FL-EXIT
           END-IF

           IF   VEZ = 1
                MOVE 2 TO VEZ
                IF   NOT FINALIZAR
                     DISPLAY CB008PA
                END-IF
           END-IF

           MOVE "23" TO FS-CBMVMS

           IF   NOT FINALIZAR
           AND  INCLUSAO
                PERFORM TEST AFTER UNTIL ((0 NOT = CBCOBA-SERIE
                                               AND CBCOBA-NUMERO)
                                           AND (FS-CBCOBA < "10"))
                                      OR FL-EXIT = 1
                        PERFORM TEST AFTER UNTIL NOT F1
                           IF   INCLUSAO
                                 DISPLAY RODAPE-INCLUSAO
                                         LINE 23 COLUMN 03
                           ELSE
                                 DISPLAY RODAPE-CONSULTA
                                         LINE 23 COLUMN 03
                           END-IF
                           ACCEPT CHAVE-BAC
                           ACCEPT TECLA FROM ESCAPE KEY
                           IF   F1
                                MOVE 11 TO CAMPO
                                PERFORM 180-HELP THRU 180-99-FIM
                           END-IF
                           IF   F3
                                DISPLAY CBCOBA-CHAVE UPON COMMAND-LINE
                                CALL "CB035PCW"
                                CANCEL "CB035PCW"
                           END-IF
                        END-PERFORM
                        IF   ESC
                             MOVE 1 TO FL-EXIT
                        ELSE
                             IF   0 = CBCOBA-SERIE OR CBCOBA-NUMERO
                                  EXEC COBOLware Send
                                       Message MSG(1)
                                  END-EXEC
                             ELSE
                                   READ CBCOBA LOCK
                                  IF   FS-CBCOBA = "23"
                                       EXEC COBOLware Send
                                            Message MSG(2)
                                       END-EXEC
                                  ELSE
                                       IF   FS-CBCOBA < "10"
                                            DISPLAY DADOS-BAC
                                       END-IF
                                  END-IF
                             END-IF
                        END-IF
                END-PERFORM
           END-IF

           IF   NOT FINALIZAR
           AND  FL-EXIT = 0
                PERFORM 700-VERIFICA-MES THRU 700-99-FIM
                MOVE "44" TO FS-CBMVMS
                PERFORM TEST AFTER UNTIL FS-CBMVMS < "10"
                                      OR FL-EXIT > 0
                IF   INCLUSAO
                     MOVE CBCOBA-AAAA  TO AAAA-REF
                     MOVE CBCOBA-MM    TO MM-REF
                     MOVE 0           TO HISTORICO-V
                     MOVE SPACES      TO HISTORICOS-VARIAVEIS
                     DISPLAY CHAVE-LANCAMENTO-INCLUSAO
                     DISPLAY "Gerando" LINE 09 COLUMN 71
                     CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                     OPEN INPUT CBMVMS
                     IF   FS-CBMVMS > "09"
                          IF   CBCOBA-LC-EFETIVOS NOT = 0
                          OR   CBCOBA-CR-EFETIVOS NOT = 0
                          OR   CBCOBA-DB-EFETIVOS NOT = 0
                               CALL "CWISAM" USING ER-CBMVMS
                               GO TO 100-99-FIM
                          END-IF
                          MOVE "05" TO FS-CBMVMS
                          CALL "CWISAM" USING ER-CBMVMS
                          CLOSE CBMVMS
                          OPEN I-O CBMVMS
                          INITIALIZE CBMVMS-REG
                          WRITE CBMVMS-REG
                          OPEN INPUT CBCOMS
                          IF   FS-CBCOMS = "30" OR "35"
                               CLOSE CBCOMS
                               OPEN OUTPUT CBCOMS
                               MOVE CBCOBA-REFERENCIA TO CBCOMS-REG
                               WRITE CBCOMS-REG
                          END-IF
                          CLOSE CBCOMS
                          OPEN I-O CBCOMS
                          PERFORM TEST AFTER
                                  UNTIL FS-CBCOMS NOT = "9D"
                                  READ CBCOMS
                                  IF  FS-CBCOMS = "9D"
                                      CALL "CWISAM" USING FS-CBCOMS
                                  END-IF
                          END-PERFORM
                          IF   CBCOBA-REFERENCIA > CBCOMS-REG
                               PERFORM 700-VERIFICA-MES THRU 700-99-FIM
                               MOVE CBCOMS-REG        TO CBGEIN-ANTERIOR
                               MOVE CBCOBA-REFERENCIA TO CBGEIN-ATUAL
                               OPEN OUTPUT CBGEIN
                               WRITE CBGEIN-REG
                               DISPLAY LINHA-BRANCA LINE 23 COLUMN 3
                               DISPLAY
                               "Gerando referˆncia " LINE 23 COLUMN 3
                               CBCOBA-MM  "/" CBCOBA-AAAA " aguarde..."
                               OPEN I-O CBCOSA
                               PERFORM UNTIL FS-CBCOSA > "09"
                                 READ CBCOSA NEXT RECORD
                                 IF  FS-CBCOSA < "10"
                                     IF   CBCOSA-AAAAMM = CBCOMS-REG
                                          MOVE CBCOSA-SALDO-ATUAL
                                            TO CBCOSA-SALDO-INICIAL
                                          MOVE 0 TO CBCOSA-A-DEBITO
                                                    CBCOSA-A-CREDITO
                                          MOVE CBCOSA-REG TO SALVA-REG
                                          MOVE CBCOSA-AAAAMM
                                            TO CONTROLE-REFERENCIA
                                          PERFORM TEST AFTER
                                            UNTIL CBCOBA-REFERENCIA =
                                                  CONTROLE-REFERENCIA
                                                ADD 1 TO CONTROLE-MM
                                                IF   CONTROLE-MM > 12
                                                     MOVE 1
                                                       TO CONTROLE-MM
                                                      ADD 1
                                                       TO CONTROLE-AAAA
                                                END-IF
                                                MOVE SALVA-REG
                                                  TO CBCOSA-REG
                                                MOVE CONTROLE-REFERENCIA
                                                  TO CBCOSA-AAAAMM
                                                WRITE CBCOSA-REG
                                                IF   FS-CBCOSA > "09"
                                                     CALL "CWISAM"
                                                     USING ER-CBCOSA
                                                END-IF
                                          END-PERFORM
                                      END-IF
                                 END-IF
                               END-PERFORM
                               MOVE CBCOBA-REFERENCIA TO CBCOMS-REG
                               REWRITE CBCOMS-REG
                               CLOSE CBCOSA CBCOMS CBGEIN
                               DELETE FILE CBGEIN
                               DISPLAY OK LINE 23 COLUMN 3
                          END-IF
                     ELSE
                          CLOSE CBMVMS
                          OPEN I-O CBMVMS
                     END-IF
                ELSE
                     PERFORM TEST AFTER UNTIL (NOT F1)
                                          AND (NOT F3)
                           MOVE LANCAMENTO TO LANCAMENTO-ANTERIOR
                           DISPLAY RODAPE-CONSULTA LINE 23 COLUMN 03
                           ACCEPT CHAVE-LANCAMENTO
                           ACCEPT TECLA FROM ESCAPE KEY
                           EVALUATE TRUE
                               WHEN F1
                                    MOVE 10 TO CAMPO
                                    PERFORM 180-HELP THRU 180-99-FIM
                               WHEN F3
                                AND (AAAA-REF NOT = 0)
                                AND (MM-REF NOT = 0)
                                    DISPLAY CBCOBA-CHAVE
                                            UPON COMMAND-LINE
                                    CALL "CB035PCW"
                                    CANCEL "CB035PCW"
                               WHEN (PAGE-DOWN OR PAGE-UP)
                                AND (AAAA-REF NOT = 0)
                                AND (MM-REF NOT = 0)
                                AND LANCAMENTO = LANCAMENTO-ANTERIOR
                                    CALL "CB045PCW"
                                         USING LB-CBMVMS (1: 6)
                                    OPEN INPUT CBMVMS
                                    MOVE LANCAMENTO TO CBMVMS-LANCAMENTO
                                    IF  PAGE-DOWN
                                        START CBMVMS
                                          KEY NOT LESS CBMVMS-CHAVE
                                    ELSE
                                        IF  LANCAMENTO = 0
                                            MOVE 9999999
                                              TO CBMVMS-LANCAMENTO
                                                       LANCAMENTO
                                        END-IF
                                        START CBMVMS
                                          KEY NOT GREATER CBMVMS-CHAVE
                                    END-IF
                                    PERFORM UNTIL CBMVMS-LANCAMENTO
                                              NOT = LANCAMENTO
                                               OR FS-CBMVMS > "09"
                                            IF  PAGE-DOWN
                                                READ CBMVMS
                                                     NEXT RECORD
                                                     IGNORE LOCK
                                            ELSE
                                                READ CBMVMS
                                                     PREVIOUS RECORD
                                                     IGNORE LOCK
                                            END-IF
                                    END-PERFORM
                                    MOVE CBMVMS-LANCAMENTO TO LANCAMENTO
                                    CLOSE CBMVMS
                               END-EVALUATE
                     END-PERFORM
                     IF   ESC
                          MOVE 1 TO FL-EXIT
                     ELSE
                          IF   NOT MM-REF-OK
                          OR   AAAA-REF < 1900
                               EXEC COBOLware Send
                                    Message MSG(3)
                               END-EXEC
                               MOVE "44" TO FS-CBMVMS
                          ELSE
                               CALL "CB045PCW" USING LB-CBMVMS (1: 6)
                               OPEN INPUT CBMVMS
                               IF   FS-CBMVMS > "09"
                                    CALL "CWISAM" USING ER-CBMVMS
                                    GO TO 100-99-FIM
                               ELSE
                                    CLOSE CBMVMS
                                    OPEN I-O CBMVMS
                               END-IF
                          END-IF
                     END-IF
                IF   FL-EXIT = 0
                     MOVE ZERO       TO COD-RED-DB COD-RED-DB-A
                                        COD-RED-CR COD-RED-CR-A
                                        SALDO-DB-A SALDO-CR-A
                                        SALDO-DB   SALDO-CR
                                        DISPONIVEL-DB-A
                                        DISPONIVEL-CR-A
                     MOVE SPACES     TO DESCRICAO-DB
                                        DESCRICAO-CR
                                        CONTA-ED-DB
                                        CONTA-ED-CR
                                        DESCRICAO-PADRAO
                     MOVE LANCAMENTO TO CBMVMS-LANCAMENTO
                     MOVE "D"        TO CBMVMS-TIPO
                     READ CBMVMS LOCK
                     MOVE FS-CBMVMS TO STATUS-C
                     IF   FS-CBMVMS < "10"
                          MOVE CBMVMS-COD-RED TO COD-RED-DB COD-RED-DB-A
                          PERFORM 110-CARREGA-DADOS THRU 110-99-FIM
                          MOVE COD-RED-DV-CALL TO COD-RED-DB-DV
                     END-IF
                     PERFORM 115-INFORMA-SALDOS THRU 115-99-FIM
                     MOVE LANCAMENTO TO CBMVMS-LANCAMENTO
                     MOVE "C"        TO CBMVMS-TIPO
                     READ CBMVMS LOCK
                     MOVE FS-CBMVMS  TO STATUS-D
                     IF   FS-CBMVMS < "10"
                          MOVE CBMVMS-COD-RED TO COD-RED-CR COD-RED-CR-A
                          PERFORM 110-CARREGA-DADOS THRU 110-99-FIM
                          MOVE COD-RED-DV-CALL TO COD-RED-CR-DV
                     END-IF
                     PERFORM 115-INFORMA-SALDOS THRU 115-99-FIM
                     IF   STATUS-C < "10"
                     OR   STATUS-D < "10"
                          IF   HISTORICO-V NOT = 0
                               PERFORM 151-CARREGA-HIST THRU 151-99-FIM
                          END-IF
                          MOVE "00" TO FS-CBMVMS
                          READ CBCOBA IGNORE LOCK
                          IF   FS-CBCOBA NOT < "10"
                               DISPLAY CHAVE-BAC
                                       DADOS-BAC
                          MOVE HISTORICO TO CBCAHI-CODIGO
                          IF CBCAHI-CODIGO NOT = 0
                             READ CBCAHI IGNORE LOCK
                             IF   FS-CBCOBA NOT < "10"
                                  MOVE "Erro hist¢rico"
                                    TO DESCRICAO-PADRAO
                                  CALL "CWISAM" USING ER-CBCOBA
                             ELSE
                                  MOVE CBCAHI-DESCRICAO
                                    TO DESCRICAO-PADRAO
                             END-IF
                          END-IF
                          DISPLAY DADOS-LANCAMENTO
                          IF   MM-REF   = CBCOSA-MM
                          AND  AAAA-REF = CBCOSA-AAAA
                               DISPLAY APAGA-C1-C2-15
                          END-IF
                     ELSE
                          EXEC COBOLware Send
                               Message MSG(4)
                          END-EXEC
                          CLOSE CBMVMS
                          MOVE "23" TO FS-CBMVMS
                     END-IF
                   END-IF
                END-IF
                END-PERFORM
                IF   FL-EXIT = 0
                     IF   INCLUSAO OR ALTERACAO
                          PERFORM TEST AFTER
                                  UNTIL MENSAGEM-ERRO = SPACES
                                        OR ESC
                                  PERFORM 150-ACCEPTS THRU 150-99-FIM
                                  IF   ESC
                                       MOVE 1 TO FL-EXIT
                                  ELSE
                                  IF   VALOR NOT = 0
                                       DISPLAY LINHA-BRANCA
                                               LINE 23 COLUMN 03
                                       PERFORM 105-CONFIRMA
                                          THRU 105-99-FIM
                                       IF   ABORTAR
                                            MOVE SPACES TO MENSAGEM-ERRO
                                       END-IF
                                  ELSE
                                       MOVE 1 TO FL-EXIT
                                  END-IF
                          END-PERFORM
                     END-IF
                     IF   VALOR NOT = 0
                     AND  FL-EXIT NOT = 1
                          IF   INCLUSAO
                               PERFORM 120-INCLUSAO  THRU 120-99-FIM
                          END-IF
                          IF   ALTERACAO
                               PERFORM 130-ALTERACAO THRU 130-99-FIM
                          END-IF
                          IF   EXCLUSAO
                               EXEC COBOLware Send
                                    Message MSG(14)
                               END-EXEC
                               PERFORM 105-CONFIRMA
                                  THRU 105-99-FIM
                               PERFORM 140-EXCLUSAO  THRU 140-99-FIM
                          END-IF
                     END-IF
                END-IF
           END-IF.

       100-99-FIM. EXIT.

       105-CONFIRMA.

           COPY CWEFAB.

       105-99-FIM. EXIT.

       110-CARREGA-DADOS.

           MOVE CBMVMS-SERIE              TO CBCOBA-SERIE
           MOVE CBMVMS-NUMERO             TO CBCOBA-NUMERO
           MOVE CBMVMS-DIA                TO DD-REF
           MOVE CBMVMS-COD-RED            TO CBPLCO-COD-RED
                                            COD-RED-CALL
           CALL "CB039PCW" USING            COD-RED-CALL
                                            COD-RED-DV-CALL
           MOVE CBMVMS-DOCTO              TO DOCTO
           MOVE CBMVMS-AAAAMMDD-DOCTO     TO CWTIME-DATE
           SET  CWTIME-REVERSED          TO TRUE
           SET  CWTIME-REVERSE           TO TRUE
           CALL "CWTIME"              USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL        TO DATA-HISTORICA
           MOVE CBMVMS-CENTRO-CUSTO       TO CENTRO-CUSTO
           MOVE CBMVMS-HISTORICO-PADRAO   TO HISTORICO
           MOVE CBMVMS-HISTORICO-VARIAVEL TO HISTORICO-V
           MOVE CBMVMS-VALOR              TO VALOR
                                            VALOR-A
           MOVE SPACES                   TO HISTORICOS-VARIAVEIS

           READ CBPLCO KEY IS CBPLCO-COD-RED.

       110-99-FIM. EXIT.

       115-INFORMA-SALDOS.

           IF   ESTORNO = 1
                GO TO 115-99-FIM
           END-IF

           MOVE CBPLCO-CONTA TO CB002PCW-CONTA
           MOVE "C"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW
           MOVE "E"          TO CB002PCW-FUNCAO
           CALL "CB002PCW"  USING PARAMETROS-CB002PCW

           OPEN INPUT CBCOMS
           PERFORM TEST AFTER UNTIL FS-CBCOSA < "09"
                   OPEN INPUT CBCOSA
                   IF   FS-CBCOSA = "30" OR "35"
                        OPEN OUTPUT CBCOSA
                        CLOSE CBCOSA
                        OPEN INPUT CBCOSA
                   END-IF
           END-PERFORM
           READ CBCOMS
           CLOSE CBCOMS
           PERFORM TEST AFTER
                   UNTIL FS-CBCOSA NOT = "9D"
                   MOVE MM-REF       TO CBCOSA-MM
                   MOVE AAAA-REF     TO CBCOSA-AAAA
                   MOVE CBPLCO-CONTA TO CBCOSA-CONTA
                   READ CBCOSA
           END-PERFORM
           IF   FS-CBCOSA < "10"
                IF   CBMVMS-TIPO = "D"
                     MOVE CBCOSA-SALDO-ATUAL TO SALDO-DB-A
                     MOVE 1                 TO DISPONIVEL-DB-A
                ELSE
                     MOVE CBCOSA-SALDO-ATUAL TO SALDO-CR-A
                     MOVE 1                 TO DISPONIVEL-CR-A
                END-IF
           ELSE
                MOVE 0                  TO SALDO-DB-A
                                           DISPONIVEL-DB-A
                                           SALDO-CR-A
                                           DISPONIVEL-CR-A
           END-IF
           MOVE CBCOMS-REG   TO CBCOSA-AAAAMM
           MOVE CBPLCO-CONTA TO CBCOSA-CONTA
           READ CBCOSA

           IF   CBMVMS-TIPO = "D"
                IF   COD-RED-DB NOT = 0
                     MOVE CBPLCO-DESCRICAO   TO DESCRICAO-DB
                     MOVE CB002PCW-CONTA-ED  TO CONTA-ED-DB
                     MOVE CBCOSA-SALDO-ATUAL TO SALDO-DB
                     DISPLAY C2
                     IF   DISPONIVEL-DB-A = 0
                          DISPLAY "       NÆo dispon¡vel"
                                  LINE 15 COLUMN 18
                     END-IF
                     IF   FS-CBCOSA > "09"
                     OR   FS-CBCOMS > "09"
                          DISPLAY "       NÆo dispon¡vel"
                                   LINE 16 COLUMN 18
                     END-IF
                ELSE
                     DISPLAY APAGA-C2
                END-IF
           ELSE
                IF   COD-RED-CR NOT = 0
                     MOVE CBPLCO-DESCRICAO   TO DESCRICAO-CR
                     MOVE CB002PCW-CONTA-ED  TO CONTA-ED-CR
                     MOVE CBCOSA-SALDO-ATUAL TO SALDO-CR
                     DISPLAY C3
                     IF   DISPONIVEL-CR-A = 0
                          DISPLAY "        NÆo dispon¡vel"
                                  LINE 15 COLUMN 57
                     END-IF
                     IF   FS-CBCOSA > "09"
                     OR   FS-CBCOMS > "09"
                          DISPLAY "        NÆo dispon¡vel"
                                   LINE 16 COLUMN 57
                     END-IF
                ELSE
                     DISPLAY APAGA-C3
                END-IF
           END-IF

           IF   MM-REF   = CBCOSA-MM
           AND  AAAA-REF = CBCOSA-AAAA
                DISPLAY APAGA-C1-C2-15.

           CLOSE CBCOSA.

       115-99-FIM. EXIT.

       120-INCLUSAO.

           IF   EFETIVAR
                PERFORM 152-SALVA-HIST THRU 152-99-FIM
                MOVE    ZERO        TO CBMVMS-LANCAMENTO
                MOVE    SPACE       TO CBMVMS-TIPO
                READ CBMVMS LOCK
                IF   FS-CBMVMS > "09"
                     STOP RUN
                END-IF
                ADD     1            TO CBMVMS-VALOR
                MOVE    CBMVMS-VALOR TO LANCAMENTO
                DISPLAY C0
                REWRITE CBMVMS-REG
                UNLOCK CBMVMS
                MOVE 0 TO VEZ-LANCAMENTO
                IF   COD-RED-DB NOT = 0
                     MOVE    "D"                    TO CBMVMS-TIPO
                     PERFORM 160-SALVA-DADOS             THRU 160-99-FIM
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     WRITE CBMVMS-REG
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
                IF   COD-RED-CR NOT = 0
                     MOVE    "C"         TO CBMVMS-TIPO
                     PERFORM 160-SALVA-DADOS  THRU 160-99-FIM
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     WRITE CBMVMS-REG
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
           END-IF.

       120-99-FIM. EXIT.

       130-ALTERACAO.

           IF   EFETIVAR
                PERFORM 152-SALVA-HIST THRU 152-99-FIM
                MOVE    0                TO VEZ-LANCAMENTO
                MOVE    1                TO ESTORNO
                IF   COD-RED-DB-A NOT = 0
                     PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                     MOVE COD-RED-DB-A              TO CBMVMS-COD-RED
                     MOVE VALOR-A                   TO CBMVMS-VALOR
                     MOVE    "C"                    TO CBMVMS-TIPO
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     MOVE    "D"                    TO CBMVMS-TIPO
                     READ CBMVMS LOCK
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                     DELETE CBMVMS RECORD
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
                IF   COD-RED-CR-A NOT = 0
                     PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                     MOVE COD-RED-CR-A              TO CBMVMS-COD-RED
                     MOVE VALOR-A                   TO CBMVMS-VALOR
                     MOVE    "D"                    TO CBMVMS-TIPO
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     MOVE    "C"                    TO CBMVMS-TIPO
                     READ CBMVMS LOCK
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                     DELETE CBMVMS RECORD
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
                MOVE 0 TO ESTORNO
                          VEZ-LANCAMENTO
                IF   COD-RED-DB NOT = 0
                     MOVE    "D"                    TO CBMVMS-TIPO
                     PERFORM 160-SALVA-DADOS             THRU 160-99-FIM
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     WRITE CBMVMS-REG
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
                IF   COD-RED-CR NOT = 0
                     MOVE    "C"         TO CBMVMS-TIPO
                     PERFORM 160-SALVA-DADOS  THRU 160-99-FIM
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     WRITE CBMVMS-REG
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
           END-IF.

       130-99-FIM. EXIT.

       140-EXCLUSAO.

           IF   EFETIVAR
                MOVE    1                TO ESTORNO
                MOVE    SPACES           TO HISTORICOS-VARIAVEIS
                PERFORM 152-SALVA-HIST THRU 152-99-FIM
                MOVE    0                TO VEZ-LANCAMENTO
                IF   COD-RED-DB NOT = 0
                     MOVE    "D"                    TO CBMVMS-TIPO
                     PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                     MOVE    "C"                    TO CBMVMS-TIPO
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     MOVE    "D"                    TO CBMVMS-TIPO
                     READ CBMVMS LOCK
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                     DELETE CBMVMS RECORD
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
                IF   COD-RED-CR NOT = 0
                     MOVE    "C"                    TO CBMVMS-TIPO
                     PERFORM 160-SALVA-DADOS      THRU 160-99-FIM
                     MOVE    "D"                    TO CBMVMS-TIPO
                     PERFORM 170-CONTROLA-SALDOS  THRU 170-99-FIM
                     MOVE    "C"                    TO CBMVMS-TIPO
                     READ CBMVMS LOCK
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                     DELETE CBMVMS RECORD
                     IF   FS-CBMVMS > "09"
                          STOP RUN
                     END-IF
                END-IF
                MOVE    0                TO ESTORNO
           END-IF.

       140-99-FIM. EXIT.

       150-ACCEPTS.

           MOVE 1      TO CAMPO
           MOVE SPACES TO MENSAGEM-ERRO
           MOVE 0      TO TECLA

           PERFORM UNTIL ESC OR (CAMPO > 8 AND MENSAGEM-ERRO = SPACES)
              IF  CAMPO = 9
                  MOVE 1 TO CAMPO
              END-IF
              EXEC COBOLware Send
                   Message MENSAGEM-ERRO
              END-EXEC
              DISPLAY RODAPE-INCLUSAO LINE 23 COLUMN 03
              MOVE SPACES TO MENSAGEM-ERRO
              IF   F5
              AND (CAMPO = 2 OR 3)
                   SET ENTER-KEY TO TRUE
                   IF   CAMPO = 2
                        MOVE 12 TO CWBOXF-LINE
                        MOVE 21 TO CWBOXF-COLUMN
                   END-IF
                   IF   CAMPO = 3
                        MOVE 12 TO CWBOXF-LINE
                        MOVE 34 TO CWBOXF-COLUMN
                   END-IF
                   PERFORM 153-PESQUISA-CONTA THRU 153-99-FIM
                   IF   COD-RED-F5 NOT = 0
                        CALL "CB039PCW" USING COD-RED-F5
                                              COD-RED-DV-CALL
                        MOVE 1 TO F5-ON
                        IF   CAMPO = 2
                             MOVE COD-RED-F5      TO COD-RED-DB
                             MOVE COD-RED-DV-CALL TO COD-RED-DB-DV
                        ELSE
                             MOVE COD-RED-F5      TO COD-RED-CR
                             MOVE COD-RED-DV-CALL TO COD-RED-CR-DV
                        END-IF
                   END-IF
              END-IF
              IF   NOT ESC
                   EVALUATE CAMPO
                   WHEN 1 ACCEPT C1
                          MOVE DD-REF          TO DATA-CRITICA (1: 2)
                          MOVE MM-REF          TO DATA-CRITICA (3: 2)
                          MOVE AAAA-REF (3: 2) TO DATA-CRITICA (5: 2)
                          CALL "GRVDAT" USING DATA-CRITICA
                          IF   DATA-CRITICA =  ZEROS
                               MOVE MSG (5) TO MENSAGEM-ERRO
                          END-IF
                   WHEN 2 ACCEPT C2
                          ACCEPT TECLA FROM ESCAPE KEY
                          IF   F5 OR ESC
                               EXIT PERFORM CYCLE
                          END-IF
                          INSPECT COD-RED-DB-DV
                          CONVERTING MINUSCULAS TO MAIUSCULAS
                          IF   COD-RED-DB = 0
                          AND  (NOT CURSOR-UP)
                               MOVE 0   TO CB002PCW-CONTA
                               MOVE " " TO CB002PCW-DV
                               MOVE "A" TO CB002PCW-FUNCAO
                               MOVE 13  TO CB002PCW-LINHA
                               MOVE 03  TO CB002PCW-COLUNA
                               CALL "CB002PCW"
                                     USING PARAMETROS-CB002PCW
                               MOVE CB002PCW-RETORNO TO TECLA
                               IF   F5 OR ESC
                                    EXIT PERFORM CYCLE
                               END-IF
                               IF   CB002PCW-CONTA NOT = 0
                                    MOVE CB002PCW-CONTA
                                      TO CBPLCO-CONTA
                                    READ CBPLCO IGNORE LOCK
                                                KEY IS CBPLCO-CHAVE
                                    IF FS-CBPLCO > "09"
                                       MOVE MSG (8) TO MENSAGEM-ERRO
                                                       CONTA-ED-DB
                                                       DESCRICAO-DB
                                       EXIT PERFORM CYCLE
                                    ELSE
                                       MOVE CBPLCO-COD-RED
                                         TO COD-RED-DB COD-RED-CALL
                                        CALL "CB039PCW"
                                              USING COD-RED-CALL
                                                    COD-RED-DV-CALL
                                       MOVE COD-RED-DV-CALL
                                         TO COD-RED-DB-DV
                                       DISPLAY C2
                                    END-IF
                               END-IF
                        END-IF
                        IF   COD-RED-DB NOT = 0
                             MOVE COD-RED-DB TO CBPLCO-COD-RED
                             READ CBPLCO IGNORE LOCK
                                         KEY IS CBPLCO-COD-RED
                             IF   FS-CBPLCO > "09"
                                  MOVE MSG (8) TO MENSAGEM-ERRO
                                                  CONTA-ED-DB
                                                  DESCRICAO-DB
                             ELSE
                                  MOVE CBPLCO-CONTA TO CB002PCW-CONTA
                                  MOVE "C"          TO CB002PCW-FUNCAO
                                  CALL "CB002PCW"
                                  USING PARAMETROS-CB002PCW
                                  MOVE "E"          TO CB002PCW-FUNCAO
                                  CALL "CB002PCW"
                                  USING PARAMETROS-CB002PCW
                                  MOVE CB002PCW-CONTA-ED TO CONTA-ED-DB
                                  MOVE CBPLCO-DESCRICAO TO DESCRICAO-DB
                                  MOVE CB002PCW-RETORNO TO TECLA
                                  MOVE CBPLCO-COD-RED   TO COD-RED-CALL
                                  CALL "CB039PCW" USING COD-RED-CALL
                                                        COD-RED-DV-CALL
                                  IF   F5-ON = 1
                                       MOVE COD-RED-DV-CALL
                                         TO COD-RED-DB-DV
                                       DISPLAY C2-R
                                  END-IF
                                  IF   COD-RED-DV-CALL
                                       NOT = COD-RED-DB-DV
                                       MOVE MSG (16) TO MENSAGEM-ERRO
                                                        CONTA-ED-DB
                                                        DESCRICAO-DB
                                  END-IF
                             END-IF
                        ELSE
                             MOVE SPACES TO CONTA-ED-DB DESCRICAO-DB
                        END-IF
                        MOVE "D" TO CBMVMS-TIPO
                        PERFORM 115-INFORMA-SALDOS THRU 115-99-FIM
                   WHEN 3 ACCEPT C3
                          ACCEPT TECLA FROM ESCAPE KEY
                          IF   F5 OR ESC
                               EXIT PERFORM CYCLE
                          END-IF
                          INSPECT COD-RED-CR-DV
                          CONVERTING MINUSCULAS TO MAIUSCULAS
                          IF   COD-RED-CR = 0
                          AND  (NOT CURSOR-UP)
                               MOVE 0   TO CB002PCW-CONTA
                               MOVE " " TO CB002PCW-DV
                               MOVE "A" TO CB002PCW-FUNCAO
                               MOVE 13  TO CB002PCW-LINHA
                               MOVE 42  TO CB002PCW-COLUNA
                               CALL "CB002PCW"
                                     USING PARAMETROS-CB002PCW
                               MOVE CB002PCW-RETORNO TO TECLA
                               IF   F5 OR ESC
                                    EXIT PERFORM CYCLE
                               END-IF
                               IF   CB002PCW-CONTA NOT = 0
                                    MOVE CB002PCW-CONTA
                                      TO CBPLCO-CONTA
                                    READ CBPLCO IGNORE LOCK
                                                KEY IS CBPLCO-CHAVE
                                    IF FS-CBPLCO > "09"
                                       MOVE MSG (8) TO MENSAGEM-ERRO
                                                       CONTA-ED-CR
                                                       DESCRICAO-CR
                                       EXIT PERFORM CYCLE
                                    ELSE
                                       MOVE CBPLCO-COD-RED
                                         TO COD-RED-CR COD-RED-CALL
                                        CALL "CB039PCW"
                                              USING COD-RED-CALL
                                                    COD-RED-DV-CALL
                                       MOVE COD-RED-DV-CALL
                                         TO COD-RED-CR-DV
                                       DISPLAY C3
                                    END-IF
                               END-IF
                        END-IF
                        IF   COD-RED-CR NOT = 0
                             MOVE COD-RED-CR TO CBPLCO-COD-RED
                             READ CBPLCO IGNORE LOCK
                                         KEY IS CBPLCO-COD-RED
                             IF   FS-CBPLCO > "09"
                                  MOVE MSG (8) TO MENSAGEM-ERRO
                                                  CONTA-ED-CR
                                                  DESCRICAO-CR
                             ELSE
                                  MOVE CBPLCO-CONTA TO CB002PCW-CONTA
                                  MOVE "C"          TO CB002PCW-FUNCAO
                                  CALL "CB002PCW"
                                  USING PARAMETROS-CB002PCW
                                  MOVE "E"          TO CB002PCW-FUNCAO
                                  CALL "CB002PCW"
                                  USING PARAMETROS-CB002PCW
                                  MOVE CB002PCW-CONTA-ED TO CONTA-ED-CR
                                  MOVE CBPLCO-DESCRICAO TO DESCRICAO-CR
                                  MOVE CB002PCW-RETORNO TO TECLA
                                  MOVE CBPLCO-COD-RED   TO COD-RED-CALL
                                  CALL "CB039PCW" USING COD-RED-CALL
                                                        COD-RED-DV-CALL
                                  IF   F5-ON = 1
                                       MOVE COD-RED-DV-CALL
                                         TO COD-RED-CR-DV
                                       DISPLAY C3-R
                                  END-IF
                                  IF   COD-RED-DV-CALL
                                       NOT = COD-RED-CR-DV
                                       MOVE MSG (16) TO MENSAGEM-ERRO
                                                        CONTA-ED-CR
                                                        DESCRICAO-CR
                                  END-IF
                             END-IF
                        ELSE
                             MOVE SPACES TO CONTA-ED-CR DESCRICAO-CR
                        END-IF
                        MOVE "C" TO CBMVMS-TIPO
                        PERFORM 115-INFORMA-SALDOS THRU 115-99-FIM
                   WHEN 4 ACCEPT C4
                          ACCEPT TECLA FROM ESCAPE KEY
                   WHEN 5 MOVE TECLA TO SALVA-TECLA2
                          ACCEPT C5
                          IF   DATA-HISTORICA NOT = 0
                               MOVE DATA-HISTORICA  TO CWTIME-DATE
                               SET  CWTIME-NORMAL   TO TRUE
                               SET  CWTIME-VALIDATE TO TRUE
                               CALL "CWTIME" USING PARAMETROS-CWTIME
                               IF   CWTIME-DATE-FINAL  = 0
                                    MOVE MSG (9) TO MENSAGEM-ERRO
                               END-IF
                          END-IF
                   WHEN 6 IF CC-FLAG-TXT = "C/C"
                          DISPLAY RODAPE-C6 LINE 23 COLUMN 03
                          ACCEPT C6
                          DISPLAY RODAPE-INCLUSAO LINE 23 COLUMN 03
                          ACCEPT TECLA FROM ESCAPE KEY
                          IF   F5
                               MOVE SPACES TO CWBOXF-OPTION
                               IF   CENTRO-CUSTO NOT = 0
                                    MOVE CENTRO-CUSTO TO CBCACC-CODIGO
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
                                      TO CENTRO-CUSTO
                                   SET ENTER-KEY TO TRUE
                               END-IF
                               DISPLAY C6
                          END-IF
                          IF   CENTRO-CUSTO NOT = 0
                               MOVE CENTRO-CUSTO TO CBCACC-CODIGO
                               READ CBCACC IGNORE LOCK
                               IF   FS-CBCACC > "09"
                                    MOVE MSG (15) TO MENSAGEM-ERRO
                               END-IF
                          END-IF
                          ELSE
                               MOVE SALVA-TECLA2 TO TECLA
                          END-IF
                   WHEN 7 MOVE TECLA TO SALVA-TECLA2
                          DISPLAY RODAPE-C7 LINE 23 COLUMN 03
                          PERFORM TEST AFTER UNTIL (NOT F1)
                                               AND (NOT F3)
                             ACCEPT C7
                             ACCEPT TECLA FROM ESCAPE KEY
                             MOVE TECLA TO TECLA-C7
                             IF   F1
                                  PERFORM 180-HELP THRU 180-99-FIM
                             END-IF
                             IF   F3
                                  DISPLAY CBCOBA-CHAVE UPON COMMAND-LINE
                                  CALL "CB035PCW"
                                  CANCEL "CB035PCW"
                             END-IF
                             IF   F5
                                  MOVE SPACES TO CWBOXF-OPTION
                                  IF   CENTRO-CUSTO NOT = 0
                                       MOVE HISTORICO TO CBCAHI-CODIGO
                                       READ CBCAHI IGNORE LOCK
                                       IF   FS-CBCAHI < "10"
                                            MOVE CBCAHI-DESCRICAO
                                              TO CWBOXF-OPTION
                                       END-IF
                                  END-IF
                                  MOVE "CB049PCW"   TO CWBOXF-PROGRAM
                                  MOVE "Hist¢ricos" TO CWBOXF-TITLE
                                  MOVE  5 TO CWBOXF-STRING-1-LENGTH
                                  MOVE 30 TO CWBOXF-STRING-2-LENGTH
                                  MOVE  2 TO CWBOXF-ORDER
                                  MOVE 10 TO CWBOXF-VERTICAL-LENGTH
                                  COMPUTE CWBOXF-HORIZONTAL-LENGTH = 6
                                        + CWBOXF-STRING-1-LENGTH
                                        + CWBOXF-STRING-2-LENGTH
                                  MOVE 10 TO CWBOXF-LINE
                                  MOVE 26 TO CWBOXF-COLUMN
                                  CALL "CWBOXF" USING PARAMETROS-CWBOXF
                                  IF   CWBOXF-OPTION NOT = SPACES
                                       MOVE CWBOXF-OPTION (1: 4)
                                         TO HISTORICO
                                       SET ENTER-KEY TO TRUE
                                  END-IF
                             END-IF
                          END-PERFORM
                          DISPLAY RODAPE-INCLUSAO LINE 23 COLUMN 03

                          IF   HISTORICO NOT = 0
                               MOVE HISTORICO TO CBCAHI-CODIGO
                               READ CBCAHI IGNORE LOCK
                               IF   FS-CBCAHI > "09"
                                    MOVE MSG (10) TO MENSAGEM-ERRO
                                                     DESCRICAO-PADRAO
                               ELSE
                                    MOVE CBCAHI-DESCRICAO
                                      TO DESCRICAO-PADRAO
                               END-IF
                          ELSE
                               MOVE SPACES TO DESCRICAO-PADRAO
                          END-IF
                          DISPLAY C7
                          IF   MENSAGEM-ERRO = SPACES
                          AND (NOT CURSOR-UP)
                               SET CWBOXW-OPEN TO TRUE
                               MOVE 8        TO CWBOXW-LINE
                               MOVE 7        TO CWBOXW-COLUMN
                               MOVE 12       TO CWBOXW-VERTICAL-LENGTH
                               MOVE 65       TO CWBOXW-HORIZONTAL-LENGTH
                               MOVE CYAN-RED-LOW
                                             TO CWBOXW-COLOR-FRAME
                                                CWBOXW-COLOR-BORDER
                               CALL "CWBOXW" USING PARAMETROS-CWBOXW
                               MOVE 1 TO DXX
                               DISPLAY CB008PB
                               MOVE TECLA TO SALVA-TECLA
                               PERFORM TEST AFTER UNTIL F2
                                                     OR ESC
                                  EVALUATE DXX
                                      WHEN 01 ACCEPT D01
                                      WHEN 02 ACCEPT D02
                                      WHEN 03 ACCEPT D03
                                      WHEN 04 ACCEPT D04
                                      WHEN 05 ACCEPT D05
                                      WHEN 06 ACCEPT D06
                                      WHEN 07 ACCEPT D07
                                      WHEN 08 ACCEPT D08
                                      WHEN 09 ACCEPT D09
                                      WHEN 10 ACCEPT D10
                                      WHEN 11 ACCEPT D11
                                      WHEN 12 ACCEPT D12
                                      WHEN 13 ACCEPT D13
                                      WHEN 14 ACCEPT D14
                                      WHEN 15 ACCEPT D15
                                      WHEN 16 ACCEPT D16
                                      WHEN 17 ACCEPT D17
                                      WHEN 18 ACCEPT D18
                                      WHEN 19 ACCEPT D19
                                      WHEN 20 ACCEPT D20
                                      WHEN 21 ACCEPT D21
                                      WHEN 22 ACCEPT D22
                                      WHEN 23 ACCEPT D23
                                      WHEN 24 ACCEPT D24
                                  END-EVALUATE
      *                           ACCEPT  CB008PB
                                  ACCEPT TECLA FROM ESCAPE KEY
                                  IF   ESC
                                       SET  F2    TO TRUE
                                       MOVE TECLA TO SALVA-TECLA
                                  END-IF
                                  EVALUATE TRUE
                                  WHEN F1
                                       MOVE 12 TO CAMPO
                                       PERFORM 180-HELP THRU 180-99-FIM
                                       MOVE 7  TO CAMPO
                                  WHEN F3
                                       DISPLAY CBCOBA-CHAVE
                                          UPON COMMAND-LINE
                                       CALL "CB035PCW"
                                       CANCEL "CB035PCW"
                                  WHEN DXX > 1
                                  AND  DESCR (DXX) = SPACES
                                  AND  DESCR (DXX - 1) = SPACES
                                       SET F2 TO TRUE
                                  WHEN ENTER-KEY
                                       ADD 1 TO DXX
                                       IF   DXX > 24
                                            MOVE TECLA TO SALVA-TECLA
                                            SET F2 TO TRUE
                                       END-IF
                                  WHEN CURSOR-UP
                                       SUBTRACT 1 FROM DXX
                                       IF   DXX = 0
                                            MOVE TECLA TO SALVA-TECLA
                                            SET F2 TO TRUE
                                       END-IF
                                  END-EVALUATE
                               END-PERFORM
                               IF  NOT F2
                                   MOVE SALVA-TECLA TO TECLA
                                   ACCEPT TECLA FROM ESCAPE KEY
                               END-IF
                               SET CWBOXW-CLOSE TO TRUE
                               CALL "CWBOXW" USING PARAMETROS-CWBOXW
                               IF   ESC
                                    NEXT SENTENCE
                               ELSE
                                    IF   NOT CURSOR-UP
                                         SET ENTER-KEY TO TRUE
                                    END-IF
                               END-IF
                          END-IF
                          IF   HISTORICO = 0
                          AND  HISTORICOS-VARIAVEIS = SPACES
                               MOVE MSG (12) TO MENSAGEM-ERRO
                          END-IF
                          MOVE TECLA-C7 TO TECLA
                          IF  CURSOR-UP
                              MOVE SPACES TO MENSAGEM-ERRO
                          END-IF
                   WHEN 8 ACCEPT C8
                          IF   VALOR = 0
                               MOVE MSG (11) TO MENSAGEM-ERRO
                          END-IF
                   END-EVALUATE
                   IF   F5-ON = 1
                        SET ENTER-KEY TO TRUE
                        MOVE 0 TO F5-ON
                   ELSE
                        IF   CAMPO NOT = 7
                             ACCEPT TECLA FROM ESCAPE KEY
                        END-IF
                   END-IF
                   IF   F4
                   AND (CAMPO = 2 OR 3)
                        PERFORM 154-INCLUI-CONTA THRU 154-99-FIM
                        IF   COD-RED-F5 NOT = 0
                             MOVE 1 TO F5-ON
                             IF   CAMPO = 2
                                  MOVE COD-RED-F5 TO COD-RED-DB
                             ELSE
                                  MOVE COD-RED-F5 TO COD-RED-CR
                             END-IF
                        END-IF
                   END-IF
              END-IF
              IF   ESC
                   NEXT SENTENCE
              END-IF
              IF   F1 OR F3
                   IF   F1
                        PERFORM 180-HELP THRU 180-99-FIM
                   ELSE
                        DISPLAY CBCOBA-CHAVE UPON COMMAND-LINE
                        CALL "CB035PCW"
                        CANCEL "CB035PCW"
                   END-IF
                   MOVE SPACES TO MENSAGEM-ERRO
              ELSE
                   IF   MENSAGEM-ERRO = SPACES
                   OR   CURSOR-UP
                        IF   ENTER-KEY
                             IF   CAMPO < 9
                                  ADD 1 TO CAMPO
                             ELSE
                                  SET F2 TO TRUE
                             END-IF
                        ELSE
                             IF   CURSOR-UP
                                  IF   CAMPO > 1
                                       SUBTRACT 1 FROM CAMPO
                                  ELSE
                                       MOVE 8 TO CAMPO
                                  END-IF
                             END-IF
                        END-IF
                   END-IF
              END-IF
              IF   COD-RED-DB = 0
              AND  COD-RED-CR = 0
              AND  MENSAGEM-ERRO = SPACE
              AND  CAMPO > 3
                   MOVE MSG (6) TO MENSAGEM-ERRO
                   MOVE 2       TO CAMPO
              END-IF
              IF   COD-RED-DB = COD-RED-CR
              AND  MENSAGEM-ERRO = SPACE
              AND  CAMPO > 3
                   MOVE MSG (13) TO MENSAGEM-ERRO
                   MOVE 2        TO CAMPO
              END-IF
           END-PERFORM.

       150-99-FIM. EXIT.

       151-CARREGA-HIST.

           MOVE "00"   TO FS-CBHIVA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                                            OR FS-CBHIVA > "09"
                   COMPUTE CBHIVA-TIPO = HISTORICO-V / 100000
                   MOVE HISTORICO-V   TO CBHIVA-CODIGO
                   MOVE I             TO CBHIVA-VARIAVEL
                   READ CBHIVA
                   IF   FS-CBHIVA < "09"
                        MOVE CBHIVA-DESCRICAO TO DESCR (I)
                   END-IF
           END-PERFORM.

       151-99-FIM. EXIT.

       152-SALVA-HIST.

           IF   HISTORICO-V = 0
                IF   HISTORICOS-VARIAVEIS = SPACES
                     GO TO 152-99-FIM
                ELSE
                     OPEN INPUT CBCOHI
                     IF   FS-CBCOHI = "30" OR "35"
                          CLOSE CBCOHI
                          OPEN OUTPUT CBCOHI
                          MOVE 200000 TO CBCOHI-ULTIMO
                          WRITE CBCOHI-REG
                          CLOSE CBCOHI
                          GO TO 152-SALVA-HIST
                     ELSE
                          CLOSE CBCOHI
                          OPEN I-O CBCOHI
                     END-IF
                     READ CBCOHI
                     ADD  1             TO CBCOHI-ULTIMO
                     MOVE CBCOHI-ULTIMO TO HISTORICO-V
                     REWRITE CBCOHI-REG
                     CLOSE CBCOHI
               END-IF
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                   COMPUTE CBHIVA-TIPO = HISTORICO-V / 100000
                   MOVE HISTORICO-V   TO CBHIVA-CODIGO
                   MOVE I             TO CBHIVA-VARIAVEL
                   READ CBHIVA
                   IF   FS-CBHIVA < "10"
                        DELETE CBHIVA RECORD
                   END-IF
           END-PERFORM.

           MOVE 0 TO Y

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
                   IF   DESCR (I) NOT = SPACES
                        COMPUTE CBHIVA-TIPO = HISTORICO-V / 100000
                        MOVE HISTORICO-V   TO CBHIVA-CODIGO
                        ADD  1             TO Y
                        MOVE Y             TO CBHIVA-VARIAVEL
                        MOVE DESCR (I)     TO CBHIVA-DESCRICAO
                        WRITE CBHIVA-REG
                   END-IF
           END-PERFORM.

       152-99-FIM. EXIT.

       153-PESQUISA-CONTA.

           MOVE "CB044PCW" TO CWBOXF-PROGRAM
           MOVE "Cod.Red Descri‡Æo" TO CWBOXF-TITLE
           MOVE  8 TO CWBOXF-STRING-1-LENGTH
           MOVE 30 TO CWBOXF-STRING-2-LENGTH
           MOVE  2 TO CWBOXF-ORDER
           MOVE 10 TO CWBOXF-VERTICAL-LENGTH
           COMPUTE CWBOXF-HORIZONTAL-LENGTH = 6
                 + CWBOXF-STRING-1-LENGTH
                 + CWBOXF-STRING-2-LENGTH

           SET CWBOXF-SHOW TO TRUE
           MOVE SPACES TO CWBOXF-OPTION
           CALL "CWBOXF" USING PARAMETROS-CWBOXF
           CANCEL "CWBOXF"
           IF   CWBOXF-OPTION = SPACES
                MOVE ZERO TO COD-RED-F5
           ELSE
                MOVE CWBOXF-OPTION (1: 5) TO COD-RED-F5
           END-IF.

       153-99-FIM. EXIT.

       154-INCLUI-CONTA.

           CLOSE CBPLCO
           SET CWBOXW-OPEN TO TRUE
           MOVE 07          TO CWBOXW-LINE
           MOVE 10          TO CWBOXW-COLUMN
           MOVE 01          TO CWBOXW-VERTICAL-LENGTH
           MOVE 38          TO CWBOXW-HORIZONTAL-LENGTH
           CALL "CWBOXW" USING PARAMETROS-CWBOXW

           MOVE "77"        TO CWDCNP-RETORNO
           MOVE ALL "9"     TO CWDCNP-CNPJ
           CALL "CWDCNP" USING PARAMETROS-CWDCNP

           DISPLAY "CB0003 - InclusÆo de conta anal¡tica"
                   LINE 08 COLUMN 12
           CANCEL "CB002PCW"
           CALL "CB003PCW"
           MOVE "00"        TO CWDCNP-RETORNO
           CALL "CWDCNP" USING PARAMETROS-CWDCNP
           IF   CWDCNP-RETORNO = "78"
                MOVE CWDCNP-CNPJ TO COD-RED-F5
           END-IF
           CANCEL "CB003PCW"
           MOVE "CLOSE"     TO CWBOXW-FUNCTION
           CALL "CWBOXW" USING PARAMETROS-CWBOXW
           OPEN I-O CBPLCO.

       154-99-FIM. EXIT.

       160-SALVA-DADOS.

           IF   CBMVMS-TIPO = "D"
                MOVE COD-RED-DB TO CBMVMS-COD-RED
           ELSE
                MOVE COD-RED-CR TO CBMVMS-COD-RED
           END-IF

           MOVE LANCAMENTO        TO CBMVMS-LANCAMENTO
           MOVE CBCOBA-SERIE      TO CBMVMS-SERIE
           MOVE CBCOBA-NUMERO     TO CBMVMS-NUMERO
           MOVE DD-REF            TO CBMVMS-DIA
           MOVE DOCTO             TO CBMVMS-DOCTO
           MOVE DATA-HISTORICA    TO CWTIME-DATE
           SET  CWTIME-NORMAL     TO TRUE
           SET  CWTIME-REVERSE    TO TRUE
           CALL "CWTIME"       USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL TO CBMVMS-AAAAMMDD-DOCTO
           MOVE CENTRO-CUSTO      TO CBMVMS-CENTRO-CUSTO
           MOVE HISTORICO         TO CBMVMS-HISTORICO-PADRAO
           MOVE HISTORICO-V       TO CBMVMS-HISTORICO-VARIAVEL
           MOVE VALOR             TO CBMVMS-VALOR.

       160-99-FIM. EXIT.

       170-CONTROLA-SALDOS.

           MOVE CBMVMS-COD-RED TO CBPLCO-COD-RED
           READ CBPLCO LOCK KEY IS CBPLCO-COD-RED

           IF   FS-CBPLCO > "09"
                STOP RUN
           END-IF

           IF   CBPLCO-VIRGEM = "S"
                MOVE "N" TO CBPLCO-VIRGEM
                REWRITE CBPLCO-REG
                GO TO 170-CONTROLA-SALDOS
           END-IF

           UNLOCK CBPLCO
           OPEN INPUT CBCOMS
           READ CBCOMS
           PERFORM TEST AFTER UNTIL FS-CBCOSA < "09"
                   OPEN I-O CBCOSA
           END-PERFORM
           COPY CBCOSACW.

           CLOSE CBCOMS CBCOSA

           READ CBCOBA LOCK

           IF   FS-CBCOBA > "09"
                STOP RUN
           END-IF

           ADD  1 TO VEZ-LANCAMENTO

           IF   VEZ-LANCAMENTO = 1
                IF   INCLUSAO
                     ADD 1 TO CBCOBA-LC-EFETIVOS
                ELSE
                IF   EXCLUSAO
                     SUBTRACT 1 FROM CBCOBA-LC-EFETIVOS
                END-IF
           END-IF

           IF   INCLUSAO
                IF   CBMVMS-TIPO = "D"
                     ADD CBMVMS-VALOR TO CBCOBA-DB-EFETIVOS
                ELSE
                     ADD CBMVMS-VALOR TO CBCOBA-CR-EFETIVOS
                END-IF
           ELSE
                IF   EXCLUSAO
                     IF   CBMVMS-TIPO = "D"
                          SUBTRACT CBMVMS-VALOR FROM CBCOBA-CR-EFETIVOS
                     ELSE
                          SUBTRACT CBMVMS-VALOR FROM CBCOBA-DB-EFETIVOS
                     END-IF
                ELSE
                     IF   CBMVMS-TIPO = "D"
                          IF   ESTORNO = 1
                               SUBTRACT VALOR-A FROM CBCOBA-CR-EFETIVOS
                          ELSE
                               ADD CBMVMS-VALOR TO CBCOBA-DB-EFETIVOS
                          END-IF
                     ELSE
                          IF   ESTORNO = 1
                               SUBTRACT VALOR-A FROM CBCOBA-DB-EFETIVOS
                          ELSE
                               ADD CBMVMS-VALOR TO CBCOBA-CR-EFETIVOS
                          END-IF
                     END-IF
                END-IF
           END-IF

           DISPLAY DADOS-BAC
           REWRITE CBCOBA-REG
           IF   FS-CBCOBA > "09"
                STOP RUN
           END-IF
           UNLOCK CBCOBA.

           PERFORM 115-INFORMA-SALDOS THRU 115-99-FIM.

       170-99-FIM. EXIT.

       180-HELP.

           EXEC COBOLware Help
                FILE    HELP-FILE
                LINE    HELP-LIN(CAMPO)
                COLUMN  HELP-COL(CAMPO)
                HEIGHT  HELP-VER(CAMPO)
                WIDTH   HELP-HOR(CAMPO)
           END-EXEC.

       180-99-FIM. EXIT.

       COPY CB021PCW.

       800-INICIAIS.

           OPEN I-O   CBCOBA
                INPUT CBCAHI
                      CBCACC
           IF   FS-CBCACC = "30" OR 35
                OPEN I-O CBCACC
           END-IF

           MOVE 9999 TO CBCACC-CODIGO
           START CBCACC KEY NOT GREATER CBCACC-CHAVE
           IF   FS-CBCACC < "10"
                READ CBCACC PREVIOUS RECORD IGNORE LOCK
                IF   FS-CBCACC < "10"
                     MOVE "C/C" TO CC-FLAG-TXT
                END-IF
           END-IF

           OPEN I-O CBHIVA
                    CBPLCO.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE CBCOBA CBCAHI CBHIVA CBPLCO CBCACC
           CANCEL "CB002PCW"
           CANCEL "CB039PCW".

       900-99-FIM. EXIT.

       END PROGRAM CB008PCW.
