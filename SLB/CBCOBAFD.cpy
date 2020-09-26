
      ******************************************************************
      *        Controle de BACs (Boletins de Apropriacao Contabil)     *
      ******************************************************************

       FD  CBCOBA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCOBA.

       01  CBCOBA-REG.
           05 CBCOBA-CHAVE.
              10 CBCOBA-SERIE                 PIC  9(004).
              10 CBCOBA-NUMERO                PIC  9(004).
           05 CBCOBA-REFERENCIA.
              10 CBCOBA-AAAA                  PIC  9(004).
              10 CBCOBA-MM                    PIC  9(002).
           05 CBCOBA-LC-PREVISTOS      COMP-3 PIC  9(008).
           05 CBCOBA-CR-PREVISTOS      COMP-3 PIC  9(012)V99.
           05 CBCOBA-DB-PREVISTOS      COMP-3 PIC  9(012)V99.
           05 CBCOBA-LC-EFETIVOS       COMP-3 PIC  9(008).
           05 CBCOBA-CR-EFETIVOS       COMP-3 PIC  9(012)V99.
           05 CBCOBA-DB-EFETIVOS       COMP-3 PIC  9(012)V99.

