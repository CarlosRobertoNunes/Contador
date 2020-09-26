
      ******************************************************************
      *                    Plano de contas                             *
      ******************************************************************

       FD  CBPLCO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBPLCO.

       01  CBPLCO-REG.
           05 CBPLCO-CHAVE.
              10 CBPLCO-CONTA    COMP-3 PIC  9(015).
           05 CBPLCO-COD-RED     COMP-3 PIC  9(005).
           05 CBPLCO-DESCRICAO          PIC  X(030).
           05 CBPLCO-VIRGEM             PIC  X(001).
           05 CBPLCO-CLASSE      COMP-3 PIC  9(015).

