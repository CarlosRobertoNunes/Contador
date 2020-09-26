
      ******************************************************************
      *            Cadastro de historicos                              *
      ******************************************************************

       FD  CBCAHI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCAHI.

       01  CBCAHI-REG.
           05 CBCAHI-CHAVE.
              10 CBCAHI-CODIGO          PIC  9(004).
           05 CBCAHI-DESCRICAO          PIC  X(030).

