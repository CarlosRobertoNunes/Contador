
      ******************************************************************
      *         Cadastro de centros de custos                          *
      ******************************************************************

       FD  CBCACC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCACC.

       01  CBCACC-REG.
           05 CBCACC-CHAVE.
              10 CBCACC-CODIGO          PIC  9(004).
           05 CBCACC-DESCRICAO          PIC  X(030).

