
      ******************************************************************
      *                Historicos variaveis                            *
      ******************************************************************

       FD  CBHIVA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBHIVA.

       01  CBHIVA-REG.
           05 CBHIVA-CHAVE.
              10 CBHIVA-TIPO            PIC  9(001).
              10 CBHIVA-CODIGO   COMP-3 PIC  9(005).
              10 CBHIVA-VARIAVEL        PIC  9(002).
           05 CBHIVA-DESCRICAO          PIC  X(030).

