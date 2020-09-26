
      *******************************************************************
      *                   Controle de moedas                            *
      *******************************************************************

       FD  CBCOMD
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCOMD.

       01  CBCOMD-REG.
           05 CBCOMD-CHAVE.
              10 CBCOMD-MOEDA                 PIC  9(002).
           05 CBCOMD-NOME-MOEDA               PIC  X(014).
           05 CBCOMD-ULTIMA                   PIC  9(002).
           05 CBCOMD-TIPO                     PIC  X(001).

