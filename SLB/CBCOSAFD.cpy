
      ******************************************************************
      *                    Controle de Saldos                          *
      ******************************************************************

       FD  CBCOSA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCOSA.

       01  CBCOSA-REG.
           05 CBCOSA-CHAVE.
              10 CBCOSA-CENTRO-CUSTO   COMP-3 PIC  9(004).
              10 CBCOSA-CONTA          COMP-3 PIC  9(015).
              10 CBCOSA-AAAAMM                PIC  9(006).
              10 FILLER REDEFINES CBCOSA-AAAAMM.
                 15 CBCOSA-AAAA               PIC  9(004).
                 15 CBCOSA-MM                 PIC  9(002).
           05 CBCOSA-SALDO-INICIAL     COMP-3 PIC S9(012)V99.
           05 CBCOSA-SALDO-ATUAL       COMP-3 PIC S9(012)V99.
           05 CBCOSA-A-DEBITO          COMP-3 PIC  9(012)V99.
           05 CBCOSA-A-CREDITO         COMP-3 PIC  9(012)V99.

