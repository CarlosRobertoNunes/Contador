
      ******************************************************************
      *                 Controle de meses                              *
      ******************************************************************

       FD  CBCOMS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCOMS.

       01  CBCOMS-REG.
           05 CBCOMS-AAAA               PIC  9(004).
           05 CBCOMS-MM                 PIC  X(002).

