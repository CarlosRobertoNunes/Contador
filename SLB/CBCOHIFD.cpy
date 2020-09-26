
      ******************************************************************
      *              Contador de historicos                            *
      ******************************************************************

       FD  CBCOHI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCOHI.

       01  CBCOHI-REG.
           05 CBCOHI-ULTIMO             PIC  9(006).

