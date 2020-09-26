
      ******************************************************************
      *            Controle de codigos reduzidos                       *
      ******************************************************************

       FD  CBCTCR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBCTCR.

       01  CBCTCR-REG.
           05 CBCTCR-CHAVE.
              10 CBCTCR-COD-RED  COMP-3 PIC  9(005).
           05 CBCTCR-ULTIMO      COMP-3 PIC  9(005).

