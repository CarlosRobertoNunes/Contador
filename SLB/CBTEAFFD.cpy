
      *******************************************************************
      *               Texto de abertura                                 *
      *******************************************************************

       FD  CBTEAF
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBTEAF.

       01  CBTEAF-REG.
           05        PIC X(001).
              88 CBTEAF-FIM-ABERTURA VALUE "*".
           05        PIC X(131).

