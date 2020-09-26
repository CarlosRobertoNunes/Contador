
      *******************************************************************
      *               Valores de determinada moeda                      *
      *******************************************************************

       FD  CBTAMD
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBTAMD.

       01  CBTAMD-REG.
           05 CBTAMD-CHAVE.
              10 CBTAMD-AAAA                  PIC  9(004).
              10 CBTAMD-MM                    PIC  9(002).
              10 CBTAMD-DD                    PIC  9(002).
           05 REDEFINES CBTAMD-CHAVE.
              10 CBTAMD-AA                    PIC  X(002).
              10 CBTAMD-DATA-ACCEPT           PIC  9(006).
           05 CBTAMD-MOEDA             COMP-3 PIC  9(006)V9(004).

