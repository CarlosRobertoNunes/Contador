
      *******************************************************************
      *                Parametros de estrutura de contas                *
      *******************************************************************

       FD  CBPAPC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBPAPC.

       01  CBPAPC-REG.
           05 CBPAPC-ESTRUTURA-CONTA.
              10 CBPAPC-DG1            PIC  9(002).
              10 CBPAPC-DG2            PIC  9(002).
              10 CBPAPC-DG3            PIC  9(002).
              10 CBPAPC-DG4            PIC  9(002).
              10 CBPAPC-DG5            PIC  9(002).
              10 CBPAPC-DG6            PIC  9(002).
              10 CBPAPC-DG7            PIC  9(002).
              10 CBPAPC-DG8            PIC  9(002).
              10 CBPAPC-DG9            PIC  9(002).
           05 REDEFINES CBPAPC-ESTRUTURA-CONTA.
              10 CBPAPC-DG OCCURS 9    PIC  9(002).
           05 CBPAPC-FORMULA-DV.
              10 CBPAPC-MUNTIPLICADORES.
                 15 CBPAPC-M01         PIC  9(002).
                 15 CBPAPC-M02         PIC  9(002).
                 15 CBPAPC-M03         PIC  9(002).
                 15 CBPAPC-M04         PIC  9(002).
                 15 CBPAPC-M05         PIC  9(002).
                 15 CBPAPC-M06         PIC  9(002).
                 15 CBPAPC-M07         PIC  9(002).
                 15 CBPAPC-M08         PIC  9(002).
                 15 CBPAPC-M09         PIC  9(002).
                 15 CBPAPC-M10         PIC  9(002).
                 15 CBPAPC-M11         PIC  9(002).
                 15 CBPAPC-M12         PIC  9(002).
                 15 CBPAPC-M13         PIC  9(002).
                 15 CBPAPC-M14         PIC  9(002).
                 15 CBPAPC-M15         PIC  9(002).
              10 REDEFINES CBPAPC-MUNTIPLICADORES.
                 15 CBPAPC-M OCCURS 15 PIC  9(002).
              10 CBPAPC-MODULO-1       PIC  9(002).
              10 CBPAPC-MODULO-2       PIC  9(002).
              10 CBPAPC-OPERADOR       PIC  X(001).
                 88 CBPAPC-OPERADOR-OK VALUE ">" "<" "=".
              10 CBPAPC-MODULO-3       PIC  9(002).
              10 CBPAPC-DV             PIC  X(001).

