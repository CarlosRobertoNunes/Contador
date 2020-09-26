
      ******************************************************************
      *                 Cadastro de formatos saldos                    *
      ******************************************************************

       FD  CBFOSD
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBFOSD.

       01  CBFOSD-REG.
           05 CBFOSD-CHAVE.
              10 CBFOSD-FORMATO                PIC  X(008).
           05 CBFOSD-COMENTARIO                PIC  X(050).
           05 CBFOSD-CAMPOS.
              10 CBFOSD-CAMPO OCCURS 8.
                 15 CBFOSD-I                   PIC  9(003).
                 15 CBFOSD-F                   PIC  9(003).

