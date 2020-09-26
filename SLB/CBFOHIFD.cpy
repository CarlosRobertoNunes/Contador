
      ******************************************************************
      *          Cadastro de formatos de historicos padrao             *
      ******************************************************************

       FD  CBFOHI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBFOHI.

       01  CBFOHI-REG.
           05 CBFOHI-CHAVE.
              10 CBFOHI-FORMATO                PIC  X(008).
           05 CBFOHI-COMENTARIO                PIC  X(050).
           05 CBFOHI-CAMPOS.
              10 CBFOHI-CAMPO OCCURS 2.
                 15 CBFOHI-I                   PIC  9(003).
                 15 CBFOHI-F                   PIC  9(003).

