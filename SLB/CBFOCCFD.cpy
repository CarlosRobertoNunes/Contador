
      ******************************************************************
      *          Cadastro de formatos de centros de custos             *
      ******************************************************************

       FD  CBFOCC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBFOCC.

       01  CBFOCC-REG.
           05 CBFOCC-CHAVE.
              10 CBFOCC-FORMATO                PIC  X(008).
           05 CBFOCC-COMENTARIO                PIC  X(050).
           05 CBFOCC-CAMPOS.
              10 CBFOCC-CAMPO OCCURS 2.
                 15 CBFOCC-I                   PIC  9(003).
                 15 CBFOCC-F                   PIC  9(003).

