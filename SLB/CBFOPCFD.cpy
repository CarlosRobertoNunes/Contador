
      ******************************************************************
      *       Cadastro de formatos plano de contas                     *
      ******************************************************************

       FD  CBFOPC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBFOPC.

       01  CBFOPC-REG.
           05 CBFOPC-CHAVE.
              10 CBFOPC-FORMATO                PIC  X(008).
           05 CBFOPC-COMENTARIO                PIC  X(050).
           05 CBFOPC-CAMPOS.
              10 CBFOPC-CAMPO OCCURS 3.
                 15 CBFOPC-I                   PIC  9(003).
                 15 CBFOPC-F                   PIC  9(003).

