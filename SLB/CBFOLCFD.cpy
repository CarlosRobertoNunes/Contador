
      ******************************************************************
      *        Cadastro de formatos de lancamentos                     *
      ******************************************************************

       FD  CBFOLC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBFOLC.

       01  CBFOLC-REG.
           05 CBFOLC-CHAVE.
              10 CBFOLC-FORMATO                PIC  X(008).
           05 CBFOLC-COMENTARIO                PIC  X(050).
           05 CBFOLC-CAMPOS.
              10 CBFOLC-CAMPO OCCURS 10.
                 15 CBFOLC-I                   PIC  9(003).
                 15 CBFOLC-F                   PIC  9(003).
           05 CBFOLC-INDICA-DEBITO             PIC  X(008).
           05 CBFOLC-INDICA-CREDITO            PIC  X(008).

