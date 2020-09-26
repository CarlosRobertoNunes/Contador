
      ******************************************************************
      *        Cadastro de formatos de lancamentos                     *
      ******************************************************************

           SELECT CBFOLC ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBFOLC-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBFOLC.

