      ******************************************************************
      *       Cadastro de formatos plano de contas                     *
      ******************************************************************

           SELECT CBFOPC ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBFOPC-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBFOPC.

