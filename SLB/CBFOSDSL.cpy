
      ******************************************************************
      *                 Cadastro de formatos saldos                    *
      ******************************************************************


           SELECT CBFOSD ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBFOSD-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBFOSD.

