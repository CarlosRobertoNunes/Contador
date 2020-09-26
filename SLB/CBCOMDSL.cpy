
      *******************************************************************
      *                   Controle de moedas                            *
      *******************************************************************

           SELECT CBCOMD ASSIGN  TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  LOCK MODE     IS EXCLUSIVE
                  RECORD  KEY   IS CBCOMD-CHAVE
                  FILE STATUS   IS FS-CBCOMD.

