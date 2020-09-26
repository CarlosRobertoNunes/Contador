
      ******************************************************************
      *                Historicos variaveis                            *
      ******************************************************************

           SELECT CBHIVA ASSIGN  TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBHIVA-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBHIVA.

