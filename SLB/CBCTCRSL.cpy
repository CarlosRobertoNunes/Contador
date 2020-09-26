
      ******************************************************************
      *            Controle de codigos reduzidos                       *
      ******************************************************************

           SELECT CBCTCR ASSIGN  TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBCTCR-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBCTCR.

