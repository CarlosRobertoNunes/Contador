
      ******************************************************************
      *                    Controle de saldos                          *
      ******************************************************************

           SELECT CBCOSA ASSIGN  TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBCOSA-CHAVE
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CBCOSA.

