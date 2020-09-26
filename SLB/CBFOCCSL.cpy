
      ******************************************************************
      *          Cadastro de formatos de centros de custos             *
      ******************************************************************

           SELECT CBFOCC ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBFOCC-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBFOCC.

