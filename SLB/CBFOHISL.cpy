
      ******************************************************************
      *          Cadastro de formatos de historicos padrao             *
      ******************************************************************

           SELECT CBFOHI ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBFOHI-CHAVE
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBFOHI.

