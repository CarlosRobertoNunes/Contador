
      ******************************************************************
      *            Cadastro de historicos                              *
      ******************************************************************

           SELECT CBCAHI ASSIGN  TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBCAHI-CHAVE
                  ALTERNATE RECORD KEY IS CBCAHI-DESCRICAO
                                   WITH DUPLICATES
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS FS-CBCAHI.

