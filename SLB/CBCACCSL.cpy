
      ******************************************************************
      *         Cadastro de centros de custos                          *
      ******************************************************************

           SELECT CBCACC ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBCACC-CHAVE
                  ALTERNATE RECORD KEY CBCACC-DESCRICAO WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CBCACC.

