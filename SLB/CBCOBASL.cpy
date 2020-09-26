
      ******************************************************************
      *        Controle de BACs (Boletins de Apropriacao Contabil)     *
      ******************************************************************

           SELECT CBCOBA ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBCOBA-CHAVE
                  ALTERNATE RECORD KEY CBCOBA-REFERENCIA WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  RESERVE       NO ALTERNATE AREA
                  FILE STATUS   IS FS-CBCOBA.

