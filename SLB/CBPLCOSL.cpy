      ******************************************************************
      *                      Plano de Contas                           *
      ******************************************************************

           SELECT CBPLCO ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBPLCO-CHAVE
                  ALTERNATE RECORD KEY CBPLCO-COD-RED   WITH DUPLICATES
                  ALTERNATE RECORD KEY CBPLCO-DESCRICAO WITH DUPLICATES
                  ALTERNATE RECORD KEY CBPLCO-CLASSE-DESCRICAO
                                     = CBPLCO-CLASSE
                                       CBPLCO-DESCRICAO
                                       WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CBPLCO.

