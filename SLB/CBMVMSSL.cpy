
      ******************************************************************
      *           Contabilidade Movimento contabil mensal              *
      ******************************************************************

           SELECT CBMVMS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS CBMVMS-CHAVE
                  ALTERNATE RECORD KEY CBMVMS-BAC-CHAVE
                                =  CBMVMS-BAC
                                   CBMVMS-CHAVE
                  ALTERNATE RECORD KEY CBMVMS-DIA-CHAVE
                                =  CBMVMS-DIA
                                   CBMVMS-CHAVE
                  ALTERNATE RECORD KEY CBMVMS-COD-RED-CHAVE
                                =  CBMVMS-COD-RED
                                   CBMVMS-DIA            WITH DUPLICATES
                  ALTERNATE RECORD KEY CBMVMS-DOCTO-CHAVE
                                =  CBMVMS-DOCTO
                                   CBMVMS-CHAVE
                  ALTERNATE RECORD KEY CBMVMS-AAAAMMDD-DOCTO-CHAVE
                                =  CBMVMS-AAAAMMDD-DOCTO
                                   CBMVMS-CHAVE
                  ALTERNATE RECORD KEY CBMVMS-CENTRO-CUSTO-CHAVE
                                =  CBMVMS-CENTRO-CUSTO
                                   CBMVMS-DIA            WITH DUPLICATES
                  RESERVE       NO ALTERNATE AREA
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CBMVMS.

