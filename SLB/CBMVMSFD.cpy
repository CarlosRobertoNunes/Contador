
      ******************************************************************
      *           Contabilidade Movimento contabil mensal              *
      ******************************************************************

       FD  CBMVMS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBMVMS.

       01  CBMVMS-REG.
           05 CBMVMS-CHAVE.
              10 CBMVMS-LANCAMENTO      COMP-3 PIC  9(007).
              10 CBMVMS-TIPO                   PIC  X(001).
           05 CBMVMS-BAC.
              10 CBMVMS-SERIE           COMP-3 PIC  9(004).
              10 CBMVMS-NUMERO          COMP-3 PIC  9(004).
           05 CBMVMS-DIA                       PIC  9(002).
           05 CBMVMS-COD-RED            COMP-3 PIC  9(005).
           05 CBMVMS-DOCTO              COMP-3 PIC  9(008).
           05 CBMVMS-AAAAMMDD-DOCTO     COMP-3 PIC  9(008).
           05 CBMVMS-CENTRO-CUSTO       COMP-3 PIC  9(004).
           05 CBMVMS-HISTORICO-PADRAO   COMP-3 PIC  9(004).
           05 CBMVMS-HISTORICO-VARIAVEL COMP-3 PIC  9(006).
           05 CBMVMS-VALOR              COMP-3 PIC  9(012)V99.

