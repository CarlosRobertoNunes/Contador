
      ******************************************************************
      *                 ContaBilidade controle GEracao INcompleta      *
      * CBGEIN-ANTERIOR -> ANO E MES QUE A CONTABILIDADE ESTA PARADA   *
      * CBGEIN-ATUAL    -> ANO E MES A SER APAGADO                     *
      ******************************************************************

       FD  CBGEIN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-CBGEIN.

       01  CBGEIN-REG.
           05 CBGEIN-ANTERIOR           PIC  X(006).
           05 CBGEIN-ATUAL              PIC  X(006).

