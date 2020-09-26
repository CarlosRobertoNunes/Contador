
      *******************************************************************
      *         Parametros para utilizar a subrotina CB043PCW           *
      *  Executa inclusao ou exclusao de lancamento contabil visando    *
      *  facilitar a integracao de outros sistemas com a contabilidade  *
      *  se apos a chamada, CB043PCW-RETORNO nao for igual a zeros, isso*
      *  significa que o lancamento foi rejeitado, a razao de cada tipo *
      *  de rejeicao e' detalhada com a flag correspondente igual a 1   *
      *******************************************************************

       01 PARAMETROS-CB043PCW.
           05 CB043PCW-COMANDO              PIC  X(001).
              88 INCLUSAO                              VALUE "i" "I".
              88 EXCLUSAO                              VALUE "e" "E".
           05 CB043PCW-LANCAMENTO.
              10 CB043PCW-LANCAMENTO-NUMERO PIC  9(007).
              10 CB043PCW-LANCAMENTO-TIPO   PIC  X(001).
                 88 A-DEBITO                          VALUE "D".
                 88 A-CREDITO                         VALUE "C".
           05 CB043P-BAC.
              10 CB043PCW-SERIE-BAC         PIC  9(004).
              10 CB043PCW-NUMERO-BAC        PIC  9(004).
           05 CB043PCW-CENTRO-CUSTO         PIC  9(004).
           05 CB043PCW-CONTA                PIC  9(015).
           05 CB043PCW-COD-RED              PIC  9(005).
           05 CB043PCW-HISTORICO            PIC  9(004).
           05 CB043PCW-HISTORICOS-VARIAVEIS.
              10 CB043PCW-DESCRICAO OCCURS 24 PIC X(030).
           05 CB043PCW-DOCTO                PIC  9(008).
           05 CB043PCW-AAAAMMDD-DOCTO       PIC  9(008).
           05 CB043PCW-DD-REFERENCIA        PIC  9(002).
           05 CB043PCW-VALOR                PIC  9(012)V99.
           05 CB043PCW-RETORNO.
              10 CB043PCW-FLAG-TIPO         PIC  9(001).
              10 CB043PCW-FLAG-BAC          PIC  9(001).
              10 CB043PCW-FLAG-CENTRO-CUSTO PIC  9(001).
              10 CB043PCW-FLAG-CONTA        PIC  9(001).
              10 CB043PCW-FLAG-COD-RED      PIC  9(001).
              10 CB043PCW-FLAG-HISTORICO    PIC  9(001).
              10 CB043PCW-FLAG-DIA          PIC  9(001).
              10 CB043PCW-FLAG-AAMMDD-DOCTO PIC  9(001).
              10 CB043PCW-FLAG-REFERENCIA   PIC  9(001).
              10 CB043PCW-FLAG-VALOR        PIC  9(001).
              10 CB043PCW-FLAG-CBMVMS        PIC  9(001).
