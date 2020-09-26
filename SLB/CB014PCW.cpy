
      *******************************************************************
      *         Parametros para utilizar a subrotina CB014PCW           *
      * Obter valor convertido,valor corrigido,cotacao e nome da moeda  *
      *  em funcao da referencia(AAMMdd) e do valor em reais            *
      *******************************************************************

       01  PARAMETROS-CB014PCW.
           05 CB014PCW-REFERENCIA-AAAA    PIC  9(004)      VALUE ZERO.
           05 CB014PCW-REFERENCIA-MM      PIC  9(002)      VALUE ZERO.
           05 CB014PCW-REFERENCIA-DD      PIC  9(002)      VALUE ZERO.
           05 CB014PCW-VALOR       COMP-3 PIC S9(012)V99   VALUE ZERO.
           05 CB014PCW-CONVERTIDO  COMP-3 PIC S9(012)V9(4) VALUE ZERO.
           05 CB014PCW-CORRIGIDO   COMP-3 PIC S9(012)V99   VALUE ZERO.
           05 CB014PCW-MOEDA       COMP-3 PIC S9(006)V9(4) VALUE ZERO.
           05 CB014PCW-MOEDA-NOME         PIC  X(014)      VALUE SPACES.

