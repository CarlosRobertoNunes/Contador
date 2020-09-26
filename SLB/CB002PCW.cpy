
      *******************************************************************
      *         Parametros para utilizar a subrotina CB002PCW           *
      *     Subtrai 1 grau,Accept,Display,Edicao e Calulo de dv         *
      *                para contas da ContaBilidade.                    *
      *******************************************************************


       01  PARAMETROS-CB002PCW.
           05 CB002PCW-FUNCAO            PIC  X(001) VALUE SPACE.
           05 CB002PCW-LINHA             PIC  9(002) VALUE ZERO.
           05 CB002PCW-COLUNA            PIC  9(002) VALUE ZERO.
           05 CB002PCW-CONTA      COMP-3 PIC  9(015) VALUE ZERO.
           05 CB002PCW-DV                PIC  X(001) VALUE ZERO.
           05 CB002PCW-FORCA-DV          PIC  X(001) VALUE SPACE.
           05 CB002PCW-RETORNO           PIC  9(002) VALUE ZERO.
           05 CB002PCW-CONTA-ED          PIC  X(026) VALUE ZERO.
           05 CB002PCW-GRAU              PIC  9(001) VALUE ZERO.
           05 CB002PCW-LANCAVEL          PIC  X(001) VALUE SPACE.

