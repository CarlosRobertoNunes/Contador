
      *******************************************************************
      *                Parametros de estrutura de contas                *
      *******************************************************************

           SELECT CBPAPC ASSIGN  TO DISK
                  RESERVE       NO ALTERNATE AREA
                  LOCK MODE     IS AUTOMATIC
                  ORGANIZATION  IS RELATIVE
                  ACCESS MODE   IS RANDOM
                  RELATIVE KEY  IS RK-CBPAPC
                  FILE STATUS   IS FS-CBPAPC.

