
      ******************************************************************
      *                 Controle de meses                              *
      ******************************************************************

           SELECT CBCOMS ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  ACCESS MODE   IS RANDOM
                  RELATIVE KEY  IS RK-CBCOMS
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CBCOMS.

