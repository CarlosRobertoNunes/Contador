
      ******************************************************************
      *              Contador de historicos                            *
      ******************************************************************

           SELECT CBCOHI ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  ACCESS MODE   IS RANDOM
                  RELATIVE KEY  IS RK-CBCOHI
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-CBCOHI.

