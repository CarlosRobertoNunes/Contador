       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB039PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  08/01/1995.
       SECURITY.      *************************************************
                      * Calcula digito verificador do codigo reduzido *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 SOMA                PIC  9(009) VALUE ZERO.
           05 RESTO               PIC  9(001) VALUE ZERO.
           05 LIXO                PIC  9(001) VALUE ZERO.

       LINKAGE SECTION.

       01  COD-RED                PIC  9(005) VALUE 0.
       01  REDEFINES COD-RED.
           05 D1                  PIC  9(001).
           05 D2                  PIC  9(001).
           05 D3                  PIC  9(001).
           05 D4                  PIC  9(001).
           05 D5                  PIC  9(001).
       01  DV                     PIC  X(001) VALUE 0.

       PROCEDURE DIVISION USING COD-RED DV.

       000-INICIO.

           COMPUTE SOMA =
            (D1 * 6) + (D2 * 5) + (D3 *  4) + (D4 * 3) + (D5 *  2)

           DIVIDE 11 INTO SOMA GIVING SOMA REMAINDER RESTO

           COMPUTE RESTO = 11 - RESTO

           IF  RESTO = 0
               MOVE "X" TO DV
           ELSE
               MOVE RESTO TO DV
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM CB039PCW.
