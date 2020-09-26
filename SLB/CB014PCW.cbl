       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB014PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  11/02/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *   Subrotina de consulta/atualizacao de moedas *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBCOMDSL REPLACING EXCLUSIVE BY AUTOMATIC.
       COPY CBTAMDSL REPLACING EXCLUSIVE BY AUTOMATIC.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCOMDFD.
       COPY CBTAMDFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 NOPATH-CBTAMD VALUE "CBMO99".
              15 FILLER                PIC  X(004).
              15 MOEDA-DESEJADA        PIC  9(002).
              15 FILLER                PIC  X(006).
           05 COLUNA                   PIC  9(002) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 CARGA                    PIC  9(003) VALUE 0.
           05 COTACAO-DE-HOJE          PIC  9(006)V9(04) VALUE 0.
           05 OBRIGADO                 PIC  X(068) VALUE
              "Grato...".
           05 NOME-MOEDA-2                         VALUE SPACES.
              10 BYTE-NOME-MOEDA       PIC  X(001) OCCURS 14.
           05 MOEDA-ED                 PIC  ZZZ.ZZ9,9999.
           05 FILLER                               VALUE ZEROS.
              10 PRIMEIRO OCCURS 5     PIC  9(002).
           05 CORRESPONDENCIAS                     VALUE ZEROS.
              10 CORRE OCCURS 15       PIC  9(002).
           05 TAMANHO                  PIC  9(014) VALUE ZERO.
           05 PAGINA                   PIC  9(001) VALUE ZERO.
           05 ULTIMA                   PIC  9(002) VALUE ZERO.
           05 I                        PIC  9(003) VALUE ZERO.
           05 TIPO                     PIC  X(001) VALUE SPACE.
              88 DIARIA                            VALUE "D".
              88 MENSAL                            VALUE "M".
           05 Y                        PIC  9(003) VALUE ZERO.
           05 ER-CBCOMD.
              10 FS-CBCOMD              PIC  X(002) VALUE "00".
              10 LB-CBCOMD              PIC  X(050) VALUE "CBCOMD.DAT".
           05 ER-CBTAMD.
              10 FS-CBTAMD              PIC  X(002) VALUE "00".
              10 LB-CBTAMD              PIC  X(050) VALUE "CBTAMD.DAT".
           05 NOMES-MOEDAS                         VALUE SPACES.
              10 NOME-MOEDA OCCURS 99  PIC  X(014).
              10 TIPO-MOEDA OCCURS 99  PIC  X(001).
            05 CHAVE-A                 PIC  X(008) VALUE SPACES.
            05 CHAVE.
               10 AAAA                 PIC  9(004).
               10 MM                   PIC  9(002).
               10 DD                   PIC  9(002).
            05 REDEFINES CHAVE.
               10 SEC                  PIC  9(002).
               10 AAMMDD               PIC  9(006).

       COPY CWBOXS.

       LINKAGE SECTION.

       01   PARAMETROS-CB014PCW.
            05 CB014PCW-REFERENCIA-AAAAMMDD  PIC  9(008).
            05 CB014PCW-VALOR       COMP-3   PIC S9(012)V99.
            05 CB014PCW-CONVERTIDO  COMP-3   PIC S9(012)V9(4).
            05 CB014PCW-CORRIGIDO   COMP-3   PIC S9(012)V99.
            05 CB014PCW-MOEDA       COMP-3   PIC S9(006)V9(4).
            05 CB014PCW-MOEDA-NOME           PIC  X(014).

       PROCEDURE DIVISION USING PARAMETROS-CB014PCW.

       BEGIN.

           EVALUATE TRUE
               WHEN VEZ = 1
                    MOVE 2 TO VEZ
                    PERFORM 100-CARREGA-MOEDAS THRU 100-99-FIM
               WHEN MOEDA-DESEJADA = 0
                    MOVE CB014PCW-VALOR TO CB014PCW-CONVERTIDO
                                           CB014PCW-CORRIGIDO
                    GOBACK
               WHEN VEZ = 2
                    MOVE 3 TO VEZ
                    ACCEPT AAMMDD FROM DATE
                    IF   AAMMDD > 850000
                         MOVE   19       TO SEC
                    ELSE
                         MOVE   20       TO SEC
                    END-IF
                    PERFORM 110-OBTEM-MOEDA THRU 110-99-FIM
                    MOVE    CB014PCW-MOEDA    TO COTACAO-DE-HOJE
           END-EVALUATE

           MOVE CB014PCW-REFERENCIA-AAAAMMDD TO CHAVE

           PERFORM 110-OBTEM-MOEDA THRU 110-99-FIM

           COMPUTE CB014PCW-CONVERTIDO ROUNDED
                                     = CB014PCW-VALOR / CB014PCW-MOEDA
           COMPUTE CB014PCW-CORRIGIDO  = CB014PCW-CONVERTIDO *
                                       COTACAO-DE-HOJE
           GOBACK.

       110-OBTEM-MOEDA.

           IF   MENSAL
                MOVE 1 TO DD
           END-IF

           IF   CHAVE = CHAVE-A
                GO TO 110-99-FIM
           END-IF

           MOVE CHAVE TO CBTAMD-CHAVE CHAVE-A

           OPEN I-O CBTAMD
           READ CBTAMD

           IF   FS-CBTAMD = "23"
                MOVE NOME-MOEDA (MOEDA-DESEJADA) TO NOME-MOEDA-2
                PERFORM VARYING TAMANHO FROM 14 BY -1
                                UNTIL BYTE-NOME-MOEDA (TAMANHO)
                                  NOT = SPACE
                                   OR TAMANHO = 0
                        CONTINUE
                END-PERFORM
                DISPLAY "Informe " LINE 23 COLUMN 3
                DISPLAY NOME-MOEDA (MOEDA-DESEJADA)
                        LINE 23
                      COLUMN 11
                   WITH BEEP SIZE TAMANHO
                COMPUTE COLUNA = 11 + TAMANHO
                DISPLAY " de " LINE 23 COLUMN COLUNA
                ADD 4 TO COLUNA
                IF   DIARIA
                     DISPLAY DD
                             LINE 23 COLUMN COLUNA
                             "/" MM "/" AAAA ": "
                     ADD 12 TO COLUNA
                ELSE
                     DISPLAY MM
                             LINE 23 COLUMN COLUNA
                             "/" AAAA ": "
                     MOVE 01 TO DD
                     ADD  09 TO COLUNA
                END-IF
                MOVE    ZERO      TO CB014PCW-MOEDA
                PERFORM UNTIL CB014PCW-MOEDA NOT = ZERO
                        ACCEPT MOEDA-ED LINE 23 COLUMN COLUNA
                        MOVE MOEDA-ED     TO CB014PCW-MOEDA
                        MOVE CB014PCW-MOEDA TO CBTAMD-MOEDA
                END-PERFORM
                WRITE CBTAMD-REG
                IF   FS-CBTAMD < "10"
                     DISPLAY OBRIGADO LINE 23 COLUMN 3
                END-IF
           ELSE
                IF   FS-CBTAMD < "10"
                     MOVE CBTAMD-MOEDA TO CB014PCW-MOEDA
                END-IF
           END-IF

           CLOSE CBTAMD.

       110-99-FIM. EXIT.

       100-CARREGA-MOEDAS.

           OPEN INPUT CBCOMD

           IF   FS-CBCOMD > "09"
                GOBACK
           END-IF

           MOVE 2 TO CBCOMD-MOEDA

           START CBCOMD KEY NOT < CBCOMD-MOEDA

           PERFORM 810-LER-CBCOMD THRU 810-99-FIM
                   UNTIL FS-CBCOMD >  "09"
           CLOSE CBCOMD

           MOVE 0 TO MOEDA-DESEJADA

           PERFORM 820-SELECIONA-MOEDA THRU 820-99-FIM.

       100-99-FIM. EXIT.

       810-LER-CBCOMD.

           READ CBCOMD NEXT RECORD IGNORE LOCK

           IF   FS-CBCOMD EQUAL "00"
           AND  CBCOMD-TIPO NOT = "E"
                ADD  1                 TO CARGA
                MOVE CBCOMD-NOME-MOEDA TO NOME-MOEDA (CBCOMD-MOEDA)
                MOVE CBCOMD-TIPO       TO TIPO-MOEDA (CBCOMD-MOEDA)
           END-IF.

       810-99-FIM. EXIT.

       820-SELECIONA-MOEDA.

           MOVE "Moedas" TO CWBOXS-TITLE
           MOVE 7        TO CWBOXS-LINE
           MOVE 5        TO CWBOXS-COLUMN
           MOVE 1        TO I
           MOVE ZERO     TO CWBOXS-OPTION

           PERFORM UNTIL I > 99 OR CWBOXS-OPTION NOT = 0
                   ADD  1      TO PAGINA
                   MOVE SPACES TO CWBOXS-ITENS
                   MOVE ZEROS  TO CORRESPONDENCIAS
                   MOVE 0      TO Y
                   PERFORM VARYING I FROM I BY 1 UNTIL I > 99
                                                    OR Y = 15
                      IF   NOME-MOEDA (I) NOT = SPACE
                           IF   PRIMEIRO (PAGINA) = 0
                           AND  Y                 = 0
                                MOVE I TO PRIMEIRO (PAGINA)
                           END-IF
                           ADD  1             TO Y
                           MOVE I             TO CORRE         (Y)
                           MOVE NOME-MOEDA(I) TO CWBOXS-TEXT   (Y)
                      END-IF
                   END-PERFORM
                   MOVE SPACE       TO CWBOXS-ARROW
                   IF   CARGA = 1
                        MOVE 1 TO CWBOXS-OPTION
                   ELSE
                        CALL "CWBOXS" USING PARAMETROS-CWBOXS
                   END-IF
                   IF   CWBOXS-ARROW = "<"
                        IF   PAGINA > 1
                             SUBTRACT 14              FROM CWBOXS-COLUMN
                             SUBTRACT 1               FROM PAGINA
                             MOVE     PRIMEIRO (PAGINA) TO I
                             SUBTRACT 1               FROM PAGINA
                        ELSE
                             MOVE 1    TO I
                             MOVE ZERO TO PAGINA
                        END-IF
                   ELSE
                        ADD  14 TO CWBOXS-COLUMN
                        IF   CWBOXS-OPTION NOT = 0
                             MOVE CORRE (CWBOXS-OPTION)
                               TO MOEDA-DESEJADA
                             MOVE TIPO-MOEDA (MOEDA-DESEJADA)
                               TO TIPO
                             MOVE NOME-MOEDA (MOEDA-DESEJADA)
                               TO CB014PCW-MOEDA-NOME
                        END-IF
                   END-IF
           END-PERFORM.

           IF   MOEDA-DESEJADA = 0
                GO TO 820-SELECIONA-MOEDA
           END-IF.

       820-99-FIM. EXIT.

       END PROGRAM CB014PCW.
