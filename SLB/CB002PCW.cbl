       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB002PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/12/1990.
       SECURITY.      *************************************************
                      *  SubRotina p/processar contas                 *
                      *  Parametros-CB002PCW.                         *
                      *   CB002PCW-FUNCAO   PIC X(01).    Input       *
                      *   Display,Edit,Accept,Subtrair 1 grau,Calc dv *
                      *   CB002PCW-LINHA    PIC 9(02).    Input       *
                      *   CB002PCW-COLUNA   PIC 9(02).    Input       *
                      *   CB002PCW-CONTA    PIC 9(15)     I/O Comp-3  *
                      *   CB002PCW-DV       PIC X(01).    I/O         *
                      *   CB002PCW-FORCA-DV PIC X(01) S/N Input       *
                      *   CB002PCW-RETORNO  PIC 9(02).    Output <Esc>*
                      *   CB002PCW-CONTA-ED PIC X(26).    Output      *
                      *   CB002PCW-GRAU     PIC 9(01)     Output      *
                      *   CB002PCW-LANCAVEL PIC X(01) S/N Output      *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPAPCSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPAPCFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 LB-HELP                  PIC  X(012) VALUE "CB002PCW.HXX".
           05 GRAUS VALUE ZEROS.
              10 GRAU-SIZE  OCCURS 9 PIC 9(002).
              10 GRAU-BEGIN OCCURS 9 PIC 9(002).
           05 VEZ                      PIC  9(001) VALUE 1.
           05 GRAU                     PIC  9(001) VALUE 0.
           05 G                        PIC  9(002) VALUE 0.
           05 G2                       PIC  9(002) VALUE 0.
           05 G3                       PIC  9(002) VALUE 0.
           05 I                        PIC  9(002) VALUE 0.
           05 D                        PIC  9(002) VALUE 0.
           05 S                        PIC  9(002) VALUE 0.
           05 B                        PIC  9(002) VALUE 1.
           05 L                        PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 C                        PIC  9(002) VALUE 0.
           05 C2                       PIC  9(002) VALUE 0.
           05 C3                       PIC  9(002) VALUE 1.
           05 X                        PIC  9(001) VALUE 0.
           05 T                        PIC  9(015) VALUE 0.
           05 G-A                      PIC  9(015) VALUE 0.
           05 T2                       PIC  9(015) VALUE 0.
           05 DV                       PIC  X(001) VALUE SPACE.
           05 DV-N REDEFINES DV        PIC  9(001).
           05 DV-A REDEFINES DV        PIC  9(002) COMP-5.
           05 DV-C                     PIC  X(001) VALUE SPACE.
           05 DV-N2                    PIC  9(001) VALUE ZERO.
           05 CONTA                    PIC  9(015) VALUE 0.
           05 REDEFINES CONTA.
              10 DG-C OCCURS 15        PIC  9(001).
           05 CONTA2                   PIC  X(015) VALUE SPACE.
           05 CONTA3 REDEFINES CONTA2  PIC  9(015).
           05 CONTA4                   PIC  9(015).
           05 ERRO                     PIC  X(030) VALUE
              "Conta inconsistente".
           05 CONTA-E                  PIC  X(026) VALUE SPACES.
           05 RK-CBPAPC                PIC  9(001) COMP VALUE 1.
           05 ER-CBPAPC.
              10 FS-CBPAPC             PIC  X(002) VALUE "00".
              10 LB-CBPAPC             PIC  X(050) VALUE "CBPAPC".
           05 FORMULA-DV                           VALUE ZEROS.
              10 MUNTIPLICADORES.
                 15 M OCCURS 15        PIC  9(002).
              10 MODULO-1              PIC  9(002).
              10 MODULO-2              PIC  9(002).
              10 OPERADOR              PIC  X(001).
                 88 OPERADOR-OK VALUE ">" "<" "=".
              10 MODULO-3              PIC  9(002).
              10 DV-X                  PIC  X(001).
           05 SOMA                     PIC  9(009) VALUE ZERO.
           05 RESTO                    PIC  9(002) VALUE ZERO.
           05 REDEFINES RESTO.
              10 FILLER                PIC  X(001).

       LINKAGE SECTION.

       01  PARAMETROS-CB002PCW.
           05 CB002PCW-FUNCAO            PIC  X(001).
           05 CB002PCW-LINHA             PIC  9(002).
           05 CB002PCW-COLUNA            PIC  9(002).
           05 CB002PCW-CONTA      COMP-3 PIC  9(015).
           05 CB002PCW-DV                PIC  X(001).
           05 CB002PCW-FORCA-DV          PIC  X(001).
           05 CB002PCW-RETORNO           PIC  9(002).
              COPY CWKEYS.
           05 CB002PCW-CONTA-ED          PIC  X(026).
           05 CB002PCW-GRAU              PIC  9(001).
           05 CB002PCW-LANCAVEL          PIC  X(001).

       PROCEDURE DIVISION USING PARAMETROS-CB002PCW.

       000-INICIO.

           IF   VEZ = 1
                MOVE 2           TO VEZ
                OPEN INPUT CBPAPC
                IF  FS-CBPAPC > "09"
                    EXEC COBOLware Send
                         Message "Defina a estrutura das contas"
                    END-EXEC
                    STOP RUN
                END-IF
                READ CBPAPC
                IF  FS-CBPAPC > "09"
                    GOBACK
                END-IF
                IF  CBPAPC-ESTRUTURA-CONTA = ZEROS
                OR  CBPAPC-ESTRUTURA-CONTA = SPACES
                    EXEC COBOLware Send
                         Message "Defina a estrutura das contas"
                    END-EXEC
                    STOP RUN
                END-IF
                MOVE CBPAPC-FORMULA-DV TO FORMULA-DV
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
                        IF   CBPAPC-DG (I) NOT = 0
                             MOVE I              TO GRAU
                             MOVE CBPAPC-DG (I)  TO GRAU-SIZE  (I)
                             MOVE B              TO GRAU-BEGIN (I)
                             ADD  GRAU-SIZE  (I) TO B
                        END-IF
                END-PERFORM
                CLOSE CBPAPC.

           EVALUATE TRUE
               WHEN CB002PCW-FUNCAO = "E" OR "e"
                    PERFORM 100-EDITAR THRU 100-99-FIM
                    MOVE CONTA-E TO CB002PCW-CONTA-ED
               WHEN CB002PCW-FUNCAO = "D" OR "d"
                                 OR "A" OR "a"
                    PERFORM 100-EDITAR THRU 100-99-FIM
                    DISPLAY CONTA-E
                            LINE   CB002PCW-LINHA
                            COLUMN CB002PCW-COLUNA
                    IF   CB002PCW-FUNCAO = "A" OR "a"
                         PERFORM 110-DIGITAR THRU 110-99-FIM
                         PERFORM 100-EDITAR THRU 100-99-FIM
                         DISPLAY CONTA-E
                                LINE   CB002PCW-LINHA
                                COLUMN CB002PCW-COLUNA
                   END-IF
               WHEN CB002PCW-FUNCAO = "S" OR "s"
                    PERFORM 100-EDITAR THRU 100-99-FIM
                    IF   CB002PCW-GRAU NOT = 0
                         MOVE CB002PCW-GRAU   TO G
                         MOVE GRAU-SIZE  (G)  TO S
                         MOVE GRAU-BEGIN (G)  TO B
                         MOVE ZEROS           TO CONTA2
                         MOVE CONTA2 (B: S)   TO CONTA (B: S)
                         MOVE CONTA           TO CB002PCW-CONTA
                         PERFORM 100-EDITAR THRU 100-99-FIM
                    END-IF
               WHEN CB002PCW-FUNCAO = "C" OR "c"
                    MOVE CB002PCW-CONTA TO CONTA
                    PERFORM 120-CALCULA-DV THRU 120-99-FIM
                    MOVE DV-C TO CB002PCW-DV
           END-EVALUATE.

       000-99-FIM. GOBACK.

       100-EDITAR.

           MOVE SPACES       TO CONTA-E
           MOVE CB002PCW-CONTA TO CONTA
           MOVE 1            TO I T

           PERFORM VARYING G FROM 1 BY 1 UNTIL (G > GRAU)
                                            OR (T = 0)
                   MOVE GRAU-SIZE  (G) TO S
                   MOVE GRAU-BEGIN (G) TO B
                   MOVE CONTA (B: S)   TO T
                   IF   T NOT = 0
                        MOVE CONTA (B: S) TO CONTA-E (I: S)
                        ADD  S            TO I
                        MOVE "."          TO CONTA-E (I: 1)
                        MOVE I            TO C3
                        ADD 1             TO I
                   ELSE
                        SUBTRACT 1 FROM G
                   END-IF
           END-PERFORM

           PERFORM 120-CALCULA-DV THRU 120-99-FIM

           IF   (G - 1) = GRAU
                MOVE "S" TO CB002PCW-LANCAVEL
                SUBTRACT 1 FROM I
                MOVE "-"          TO CONTA-E (I: 1)
                ADD 1             TO I
                MOVE CB002PCW-DV  TO CONTA-E (I: 1)
           ELSE
                MOVE "N"   TO CB002PCW-LANCAVEL
                IF   C3 > 26
                     MOVE 26 TO C3
                END-IF
                MOVE SPACE TO CONTA-E (C3: 1).

           IF   CB002PCW-DV NOT = DV-C
                MOVE "N" TO CB002PCW-LANCAVEL.

           COMPUTE CB002PCW-GRAU = G - 1

           COMPUTE B = B + S
           COMPUTE S = 16 - B
           MOVE CONTA  TO CONTA3
           MOVE ZEROS  TO CONTA2 (B: S)
           MOVE CONTA3 TO CONTA CB002PCW-CONTA.

       100-99-FIM. EXIT.

       110-DIGITAR.

           MOVE CB002PCW-CONTA  TO CONTA
           MOVE CB002PCW-LINHA  TO L
           MOVE CB002PCW-COLUNA TO C C3
           MOVE 1             TO G.
           MOVE 0             TO G2.

       110-VOLTA-DIGITAR.

           MOVE 1             TO T

           PERFORM VARYING G FROM G BY 1 UNTIL (G > GRAU)
                                            OR (T = 0)
                   MOVE GRAU-SIZE  (G) TO S
                   MOVE GRAU-BEGIN (G) TO B
                   MOVE CONTA (B: S)   TO T T2
                   ACCEPT T
                          LINE L
                          COLUMN C
                          WITH PROMPT UPDATE SIZE S AUTO-SKIP
                   ACCEPT CB002PCW-RETORNO FROM ESCAPE KEY
                   IF   CURSOR-UP
                   AND  G = 1
                        GOBACK
                   END-IF
                   IF   G > G2
                   AND  T NOT = 0
                        MOVE G TO G2
                   END-IF
                   IF  (T NOT = 0)
                   OR  (G < G2)
                   OR  (T2 NOT = 0)
                   OR  (CURSOR-UP)
                        MOVE T            TO CONTA3
                        COMPUTE Y = 16 - S
                        MOVE CONTA2 (Y: S) TO CONTA (B: S)
                        MOVE C             TO C2
                        MOVE B             TO Y
                        PERFORM S TIMES
                                  MOVE CONTA (Y: 1) TO X
                                  DISPLAY X LINE L COLUMN C2
                                  ADD 1             TO C2
                                                       Y
                        END-PERFORM
                        ADD  S            TO C
                        MOVE C            TO C3
                        IF  G = GRAU
                            DISPLAY "-" LINE L COLUMN C
                        ELSE
                            DISPLAY "." LINE L COLUMN C
                        END-IF
                        ADD 1             TO C
                        IF   CURSOR-UP
                             SUBTRACT 1             FROM C
                             SUBTRACT S             FROM C
                             SUBTRACT 1             FROM G
                             IF   G > 0
                                  SUBTRACT GRAU-SIZE (G) FROM C
                                  SUBTRACT 1             FROM C
                                  SUBTRACT 1             FROM G
                             ELSE
                                  MOVE CB002PCW-COLUNA TO C C3
                             END-IF
                        END-IF
                        IF   T = 0
                             MOVE 1 TO T
                        END-IF
                   ELSE
                        IF   G2 > G
                             MOVE 1 TO T
                        END-IF
                        SUBTRACT 1 FROM G
                   END-IF
                   IF   NOT CURSOR-UP
                   AND  NOT ENTER-KEY
                        MOVE 0 TO T
                   END-IF
           END-PERFORM

           MOVE CONTA TO CB002PCW-CONTA

           MOVE C3  TO C
           DISPLAY " " LINE L COLUMN C

           IF   CB002PCW-RETORNO = 0
                IF   (G - 1) = GRAU
                     DISPLAY "-" LINE L COLUMN C
                     ADD 1             TO C
                     PERFORM 120-CALCULA-DV THRU 120-99-FIM
                     IF   CB002PCW-FORCA-DV = "S" OR "s"
                          MOVE    DV-C TO DV
                          DISPLAY DV LINE L COLUMN C
                     ELSE
                          IF   DV-X NOT NUMERIC
                               ACCEPT DV LINE L COLUMN C
                                         WITH UPDATE PROMPT
                               IF   DV-A > 96 AND < 123
                                    SUBTRACT 32 FROM DV-A
                                    DISPLAY DV LINE L COLUMN C
                               END-IF
                          ELSE
                               ACCEPT DV-N LINE L COLUMN C
                                           WITH UPDATE PROMPT
                          END-IF
                          ACCEPT CB002PCW-RETORNO FROM ESCAPE KEY
                     END-IF
                     MOVE DV TO CB002PCW-DV
                     IF   (DV-C NOT = DV)
                     AND  (NOT ESC)
                     AND  (NOT CURSOR-UP)
                          EXEC COBOLware Send
                               Message ERRO
                          END-EXEC
                          SET CURSOR-UP TO TRUE
                     END-IF
                     IF   CURSOR-UP
                          MOVE     GRAU  TO G
                          SUBTRACT S   FROM C
                          SUBTRACT 1   FROM C
                          GO TO 110-VOLTA-DIGITAR
                     END-IF
                ELSE
                     MOVE " " TO CB002PCW-DV
                END-IF
                MOVE CB002PCW-COLUNA TO C C3
                MOVE    1 TO T
                MOVE    0 TO G-A
                PERFORM VARYING G FROM G BY -1
                        UNTIL G = 0
                        IF  GRAU-BEGIN (G) NOT = 0
                        AND GRAU-SIZE  (G) NOT = 0
                            MOVE ZEROS          TO CONTA3
                            MOVE GRAU-BEGIN (G) TO B
                            MOVE GRAU-SIZE  (G) TO S
                            MOVE CONTA (B: S)   TO CONTA3 (B: S)
                            IF   T NOT = 0
                            AND  G-A NOT = 0
                                 MOVE CONTA3 TO T
                            END-IF
                            MOVE CONTA3 TO G-A
                        END-IF
                END-PERFORM
                IF   T = 0
                     EXEC COBOLware Send
                          Message ERRO
                     END-EXEC
                     MOVE     1     TO G
                     GO TO 110-VOLTA-DIGITAR
                END-IF
           ELSE
                MOVE "N" TO CB002PCW-LANCAVEL
           END-IF.

       110-99-FIM. EXIT.

       120-CALCULA-DV.

           MOVE ZERO TO SOMA

           PERFORM VARYING D FROM 1 BY 1 UNTIL D > 15
                   COMPUTE SOMA = SOMA + (M (D) * DG-C (D))
           END-PERFORM

           DIVIDE MODULO-1 INTO SOMA GIVING SOMA REMAINDER RESTO

           COMPUTE RESTO = MODULO-2 - RESTO
           MOVE RESTO TO DV-N2
           MOVE DV-N2 TO DV-C

           IF   OPERADOR-OK
                IF ((OPERADOR = "=") AND (RESTO = MODULO-3))
                OR ((OPERADOR = ">") AND (RESTO > MODULO-3))
                OR ((OPERADOR = "<") AND (RESTO < MODULO-3))
                     MOVE DV-X TO DV-C.

       120-99-FIM. EXIT.
       END PROGRAM CB002PCW.
