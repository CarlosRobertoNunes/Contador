       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/08/2019.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exemplo de cadastro                          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FileName ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS FileName-CHAVE
                  ALTERNATE RECORD KEY IS FileName-DESCRICAO
                                          WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-FileName.

       DATA DIVISION.
       FILE SECTION.

       FD  FileName
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FileName.

       01  FileName-REG.
           05 FileName-CHAVE.
              10 FileName-CODIGO          PIC  9(005).
           05 FileName-DESCRICAO          PIC  X(030).
           05 FileName-PRECO              PIC  9(008)V99.
           05 FileName-TIPO               PIC  9(001).
              88 FileName-PECA                         VALUE 1.
              88 FileName-ACABADO                      VALUE 2.
              88 FileName-MATERIAL                     VALUE 3.
           05 FileName-OPCOES.
              10 FileName-IMPORTADO       PIC  9(001).
              10 FileName-GARANTIA        PIC  9(001).
              10 FileName-DURAVEL         PIC  9(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 TIPOS.
              10 CWRADIO      OCCURS 3 PIC  9(001).
           05 OPCOES.
              10 CWCHECK      OCCURS 3 PIC  9(001).
           05 MENSAGEM                 PIC  X(074) VALUE SPACES.
           05 POSICIONA                PIC  9(001) VALUE ZERO.
           05 ERRO                     PIC  9(001) VALUE ZERO.
           05 CHAVE                    PIC  9(002) VALUE ZERO.
           05 CHAVE-ANTERIOR           PIC  9(002) VALUE ZERO.
           05 CAMPO                    PIC  9(002) VALUE ZERO.
           05 SALVA-REG                PIC X(4096) VALUE ZERO.
           05 SALVA-CHAVE              PIC  X(255) VALUE SPACES.
           05 TECLA                    PIC  9(003) VALUE ZERO.
              COPY CWKEYS.
           05 FUNCAO-ANTERIOR          PIC  X(001) VALUE SPACE.
           05 FS-FileName              PIC  X(002) VALUE "00".
           05 LB-FileName              PIC  X(050) VALUE "CWMO99".

       COPY CWFUNC.

       SCREEN SECTION.

       01  CTAC-LIT-CWCADS.
           05 LINE 08 COLUMN 03 VALUE "C¢digo".
           05 LINE 10 COLUMN 03 VALUE "Descri‡Æo".
           05 LINE 12 COLUMN 03 VALUE "Valor".
           05 LINE 15 COLUMN 08 VALUE "Pe‡a".
           05 LINE 15 COLUMN 17 VALUE "Acabado".
           05 LINE 15 COLUMN 29 VALUE "Material".
           05 LINE 19 COLUMN 08 VALUE "Importado".
           05 LINE 19 COLUMN 22 VALUE "Garantia".
           05 LINE 19 COLUMN 35 VALUE "Dur vel".

       01  CTAC-VAR-CWCADS.
           05 TELA-CHAVE.
              10 LINE 08 COLUMN 13 PIC Z(005) USING FileName-CODIGO.
           05 TELA-DADOS.
              10 LINE 10 COLUMN 13 PIC X(030) USING FileName-DESCRICAO.
              10 LINE 12 COLUMN 13 PIC ZZ.ZZZ.ZZ9,99
                 USING FileName-PRECO.
              10 LINE 15 COLUMN 05 PIC X(001) USING CWRADIO(1).
              10 LINE 15 COLUMN 14 PIC X(001) USING CWRADIO(2).
              10 LINE 15 COLUMN 26 PIC X(001) USING CWRADIO(3).
              10 LINE 19 COLUMN 05 PIC X(001) USING CWCHECK(1).
              10 LINE 19 COLUMN 19 PIC X(001) USING CWCHECK(2).
              10 LINE 19 COLUMN 32 PIC X(001) USING CWCHECK(3).

       01  TELA-DESCRICAO.
           10 LINE 10 COLUMN 13 PIC X(030) USING FileName-DESCRICAO.

       PROCEDURE DIVISION.

       000-INICIO.

           OPEN INPUT FileName

           IF   FS-FileName = "30" OR "35"
                OPEN OUTPUT FileName
                CLOSE FileName
           ELSE
                IF   FS-FileName < "10"
                     CLOSE FileName
                ELSE
                     GOBACK
                END-IF
           END-IF

           OPEN I-O FileName

           PERFORM 110-GRUPO THRU 110-99-FIM
           INITIALIZE FileName-REG
           DISPLAY CTAC-LIT-CWCADS
           PERFORM 111-EXIBE THRU 111-99-FIM

           PERFORM TEST AFTER UNTIL FINALIZAR
                   PERFORM 110-GRUPO THRU 110-99-FIM
                   IF   FUNCAO = SPACE
                        MOVE 0 TO CHAVE
                   END-IF
                   EXEC COBOLware Option
                        Function FUNCAO
                   END-EXEC
                   IF  NOT FINALIZAR
                       PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
                   END-IF
           END-PERFORM

           CLOSE FileName

           IF   PARAR
                STOP RUN
           ELSE
                EXEC COBOLware Picture Erase
                          LINE 9 COLUMN 52
                END-EXEC
                GOBACK
           END-IF.

       100-PROCESSAMENTO.

           PERFORM 110-GRUPO THRU 110-99-FIM
           COPY CWESCP.

           PERFORM TEST AFTER
                   UNTIL(NOT (PAGE-UP   AND PAGE-UP-OFF))
                     AND(NOT (PAGE-DOWN AND PAGE-DOWN-OFF))
                   EVALUATE TRUE
                       WHEN INCLUSAO
                            PERFORM 140-NOVA-CHAVE THRU 140-99-FIM
                            ACCEPT TELA-CHAVE
                            IF   FileName-CHAVE = SALVA-CHAVE
                                 PERFORM 140-NOVA-CHAVE THRU 140-99-FIM
                            END-IF
                       WHEN ALTERACAO OR EXCLUSAO OR CONSULTA
                            COPY CWPGON.
                            MOVE 0            TO POSICIONA
                            MOVE FileName-REG TO SALVA-REG
                            PERFORM UNTIL CHAVE NOT = 0
                                    EXEC COBOLware BoxSelect
                                         TITLE "Pesquisar:"
                                         LINE 10 COLUMN 10
                                         TEXT(1) "~C¢digo"
                                         TEXT(2) "~Descri‡Æo"
                                         OPTION CHAVE;CHAVE
                                    END-EXEC
                                    MOVE CHAVE TO POSICIONA
                                    IF  CHAVE = 0
                                        MOVE 99    TO CHAVE
                                        MOVE SPACE TO FUNCAO
                                    END-IF
                            END-PERFORM
                            EVALUATE CHAVE
                                WHEN 1
                                     EXEC COBOLware Object COMBO-BOX
                                          LINE 08 COLUMN 13
                                          HEIGHT 10 WIDTH 5
                                          PROVIDER "CWCAD3"
                                          FIELD Filename-CODIGO
                                          ORDER-LEFT RETURN-LEFT
                                          LEFT-WIDTH  6
                                          RIGHT-WIDTH 30
                                     END-EXEC
                                     ACCEPT TELA-CHAVE
                                WHEN 2
                                     EXEC COBOLware Object COMBO-BOX
                                          LINE 10 COLUMN 13
                                          HEIGHT 10 WIDTH 30
                                          PROVIDER "CWCAD3"
                                          FIELD Filename-DESCRICAO
                                          ORDER-RIGHT RETURN-RIGHT
                                          RIGHT-WIDTH 30
                                     END-EXEC
                                     ACCEPT TELA-DESCRICAO
                            END-EVALUATE
                            PERFORM 110-GRUPO THRU 110-99-FIM
                            IF   FileName-REG = SALVA-REG
                            AND  CHAVE        = CHAVE-ANTERIOR
                            AND  FUNCAO       = FUNCAO-ANTERIOR
                                 MOVE 0 TO POSICIONA
                            END-IF
                            MOVE CHAVE  TO CHAVE-ANTERIOR
                            MOVE FUNCAO TO FUNCAO-ANTERIOR
                            IF  (POSICIONA    NOT = 0)
                            OR  (FileName-REG NOT = SALVA-REG)
                                 MOVE 1 TO POSICIONA
                                 SET PAGE-UP-ON   TO TRUE
                                 SET PAGE-DOWN-ON TO TRUE
                                 SET READY-OFF    TO TRUE
                                 EVALUATE CHAVE
                                     WHEN 1
                                    START FileName KEY
                                          NOT < FileName-CHAVE
                                          INVALID KEY
                                                  START FileName KEY
                                                  NOT > FileName-CHAVE
                                          END-START
                                    END-START
                                     WHEN 2
                                    START FileName KEY
                                          NOT < FileName-DESCRICAO
                                          INVALID KEY
                                                START FileName KEY
                                                NOT > FileName-DESCRICAO
                                          END-START
                                    END-START
                                 END-EVALUATE
                            END-IF
                   END-EVALUATE
                   IF   POSICIONA = 0
                        ACCEPT TECLA FROM ESCAPE KEY
                   ELSE
                        SET PAGE-DOWN TO TRUE
                        EXIT PERFORM
                   END-IF
           END-PERFORM

           MOVE SPACES TO MENSAGEM

           EVALUATE TRUE
               WHEN ESC
                    MOVE SPACE TO FUNCAO
               WHEN INCLUSAO
                AND FileName-CODIGO = 0
                    EXEC COBOLware Send
                         Message "Informe c¢digo do produto"
                    END-EXEC
               WHEN INCLUSAO
                    READ FileName
                    IF   FS-FileName < "10"
                         PERFORM 111-EXIBE THRU 111-99-FIM
                         EXEC COBOLware Send
                              Message "Produto j  cadastrado"
                         END-EXEC
                    ELSE
                         IF   FS-FileName = "23"
                              WRITE FileName-REG
                              READ FileName WITH LOCK
                              PERFORM 120-CRITICA THRU 120-99-FIM
                              IF   EFETIVAR
                                   REWRITE FileName-REG
                                   UNLOCK FileName
                              ELSE
                                   EXEC COBOLware Picture Erase
                                             LINE 9 COLUMN 52
                                   END-EXEC
                                   DELETE FileName RECORD
                                   EXEC COBOLware Picture Remove
                                             RECORD FileName-CODIGO
                                             FILE "fotos"
                                   END-EXEC
                              END-IF
                         END-IF
                    END-IF
               WHEN (ALTERACAO OR EXCLUSAO OR CONSULTA)
                AND (PAGE-DOWN OR PAGE-UP)
                    IF   FS-FileName = "23"
                         EVALUATE CHAVE
                             WHEN 1
                                  START FileName
                                        KEY NOT < FileName-CHAVE
                                        INVALID KEY
                                          START FileName
                                            KEY NOT > FileName-CHAVE
                                        END-START
                                  END-START
                             WHEN 2
                                  START FileName
                                        KEY NOT < FileName-DESCRICAO
                                        INVALID KEY
                                          START FileName
                                            KEY NOT > FileName-DESCRICAO
                                        END-START
                                  END-START
                         END-EVALUATE
                    END-IF
                    EVALUATE TRUE
                             WHEN PAGE-DOWN
                              AND PAGE-DOWN-ON
                                  READ FileName
                                       NEXT RECORD IGNORE LOCK
                             WHEN PAGE-UP
                              AND PAGE-UP-ON
                                  READ FileName
                                       PREVIOUS RECORD IGNORE LOCK
                    END-EVALUATE
                    IF   FS-FileName > "09"
                         IF   FS-FileName > "10"
                              MOVE "V" TO FUNCAO
                         END-IF
                    ELSE
                         EVALUATE TRUE
                                  WHEN PAGE-DOWN
                                   AND PAGE-UP-OFF
                                       SET PAGE-UP-ON   TO TRUE
                                  WHEN PAGE-UP
                                   AND PAGE-DOWN-OFF
                                       SET PAGE-DOWN-ON TO TRUE
                         END-EVALUATE
                         SET READY-ON TO TRUE
                         PERFORM 111-EXIBE THRU 111-99-FIM
                    END-IF
                    IF   FS-FileName = "10"
                         SET READY-OFF TO TRUE
                         EVALUATE TRUE
                                  WHEN PAGE-DOWN
                                       SET PAGE-DOWN-OFF TO TRUE
                                       READ FileName
                                       PREVIOUS RECORD IGNORE LOCK
                                  WHEN PAGE-UP
                                       SET PAGE-UP-OFF   TO TRUE
                                       READ FileName
                                       NEXT RECORD IGNORE LOCK
                         END-EVALUATE
                         IF  (FS-FileName NOT = "10")
                         AND (FS-FileName NOT = "46")
                              SET  READY-ON TO TRUE
                              MOVE "10"     TO FS-FileName
                         END-IF
                    END-IF
               WHEN (ALTERACAO OR EXCLUSAO OR CONSULTA)
                AND (ENTER-KEY OR F2)
                    IF   CONSULTA
                         READ FileName IGNORE LOCK
                    ELSE
                         READ FileName WITH LOCK
                         IF   FS-FileName = "9D"
                              READ FileName IGNORE LOCK
                              MOVE "9D" TO FS-FileName
                         END-IF
                    END-IF
                    MOVE 1      TO POSICIONA
                    MOVE SPACES TO SALVA-REG
                    PERFORM 111-EXIBE THRU 111-99-FIM
                    EVALUATE TRUE
                        WHEN FS-FileName = "23"
                             SET READY-OFF TO TRUE
                             EXEC COBOLware Send
                                  Message "Produto nÆo cadastrado"
                             END-EXEC
                        WHEN FS-FileName = "9D"
                             SET READY-OFF TO TRUE
                             EXEC COBOLware Send
                                  Message "Produto ocupado"
                             END-EXEC
                        WHEN FS-FileName > "09"
                             MOVE "V" TO FUNCAO
                        WHEN ALTERACAO
                             MOVE FileName-REG TO SALVA-REG
                             EXEC COBOLware Object DROP END-EXEC
                             PERFORM 120-CRITICA THRU 120-99-FIM
                             IF   EFETIVAR
                                  REWRITE FileName-REG
                             ELSE
                                  UNLOCK FileName
                             END-IF
                        WHEN EXCLUSAO
                             PERFORM 130-CONFIRMA THRU 130-99-FIM
                             IF   EFETIVAR
                                  DELETE FileName RECORD
                             ELSE
                                  UNLOCK FileName
                             END-IF
                             EXEC COBOLware Picture Erase
                                       LINE 9 COLUMN 52
                             END-EXEC
                             EXEC COBOLware Picture Remove
                                     RECORD FileName-CODIGO
                                       FILE "fotos"
                             END-EXEC
                    END-EVALUATE
                    IF   FS-FileName > "09"
                    AND  EFETIVAR
                         MOVE "V" TO FUNCAO
                    END-IF
           END-EVALUATE.

       100-99-FIM. EXIT.

       110-GRUPO.

           EXEC COBOLware Object DROP END-EXEC
           EXEC COBOLware Object GROUP
                LINE 14 COLUMN 3 WIDTH 36 CAPTION "Tipo"
           END-EXEC
           EXEC COBOLware Object GROUP
                LINE 18 COLUMN 3 WIDTH 41 CAPTION "Op‡äes"
           END-EXEC.

       110-99-FIM. EXIT.

       111-EXIBE.

           INITIALIZE TIPOS

           IF   FileName-TIPO > 0
                MOVE "1" TO CWRADIO(FileName-TIPO)
           END-IF

           MOVE FileName-OPCOES TO OPCOES
           EXEC COBOLware Picture Display
                     LINE 9 COLUMN 52 WIDTH  21
                                      HEIGHT 13
                     RECORD FileName-CODIGO
                     FILE "fotos"
           END-EXEC
           DISPLAY CTAC-VAR-CWCADS.

       111-99-FIM. EXIT.

       120-CRITICA.

           PERFORM 110-GRUPO THRU 110-99-FIM
           COPY CWESCP.
           EXEC COBOLware Object Push-Button Small
                     LINE 23 COLUMN 02 WIDTH 13
                     CAPTION " f3-~Concluir "
                     KEY F3 TAB-OFF
           END-EXEC
           EXEC COBOLware Object Push-Button Small
                     LINE 23 COLUMN 17 WIDTH 9
                     CAPTION " f4-~Foto "
                     KEY F4 TAB-OFF
           END-EXEC
           EXEC COBOLware Object Validate
                  PROGRAM "CWCAD2" USING FileName-DESCRICAO
                                         FileName-PRECO
                                         CWRADIO(1)
                                         CWRADIO(2)
                                         CWRADIO(3)
                                         CWCHECK(1)
                                         CWCHECK(2)
                                         CWCHECK(3)
                    FIELD ANY
           END-EXEC

           PERFORM TEST AFTER UNTIL NOT F4
                   ACCEPT TELA-DADOS
                   ACCEPT TECLA FROM ESCAPE KEY
                   IF  F4
                       EXEC COBOLware Picture Update
                                 RECORD FileName-CODIGO
                                 FILE "fotos"
                       END-EXEC
                       EXEC COBOLware Picture Display
                                 LINE 9 COLUMN 52 WIDTH  21
                                                  HEIGHT 13
                               RECORD FileName-CODIGO
                                 FILE "fotos"
                       END-EXEC
                   END-IF
           END-PERFORM

           MOVE SPACE TO COMANDO

           IF   NOT ESC
                PERFORM VARYING FileName-TIPO FROM 3 BY -1
                                UNTIL FileName-TIPO = 0
                                   OR CWRADIO (FileName-TIPO) = "1"
                         CONTINUE
                END-PERFORM
                MOVE OPCOES TO FileName-OPCOES
                IF    FileName-REG = SALVA-REG
                AND   ALTERACAO
                      SET ABORTAR TO TRUE
                ELSE
                      PERFORM 130-CONFIRMA THRU 130-99-FIM
                END-IF
           ELSE
                MOVE SPACE TO FUNCAO
           END-IF

           PERFORM 110-GRUPO THRU 110-99-FIM.

       120-99-FIM. EXIT.

       130-CONFIRMA.

           COPY CWEFAB.
           IF   EFETIVAR
                EXEC COBOLware LogWrite
                     FUNCTION FUNCAO
                     TEXT FileName-CODIGO
                END-EXEC
           END-IF.

       130-99-FIM. EXIT.

       140-NOVA-CHAVE.

           MOVE ALL X"FF" TO FileName-REG
           START FileName KEY NOT > FileName-CHAVE
            INVALID KEY
                    INITIALIZE FileName-REG
                    MOVE 1 TO FileName-CODIGO
               NOT INVALID KEY
                   READ FileName PREVIOUS RECORD IGNORE LOCK
                   ADD  1              TO FileName-CODIGO
                   MOVE FileName-CHAVE TO SALVA-CHAVE
                   INITIALIZE FileName-REG
                   MOVE SALVA-CHAVE    TO FileName-CHAVE
           END-START
           PERFORM 111-EXIBE THRU 111-99-FIM
           DISPLAY TELA-CHAVE.

       140-99-FIM. EXIT.

       END PROGRAM TESTE.
