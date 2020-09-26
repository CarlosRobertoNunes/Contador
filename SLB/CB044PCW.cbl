       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB044PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/02/1998.
       SECURITY.      *************************************************
                      *                                               *
                      *  Programas de apoio a CWBOXF para o           *
                      *  Plano de contas                              *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CBPLCOSL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBPLCOFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 ER-CBPLCO.
              10 FS-CBPLCO        PIC  X(002) VALUE "00".
              10 LB-CBPLCO        PIC  X(050) VALUE "CBPLCO".
           05 COD-RED-CALL        PIC  9(005) VALUE 0.
           05 COD-RED-DV-CALL     PIC  X(001) VALUE SPACE.

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                               VALUE "O" "o".
           88 CLOSE-FILE                              VALUE "C" "c".
           88 BEGIN-FILE                              VALUE "B" "b".
           88 END-FILE                                VALUE "E" "e".
           88 AT-END                                  VALUE "*".
           88 READ-NEXT                               VALUE "N" "n".
           88 READ-PREVIOUS                           VALUE "P" "p".
           88 NOT-LESS                                VALUE ">".
           88 NOT-GREATER                             VALUE "<".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(040).
       01  STRING-2                           PIC  X(040).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA                          PIC  X(050).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA.
       000-INICIO.

           IF   VEZ = 1
                MOVE 2           TO VEZ
           END-IF

           PERFORM TEST AFTER UNTIL (FS-CBPLCO NOT = "99")
                                AND (FS-CBPLCO NOT = "9D")
           EVALUATE TRUE
               WHEN OPEN-FILE
                    OPEN INPUT CBPLCO
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-CBPLCO > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            READ CBPLCO NEXT RECORD IGNORE LOCK
                            IF   FS-CBPLCO < "10"
                            AND  CBPLCO-COD-RED NOT = 0
                                 ADD 1 TO REGISTROS
                            END-IF
                    END-PERFORM
                    IF   REGISTROS = 0
                         MOVE 1 TO REGISTROS
                    END-IF
                    IF   REGISTROS < VERTICAL-LENGTH
                         MOVE REGISTROS TO VERTICAL-LENGTH
                    END-IF
               WHEN CLOSE-FILE
                    CLOSE CBPLCO
               WHEN BEGIN-FILE
                    INITIALIZE CBPLCO-REG
                    EVALUATE ORDER-X
                        WHEN 1
                             START CBPLCO KEY NOT < CBPLCO-COD-RED
                        WHEN 2
                             START CBPLCO KEY NOT < CBPLCO-DESCRICAO
                        WHEN OTHER
                             START CBPLCO KEY NOT < CBPLCO-COD-RED
                    END-EVALUATE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO CBPLCO-REG
                    EVALUATE ORDER-X
                        WHEN 1
                             START CBPLCO KEY NOT > CBPLCO-COD-RED
                        WHEN 2
                             START CBPLCO KEY NOT > CBPLCO-DESCRICAO
                        WHEN OTHER
                             START CBPLCO KEY NOT > CBPLCO-COD-RED
                    END-EVALUATE
               WHEN READ-NEXT
                    PERFORM TEST AFTER UNTIL CBPLCO-COD-RED NOT = 0
                                          OR AT-END
                    READ CBPLCO NEXT RECORD IGNORE LOCK
                    IF   FS-CBPLCO > "09"
                         SET AT-END TO TRUE
                    END-IF
                    END-PERFORM
               WHEN READ-PREVIOUS
                    PERFORM TEST AFTER UNTIL CBPLCO-COD-RED NOT = 0
                                          OR AT-END
                    READ CBPLCO PREVIOUS RECORD
                                  IGNORE LOCK
                    IF   FS-CBPLCO > "09"
                         SET AT-END TO TRUE
                    END-IF
                    END-PERFORM
               WHEN NOT-LESS
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO CBPLCO-COD-RED
                             START CBPLCO KEY NOT < CBPLCO-COD-RED
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                        WHEN 2
                             MOVE STRING-2 TO CBPLCO-DESCRICAO
                             START CBPLCO KEY NOT < CBPLCO-DESCRICAO
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                    END-EVALUATE
               WHEN NOT-GREATER
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO CBPLCO-COD-RED
                             START CBPLCO KEY NOT > CBPLCO-COD-RED
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                        WHEN 2
                             MOVE STRING-2 TO CBPLCO-DESCRICAO
                             START CBPLCO KEY NOT > CBPLCO-DESCRICAO
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                    END-EVALUATE
           END-EVALUATE
           END-PERFORM

           MOVE CBPLCO-COD-RED    TO STRING-1
           MOVE "-"               TO STRING-1  (6: 1)
           MOVE CBPLCO-COD-RED    TO COD-RED-CALL
           CALL "CB039PCW"     USING COD-RED-CALL
                                     COD-RED-DV-CALL
           MOVE COD-RED-DV-CALL   TO STRING-1 (7: 1)
           MOVE CBPLCO-DESCRICAO  TO STRING-2
           GOBACK.

       END PROGRAM CB044PCW.
