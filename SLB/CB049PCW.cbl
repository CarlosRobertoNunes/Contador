       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CB049PCW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/06/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Provedor para a CWBOXF Historicos padrao     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           COPY CBCAHISL.

       DATA DIVISION.
       FILE SECTION.

       COPY CBCAHIFD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 ER-CBCAHI.
              10 FS-CBCAHI      PIC  X(002) VALUE "00".
              10 LB-CBCAHI      PIC  X(050) VALUE "CBCAHI".

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                            VALUE "O" "o".
           88 CLOSE-FILE                           VALUE "C" "c".
           88 BEGIN-FILE                           VALUE "B" "b".
           88 END-FILE                             VALUE "E" "e".
           88 AT-END                               VALUE "*".
           88 READ-NEXT                            VALUE "N" "n".
           88 READ-PREVIOUS                        VALUE "P" "p".
           88 NOT-LESS                             VALUE ">".
           88 NOT-GREATER                          VALUE "<".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(080).
       01  STRING-2                           PIC  X(080).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA                          PIC  X(050).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA.
       000-INICIO.

           EVALUATE TRUE
               WHEN OPEN-FILE
                    OPEN INPUT CBCAHI
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-CBCAHI > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            READ CBCAHI NEXT RECORD IGNORE LOCK
                            IF   FS-CBCAHI < "10"
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
                    CLOSE CBCAHI
               WHEN BEGIN-FILE
                    INITIALIZE CBCAHI-REG
                    EVALUATE ORDER-X
                        WHEN 1
                             START CBCAHI KEY NOT < CBCAHI-CHAVE
                        WHEN 2
                             START CBCAHI KEY NOT < CBCAHI-DESCRICAO
                        WHEN OTHER
                             START CBCAHI KEY NOT < CBCAHI-CHAVE
                    END-EVALUATE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO CBCAHI-REG
                    EVALUATE ORDER-X
                        WHEN 1
                             START CBCAHI KEY NOT > CBCAHI-CHAVE
                        WHEN 2
                             START CBCAHI KEY NOT > CBCAHI-DESCRICAO
                        WHEN OTHER
                             START CBCAHI KEY NOT > CBCAHI-CHAVE
                    END-EVALUATE
               WHEN READ-NEXT
                    READ CBCAHI NEXT RECORD IGNORE LOCK
                    IF   FS-CBCAHI > "09"
                         SET AT-END TO TRUE
                    END-IF
               WHEN READ-PREVIOUS
                    READ CBCAHI PREVIOUS RECORD IGNORE LOCK
                    IF   FS-CBCAHI > "09"
                         SET AT-END TO TRUE
                    END-IF
               WHEN NOT-LESS
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO CBCAHI-CODIGO
                             START CBCAHI KEY NOT < CBCAHI-CHAVE
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                        WHEN 2
                             MOVE STRING-2 TO CBCAHI-DESCRICAO
                             START CBCAHI KEY NOT < CBCAHI-DESCRICAO
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                    END-EVALUATE
               WHEN NOT-GREATER
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO CBCAHI-CODIGO
                             START CBCAHI KEY NOT > CBCAHI-CHAVE
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                        WHEN 2
                             MOVE STRING-2 TO CBCAHI-DESCRICAO
                             START CBCAHI KEY NOT > CBCAHI-DESCRICAO
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                    END-EVALUATE
           END-EVALUATE

           MOVE CBCAHI-CODIGO    TO STRING-1
           MOVE CBCAHI-DESCRICAO TO STRING-2
           GOBACK.

       END PROGRAM CB049PCW.
