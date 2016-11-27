*                                                                       00000100
*  PROGRAM:    ZNC                                                      00000200
*  AUTHOR:     Randy Frerking.                                          00000300
*  SOURCE:     J1FRERK.CICS.ZCACHE(ZNC)                                 00000400
*  DATE:       May 14, 2012                                             00000500
*  COMMENTS:   Create zCache 'named counter' for a specific partition.  00000600
*              The 'named counter' is used to create a key for the      00000600
*              file/data store within zCache.                           00000600
*                                                                       00000800
*              This program can be executed from another program via    00000600
*              LINK command for from the terminal using the following   00000600
*              format:                                                  00000600
*                                                                       00000800
*              ZCNC,XXXX    Where 'xxxx' is the zCache transactionID    00000600
*                                                                       00000800
*  2013/05/13  J1FRERK - CREATED                                        00000810
*                                                                       00000840
*********************************************************************** 00000900
* Dynamic Storage Area (Start)                                        * 00001000
*********************************************************************** 00000900
DFHEISTG DSECT                                                          00001200
ABSTIME  DS    D                  Absolute time                         00001300
         DS   0F
APPLID   DS    CL08               CICS/VTAM APPLID                      00001300
         DS   0F
SYSID    DS    CL04               CICS SYSID                            00001400
         DS   0F
STCODE   DS    CL02               Transaction start code                00001400
         DS   0F
USERID   DS    CL08               UserID                                00001400
         DS   0F
Z_NC     DS   0CL16               zCache Named Counter                  00001400
Z_TRAN   DS    CL04               zCache transaction ID
Z_SUFFIX DS    CL12               _ZCACHE
         DS   0F
BAS_REG  DS    F                  Return register
         DS   0F
*
TC_LEN   DS    H                  Terminal input length
*
         DS   0F
TC_DATA  DS   0CL09               TC input
TC_ZCNC  DS    CL04               ZCNC transaction ID
         DS    CL01               comma
TC_TRAN  DS    CL04               zCache transaction ID
TC_L     EQU   *-TC_DATA
*
WTO_LEN  DS    F                  WTO length
TD_LEN   DS    H                  Transient Data message length
*
         DS   0F
TD_DATA  DS   0CL74               TD/WTO output
TD_DATE  DS    CL10
         DS    CL01
TD_TIME  DS    CL08
         DS    CL01
TD_TRAN  DS    CL04
         DS    CL50
TD_L     EQU   *-TD_DATA
*
ER_LEN   DS    H                  Error/Invalid message length
*
         DS   0F
ER_DATA  DS   0CL73               Error/Invalid message
ER_TRAN  DS    CL04
         DS    CL69
ER_L     EQU   *-ER_DATA
*
*********************************************************************** 00000900
* Dynamic Storage Area (End)                                          * 00001000
*********************************************************************** 00000900
*
*********************************************************************** 00000900
* DFHCOMMAREA                                                         * 00001000
*********************************************************************** 00000900
DFHCA    DSECT                                                          00001200
CA_RC    DS    CL02               Return Code                           00001300
         DS    CL02               not used (alignment)                  00001300
CA_TRAN  DS    CL04               zCache Transaction ID
CA_L     EQU   *-CA_RC            DFHCA length
*
*********************************************************************** 00000900
* Control Section                                                     * 00001000
*********************************************************************** 00000900
ZNC      DFHEIENT CODEREG=(R12),DATAREG=R10,EIBREG=R11
ZNC      AMODE 31
ZNC      RMODE 31
         B     SYSDATE                 BRANCH AROUND LITERALS           00014700
         DC    CL08'ZNC  '                                              00014800
         DC    CL48' -- zCache Named Counter creation               '   00014900
         DC    CL08'        '                                           00015000
         DC    CL08'&SYSDATE'                                           00015100
         DC    CL08'        '                                           00015200
         DC    CL08'&SYSTIME'                                           00015300
SYSDATE  DS   0H                                                        00015400
*********************************************************************** 00000900
* Address DFHCOMMAREA                                                 * 00001000
* ABEND if the DFHCOMMAREA length is not the same as the DSECT.       * 00001000
*********************************************************************** 00000900
SY_0010  DS   0H                                                        00015900
         EXEC CICS ASSIGN APPLID(APPLID) SYSID(SYSID)                  X
               STARTCODE(STCODE) NOHANDLE
*
         MVC   Z_SUFFIX,C_SUFFIX       Move zCache NC suffix
*
         L     R9,DFHEICAP             Load DFHCOMMAREA address
         USING DFHCA,R9                ... tell assembler
         CLC   EIBCALEN,=H'0'          DFHCOMMAREA length zero?
         BC    B'1000',SY_0030         ... yes, RECEIVE terminal input
         LA    R1,CA_L                 Load DFHCOMMAREA length
         CH    R1,EIBCALEN             DFHCOMMAREA equal to DSECT?
         BC    B'1000',SY_0020         ... yes, continue
         EXEC CICS ABEND ABCODE('ZCNC') NOHANDLE
*********************************************************************** 00000900
* Set xxxx_ZCACHE using DFHCOMMAREA                                   * 00001000
*********************************************************************** 00000900
SY_0020  DS   0H                                                        00015900
         MVC   Z_TRAN,CA_TRAN          Move NC TranID from COMMAREA
         BC    B'1111',SY_0100         Continue process
*
*********************************************************************** 00000900
* Set xxxx_ZCACHE using terminal input.                               * 00001000
*********************************************************************** 00000900
SY_0030  DS   0H                                                        00015900
         LA    R1,TC_L                 Load maximum TC length
         STH   R1,TC_LEN               Save maximum TC length
*
         EXEC CICS RECEIVE INTO(TC_DATA) NOHANDLE
*
         CLC   TC_ZCNC,=C'ZCNC'        TransID in first four bytes?
         BC    B'0111',ER_0010         ... no,  invalid format
         MVC   Z_TRAN,TC_TRAN          Move NC TranID from TC input
         BC    B'1111',SY_0100         Continue process
*
*********************************************************************** 00000900
* Create xxxx_ZCACHE named counter.                                   * 00001000
*********************************************************************** 00000900
SY_0100  DS   0H                                                        00015900
         EXEC CICS DEFINE DCOUNTER(Z_NC)                               X
               VALUE  (C_VAL)                                          X
               MINIMUM(C_MIN)                                          X
               MAXIMUM(C_MAX)                                          X
               NOHANDLE
*
         OC    EIBRESP,EIBRESP         Normal response?
         BC    B'0111',ER_0020         ... no,  Duplicate Counter
*
         BAS   R14,SY_9000             WTO and WRITEQ TD
*********************************************************************** 00000900
* Send terminal response                                              * 00001000
*********************************************************************** 00000900
SY_0800  DS   0H                                                        00015900
         CLI   STCODE,C'T'             Terminal task?
         BC    B'0111',SY_0900         ... no,  bypass SEND
         EXEC CICS SEND FROM(TD_DATA) LENGTH(TD_LEN)                   X
               ERASE NOHANDLE
*********************************************************************** 00000900
* RETURN                                                              * 00001000
*********************************************************************** 00000900
SY_0900  DS   0H                                                        00015900
         EXEC CICS RETURN
*********************************************************************** 00000900
* Invalid terminal input                                              *
*********************************************************************** 00000900
ER_0010  DS   0H                                                        00015900
         LA    R1,TD_L                 Load TD message length
         STH   R1,TD_LEN               Save TD Message length
         EXEC CICS SEND FROM(MSG_0010) LENGTH(TD_LEN)                  X
               ERASE NOHANDLE
         BC   B'1111',SY_0900          Return to caller
*********************************************************************** 00000900
* Named Counter already defined                                       *
*********************************************************************** 00000900
ER_0020  DS   0H                                                        00015900
         MVC   ER_DATA,MSG_0020        Move template
         MVC   ER_TRAN,Z_TRAN          Move TransID
         LA    R1,ER_L                 Load ER message length
         STH   R1,ER_LEN               Save ER Message length
         EXEC CICS SEND FROM(ER_DATA)  LENGTH(ER_LEN)                  X
               ERASE NOHANDLE
         BC   B'1111',SY_0900          Return to caller
*********************************************************************** 00000900
* Format time stamp                                                   * 00001000
* Write TD Message                                                    * 00001000
* Issue WTO                                                           * 00001000
*********************************************************************** 00000900
SY_9000  DS   0H                                                        00015900
         ST    R14,BAS_REG             Save return register
*
         MVC   TD_DATA,MSG_TEXT        Set message text
         MVC   TD_TRAN,Z_TRAN          Move NC TranID
*
         EXEC CICS ASKTIME ABSTIME(ABSTIME) NOHANDLE
         EXEC CICS FORMATTIME ABSTIME(ABSTIME) YYYYMMDD(TD_DATE)       X
               TIME(TD_TIME)  DATESEP('/') TIMESEP(':') NOHANDLE
*
         LA    R1,TD_L                 Load TD message length
         STH   R1,TD_LEN               Save TD Message length
         ST    R1,WTO_LEN              WTO length
*
         EXEC CICS WRITEQ TD QUEUE('@tdq@') FROM(TD_DATA)               X
               LENGTH(TD_LEN) NOHANDLE
*
         BC    B'0000',SY_9100         Bypass WTO
*
         EXEC CICS WRITE OPERATOR TEXT(TD_DATA) TEXTLENGTH(WTO_LEN)    X
               ROUTECODES(WTO_RC) NUMROUTES(WTO_RC_L) EVENTUAL         X
               NOHANDLE
*********************************************************************** 00000900
* Label to bypass WTO                                                 * 00001000
*********************************************************************** 00000900
SY_9100  DS   0H                                                        00015900
         L     R14,BAS_REG             Load return register
         BCR   B'1111',R14             Return to caller
*
*                                                                       00051400
*********************************************************************** 00000900
* Literal Pool                                                        * 00001000
*********************************************************************** 00000900
         LTORG                                                          00075000
*                                                                       00075100
         DS   0F
*                                                                       00075100
         DS   0F
C_VAL    DC    XL08'1'
C_MIN    DC    XL08'1'
C_MAX    DC    XL08'00000000FFFFFFFF'
*_MAX    DC    XL08'38D7EA4C67FFF'
*                                                                       00075100
         DS   0F
C_SUFFIX DC    CL12'_ZCACHE  '
         DS   0F
*                                                                       00075100
MSG_0010 DC   0CL74
         DC    CL25'Invalid format.  Must be '
         DC    CL25'ZCNC,xxxx where xxxx is t'
         DC    CL24'he zCache Transaction ID'
         DS   0F
*                                                                       00075100
MSG_0020 DC   0CL74
         DC    CL25'xxxx_ZCACHE already defin'
         DC    CL25'ed.  You might want to co'
         DC    CL24'nsider checking DFHNC*  '
         DS   0F
*                                                                       00075100
MSG_TEXT DC   0CL74
         DC    CL25'YYYY/MM/DD HH:MM:SS tttt_'
         DC    CL25'ZCACHE - zCache Named Cou'
         DC    CL24'nter created            '
         DS   0F
WTO_RC_L DC    F'02'                   WTO Routecode length
WTO_RC   DC    XL02'0111'
         DS   0F
*
*********************************************************************** 00000900
* Register assignments                                                * 00001000
*********************************************************************** 00000900
         DS   0F                                                        00085100
R0       EQU   0                                                        00085200
R1       EQU   1                                                        00085300
R2       EQU   2                                                        00085400
R3       EQU   3                                                        00085500
R4       EQU   4                                                        00085600
R5       EQU   5                                                        00085700
R6       EQU   6                                                        00085800
R7       EQU   7                                                        00085900
R8       EQU   8                                                        00086000
R9       EQU   9                                                        00086100
R10      EQU   10                                                       00086200
R11      EQU   11                                                       00086300
R12      EQU   12                                                       00086400
R13      EQU   13                                                       00086500
R14      EQU   14                                                       00086600
R15      EQU   15                                                       00086700
*
         PRINT ON                                                       00087100
*********************************************************************** 00000900
* End of Program                                                      * 00001000
*********************************************************************** 00000900
         END   ZNC                                                      00087500