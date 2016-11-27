       CBL CICS("SP")                                                   00010000
       IDENTIFICATION DIVISION.                                         00020000
       PROGRAM-ID. ZECS001.                                             00030001
       AUTHOR.     Randy Frerking and Rich Jackson.                     00031001
      ***************************************************************** 00040000
      *                                                               * 00050000
      * z/OS Enterprise Caching Services.                             * 00060001
      *                                                               * 00070000
      * This program executes as a REST service.                      * 00080000
      * POST   - Create entry in   Cache.                             * 00090000
      * GET    - Read   entry from Cache.                             * 00100000
      * PUT    - Update entry in   Cache.                             * 00110000
      * DELETE - Delete entry from Cache.                             * 00120000
      *                                                               * 00130000
      * The KEY store will utilize VSAM/RLS.                          * 00140001
      * The FIEL/DATA store will utilize either a CICS Coupling       * 00150001
      * Facility (CFDT), VSAM/RLS or CICS Shared Data Table (SDT),    * 00150101
      * which is determined by the RDO FILE definition.               * 00151001
      *                                                               * 00160000
      * Date       UserID    Description                              * 00170001
      * ---------- --------  ---------------------------------------- * 00180001
      *                                                               * 00190000
      ***************************************************************** 00775001
       ENVIRONMENT DIVISION.                                            00780000
       DATA DIVISION.                                                   00790000
       WORKING-STORAGE SECTION.                                         00800000
                                                                        00810000
      ***************************************************************** 00820000
      * DEFINE LOCAL VARIABLES                                        * 00830000
      ***************************************************************** 00840000
       01  USERID                 PIC  X(08) VALUE SPACES.              00850000
       01  APPLID                 PIC  X(08) VALUE SPACES.              00860000
       01  SYSID                  PIC  X(04) VALUE SPACES.              00870000
       01  ST-CODE                PIC  X(02) VALUE SPACES.              00880000
       01  BINARY-ZEROES          PIC  X(01) VALUE LOW-VALUES.          00890001
       01  DUPLICATE-POST         PIC  X(01) VALUE LOW-VALUES.          00900001
       01  ZECS002                PIC  X(08) VALUE 'ZECS002 '.          00911001
       01  ZECS003                PIC  X(08) VALUE 'ZECS003 '.          00911101
       01  INTERNAL-KEY           PIC  X(08) VALUE LOW-VALUES.          00912001
       01  ZRECOVERY              PIC  X(10) VALUE '/zRecovery'.        00920001
       01  ZCOMPLETE              PIC  X(10) VALUE '/zComplete'.        00930001
       01  RESOURCES              PIC  X(10) VALUE '/resources'.        00940001
       01  REPLICATE              PIC  X(10) VALUE '/replicate'.        00950001
       01  DEPLICATE              PIC  X(10) VALUE '/deplicate'.        00960001
       01  CRLF                   PIC  X(02) VALUE X'0D25'.             00970401
       01  BINARY-ZERO            PIC  X(01) VALUE X'00'.               00970501
                                                                        00990000
       01  ZUIDSTCK               PIC  X(08) VALUE 'ZUIDSTCK'.          00991001
       01  THE-TOD                PIC  X(16) VALUE LOW-VALUES.          00992001
                                                                        00994001
       01  LINKAGE-ADDRESSES.                                           01000001
           02  CACHE-ADDRESS      USAGE POINTER.                        01010001
           02  CACHE-ADDRESS-X    REDEFINES CACHE-ADDRESS               01020001
                                  PIC S9(08) COMP.                      01030001
                                                                        01040000
           02  SAVE-ADDRESS       USAGE POINTER.                        01050001
           02  SAVE-ADDRESS-X     REDEFINES SAVE-ADDRESS                01060001
                                  PIC S9(08) COMP.                      01070001
                                                                        01088001
       01  GETMAIN-LENGTH         PIC S9(08) COMP VALUE ZEROES.         01090001
                                                                        01100000
       01  ZECS-COUNTER.                                                01110001
           02  NC-TRANID          PIC  X(04) VALUE 'ZC##'.              01120001
           02  FILLER             PIC  X(05) VALUE '_ZECS'.             01130001
           02  FILLER             PIC  X(07) VALUE SPACES.              01140001
                                                                        01150000
       01  FILLER.                                                      01160101
           02  ZECS-VALUE         PIC  9(16) COMP VALUE ZEROES.         01160201
           02  FILLER REDEFINES ZECS-VALUE.                             01160301
               05  FILLER         PIC  X(06).                           01160401
               05  ZECS-NC-HW     PIC  X(02).                           01160501
                                                                        01161001
       01  ZECS-INCREMENT         PIC  9(16) COMP VALUE  1.             01170001
       01  WEBRESP                PIC S9(08) COMP VALUE ZEROES.         01180001
       01  READ-RESP              PIC S9(08) COMP VALUE ZEROES.         01181001
       01  WRITE-RESP             PIC S9(08) COMP VALUE ZEROES.         01182001
       01  ETTL-STATUS            PIC S9(08) COMP VALUE ZEROES.         01183001
       01  ETTL-RESP              PIC S9(08) COMP VALUE ZEROES.         01184001
       01  SEVEN-DAYS             PIC S9(08) COMP VALUE 604800.         01190001
       01  TWENTY-FOUR-HOURS      PIC S9(08) COMP VALUE 86400.          01191001
       01  THIRTY-MINUTES         PIC S9(08) COMP VALUE 1800.           01200001
       01  FIVE-MINUTES           PIC S9(08) COMP VALUE 300.            01210001
       01  TWO-FIFTY-FIVE         PIC S9(08) COMP VALUE 255.            01211001
       01  THIRTY                 PIC S9(08) COMP VALUE 30.             01220001
       01  TWELVE                 PIC S9(08) COMP VALUE 12.             01230001
       01  TEN                    PIC S9(08) COMP VALUE 10.             01240001
       01  SEVEN                  PIC S9(08) COMP VALUE  7.             01250001
       01  SIX                    PIC S9(08) COMP VALUE  6.             01250101
       01  FIVE                   PIC S9(08) COMP VALUE  5.             01251001
       01  TWO                    PIC S9(08) COMP VALUE  2.             01260001
       01  ONE                    PIC S9(08) COMP VALUE  1.             01270001
       01  HTTP-NAME-LENGTH       PIC S9(08) COMP VALUE ZEROES.         01280001
       01  HTTP-VALUE-LENGTH      PIC S9(08) COMP VALUE ZEROES.         01290001
       01  CLIENT-CONVERT         PIC S9(08) COMP VALUE ZEROES.         01291001
                                                                        01300000
       01  HTTP-HEADER            PIC  X(13) VALUE 'Authorization'.     01310001
       01  HTTP-HEADER-VALUE      PIC  X(64) VALUE SPACES.              01320001
                                                                        01330000
       01  HEADER-ACAO.                                                 01331001
           02  FILLER             PIC  X(16) VALUE 'Access-Control-A'.  01332001
           02  FILLER             PIC  X(11) VALUE 'llow-Origin'.       01333001
                                                                        01334001
       01  HEADER-ACAO-LENGTH     PIC S9(08) COMP VALUE 27.             01335001
                                                                        01336001
       01  VALUE-ACAO             PIC  X(01) VALUE '*'.                 01337001
       01  VALUE-ACAO-LENGTH      PIC S9(08) COMP VALUE 01.             01338001
                                                                        01339001
       01  ZECS003-COMM-AREA.                                           01340001
           02  CA-TYPE            PIC  X(03) VALUE 'ADR'.               01350001
           02  CA-URI-FIELD-01    PIC  X(10) VALUE SPACES.              01350101
                                                                        01351001
       01  ZECS002-COMM-AREA.                                           01352001
           02  CA-RETURN-CODE     PIC  X(02) VALUE '00'.                01353001
           02  FILLER             PIC  X(02) VALUE SPACES.              01360001
           02  CA-USERID          PIC  X(08) VALUE SPACES.              01370001
           02  CA-PASSWORD        PIC  X(08) VALUE SPACES.              01380001
           02  CA-ENCODE          PIC  X(24) VALUE SPACES.              01390001
           02  FILLER             PIC  X(04) VALUE SPACES.              01400001
           02  CA-DECODE          PIC  X(18) VALUE SPACES.              01410001
                                                                        01470000
       01  HTTP-STATUS-200        PIC S9(04) COMP VALUE 200.            01480000
       01  HTTP-STATUS-201        PIC S9(04) COMP VALUE 201.            01490001
       01  HTTP-STATUS-204        PIC S9(04) COMP VALUE 204.            01491001
       01  HTTP-STATUS-400        PIC S9(04) COMP VALUE 400.            01500001
       01  HTTP-STATUS-401        PIC S9(04) COMP VALUE 401.            01510001
       01  HTTP-STATUS-409        PIC S9(04) COMP VALUE 409.            01511001
       01  HTTP-STATUS-507        PIC S9(04) COMP VALUE 507.            01520001
                                                                        01530000
       01  HTTP-201-TEXT          PIC  X(32) VALUE SPACES.              01561001
       01  HTTP-201-LENGTH        PIC S9(08) COMP VALUE 32.             01562001
                                                                        01562101
       01  HTTP-204-TEXT          PIC  X(24) VALUE SPACES.              01562201
       01  HTTP-204-LENGTH        PIC S9(08) COMP VALUE ZEROES.         01562301
                                                                        01563001
       01  HTTP-400-TEXT          PIC  X(32) VALUE SPACES.              01570001
       01  HTTP-400-LENGTH        PIC S9(08) COMP VALUE 32.             01580001
                                                                        01590000
       01  HTTP-409-TEXT          PIC  X(32) VALUE SPACES.              01590101
       01  HTTP-409-LENGTH        PIC S9(08) COMP VALUE 32.             01590201
                                                                        01590301
       01  HTTP-507-TEXT          PIC  X(24) VALUE SPACES.              01591001
       01  HTTP-507-LENGTH        PIC S9(08) COMP VALUE ZEROES.         01592001
                                                                        01593001
       01  HTTP-OK                PIC  X(02) VALUE 'OK'.                01600000
       01  HTTP-NOT-FOUND         PIC  X(16) VALUE 'Record not found'.  01610000
       01  HTTP-KEY-ERROR         PIC  X(16) VALUE 'ZCxxKEY  error'.    01623001
       01  HTTP-FILE-ERROR        PIC  X(16) VALUE 'ZCxxFILE error'.    01630001
                                                                        01650000
       01  FILLER.                                                      01660001
           02  HTTP-ABSTIME       PIC  9(15) VALUE ZEROES.              01670001
                                                                        01680000
       01  HTTP-NOT-FOUND-LENGTH  PIC S9(08) COMP VALUE 16.             01700001
       01  HTTP-KEY-LENGTH        PIC S9(08) COMP VALUE 16.             01710001
       01  HTTP-FILE-LENGTH       PIC S9(08) COMP VALUE 16.             01720001
       01  HTTP-ABSTIME-LENGTH    PIC S9(08) COMP VALUE 15.             01730001
                                                                        01740000
       01  TEXT-ANYTHING          PIC  X(04) VALUE 'text'.              01750001
       01  TEXT-PLAIN             PIC  X(56) VALUE 'text/plain'.        01751001
       01  TEXT-HTML              PIC  X(56) VALUE 'text/html'.         01760000
       01  APPLICATION-XML        PIC  X(56) VALUE 'application/xml'.   01761001
                                                                        01770000
       01  THE-URI.                                                     01771001
           02  URI-TRANID         PIC  X(04) VALUE SPACES.              01771101
           02  FILLER             PIC  X(04) VALUE SPACES.              01771201
                                                                        01771301
       01  URI-USERID             PIC  X(08) VALUE SPACES.              01771401
       01  AUTHENTICATE           PIC  X(01) VALUE SPACES.              01772001
       01  USER-ACCESS            PIC  X(01) VALUE SPACES.              01772101
       01  PROCESS-COMPLETE       PIC  X(01) VALUE SPACES.              01772201
       01  ZF-SUCCESSFUL          PIC  X(01) VALUE SPACES.              01772301
                                                                        01801001
       01  HTTP-WEB-ERROR.                                              01801501
           02  FILLER             PIC  X(16) VALUE 'WEB RECEIVE erro'.  01801601
           02  FILLER             PIC  X(16) VALUE 'r               '.  01801701
                                                                        01801801
       01  HTTP-KEY-PLUS.                                               01802001
           02  FILLER             PIC  X(16) VALUE 'Key exceeds maxi'.  01803001
           02  FILLER             PIC  X(16) VALUE 'mum 255 bytes   '.  01804001
                                                                        01810000
       01  HTTP-KEY-ZERO.                                               01810101
           02  FILLER             PIC  X(16) VALUE 'Key must be grea'.  01810201
           02  FILLER             PIC  X(16) VALUE 'ter than 0 bytes'.  01810301
                                                                        01810401
       01  HTTP-INVALID-URI.                                            01810501
           02  FILLER             PIC  X(16) VALUE 'Invalid URI form'.  01810601
           02  FILLER             PIC  X(16) VALUE 'at              '.  01810701
                                                                        01810801
       01  HTTP-AUTH-ERROR.                                             01811001
           02  FILLER             PIC  X(16) VALUE 'Basic Authentica'.  01812001
           02  FILLER             PIC  X(16) VALUE 'tion failed     '.  01813001
                                                                        01814001
       01  HTTP-CONFLICT.                                               01815001
           02  FILLER             PIC  X(16) VALUE 'POST/PUT conflic'.  01816001
           02  FILLER             PIC  X(16) VALUE 't with DELETE   '.  01817001
                                                                        01818001
       01  HTTP-NOT-EXPIRED.                                            01819001
           02  FILLER             PIC  X(16) VALUE 'Record has not e'.  01819101
           02  FILLER             PIC  X(16) VALUE 'xpired.         '.  01819201
                                                                        01819301
       01  CURRENT-ABS            PIC S9(15) VALUE ZEROES COMP-3.       01820001
       01  RELATIVE-TIME          PIC S9(15) VALUE ZEROES COMP-3.       01830001
                                                                        01840001
       01  TTL-MILLISECONDS       PIC S9(15) VALUE ZEROES COMP-3.       01850001
       01  FILLER.                                                      01860001
           02  TTL-SEC-MS.                                              01870001
               03  TTL-SECONDS    PIC  9(06) VALUE ZEROES.              01880001
               03  FILLER         PIC  9(03) VALUE ZEROES.              01890001
           02  FILLER REDEFINES TTL-SEC-MS.                             01900001
               03  TTL-TIME       PIC  9(09).                           01910001
                                                                        01920000
       01  URI-FIELD-00           PIC  X(01).                           01930001
       01  URI-FIELD-01           PIC  X(64).                           01940000
       01  URI-FIELD-02           PIC  X(64).                           01950000
       01  URI-FIELD-03           PIC  X(64).                           01960000
       01  URI-FIELD-04           PIC  X(64).                           01970000
       01  URI-KEY                PIC X(255) VALUE LOW-VALUES.          01980001
       01  URI-KEY-LENGTH         PIC S9(08) COMP VALUE ZEROES.         01981001
       01  URI-PATH-POINTER       PIC S9(08) COMP VALUE ZEROES.         01982001
       01  URI-PATH-LENGTH        PIC S9(08) COMP VALUE ZEROES.         01983001
                                                                        01990000
       01  WEB-MEDIA-TYPE         PIC  X(56).                           02010000
       01  SPACE-COUNTER          PIC S9(04) COMP VALUE ZEROES.         02020000
       01  SLASH-COUNTER          PIC S9(04) COMP VALUE ZEROES.         02030001
       01  SLASH                  PIC  X(01) VALUE '/'.                 02040001
       01  EQUAL-SIGN             PIC  X(01) VALUE '='.                 02050001
       01  QUERY-TEXT             PIC  X(10) VALUE SPACES.              02060001
       01  CLEAR-TEXT             PIC  X(01) VALUE SPACES.              02061001
                                                                        02070000
       01  TTL-TYPE               PIC  X(03) VALUE SPACES.              02080001
       01  LAST-ACCESS-TIME       PIC  X(03) VALUE 'LAT'.               02090001
       01  LAST-UPDATE-TIME       PIC  X(03) VALUE 'LUT'.               02091001
                                                                        02100000
       01  CONTAINER-LENGTH       PIC S9(08) COMP VALUE ZEROES.         02110000
       01  SEND-LENGTH            PIC S9(08) COMP VALUE ZEROES.         02120001
       01  RECEIVE-LENGTH         PIC S9(08) COMP VALUE 3200000.        02130001
       01  MAXIMUM-LENGTH         PIC S9(08) COMP VALUE 3200000.        02140001
       01  THREE-POINT-TWO-MB     PIC S9(08) COMP VALUE 3200000.        02150001
       01  THIRTY-TWO-KB          PIC S9(08) COMP VALUE 32000.          02160001
       01  MAX-SEGMENT-COUNT      PIC S9(08) COMP VALUE ZEROES.         02170001
       01  SEGMENT-COUNT          PIC S9(08) COMP VALUE ZEROES.         02180001
       01  SEGMENT-REMAINDER      PIC S9(08) COMP VALUE ZEROES.         02190001
       01  UNSEGMENTED-LENGTH     PIC S9(08) COMP VALUE ZEROES.         02200001
       01  SEND-ACTION            PIC S9(08) COMP VALUE ZEROES.         02210001
                                                                        02220000
       01  ZECS-CONTAINER         PIC  X(16) VALUE 'ZECS_CONTAINER'.    02230001
       01  ZECS-CHANNEL           PIC  X(16) VALUE 'ZECS_CHANNEL'.      02240001
                                                                        02260000
       01  WEB-METHOD             PIC S9(08) COMP VALUE ZEROES.         02270000
       01  WEB-SCHEME             PIC S9(08) COMP VALUE ZEROES.         02280000
       01  WEB-HOST-LENGTH        PIC S9(08) COMP VALUE 120.            02290000
       01  WEB-HTTPMETHOD-LENGTH  PIC S9(08) COMP VALUE 10.             02300000
       01  WEB-HTTPVERSION-LENGTH PIC S9(08) COMP VALUE 15.             02310000
       01  WEB-PATH-LENGTH        PIC S9(08) COMP VALUE 512.            02320001
       01  WEB-QUERYSTRING-LENGTH PIC S9(08) COMP VALUE 256.            02330000
       01  WEB-REQUESTTYPE        PIC S9(08) COMP VALUE ZEROES.         02340000
       01  WEB-PORT               PIC S9(08) COMP VALUE ZEROES.         02350000
       01  WEB-PORT-NUMBER        PIC  9(05)      VALUE ZEROES.         02360000
                                                                        02370000
       01  WEB-HTTPMETHOD         PIC  X(10) VALUE SPACES.              02380000
       01  WEB-HTTP-PUT           PIC  X(10) VALUE 'PUT'.               02390000
       01  WEB-HTTP-GET           PIC  X(10) VALUE 'GET'.               02400000
       01  WEB-HTTP-POST          PIC  X(10) VALUE 'POST'.              02410000
       01  WEB-HTTP-DELETE        PIC  X(10) VALUE 'DELETE'.            02420000
                                                                        02430000
       01  WEB-HTTPVERSION        PIC  X(15) VALUE SPACES.              02440000
                                                                        02450000
       01  WEB-HOST               PIC X(120) VALUE SPACES.              02460000
       01  WEB-PATH               PIC X(512) VALUE LOW-VALUES.          02470001
       01  WEB-QUERYSTRING        PIC X(256) VALUE SPACES.              02480000
                                                                        02490000
       01  FC-READ                PIC  X(07) VALUE 'READ   '.           02500001
       01  FC-WRITE               PIC  X(07) VALUE 'WRITE  '.           02510001
       01  FC-REWRITE             PIC  X(07) VALUE 'REWRITE'.           02511001
       01  TD-QUEUE               PIC  X(04) VALUE '@tdq@'.             02520001
       01  TD-LENGTH              PIC S9(04) COMP VALUE ZEROES.         02530001
                                                                        02540000
       01  TD-RECORD.                                                   02550001
           02  TD-DATE            PIC  X(10).                           02560001
           02  FILLER             PIC  X(01) VALUE SPACES.              02570001
           02  TD-TIME            PIC  X(08).                           02580001
           02  FILLER             PIC  X(01) VALUE SPACES.              02590001
           02  TD-TRANID          PIC  X(04).                           02600001
           02  FILLER             PIC  X(01) VALUE SPACES.              02610001
           02  TD-MESSAGE         PIC  X(90) VALUE SPACES.              02620001
                                                                        02630001
       01  NO-SPACE-MESSAGE       PIC  X(08) VALUE ' NOSPACE'.          02631001
                                                                        02632001
       01  50702-MESSAGE.                                               02634001
           02  FILLER             PIC  X(16) VALUE 'GET/READ primary'.  02635001
           02  FILLER             PIC  X(16) VALUE ' key references '.  02636001
           02  FILLER             PIC  X(16) VALUE 'an internal key '.  02637001
           02  FILLER             PIC  X(16) VALUE 'on *FILE that do'.  02638001
           02  FILLER             PIC  X(16) VALUE 'es not exist:   '.  02639001
           02  FILLER             PIC  X(02) VALUE SPACES.              02639101
           02  50702-KEY          PIC  X(08) VALUE 'xxxxxxxx'.          02639201
                                                                        02639301
       01  FILE-ERROR.                                                  02640001
           02  FE-DS              PIC  X(08) VALUE SPACES.              02650001
           02  FILLER             PIC  X(07) VALUE ' error '.           02651001
           02  FILLER             PIC  X(07) VALUE 'EIBFN: '.           02660001
           02  FE-FN              PIC  X(07) VALUE SPACES.              02670001
           02  FILLER             PIC  X(10) VALUE ' EIBRESP: '.        02680001
           02  FE-RESP            PIC  9(08) VALUE ZEROES.              02690001
           02  FILLER             PIC  X(11) VALUE ' EIBRESP2: '.       02700001
           02  FE-RESP2           PIC  9(08) VALUE ZEROES.              02710001
           02  FILLER             PIC  X(12) VALUE ' Paragraph: '.      02720001
           02  FE-PARAGRAPH       PIC  X(04) VALUE SPACES.              02730001
           02  FE-NOSPACE         PIC  X(08) VALUE SPACES.              02740001
           02  FILLER REDEFINES FE-NOSPACE.                             02741001
               05  FE-RCODE       PIC  X(06).                           02742001
               05  FILLER         PIC  X(02).                           02743001
                                                                        02750000
       01  KEY-ERROR.                                                   02760001
           02  KE-DS              PIC  X(08) VALUE SPACES.              02761001
           02  FILLER             PIC  X(07) VALUE ' error '.           02762001
           02  FILLER             PIC  X(07) VALUE 'EIBFN: '.           02780001
           02  KE-FN              PIC  X(07) VALUE SPACES.              02790001
           02  FILLER             PIC  X(10) VALUE ' EIBRESP: '.        02800001
           02  KE-RESP            PIC  9(08) VALUE ZEROES.              02810001
           02  FILLER             PIC  X(11) VALUE ' EIBRESP2: '.       02820001
           02  KE-RESP2           PIC  9(08) VALUE ZEROES.              02830001
           02  FILLER             PIC  X(12) VALUE ' Paragraph: '.      02840001
           02  KE-PARAGRAPH       PIC  X(04) VALUE SPACES.              02850001
           02  KE-NOSPACE         PIC  X(08) VALUE SPACES.              02860001
                                                                        02870000
       01  WEB-ERROR.                                                   02880001
           02  FILLER             PIC  X(14) VALUE 'WEB RECEIVE er'.    02890001
           02  FILLER             PIC  X(07) VALUE 'ror -- '.           02900001
           02  FILLER             PIC  X(10) VALUE ' EIBRESP: '.        02910001
           02  WEB-RESP           PIC  9(08) VALUE ZEROES.              02920001
           02  FILLER             PIC  X(11) VALUE ' EIBRESP2: '.       02930001
           02  WEB-RESP2          PIC  9(08) VALUE ZEROES.              02940001
           02  FILLER             PIC  X(32) VALUE SPACES.              02950001
                                                                        02960000
      ***************************************************************** 02961001
      * Security Definition                                           * 02962001
      ***************************************************************** 02963001
       01  SD-RESP                PIC S9(08) COMP.                      02970001
       01  SD-INDEX               PIC S9(08) COMP.                      02970101
       01  SD-LENGTH              PIC S9(08) COMP.                      02970201
                                                                        02970301
       01  SD-SELECT              PIC  X(06) VALUE 'SELECT'.            02970401
       01  SD-UPDATE              PIC  X(06) VALUE 'UPDATE'.            02970501
       01  SD-DELETE              PIC  X(06) VALUE 'DELETE'.            02970601
                                                                        02970701
       01  SD-TOKEN               PIC  X(16) VALUE SPACES.              02970801
       01  ZECS-SD.                                                     02970901
           02  SD-TRANID          PIC  X(04) VALUE 'ZC##'.              02971001
           02  SD-TYPE            PIC  X(02) VALUE 'SD'.                02971101
           02  FILLER             PIC  X(42) VALUE SPACES.              02971201
                                                                        02971301
       01  SD-DSECT.                                                    02971401
           02  SD-TABLE        OCCURS    63 TIMES.                      02971501
               05  FILLER         PIC  X(05).                           02971601
               05  SD-USER-ID     PIC  X(08).                           02971701
               05  SD-COMMA       PIC  X(01).                           02971801
               05  SD-ACCESS      PIC  X(06).                           02971901
               05  SD-CRLF        PIC  X(02).                           02972001
                                                                        02972101
      ***************************************************************** 02972201
      * LAT support enabled via PROGRAM definition.                   * 02972301
      ***************************************************************** 02972401
       01  LAT-PROGRAM.                                                 02972501
           02  LAT-TRANID         PIC  X(04) VALUE 'ZC##'.              02972601
           02  LAT-ID             PIC  X(03) VALUE 'LAT'.               02972701
           02  FILLER             PIC  X(01) VALUE SPACES.              02972801
                                                                        02972901
      ***************************************************************** 02973001
      * Extended TTL support enabled via PROGRAM definition.          * 02973101
      ***************************************************************** 02973201
       01  ETTL-PROGRAM.                                                02973301
           02  ETTL-TRANID        PIC  X(04) VALUE 'ZC##'.              02973401
           02  ETTL-ID            PIC  X(04) VALUE 'ETTL'.              02973501
                                                                        02973701
       01  THE-OTHER-DC-LENGTH    PIC S9(08) COMP VALUE ZEROES.         03010101
                                                                        03010201
       01  DC-TOKEN               PIC  X(16) VALUE SPACES.              03011001
       01  DC-LENGTH              PIC S9(08) COMP VALUE ZEROES.         03012001
       01  ZECS-DC.                                                     03020001
           02  DC-TRANID          PIC  X(04) VALUE 'ZC##'.              03030001
           02  FILLER             PIC  X(02) VALUE 'DC'.                03040001
           02  FILLER             PIC  X(42) VALUE SPACES.              03050001
                                                                        03060000
       01  DC-CONTROL.                                                  03072001
           02  FILLER             PIC  X(06).                           03080001
           02  DC-TYPE            PIC  X(02) VALUE SPACES.              03090001
           02  DC-CRLF            PIC  X(02).                           03100001
           02  THE-OTHER-DC       PIC X(160) VALUE SPACES.              03110001
           02  FILLER             PIC  X(02).                           03120001
                                                                        03130000
       01  ACTIVE-SINGLE          PIC  X(02) VALUE 'A1'.                03140001
       01  ACTIVE-ACTIVE          PIC  X(02) VALUE 'AA'.                03150001
       01  ACTIVE-STANDBY         PIC  X(02) VALUE 'AS'.                03160001
                                                                        03170001
       01  SESSION-TOKEN          PIC  9(18) COMP VALUE ZEROES.         03171001
                                                                        03172001
       01  URL-SCHEME-NAME        PIC  X(16) VALUE SPACES.              03180001
       01  URL-SCHEME             PIC S9(08) COMP VALUE ZEROES.         03190001
       01  URL-PORT               PIC S9(08) COMP VALUE ZEROES.         03200001
       01  URL-HOST-NAME          PIC  X(80) VALUE SPACES.              03210001
       01  URL-HOST-NAME-LENGTH   PIC S9(08) COMP VALUE 80.             03220001
       01  WEB-STATUS-CODE        PIC S9(04) COMP VALUE 00.             03230001
       01  WEB-STATUS-LENGTH      PIC S9(08) COMP VALUE 24.             03240001
       01  WEB-STATUS-TEXT        PIC  X(24) VALUE SPACES.              03250001
                                                                        03260000
       01  CONVERSE-LENGTH        PIC S9(08) COMP VALUE 40.             03270001
       01  CONVERSE-RESPONSE      PIC  X(40) VALUE SPACES.              03280001
                                                                        03290000
       01  ZK-FCT.                                                      03300001
           02  ZK-TRANID          PIC  X(04) VALUE 'ZC##'.              03310001
           02  FILLER             PIC  X(04) VALUE 'KEY '.              03320001
                                                                        03330000
       01  ZF-FCT.                                                      03340001
           02  ZF-TRANID          PIC  X(04) VALUE 'ZC##'.              03350001
           02  FILLER             PIC  X(04) VALUE 'FILE'.              03360001
                                                                        03370000
       01  ZK-LENGTH              PIC S9(04) COMP VALUE ZEROES.         03380000
       01  ZF-LENGTH              PIC S9(04) COMP VALUE ZEROES.         03390000
       01  DELETE-LENGTH          PIC S9(04) COMP VALUE 8.              03400001
                                                                        03410000
      ***************************************************************** 03411001
      * zECS KEY  record definition.                                  * 03412001
      ***************************************************************** 03413001
       COPY ZECSZKC.                                                    03414001
                                                                        03470000
      ***************************************************************** 03470101
      * zECS FILE record definition.                                  * 03470201
      ***************************************************************** 03470301
       COPY ZECSZFC.                                                    03470401
                                                                        03640000
       01  DELETE-RECORD.                                               03650001
           02  DELETE-KEY-16.                                           03660001
               05  DELETE-KEY     PIC  X(08).                           03670001
               05  DELETE-SEGMENT PIC  9(04) VALUE ZEROES COMP.         03680001
               05  DELETE-SUFFIX  PIC  9(04) VALUE ZEROES COMP.         03690001
               05  DELETE-ZEROES  PIC  9(08) VALUE ZEROES COMP.         03700001
                                                                        03710000
       01  CACHE-LENGTH           PIC S9(08) COMP VALUE ZEROES.         03720001
                                                                        03730000
      ***************************************************************** 03740000
      * Dynamic Storage                                               * 03750000
      ***************************************************************** 03760000
       LINKAGE SECTION.                                                 03770000
       01  DFHCOMMAREA            PIC  X(01).                           03780000
                                                                        03790000
      ***************************************************************** 03866701
      * Cache message.                                                * 03866801
      * This is the complete message, which is then stored in Cache   * 03866901
      * as record segments.                                           * 03867001
      ***************************************************************** 03867101
       01  CACHE-MESSAGE          PIC  X(32000).                        03867201
                                                                        03868001
       PROCEDURE DIVISION.                                              03870000
                                                                        03880000
      ***************************************************************** 03890000
      * Main process.                                                 * 03900000
      ***************************************************************** 03910000
           PERFORM 1000-ACCESS-PARMS       THRU 1000-EXIT.              03920000
           PERFORM 2000-PROCESS-REQUEST    THRU 2000-EXIT.              03930000
           PERFORM 9000-RETURN             THRU 9000-EXIT.              03940000
                                                                        03950000
      ***************************************************************** 03960000
      * Access parms.                                                 * 03970000
      ***************************************************************** 03980000
       1000-ACCESS-PARMS.                                               03990000
                                                                        04000001
           EXEC CICS WEB EXTRACT                                        04010000
                SCHEME(WEB-SCHEME)                                      04020000
                HOST(WEB-HOST)                                          04030000
                HOSTLENGTH(WEB-HOST-LENGTH)                             04040000
                HTTPMETHOD(WEB-HTTPMETHOD)                              04050000
                METHODLENGTH(WEB-HTTPMETHOD-LENGTH)                     04060000
                HTTPVERSION(WEB-HTTPVERSION)                            04070000
                VERSIONLEN(WEB-HTTPVERSION-LENGTH)                      04080000
                PATH(WEB-PATH)                                          04090000
                PATHLENGTH(WEB-PATH-LENGTH)                             04100000
                PORTNUMBER(WEB-PORT)                                    04110000
                QUERYSTRING(WEB-QUERYSTRING)                            04120000
                QUERYSTRLEN(WEB-QUERYSTRING-LENGTH)                     04130000
                REQUESTTYPE(WEB-REQUESTTYPE)                            04140000
                NOHANDLE                                                04150000
           END-EXEC.                                                    04160000
                                                                        04170001
           IF  WEB-PATH(1:10) EQUAL RESOURCES                           04180001
               PERFORM 1200-VALIDATION        THRU 1200-EXIT            04190001
               IF  AUTHENTICATE EQUAL 'Y'                               04200001
                   PERFORM 1500-AUTHENTICATE  THRU 1500-EXIT            04210001
                   PERFORM 1600-USER-ACCESS   THRU 1600-EXIT.           04211001
                                                                        04220001
           MOVE WEB-PORT TO WEB-PORT-NUMBER.                            04390000
                                                                        04400000
           IF  WEB-PATH-LENGTH GREATER THAN ZEROES                      04410000
               PERFORM 1100-PARSE-URI  THRU 1100-EXIT                   04411001
                   WITH TEST AFTER                                      04412001
                   VARYING URI-PATH-POINTER FROM  1 BY 1                04413001
                   UNTIL   URI-PATH-POINTER EQUAL TO WEB-PATH-LENGTH    04414001
                   OR      SLASH-COUNTER    EQUAL FIVE                  04415001
                                                                        04415101
               PERFORM 1150-CHECK-URI  THRU 1150-EXIT                   04416001
               PERFORM 1160-MOVE-URI   THRU 1160-EXIT                   04416101
                                                                        04416201
               UNSTRING WEB-PATH(1:WEB-PATH-LENGTH)                     04420000
               DELIMITED BY ALL '/'                                     04430001
               INTO URI-FIELD-00                                        04440001
                    URI-FIELD-01                                        04450000
                    URI-FIELD-02                                        04460000
                    URI-FIELD-03                                        04470000
                    URI-FIELD-04.                                       04480001
                                                                        04500000
           PERFORM 1300-QUERY-STRING          THRU 1300-EXIT.           04501001
                                                                        04502001
      ***************************************************************** 04510000
      * Sending payload on a GET or DELETE is not permitted.          * 04520001
      * Sending payload is only permitted on POST or PUT.             * 04530001
      * POST and PUT will be handled the same.                        * 04540001
      ***************************************************************** 04550000
                                                                        04560000
           IF  WEB-HTTPMETHOD EQUAL WEB-HTTP-POST  OR                   04570000
               WEB-HTTPMETHOD EQUAL WEB-HTTP-PUT                        04580001
                                                                        04590000
      ***************************************************************** 04680000
      * Converted RECEIVE from TOCONTAINER to INTO because the        * 04690001
      * TOCONTAINER option causes conversion of the content.          * 04700001
      * Convert INTO to SET to support 3.2MB messages.                * 04710001
      * When MEDIATYPE is 'text/*' or 'application/xml', convert the  * 04711001
      * data, as this information is accessed by both zEnterprise     * 04712001
      * applications and those in darkness (Unix/Linux based).        * 04713001
      ***************************************************************** 04720000
                                                                        04730000
               EXEC CICS WEB RECEIVE                                    04740000
                    SET(CACHE-ADDRESS)                                  04750001
                    LENGTH(RECEIVE-LENGTH)                              04760001
                    MAXLENGTH(MAXIMUM-LENGTH)                           04770001
                    NOSRVCONVERT                                        04780001
                    MEDIATYPE(WEB-MEDIA-TYPE)                           04790000
                    RESP(WEBRESP)                                       04800001
                    NOHANDLE                                            04801001
               END-EXEC                                                 04810001
                                                                        04820000
               IF  WEB-MEDIA-TYPE(1:04) EQUAL TEXT-ANYTHING    OR       04820101
                   WEB-MEDIA-TYPE(1:15) EQUAL APPLICATION-XML           04820201
                   EXEC CICS WEB RECEIVE                                04821001
                        SET(CACHE-ADDRESS)                              04822001
                        LENGTH(RECEIVE-LENGTH)                          04823001
                        MAXLENGTH(MAXIMUM-LENGTH)                       04824001
                        SRVCONVERT                                      04825001
                        MEDIATYPE(WEB-MEDIA-TYPE)                       04826001
                        RESP(WEBRESP)                                   04826101
                        NOHANDLE                                        04827001
                   END-EXEC.                                            04828001
                                                                        04829001
           IF  WEBRESP NOT EQUAL DFHRESP(NORMAL)    OR                  04830101
               RECEIVE-LENGTH EQUAL ZEROES                              04831001
               PERFORM 9300-WEB-ERROR     THRU 9300-EXIT                04840001
               MOVE HTTP-WEB-ERROR          TO HTTP-400-TEXT            04850001
               PERFORM 9400-STATUS-400    THRU 9400-EXIT                04870001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               04880001
                                                                        04890000
           MOVE EIBTRNID(3:2)               TO NC-TRANID(3:2).          04900001
           MOVE EIBTRNID(3:2)               TO ZK-TRANID(3:2).          04910001
           MOVE EIBTRNID(3:2)               TO ZF-TRANID(3:2).          04920001
           MOVE EIBTRNID(3:2)               TO DC-TRANID(3:2).          04930001
                                                                        04940000
       1000-EXIT.                                                       04950000
           EXIT.                                                        04960000
                                                                        04970000
      ***************************************************************** 04980000
      * Parse WEB-PATH to determine length of path prefix preceeding  * 04990001
      * the URI-KEY.  This will be used to determine the URI-KEY      * 04991001
      * length which is used on the UNSTRING command.  Without the    * 04992001
      * URI-KEY length, the UNSTRING command pads the URI-KEY with    * 04993001
      * spaces.  The URI-KEY needs to be padded with low-values to    * 04994001
      * allow zECS to support KEY search patterns.                    * 04995001
      ***************************************************************** 05000000
       1100-PARSE-URI.                                                  05010001
           ADD ONE     TO URI-PATH-LENGTH.                              05010101
           IF  WEB-PATH(URI-PATH-POINTER:1) EQUAL SLASH                 05010201
               ADD ONE TO SLASH-COUNTER.                                05010301
                                                                        05010401
       1100-EXIT.                                                       05010501
           EXIT.                                                        05010601
                                                                        05010701
      ***************************************************************** 05010801
      * Check URI for the correct number of slashes.                  * 05010901
      * /resources/datacaches/BU_SBU/application/key                  * 05011001
      * There must be five, otherwise reject with STATUS(400).        * 05011101
      ***************************************************************** 05011501
       1150-CHECK-URI.                                                  05011601
           IF  SLASH-COUNTER NOT EQUAL FIVE                             05011701
               MOVE HTTP-INVALID-URI        TO HTTP-400-TEXT            05011801
               PERFORM 9400-STATUS-400    THRU 9400-EXIT                05011901
               PERFORM 9000-RETURN        THRU 9000-EXIT.               05012001
                                                                        05012101
       1150-EXIT.                                                       05012201
           EXIT.                                                        05012301
                                                                        05012401
      ***************************************************************** 05012501
      * Move URI key when present.                                    * 05012601
      * When ?clear=* is present, the key is ignored.  In this case,  * 05012701
      * a URI key is probably not be present.                         * 05012801
      ***************************************************************** 05012901
       1160-MOVE-URI.                                                   05013001
           SUBTRACT   URI-PATH-POINTER  FROM  WEB-PATH-LENGTH           05013101
               GIVING URI-PATH-LENGTH.                                  05013201
                                                                        05013301
           IF  URI-PATH-LENGTH GREATER THAN TWO-FIFTY-FIVE              05013401
               MOVE HTTP-KEY-PLUS           TO HTTP-400-TEXT            05013501
               PERFORM 9400-STATUS-400    THRU 9400-EXIT                05013601
               PERFORM 9000-RETURN        THRU 9000-EXIT.               05013701
                                                                        05013801
           ADD  ONE   TO URI-PATH-POINTER.                              05013901
           IF  URI-PATH-LENGTH GREATER THAN ZEROES                      05014001
               MOVE WEB-PATH(URI-PATH-POINTER:URI-PATH-LENGTH)          05014101
               TO   URI-KEY(1:URI-PATH-LENGTH).                         05014201
                                                                        05014301
       1160-EXIT.                                                       05014401
           EXIT.                                                        05014501
                                                                        05014601
      ***************************************************************** 05014701
      * Basic Authentication is optional.                             * 05014801
      * When HTTP,  Basic Authentication is not performed.            * 05014901
      * When HTTPS, Basic Authentication is perform when the security * 05015001
      * model (ZCxxSD) is defined.                                    * 05015101
      ***************************************************************** 05015301
       1200-VALIDATION.                                                 05015401
           MOVE 'Y'                    TO AUTHENTICATE.                 05015501
                                                                        05015601
           IF  WEB-SCHEME EQUAL DFHVALUE(HTTP)                          05016701
               MOVE 'N'                TO AUTHENTICATE.                 05016801
                                                                        05016901
           IF  WEB-SCHEME EQUAL DFHVALUE(HTTPS)                         05017001
               PERFORM 1210-ZCXXSD   THRU 1210-EXIT.                    05017101
                                                                        05017201
       1200-EXIT.                                                       05017301
           EXIT.                                                        05017401
                                                                        05017501
      ***************************************************************** 05017601
      * Access Security Model as a document template.                 * 05017701
      ***************************************************************** 05017801
       1210-ZCXXSD.                                                     05017901
           MOVE EIBTRNID               TO SD-TRANID.                    05018101
                                                                        05018201
           EXEC CICS DOCUMENT CREATE DOCTOKEN(SD-TOKEN)                 05018301
                TEMPLATE(ZECS-SD)                                       05018401
                RESP(SD-RESP)                                           05018501
                NOHANDLE                                                05018601
           END-EXEC.                                                    05018701
                                                                        05018801
           MOVE LENGTH OF SD-DSECT     TO SD-LENGTH.                    05018901
                                                                        05019001
           IF  SD-RESP EQUAL DFHRESP(NORMAL)                            05019101
               EXEC CICS DOCUMENT RETRIEVE DOCTOKEN(SD-TOKEN)           05019201
                    INTO     (SD-DSECT)                                 05019301
                    LENGTH   (SD-LENGTH)                                05019401
                    MAXLENGTH(SD-LENGTH)                                05019501
                    DATAONLY                                            05019601
                    NOHANDLE                                            05019701
               END-EXEC.                                                05019801
                                                                        05019901
           IF  SD-RESP NOT EQUAL DFHRESP(NORMAL)                        05020001
               MOVE 'N'                TO AUTHENTICATE.                 05020201
                                                                        05020501
       1210-EXIT.                                                       05020601
           EXIT.                                                        05020701
                                                                        05020801
                                                                        05020901
      ***************************************************************** 05021001
      * Process query string.                                         * 05021101
      * In this paragraph, all special processing must be handled in  * 05021201
      * one of the PERFORM statements and must XCTL from the zECS     * 05021301
      * service program.  After special processing has been checked,  * 05021401
      * this paragraph will check the KEY length as determined in the * 05021501
      * 1160-MOVE-URI paragraph.  If the KEY length (URI-PATH-LENGTH) * 05021601
      * is zero, then issue a 400 status code, as the key must be     * 05021701
      * provided on all non-special processing.                       * 05021801
      ***************************************************************** 05021901
       1300-QUERY-STRING.                                               05022001
           IF  WEB-HTTPMETHOD EQUAL WEB-HTTP-POST    OR                 05022101
               WEB-HTTPMETHOD EQUAL WEB-HTTP-PUT                        05022201
               PERFORM 1310-TTL          THRU 1310-EXIT.                05022301
                                                                        05022401
           IF  WEB-HTTPMETHOD EQUAL WEB-HTTP-DELETE                     05022501
               PERFORM 1320-CLEAR        THRU 1320-EXIT.                05022601
                                                                        05022701
           IF  URI-PATH-LENGTH EQUAL ZEROES                             05022801
               MOVE HTTP-KEY-ZERO          TO HTTP-400-TEXT             05022901
               PERFORM 9400-STATUS-400   THRU 9400-EXIT                 05023001
               PERFORM 9000-RETURN       THRU 9000-EXIT.                05023101
                                                                        05023201
       1300-EXIT.                                                       05023301
           EXIT.                                                        05023401
                                                                        05023501
      ***************************************************************** 05023601
      * Process TTL query string for POST/PUT.                        * 05023701
      ***************************************************************** 05023801
       1310-TTL.                                                        05023901
           MOVE THIRTY-MINUTES         TO ZF-TTL.                       05024001
                                                                        05024101
           IF WEB-QUERYSTRING-LENGTH > +0                               05024201
               UNSTRING WEB-QUERYSTRING(1:WEB-QUERYSTRING-LENGTH)       05024301
               DELIMITED BY ALL '='                                     05024401
               INTO QUERY-TEXT                                          05024501
                    TTL-SECONDS                                         05024601
               IF  TTL-SECONDS NUMERIC                                  05024701
                   MOVE TTL-SECONDS    TO ZF-TTL.                       05024801
                                                                        05024901
           IF  ZF-TTL LESS THAN FIVE-MINUTES                            05025001
               MOVE FIVE-MINUTES       TO ZF-TTL.                       05025101
                                                                        05025201
           PERFORM 1312-CHECK-ETTL   THRU 1312-EXIT.                    05025301
                                                                        05025401
           IF  ZF-TTL GREATER THAN TWENTY-FOUR-HOURS                    05025501
               IF  ETTL-RESP   NOT EQUAL DFHRESP(NORMAL)                05025601
               OR  ETTL-STATUS     EQUAL DFHVALUE(DISABLED)             05025701
                   MOVE TWENTY-FOUR-HOURS  TO ZF-TTL.                   05025801
                                                                        05025901
           IF  ZF-TTL GREATER THAN SEVEN-DAYS                           05026001
               IF  ETTL-RESP       EQUAL DFHRESP(NORMAL)                05026101
               OR  ETTL-STATUS     EQUAL DFHVALUE(ENABLED)              05026201
                   MOVE SEVEN-DAYS         TO ZF-TTL.                   05026301
                                                                        05026401
       1310-EXIT.                                                       05026501
           EXIT.                                                        05026601
                                                                        05026701
      ***************************************************************** 05026801
      * Check for extended TTL (ETTL) enable/disable.                 * 05026901
      * Extended TTL support enabled via PROGRAM definition.          * 05027001
      ***************************************************************** 05027101
       1312-CHECK-ETTL.                                                 05027201
           MOVE EIBTRNID                   TO ETTL-TRANID.              05027301
           EXEC CICS INQUIRE                                            05027401
                PROGRAM(ETTL-PROGRAM)                                   05027501
                STATUS (ETTL-STATUS)                                    05027601
                RESP   (ETTL-RESP)                                      05027701
                NOHANDLE                                                05027801
           END-EXEC.                                                    05027901
                                                                        05028001
       1312-EXIT.                                                       05028101
           EXIT.                                                        05028201
                                                                        05028301
      ***************************************************************** 05028401
      * Process CLEAR query string for DELETE.                        * 05028501
      * When CLEAR is set to '*' only, XCTL to ZECS003.               * 05028601
      ***************************************************************** 05028701
       1320-CLEAR.                                                      05028801
           IF WEB-QUERYSTRING-LENGTH EQUAL SEVEN                        05028901
               UNSTRING WEB-QUERYSTRING(1:WEB-QUERYSTRING-LENGTH)       05029001
               DELIMITED BY ALL '='                                     05029101
               INTO QUERY-TEXT                                          05029201
                    CLEAR-TEXT                                          05029301
               PERFORM 1325-CLEAR-TYPE     THRU 1325-EXIT               05029401
               IF  CLEAR-TEXT EQUAL '*'                                 05029501
                   EXEC CICS XCTL PROGRAM(ZECS003)                      05029601
                        COMMAREA(ZECS003-COMM-AREA)                     05029701
                        NOHANDLE                                        05029801
                   END-EXEC.                                            05029901
                                                                        05030001
       1320-EXIT.                                                       05030101
           EXIT.                                                        05030201
                                                                        05030301
      ***************************************************************** 05030401
      * Extract CLEAR type from URIMAP.                               * 05030501
      ***************************************************************** 05030601
       1325-CLEAR-TYPE.                                                 05030701
           UNSTRING URI-FIELD-04                                        05030801
               DELIMITED BY ALL '.'                                     05030901
               INTO URI-FIELD-00                                        05031001
                    CA-TYPE.                                            05031101
                                                                        05031201
           MOVE WEB-PATH(1:10) TO CA-URI-FIELD-01.                      05031401
                                                                        05031501
       1325-EXIT.                                                       05031601
           EXIT.                                                        05031701
                                                                        05031801
      ***************************************************************** 05031901
      * LINK to ZECS002 to perform Basic Authentication.              * 05032001
      ***************************************************************** 05032101
       1500-AUTHENTICATE.                                               05032201
           MOVE LENGTH OF HTTP-HEADER       TO HTTP-NAME-LENGTH.        05032301
           MOVE LENGTH OF HTTP-HEADER-VALUE TO HTTP-VALUE-LENGTH.       05033001
                                                                        05040000
           EXEC CICS WEB READ HTTPHEADER(HTTP-HEADER)                   05050001
                NAMELENGTH(HTTP-NAME-LENGTH)                            05060001
                VALUE(HTTP-HEADER-VALUE)                                05070001
                VALUELENGTH(HTTP-VALUE-LENGTH)                          05080001
                NOHANDLE                                                05090001
           END-EXEC.                                                    05100001
                                                                        05110000
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)                        05120001
               PERFORM 9600-AUTH-ERROR     THRU 9600-EXIT               05130001
               PERFORM 9000-RETURN         THRU 9000-EXIT.              05140001
                                                                        05150001
           IF  HTTP-VALUE-LENGTH GREATER THAN SIX                       05160001
               MOVE HTTP-HEADER-VALUE(7:24) TO CA-ENCODE                05170001
               EXEC CICS LINK PROGRAM(ZECS002)                          05180001
                    COMMAREA(ZECS002-COMM-AREA)                         05190001
                    NOHANDLE                                            05200001
               END-EXEC                                                 05210001
                                                                        05220001
               IF  CA-RETURN-CODE NOT EQUAL '00'                        05230001
                   PERFORM 9600-AUTH-ERROR THRU 9600-EXIT               05240001
                   PERFORM 9000-RETURN     THRU 9000-EXIT.              05250001
                                                                        05260000
           IF  HTTP-VALUE-LENGTH EQUAL        SIX   OR                  05270001
               HTTP-VALUE-LENGTH LESS THAN    SIX                       05280001
                   PERFORM 9600-AUTH-ERROR THRU 9600-EXIT               05290001
                   PERFORM 9000-RETURN     THRU 9000-EXIT.              05300001
                                                                        05310001
       1500-EXIT.                                                       05320000
           EXIT.                                                        05330000
                                                                        05340000
      ***************************************************************** 05341001
      * Verify the UserID in the Basic Authentication header is in    * 05342001
      * the ZCxxSD security definition.                               * 05342101
      ***************************************************************** 05343001
       1600-USER-ACCESS.                                                05344001
           MOVE 'N' TO USER-ACCESS.                                     05344101
                                                                        05344201
           PERFORM 1610-SCAN-ZCXXSD        THRU 1610-EXIT               05345001
               WITH TEST AFTER                                          05346001
               VARYING SD-INDEX FROM 1 BY 1                             05346101
               UNTIL   SD-INDEX    EQUAL 20  OR                         05346201
                       USER-ACCESS EQUAL 'Y' OR                         05346301
                       SD-LENGTH   EQUAL ZEROES.                        05346401
                                                                        05347001
           IF  USER-ACCESS = 'N'                                        05347101
               PERFORM 9600-AUTH-ERROR     THRU 9600-EXIT               05347201
               PERFORM 9000-RETURN         THRU 9000-EXIT.              05347301
                                                                        05347401
       1600-EXIT.                                                       05348001
           EXIT.                                                        05348101
                                                                        05349001
      ***************************************************************** 05349101
      * Scan Security Model (ZCxxSD) until UserID and Access match.   * 05349201
      ***************************************************************** 05349301
       1610-SCAN-ZCXXSD.                                                05349401
           IF  SD-USER-ID(SD-INDEX) EQUAL CA-USERID                     05349501
               IF  SD-ACCESS(SD-INDEX) EQUAL SD-SELECT                  05349601
                   IF  WEB-HTTPMETHOD  EQUAL WEB-HTTP-GET               05349701
                   MOVE 'Y' TO USER-ACCESS.                             05349801
                                                                        05349901
           IF  SD-USER-ID(SD-INDEX) EQUAL CA-USERID                     05350001
               IF  SD-ACCESS(SD-INDEX) EQUAL SD-UPDATE                  05350101
                   IF  WEB-HTTPMETHOD  EQUAL WEB-HTTP-PUT               05350201
                   MOVE 'Y' TO USER-ACCESS.                             05350301
                                                                        05350401
           IF  SD-USER-ID(SD-INDEX) EQUAL CA-USERID                     05350501
               IF  SD-ACCESS(SD-INDEX) EQUAL SD-UPDATE                  05350601
                   IF  WEB-HTTPMETHOD  EQUAL WEB-HTTP-POST              05350701
                   MOVE 'Y' TO USER-ACCESS.                             05350801
                                                                        05350901
           IF  SD-USER-ID(SD-INDEX) EQUAL CA-USERID                     05351001
               IF  SD-ACCESS(SD-INDEX) EQUAL SD-DELETE                  05351101
                   IF  WEB-HTTPMETHOD  EQUAL WEB-HTTP-DELETE            05351201
                   MOVE 'Y' TO USER-ACCESS.                             05351301
                                                                        05351401
           SUBTRACT LENGTH OF SD-TABLE FROM SD-LENGTH.                  05351501
                                                                        05351601
       1610-EXIT.                                                       05351701
           EXIT.                                                        05351801
                                                                        05351901
      ***************************************************************** 05352001
      * Process HTTP request.                                         * 05360000
      ***************************************************************** 05370000
       2000-PROCESS-REQUEST.                                            05380000
           IF  WEB-HTTPMETHOD EQUAL WEB-HTTP-GET                        05390000
               PERFORM 3000-READ-CACHE     THRU 3000-EXIT               05400001
               PERFORM 3600-SEND-RESPONSE  THRU 3600-EXIT.              05420001
                                                                        05430000
           IF  WEB-HTTPMETHOD EQUAL WEB-HTTP-POST     OR                05440000
               WEB-HTTPMETHOD EQUAL WEB-HTTP-PUT                        05450001
               PERFORM 4000-GET-COUNTER    THRU 4000-EXIT               05460001
               PERFORM 4100-READ-KEY       THRU 4100-EXIT               05470001
               PERFORM 4200-PROCESS-FILE   THRU 4200-EXIT               05480001
               PERFORM 4300-SEND-RESPONSE  THRU 4300-EXIT.              05490001
                                                                        05500000
           IF  WEB-HTTPMETHOD EQUAL WEB-HTTP-DELETE                     05510000
               PERFORM 5000-READ-KEY       THRU 5000-EXIT               05520001
               PERFORM 5100-DELETE-KEY     THRU 5100-EXIT               05530001
               PERFORM 5200-DELETE-FILE    THRU 5200-EXIT               05540001
                       WITH TEST AFTER                                  05540101
                       VARYING ZF-SEGMENT  FROM 1 BY 1                  05541001
                       UNTIL EIBRESP NOT EQUAL DFHRESP(NORMAL)          05542001
               PERFORM 5300-SEND-RESPONSE  THRU 5300-EXIT.              05550001
                                                                        05560000
       2000-EXIT.                                                       05570000
           EXIT.                                                        05580000
                                                                        05590000
      ***************************************************************** 05600000
      * HTTP GET.                                                     * 05610000
      * Perform the READ process.                                     * 05620001
      ***************************************************************** 05630000
       3000-READ-CACHE.                                                 05640001
           PERFORM 3100-READ-PROCESS   THRU 3100-EXIT                   05640201
               WITH TEST AFTER                                          05640301
               UNTIL PROCESS-COMPLETE  EQUAL 'Y'.                       05640401
       3000-EXIT.                                                       05641101
           EXIT.                                                        05641201
                                                                        05641401
      ***************************************************************** 05641501
      * HTTP GET.                                                     * 05641601
      *                                                               * 05641701
      * Read the primary key store (ZK), which contains the secondary * 05641801
      * or 'file' key.                                                * 05641901
      *                                                               * 05642001
      * Read the secondary file store (ZF), which contains the cached * 05642101
      * data as record segments.                                      * 05642201
      ***************************************************************** 05642301
       3100-READ-PROCESS.                                               05642401
           MOVE 'Y'                          TO PROCESS-COMPLETE.       05642601
           PERFORM 3200-READ-KEY           THRU 3200-EXIT.              05642701
           PERFORM 3300-READ-FILE          THRU 3300-EXIT.              05642801
           IF  ZF-SUCCESSFUL EQUAL 'Y'                                  05642901
               PERFORM 3400-STAGE          THRU 3400-EXIT.              05643001
       3100-EXIT.                                                       05643201
           EXIT.                                                        05643301
                                                                        05643401
      ***************************************************************** 05643501
      * HTTP GET.                                                     * 05643601
      * Read KEY structure.                                           * 05644001
      ***************************************************************** 05645001
       3200-READ-KEY.                                                   05646001
                                                                        05650000
           MOVE URI-KEY TO ZK-KEY.                                      05660000
           MOVE LENGTH  OF ZK-RECORD TO ZK-LENGTH.                      05670000
                                                                        05680000
           EXEC CICS READ FILE(ZK-FCT)                                  05690000
                INTO(ZK-RECORD)                                         05700000
                RIDFLD(ZK-KEY)                                          05710000
                LENGTH(ZK-LENGTH)                                       05720000
                NOHANDLE                                                05730000
           END-EXEC.                                                    05740000
                                                                        05750000
           IF  EIBRESP     EQUAL DFHRESP(NOTFND)                        05760001
               MOVE HTTP-NOT-FOUND          TO HTTP-204-TEXT            05770001
               MOVE HTTP-NOT-FOUND-LENGTH   TO HTTP-204-LENGTH          05780001
               PERFORM 9700-STATUS-204    THRU 9700-EXIT                05790001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               05800000
                                                                        05810000
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)                        05810101
               MOVE '3200'                  TO KE-PARAGRAPH             05810201
               MOVE FC-READ                 TO KE-FN                    05810301
               PERFORM 9200-KEY-ERROR     THRU 9200-EXIT                05810401
               MOVE EIBDS(1:8)              TO HTTP-KEY-ERROR(1:8)      05810501
               MOVE HTTP-KEY-ERROR          TO HTTP-507-TEXT            05810601
               MOVE HTTP-KEY-LENGTH         TO HTTP-507-LENGTH          05810701
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                05810801
               PERFORM 9000-RETURN        THRU 9000-EXIT.               05810901
                                                                        05811001
      ***************************************************************** 05811101
      * When the KEY structure points to an internal FILE structure   * 05811201
      * that does not exist, one of two conditions has occurred:      * 05811301
      *                                                               * 05811401
      * 1).  KEY and/or FILE VSAM definition specifies LOG(NONE).     * 05811501
      *      When a zECS request doesn't complete, due to region      * 05811601
      *      or client termination, rollback does not occur, causing  * 05811701
      *      inconsistent KEY/FILE pointers.                          * 05811801
      * 2).  Expiration process is in progress for a KEY/FILE record. * 05811901
      *      When a zECS record is being expired, zEXPIRE browses     * 05812001
      *      FILE structure for TTL.  When an expired record is found * 05812101
      *      zEXPIRE issues a DELETE for each FILE entry, then issues * 05812201
      *      the DELETE for the KEY entry, causing an expiration      * 05812301
      *      'in progress'.                                           * 05812401
      *                                                               * 05812501
      * Both of the conditions will now return HTTP status 204 and    * 05812601
      * HTTP status text '204 Record not found'.  The error message   * 05812701
      * to TD-QUEUE will no longer be written, as both conditions will *05812801
      * ultimately be resolved by zEXPIRE deleting both KEY and FILE  * 05812901
      * structures when a FILE entry TTL has exceed the limit.        * 05813001
      *                                                               * 05813101
      ***************************************************************** 05813201
           IF  ZK-ZF-KEY EQUAL INTERNAL-KEY                             05813301
               MOVE HTTP-NOT-FOUND          TO HTTP-204-TEXT            05813401
               MOVE HTTP-NOT-FOUND-LENGTH   TO HTTP-204-LENGTH          05813501
               PERFORM 9700-STATUS-204    THRU 9700-EXIT                05813601
               PERFORM 9000-RETURN        THRU 9000-EXIT.               05813701
                                                                        05813801
       3200-EXIT.                                                       05820001
           EXIT.                                                        05830000
                                                                        05840000
      ***************************************************************** 05850000
      * HTTP GET.                                                     * 05860000
      * Read FILE structure.                                          * 05870001
      * Only update access timestamp when LAT is present in the URI.  * 05880000
      * A logical record can span one hundred physical records.       * 05890001
      ***************************************************************** 05900000
       3300-READ-FILE.                                                  05910001
           MOVE 'Y'                     TO ZF-SUCCESSFUL.               05911001
                                                                        05912001
           UNSTRING URI-FIELD-04                                        05920001
               DELIMITED BY ALL '.'                                     05930001
               INTO URI-FIELD-00                                        05940001
                    TTL-TYPE.                                           05950001
                                                                        05960000
           MOVE ZK-ZF-KEY               TO ZF-KEY.                      05970000
           MOVE ZEROES                  TO ZF-ZEROES.                   05980000
           MOVE LENGTH OF ZF-RECORD     TO ZF-LENGTH.                   05990000
                                                                        06000000
           IF  ZK-SEGMENTS EQUAL 'Y'                                    06010001
               MOVE ONE                 TO ZF-SEGMENT.                  06020001
                                                                        06030000
           IF  TTL-TYPE EQUAL LAST-ACCESS-TIME                          06040001
               MOVE EIBTRNID  TO LAT-TRANID                             06040101
               EXEC CICS INQUIRE PROGRAM(LAT-PROGRAM)                   06040201
                    NOHANDLE                                            06040301
               END-EXEC                                                 06040401
               IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)                    06040501
                   MOVE LAST-UPDATE-TIME TO TTL-TYPE.                   06040601
                                                                        06041001
           IF  TTL-TYPE EQUAL LAST-ACCESS-TIME                          06042001
               EXEC CICS READ FILE(ZF-FCT)                              06050000
                    INTO(ZF-RECORD)                                     06060000
                    RIDFLD(ZF-KEY-16)                                   06070000
                    LENGTH(ZF-LENGTH)                                   06080000
                    UPDATE                                              06090000
                    NOHANDLE                                            06100000
               END-EXEC                                                 06110000
                                                                        06120000
               PERFORM 9950-ABS  THRU 9950-EXIT                         06130000
                                                                        06140000
               MOVE FC-REWRITE     TO FE-FN                             06141001
                                                                        06142001
               EXEC CICS REWRITE FILE(ZF-FCT)                           06150000
                    FROM(ZF-RECORD)                                     06160000
                    LENGTH(ZF-LENGTH)                                   06170000
                    NOHANDLE                                            06180000
               END-EXEC                                                 06190000
           ELSE                                                         06200001
               MOVE FC-READ        TO FE-FN                             06201001
               EXEC CICS READ FILE(ZF-FCT)                              06210001
                    INTO(ZF-RECORD)                                     06220001
                    RIDFLD(ZF-KEY-16)                                   06230001
                    LENGTH(ZF-LENGTH)                                   06240001
                    NOHANDLE                                            06250001
               END-EXEC.                                                06260001
                                                                        06270000
           IF  EIBRESP EQUAL DFHRESP(NOTFND)                            06350101
               MOVE ZK-ZF-KEY                TO INTERNAL-KEY            06350201
               MOVE 'N'                      TO PROCESS-COMPLETE        06350301
               MOVE 'N'                      TO ZF-SUCCESSFUL.          06350401
                                                                        06350501
           IF  EIBRESP EQUAL DFHRESP(NOTFND) OR                         06350601
               EIBRESP EQUAL DFHRESP(NORMAL)                            06350701
               NEXT SENTENCE                                            06350801
           ELSE                                                         06350901
               MOVE FC-READ                 TO FE-FN                    06351001
               MOVE '3300'                  TO FE-PARAGRAPH             06351101
               PERFORM 9100-FILE-ERROR    THRU 9100-EXIT                06351201
               MOVE EIBDS(1:8)              TO HTTP-FILE-ERROR(1:8)     06351301
               MOVE HTTP-FILE-ERROR         TO HTTP-507-TEXT            06351401
               MOVE HTTP-FILE-LENGTH        TO HTTP-507-LENGTH          06351501
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                06351601
               PERFORM 9000-RETURN        THRU 9000-EXIT.               06351701
                                                                        06351801
           IF  EIBRESP EQUAL DFHRESP(NORMAL)                            06351901
               PERFORM 3310-CHECK-TTL     THRU 3310-EXIT.               06352001
                                                                        06352301
       3300-EXIT.                                                       06352401
           EXIT.                                                        06352501
                                                                        06353001
      ***************************************************************** 06360001
      * Check for expired TTL.                                        * 06370001
      ***************************************************************** 06450001
       3310-CHECK-TTL.                                                  06451001
           EXEC CICS ASKTIME ABSTIME(CURRENT-ABS) NOHANDLE              06451101
           END-EXEC.                                                    06451201
                                                                        06451301
           MOVE ZF-TTL                      TO TTL-SECONDS.             06451401
           MOVE TTL-TIME                    TO TTL-MILLISECONDS.        06451501
                                                                        06451601
           SUBTRACT ZF-ABS FROM CURRENT-ABS GIVING RELATIVE-TIME.       06451701
           IF  RELATIVE-TIME GREATER THAN TTL-MILLISECONDS              06451801
               MOVE HTTP-NOT-FOUND          TO HTTP-204-TEXT            06452001
               MOVE HTTP-NOT-FOUND-LENGTH   TO HTTP-204-LENGTH          06452101
               PERFORM 9700-STATUS-204    THRU 9700-EXIT                06452201
               PERFORM 5100-DELETE-KEY    THRU 5100-EXIT                06452301
               PERFORM 5200-DELETE-FILE   THRU 5200-EXIT                06452401
                       WITH TEST AFTER                                  06452501
                       VARYING ZF-SEGMENT FROM 1 BY 1                   06452601
                       UNTIL EIBRESP NOT EQUAL DFHRESP(NORMAL)          06452701
               PERFORM 9000-RETURN        THRU 9000-EXIT.               06453101
                                                                        06453301
       3310-EXIT.                                                       06453401
           EXIT.                                                        06453501
                                                                        06453601
      ***************************************************************** 06453701
      * Issue GETMAIN only when multiple segments.                    * 06458001
      * When the logical record is a single segment, set the          * 06459001
      * CACHE-MESSAGE buffer in the LINKAGE SECTION to the record     * 06459101
      * buffer address.                                               * 06459201
      ***************************************************************** 06459301
       3400-STAGE.                                                      06459401
           IF  ZF-SEGMENT EQUAL ZEROES                                  06459501
               MOVE ONE                      TO ZF-SEGMENT.             06459601
                                                                        06459701
           IF  ZF-SEGMENTS EQUAL ONE                                    06490001
               SUBTRACT ZF-PREFIX          FROM ZF-LENGTH               06500001
               SET  ADDRESS OF CACHE-MESSAGE TO ADDRESS OF ZF-DATA.     06510001
                                                                        06520001
           IF  ZF-SEGMENTS GREATER THAN ONE                             06530001
               MULTIPLY ZF-SEGMENTS BY THIRTY-TWO-KB                    06540001
                   GIVING GETMAIN-LENGTH                                06550001
                                                                        06560001
               EXEC CICS GETMAIN SET(CACHE-ADDRESS)                     06570001
                    FLENGTH(GETMAIN-LENGTH)                             06580001
                    INITIMG(BINARY-ZEROES)                              06590001
                    NOHANDLE                                            06600001
               END-EXEC                                                 06610001
                                                                        06620001
               SET ADDRESS OF CACHE-MESSAGE      TO CACHE-ADDRESS       06630001
               MOVE CACHE-ADDRESS-X              TO SAVE-ADDRESS-X      06640001
                                                                        06650001
               SUBTRACT ZF-PREFIX              FROM ZF-LENGTH           06660001
               MOVE ZF-DATA(1:ZF-LENGTH)         TO CACHE-MESSAGE       06670001
               ADD  ZF-LENGTH                    TO CACHE-ADDRESS-X.    06680001
                                                                        06690001
           ADD  ONE                              TO ZF-SEGMENT.         06700001
           MOVE ZF-LENGTH                        TO CACHE-LENGTH.       06710001
                                                                        06720001
           IF  ZF-SEGMENTS GREATER THAN ONE                             06730001
               PERFORM 3500-READ-SEGMENTS THRU 3500-EXIT                06740001
                   WITH TEST AFTER                                      06750001
                   UNTIL ZF-SEGMENT GREATER THAN ZF-SEGMENTS  OR        06760001
                         ZF-SUCCESSFUL EQUAL 'N'.                       06761001
                                                                        06762001
       3400-EXIT.                                                       06771001
           EXIT.                                                        06772001
                                                                        06773001
      ***************************************************************** 06810001
      * HTTP GET.                                                     * 06820001
      * Read FILE segment records.                                    * 06830001
      ***************************************************************** 06840001
       3500-READ-SEGMENTS.                                              06850001
           SET ADDRESS OF CACHE-MESSAGE          TO CACHE-ADDRESS.      06860001
           MOVE LENGTH OF ZF-RECORD              TO ZF-LENGTH.          06870001
                                                                        06880001
           EXEC CICS READ FILE(ZF-FCT)                                  06890001
                INTO(ZF-RECORD)                                         06900001
                RIDFLD(ZF-KEY-16)                                       06910001
                LENGTH(ZF-LENGTH)                                       06920001
                NOHANDLE                                                06930001
           END-EXEC.                                                    06940001
                                                                        06950001
           IF  EIBRESP EQUAL DFHRESP(NORMAL)                            06951001
               SUBTRACT ZF-PREFIX              FROM ZF-LENGTH           06951101
               MOVE ZF-DATA(1:ZF-LENGTH)         TO CACHE-MESSAGE       06951201
               ADD  ZF-LENGTH                    TO CACHE-ADDRESS-X     06951301
               ADD  ONE                          TO ZF-SEGMENT          06951401
               ADD  ZF-LENGTH                    TO CACHE-LENGTH.       06951501
                                                                        06951601
           IF  EIBRESP EQUAL DFHRESP(NOTFND)                            06951701
               MOVE ZK-ZF-KEY                TO INTERNAL-KEY            06951801
               MOVE 'N'                          TO PROCESS-COMPLETE    06961001
               MOVE 'N'                          TO ZF-SUCCESSFUL       06962001
               PERFORM 3510-FREEMAIN           THRU 3510-EXIT.          06963001
                                                                        07030001
                                                                        07080001
           IF  EIBRESP EQUAL DFHRESP(NOTFND) OR                         07090001
               EIBRESP EQUAL DFHRESP(NORMAL)                            07091001
               NEXT SENTENCE                                            07092001
           ELSE                                                         07093001
               MOVE FC-READ                 TO FE-FN                    07094001
               MOVE '3500'                  TO FE-PARAGRAPH             07095001
               PERFORM 9100-FILE-ERROR    THRU 9100-EXIT                07096001
               MOVE EIBDS(1:8)              TO HTTP-FILE-ERROR(1:8)     07096101
               MOVE HTTP-FILE-ERROR         TO HTTP-507-TEXT            07097001
               MOVE HTTP-FILE-LENGTH        TO HTTP-507-LENGTH          07098001
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                07099001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               07099101
                                                                        07099201
       3500-EXIT.                                                       07100001
           EXIT.                                                        07110001
                                                                        07120000
      ***************************************************************** 07130001
      * HTTP GET.                                                     * 07140001
      * FREEMAIN message segment buffer.                              * 07150001
      * This is required to reprocess a GET request after a key swap. * 07151001
      ***************************************************************** 07160001
       3510-FREEMAIN.                                                   07170001
           EXEC CICS FREEMAIN                                           07171001
                DATAPOINTER(SAVE-ADDRESS)                               07172001
                NOHANDLE                                                07174001
           END-EXEC.                                                    07175001
                                                                        07180001
       3510-EXIT.                                                       07180101
           EXIT.                                                        07180201
                                                                        07180301
      ***************************************************************** 07181001
      * HTTP GET.                                                     * 07182001
      * Send cached information.                                      * 07183001
      ***************************************************************** 07184001
       3600-SEND-RESPONSE.                                              07185001
                                                                        07186001
           IF  ZF-SEGMENTS EQUAL ONE                                    07190001
               SET ADDRESS OF CACHE-MESSAGE  TO ADDRESS OF ZF-DATA.     07200001
                                                                        07210001
           IF  ZF-SEGMENTS GREATER THAN ONE                             07220001
               SET ADDRESS OF CACHE-MESSAGE  TO SAVE-ADDRESS.           07230001
                                                                        07240001
           MOVE ZF-MEDIA         TO WEB-MEDIA-TYPE.                     07350000
                                                                        07360000
           IF  WEB-MEDIA-TYPE EQUAL SPACES                              07370001
               MOVE TEXT-PLAIN   TO WEB-MEDIA-TYPE.                     07380001
                                                                        07390000
           MOVE DFHVALUE(IMMEDIATE)    TO SEND-ACTION.                  07391001
                                                                        07392001
           INSPECT WEB-MEDIA-TYPE                                       07393001
           REPLACING ALL SPACES BY LOW-VALUES.                          07394001
                                                                        07395001
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    07395101
                                                                        07395201
           IF  WEB-MEDIA-TYPE(1:04) EQUAL TEXT-ANYTHING      OR         07396001
               WEB-MEDIA-TYPE(1:15) EQUAL APPLICATION-XML               07397001
               EXEC CICS WEB SEND                                       07400001
                    FROM      (CACHE-MESSAGE)                           07410001
                    FROMLENGTH(CACHE-LENGTH)                            07420001
                    MEDIATYPE (WEB-MEDIA-TYPE)                          07430001
                    STATUSCODE(HTTP-STATUS-200)                         07450001
                    STATUSTEXT(HTTP-OK)                                 07460001
                    ACTION    (SEND-ACTION)                             07461001
                    SRVCONVERT                                          07462001
                    NOHANDLE                                            07470001
               END-EXEC                                                 07480001
           ELSE                                                         07490001
               EXEC CICS WEB SEND                                       07491001
                    FROM      (CACHE-MESSAGE)                           07492001
                    FROMLENGTH(CACHE-LENGTH)                            07493001
                    MEDIATYPE (WEB-MEDIA-TYPE)                          07494001
                    STATUSCODE(HTTP-STATUS-200)                         07495001
                    STATUSTEXT(HTTP-OK)                                 07496001
                    ACTION    (SEND-ACTION)                             07497001
                    NOSRVCONVERT                                        07498001
                    NOHANDLE                                            07499001
               END-EXEC.                                                07499101
                                                                        07499201
       3600-EXIT.                                                       07500001
           EXIT.                                                        07510000
                                                                        07520000
      ***************************************************************** 07530000
      * HTTP POST/PUT.                                                * 07540000
      * Get counter, which is used as zECS FILE internal key.         * 07550001
      ***************************************************************** 07560000
       4000-GET-COUNTER.                                                07570000
           CALL ZUIDSTCK USING BY REFERENCE THE-TOD.                    07571001
                                                                        07580000
           EXEC CICS GET DCOUNTER(ZECS-COUNTER)                         07590001
                VALUE(ZECS-VALUE)                                       07600001
                INCREMENT(ZECS-INCREMENT)                               07610001
                WRAP                                                    07611001
                NOHANDLE                                                07620000
           END-EXEC.                                                    07630000
                                                                        07640000
       4000-EXIT.                                                       07650000
           EXIT.                                                        07660000
                                                                        07670000
      ***************************************************************** 07671001
      * HTTP POST/PUT.                                                * 07672001
      * Issue READ UPDATE for KEY structure.  If the record is not    * 07673001
      * found, issue WRITE.                                           * 07674001
      ***************************************************************** 07678001
       4100-READ-KEY.                                                   07679001
           MOVE URI-KEY TO ZK-KEY.                                      07679201
           MOVE LENGTH  OF ZK-RECORD TO ZK-LENGTH.                      07679301
                                                                        07679401
           EXEC CICS READ                                               07679501
                FILE  (ZK-FCT)                                          07679601
                INTO  (ZK-RECORD)                                       07679701
                RIDFLD(ZK-KEY)                                          07679801
                LENGTH(ZK-LENGTH)                                       07679901
                RESP  (READ-RESP)                                       07680001
                NOHANDLE                                                07680101
                UPDATE                                                  07680201
           END-EXEC.                                                    07680301
                                                                        07680401
           IF  READ-RESP EQUAL DFHRESP(NORMAL)                          07680501
               PERFORM 4110-PRIME-KEY     THRU 4110-EXIT.               07680601
                                                                        07680701
           IF  READ-RESP EQUAL DFHRESP(NOTFND)                          07680801
               PERFORM 4120-WRITE-KEY     THRU 4120-EXIT.               07680901
                                                                        07681001
           IF  READ-RESP NOT EQUAL DFHRESP(NORMAL)                      07681101
           AND READ-RESP NOT EQUAL DFHRESP(NOTFND)                      07681201
               MOVE '4100'                  TO KE-PARAGRAPH             07681301
               MOVE FC-READ                 TO KE-FN                    07681401
               PERFORM 9200-KEY-ERROR     THRU 9200-EXIT                07681501
               MOVE EIBDS(1:8)              TO HTTP-KEY-ERROR(1:8)      07681601
               MOVE HTTP-KEY-ERROR          TO HTTP-507-TEXT            07681701
               MOVE HTTP-KEY-LENGTH         TO HTTP-507-LENGTH          07681801
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                07681901
               PERFORM 9000-RETURN        THRU 9000-EXIT.               07682001
                                                                        07682101
       4100-EXIT.                                                       07682201
           EXIT.                                                        07682301
                                                                        07682401
      ***************************************************************** 07682501
      * HTTP POST/PUT.                                                * 07682601
      * Prime KEY structure record.                                   * 07682701
      ***************************************************************** 07682801
       4110-PRIME-KEY.                                                  07682901
                                                                        07683001
           MOVE ZK-ZF-KEY                   TO DELETE-KEY.              07683101
           MOVE ZEROES                      TO DELETE-ZEROES.           07683201
                                                                        07683301
           MOVE THE-TOD(1:6)                TO ZK-ZF-IDN.               07683401
           MOVE ZECS-NC-HW                  TO ZK-ZF-NC.                07683501
                                                                        07683601
           MOVE 'Y'                         TO ZK-SEGMENTS.             07684001
                                                                        07684101
       4110-EXIT.                                                       07684201
           EXIT.                                                        07684301
                                                                        07685000
      ***************************************************************** 07690000
      * HTTP POST/PUT.                                                * 07700000
      * Write KEY structure record.                                   * 07710001
      * If the WRITE receives a DUPREC, issue a READ for UPDATE and   * 07720001
      * process as a PUT request.  If the READ fails, issue a 409     * 07730001
      * indicating a DUPREC for the WRITE, as there has been a        * 07740001
      * conflict between POST/PUT and a DELETE request.               * 07750001
      ***************************************************************** 07770000
       4120-WRITE-KEY.                                                  07780001
           MOVE URI-KEY               TO ZK-KEY.                        07800000
                                                                        07801001
           MOVE THE-TOD(1:6)          TO ZK-ZF-IDN.                     07810001
           MOVE ZECS-NC-HW            TO ZK-ZF-NC.                      07811001
                                                                        07811101
           MOVE 'Y'                   TO ZK-SEGMENTS.                   07820001
           MOVE LENGTH OF ZK-RECORD   TO ZK-LENGTH.                     07830000
                                                                        07840000
           EXEC CICS WRITE                                              07850001
                FILE  (ZK-FCT)                                          07851001
                FROM  (ZK-RECORD)                                       07860001
                RIDFLD(ZK-KEY)                                          07870000
                LENGTH(ZK-LENGTH)                                       07880000
                RESP  (WRITE-RESP)                                      07890001
                NOHANDLE                                                07900000
           END-EXEC.                                                    07910000
                                                                        07920000
           IF  WRITE-RESP EQUAL DFHRESP(DUPREC)                         07930001
               PERFORM 4130-READ-KEY      THRU 4130-EXIT.               07940001
                                                                        07960101
           IF  WRITE-RESP NOT EQUAL DFHRESP(NORMAL)                     07961001
           AND WRITE-RESP NOT EQUAL DFHRESP(DUPREC)                     07962001
               MOVE '4120'                  TO KE-PARAGRAPH             07970001
               MOVE FC-WRITE                TO KE-FN                    07971001
               PERFORM 9200-KEY-ERROR     THRU 9200-EXIT                07980001
               MOVE EIBDS(1:8)              TO HTTP-KEY-ERROR(1:8)      07981001
               MOVE HTTP-KEY-ERROR          TO HTTP-507-TEXT            07990001
               MOVE HTTP-KEY-LENGTH         TO HTTP-507-LENGTH          08000001
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                08010001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               08020001
                                                                        08030000
       4120-EXIT.                                                       08040001
           EXIT.                                                        08050000
                                                                        08060000
      ***************************************************************** 08061001
      * HTTP POST/PUT.                                                * 08062001
      * The WRITE received a DUPREC.  Issue a READ and process as a   * 08064001
      * PUT requeset.  If the READ is NOTFND, issue a 409 to indicate * 08064101
      * DUPREC on the WRITE.                                          * 08065001
      ***************************************************************** 08069001
       4130-READ-KEY.                                                   08069101
           MOVE URI-KEY TO ZK-KEY.                                      08069201
           MOVE LENGTH  OF ZK-RECORD TO ZK-LENGTH.                      08069301
                                                                        08069401
           EXEC CICS READ                                               08069501
                FILE  (ZK-FCT)                                          08069601
                INTO  (ZK-RECORD)                                       08069701
                RIDFLD(ZK-KEY)                                          08069801
                LENGTH(ZK-LENGTH)                                       08069901
                RESP  (READ-RESP)                                       08070001
                NOHANDLE                                                08070101
                UPDATE                                                  08070201
           END-EXEC.                                                    08070301
                                                                        08070401
           IF  READ-RESP     EQUAL DFHRESP(NOTFND)                      08070901
               MOVE HTTP-CONFLICT           TO HTTP-409-TEXT            08071001
               PERFORM 9500-STATUS-409    THRU 9500-EXIT                08071601
               PERFORM 9000-RETURN        THRU 9000-EXIT.               08071701
                                                                        08071801
           IF  READ-RESP NOT EQUAL DFHRESP(NORMAL)                      08072001
               MOVE '4130'                  TO KE-PARAGRAPH             08072201
               MOVE FC-READ                 TO KE-FN                    08072301
               PERFORM 9200-KEY-ERROR     THRU 9200-EXIT                08072401
               MOVE EIBDS(1:8)              TO HTTP-KEY-ERROR(1:8)      08072501
               MOVE HTTP-KEY-ERROR          TO HTTP-507-TEXT            08072601
               MOVE HTTP-KEY-LENGTH         TO HTTP-507-LENGTH          08072701
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                08072801
               PERFORM 9000-RETURN        THRU 9000-EXIT.               08072901
                                                                        08073001
           PERFORM 4110-PRIME-KEY         THRU 4110-EXIT.               08073101
                                                                        08073201
       4130-EXIT.                                                       08073301
           EXIT.                                                        08073401
                                                                        08073501
      ***************************************************************** 08074001
      * HTTP POST/PUT.                                                * 08080000
      * Write FILE structure record                                   * 08090001
      ***************************************************************** 08190000
       4200-PROCESS-FILE.                                               08200001
           MOVE CACHE-ADDRESS-X             TO SAVE-ADDRESS-X.          08210001
                                                                        08220000
           MOVE URI-KEY                     TO ZF-ZK-KEY.               08230000
           MOVE ZK-ZF-KEY                   TO ZF-KEY.                  08240000
           MOVE ZEROES                      TO ZF-ZEROES.               08250000
           MOVE WEB-MEDIA-TYPE              TO ZF-MEDIA.                08260001
                                                                        08270000
           MOVE RECEIVE-LENGTH              TO UNSEGMENTED-LENGTH.      08280001
                                                                        08290000
           DIVIDE RECEIVE-LENGTH BY THIRTY-TWO-KB                       08300001
               GIVING    MAX-SEGMENT-COUNT                              08310001
               REMAINDER SEGMENT-REMAINDER.                             08320001
                                                                        08330000
           IF  SEGMENT-REMAINDER GREATER THAN ZEROES                    08340001
               ADD ONE TO MAX-SEGMENT-COUNT.                            08350001
                                                                        08360000
           MOVE MAX-SEGMENT-COUNT           TO ZF-SEGMENTS.             08370001
                                                                        08380000
           PERFORM 9950-ABS               THRU 9950-EXIT.               08390000
                                                                        08400000
           PERFORM 4400-WRITE-FILE        THRU 4400-EXIT                08410001
               WITH TEST AFTER                                          08420001
               VARYING SEGMENT-COUNT FROM 1 BY 1 UNTIL                  08430001
                       SEGMENT-COUNT EQUAL  MAX-SEGMENT-COUNT.          08440001
                                                                        08450000
           IF  READ-RESP EQUAL DFHRESP(NORMAL)                          08460001
               PERFORM 4500-UPDATE-KEY    THRU 4500-EXIT.               08470001
                                                                        08480000
       4200-EXIT.                                                       08490000
           EXIT.                                                        08500000
                                                                        08510000
      ***************************************************************** 08520000
      * HTTP POST/PUT.                                                * 08530000
      * Replicate across active/active Data Center.                   * 08540001
      * Send POST response.                                           * 08550000
      * Set IMMEDIATE action on WEB SEND command.                     * 08560001
      * Get URL and replication type from document template.          * 08570001
      * When ACTIVE-SINGLE,  there is no Data Center replication.     * 08580001
      * When ACTIVE-ACTIVE,  perfrom Data Center replication before   * 08590001
      *      sending the response to the client.                      * 08600001
      * When ACTIVE-STANDBY, perform Data Center replication after    * 08610001
      *      sending the response to the client.                      * 08620001
      ***************************************************************** 08630000
       4300-SEND-RESPONSE.                                              08640000
           EXEC CICS SYNCPOINT NOHANDLE                                 08641001
           END-EXEC.                                                    08642001
                                                                        08643001
           PERFORM 8000-GET-URL               THRU 8000-EXIT.           08650001
                                                                        08660000
           IF  DC-TYPE EQUAL ACTIVE-ACTIVE AND                          08670001
               WEB-PATH(1:10) EQUAL RESOURCES                           08680001
               PERFORM 4600-REPLICATE    THRU 4600-EXIT.                08690001
                                                                        08700000
           MOVE DFHVALUE(IMMEDIATE)    TO SEND-ACTION.                  08840001
                                                                        08850000
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    08850101
                                                                        08851001
           EXEC CICS WEB SEND                                           08860000
                FROM      (CRLF)                                        08880001
                FROMLENGTH(TWO)                                         08890001
                MEDIATYPE(TEXT-PLAIN)                                   08910001
                SRVCONVERT                                              08920000
                NOHANDLE                                                08930000
                ACTION(SEND-ACTION)                                     08940001
                STATUSCODE(HTTP-STATUS-200)                             08950000
                STATUSTEXT(HTTP-OK)                                     08960000
           END-EXEC.                                                    08970000
                                                                        08980000
           IF  DC-TYPE EQUAL ACTIVE-STANDBY AND                         08990001
               WEB-PATH(1:10) EQUAL RESOURCES                           09000001
               PERFORM 4600-REPLICATE    THRU 4600-EXIT.                09010001
                                                                        09020000
           IF  DUPLICATE-POST EQUAL 'Y'                                 09030001
               PERFORM 4700-DELETE       THRU 4700-EXIT                 09040001
                   WITH TEST AFTER                                      09041001
                   VARYING DELETE-SEGMENT FROM 1 BY 1                   09042001
                   UNTIL EIBRESP NOT EQUAL DFHRESP(NORMAL).             09043001
                                                                        09050000
       4300-EXIT.                                                       09060000
           EXIT.                                                        09070000
                                                                        09080000
      ***************************************************************** 09090000
      * HTTP POST/PUT.                                                * 09100000
      * Write FILE structure record.                                  * 09110001
      * A logical record can span one hundred 32,000 byte segments.   * 09120001
      ***************************************************************** 09130000
       4400-WRITE-FILE.                                                 09140001
           SET ADDRESS OF CACHE-MESSAGE         TO CACHE-ADDRESS.       09150001
           MOVE SEGMENT-COUNT                   TO ZF-SEGMENT.          09160001
                                                                        09170001
           IF  UNSEGMENTED-LENGTH LESS THAN     OR EQUAL THIRTY-TWO-KB  09180001
               MOVE UNSEGMENTED-LENGTH          TO ZF-LENGTH            09190001
           ELSE                                                         09200001
               MOVE THIRTY-TWO-KB               TO ZF-LENGTH.           09210001
                                                                        09220001
           MOVE LOW-VALUES                      TO ZF-DATA.             09230001
           MOVE CACHE-MESSAGE(1:ZF-LENGTH)      TO ZF-DATA.             09240001
           ADD  ZF-PREFIX TO ZF-LENGTH.                                 09250001
                                                                        09260001
           EXEC CICS WRITE FILE(ZF-FCT)                                 09270001
                FROM(ZF-RECORD)                                         09280001
                RIDFLD(ZF-KEY-16)                                       09290001
                LENGTH(ZF-LENGTH)                                       09300001
                NOHANDLE                                                09310001
           END-EXEC.                                                    09320001
                                                                        09330000
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)                        09340001
               MOVE FC-WRITE                TO FE-FN                    09350001
               MOVE '4400'                  TO FE-PARAGRAPH             09351001
               PERFORM 9100-FILE-ERROR    THRU 9100-EXIT                09360001
               PERFORM 9999-ROLLBACK      THRU 9999-EXIT                09371001
               MOVE EIBDS(1:8)              TO HTTP-FILE-ERROR(1:8)     09372001
               MOVE HTTP-FILE-ERROR         TO HTTP-507-TEXT            09380001
               MOVE HTTP-FILE-LENGTH        TO HTTP-507-LENGTH          09390001
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                09400001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               09410001
                                                                        09420000
           IF  UNSEGMENTED-LENGTH GREATER THAN  OR EQUAL THIRTY-TWO-KB  09430001
               SUBTRACT THIRTY-TWO-KB         FROM UNSEGMENTED-LENGTH   09440001
               ADD      THIRTY-TWO-KB           TO CACHE-ADDRESS-X.     09450001
                                                                        09460000
       4400-EXIT.                                                       09470001
           EXIT.                                                        09480001
                                                                        09490000
      ***************************************************************** 09500000
      * HTTP POST/PUT.                                                * 09510000
      * Rewrite KEY structure record.                                 * 09550001
      ***************************************************************** 09600000
       4500-UPDATE-KEY.                                                 09610001
           EXEC CICS REWRITE FILE(ZK-FCT)                               09880001
                FROM(ZK-RECORD)                                         09890001
                LENGTH(ZK-LENGTH)                                       09900001
                NOHANDLE                                                09910001
           END-EXEC.                                                    09920001
                                                                        09930000
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)                        09940001
               MOVE '4500'                  TO FE-PARAGRAPH             09950001
               MOVE FC-REWRITE              TO FE-FN                    09951001
               PERFORM 9200-KEY-ERROR     THRU 9200-EXIT                09960001
               MOVE EIBDS(1:8)              TO HTTP-KEY-ERROR(1:8)      09961001
               MOVE HTTP-KEY-ERROR          TO HTTP-507-TEXT            09970001
               MOVE HTTP-KEY-LENGTH         TO HTTP-507-LENGTH          09980001
               PERFORM 9800-STATUS-507    THRU 9800-EXIT                09990001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               10000001
                                                                        10010000
           MOVE 'Y'                         TO DUPLICATE-POST.          10020001
                                                                        10030000
       4500-EXIT.                                                       10040001
           EXIT.                                                        10050001
                                                                        10060000
      ***************************************************************** 10070000
      * HTTP POST/PUT.                                                * 10080000
      * Replicate POST/PUT request to partner Data Center.            * 10090001
      ***************************************************************** 10100000
       4600-REPLICATE.                                                  10110001
                                                                        10120001
           PERFORM 8100-WEB-OPEN          THRU 8100-EXIT.               10130001
                                                                        10140001
           MOVE DFHVALUE(POST)              TO WEB-METHOD               10150001
           PERFORM 8200-WEB-CONVERSE      THRU 8200-EXIT.               10160001
                                                                        10170001
           PERFORM 8300-WEB-CLOSE         THRU 8300-EXIT.               10180001
                                                                        10190001
       4600-EXIT.                                                       10200001
           EXIT.                                                        10210001
                                                                        10220000
      ***************************************************************** 10230001
      * HTTP POST/PUT.                                                * 10240001
      * Delete obsolete record(s).                                    * 10250001
      ***************************************************************** 10260001
       4700-DELETE.                                                     10270001
                                                                        10280001
           EXEC CICS DELETE FILE(ZF-FCT)                                10290001
                RIDFLD(DELETE-KEY-16)                                   10300001
                NOHANDLE                                                10330001
           END-EXEC.                                                    10340001
                                                                        10350000
       4700-EXIT.                                                       10360001
           EXIT.                                                        10370001
                                                                        10380000
      ***************************************************************** 10390000
      * HTTP DELETE                                                   * 10400000
      * Read KEY structure.                                           * 10410001
      ***************************************************************** 10420000
       5000-READ-KEY.                                                   10430001
                                                                        10440000
           MOVE URI-KEY TO ZK-KEY.                                      10450000
           MOVE LENGTH  OF ZK-RECORD TO ZK-LENGTH.                      10460000
                                                                        10470000
           EXEC CICS READ FILE(ZK-FCT)                                  10480000
                INTO(ZK-RECORD)                                         10490000
                RIDFLD(ZK-KEY)                                          10500000
                LENGTH(ZK-LENGTH)                                       10510000
                NOHANDLE                                                10520000
           END-EXEC.                                                    10530000
                                                                        10540000
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)                        10541001
               MOVE HTTP-NOT-FOUND          TO HTTP-204-TEXT            10542001
               MOVE HTTP-NOT-FOUND-LENGTH   TO HTTP-204-LENGTH          10543001
               PERFORM 9700-STATUS-204    THRU 9700-EXIT                10544001
               PERFORM 9000-RETURN        THRU 9000-EXIT.               10545001
                                                                        10546001
           IF  WEB-PATH(1:10) EQUAL DEPLICATE                           10550001
               PERFORM 5500-DEPLICATE-DELETE      THRU 5500-EXIT.       10560001
                                                                        10570000
       5000-EXIT.                                                       10580000
           EXIT.                                                        10590000
                                                                        10600000
      ***************************************************************** 10610000
      * HTTP DELETE                                                   * 10620000
      * Delete KEY structure.                                         * 10630001
      ***************************************************************** 10640000
       5100-DELETE-KEY.                                                 10650001
                                                                        10660000
           EXEC CICS DELETE FILE(ZK-FCT)                                10670000
                RIDFLD(ZK-KEY)                                          10680000
                NOHANDLE                                                10690000
           END-EXEC.                                                    10700000
                                                                        10710000
       5100-EXIT.                                                       10720000
           EXIT.                                                        10730000
                                                                        10740000
      ***************************************************************** 10750000
      * HTTP DELETE                                                   * 10760000
      * Delete FILE structure.                                        * 10770001
      ***************************************************************** 10790000
       5200-DELETE-FILE.                                                10800001
                                                                        10810000
           MOVE ZK-ZF-KEY               TO ZF-KEY.                      10820000
           MOVE ZEROES                  TO ZF-ZEROES.                   10830000
                                                                        10840000
           EXEC CICS DELETE FILE(ZF-FCT)                                10850000
                RIDFLD(ZF-KEY-16)                                       10860000
                NOHANDLE                                                10890000
           END-EXEC.                                                    10900000
                                                                        10910000
       5200-EXIT.                                                       10920000
           EXIT.                                                        10930000
                                                                        10940000
      ***************************************************************** 10950000
      * HTTP DELETE                                                   * 10960000
      * Replicate across active/active Data Center.                   * 10970001
      * When ACTIVE-SINGLE,  there is no Data Center replication.     * 10980001
      * When ACTIVE-ACTIVE,  perfrom Data Center replication before   * 10990001
      *      sending the response to the client.                      * 11000001
      * When ACTIVE-STANDBY, perform Data Center replication after    * 11010001
      *      sending the response to the client.                      * 11020001
      ***************************************************************** 11030000
       5300-SEND-RESPONSE.                                              11040000
           PERFORM 8000-GET-URL               THRU 8000-EXIT.           11050001
                                                                        11060000
           IF  DC-TYPE EQUAL ACTIVE-ACTIVE AND                          11070001
               WEB-PATH(1:10) EQUAL RESOURCES                           11080001
               PERFORM 5400-REPLICATE    THRU 5400-EXIT.                11090001
                                                                        11100000
           MOVE DFHVALUE(IMMEDIATE)    TO SEND-ACTION.                  11230001
                                                                        11240000
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    11240101
                                                                        11241001
           EXEC CICS WEB SEND                                           11250000
                FROM      (CRLF)                                        11260001
                FROMLENGTH(TWO)                                         11270001
                MEDIATYPE(TEXT-PLAIN)                                   11280001
                SRVCONVERT                                              11290000
                NOHANDLE                                                11300000
                ACTION(SEND-ACTION)                                     11310001
                STATUSCODE(HTTP-STATUS-200)                             11320000
                STATUSTEXT(HTTP-OK)                                     11330000
           END-EXEC.                                                    11340000
                                                                        11350000
           IF  DC-TYPE EQUAL ACTIVE-STANDBY AND                         11360001
               WEB-PATH(1:10) EQUAL RESOURCES                           11370001
               PERFORM 5400-REPLICATE    THRU 5400-EXIT.                11380001
                                                                        11390000
       5300-EXIT.                                                       11400000
           EXIT.                                                        11410000
                                                                        11420000
      ***************************************************************** 11430000
      * HTTP DELETE.                                                  * 11440000
      * Replicate DELETE quest to active/active Data Center.          * 11450000
      ***************************************************************** 11460000
       5400-REPLICATE.                                                  11470001
                                                                        11480001
           PERFORM 8100-WEB-OPEN          THRU 8100-EXIT.               11490001
                                                                        11500001
           MOVE DFHVALUE(DELETE)            TO WEB-METHOD               11510001
           PERFORM 8200-WEB-CONVERSE      THRU 8200-EXIT.               11520001
                                                                        11530001
           PERFORM 8300-WEB-CLOSE         THRU 8300-EXIT.               11540001
                                                                        11550001
                                                                        11560000
       5400-EXIT.                                                       11570001
           EXIT.                                                        11580001
                                                                        11590000
      ***************************************************************** 11600000
      * HTTP DELETE                                                   * 11610000
      * Deplicate request from zECS expiration task from the partner  * 11620001
      * Data Center.                                                  * 11630001
      * Check for expired message.                                    * 11632001
      * Delete when expired.                                          * 11640000
      * Return ABSTIME when not expired.                              * 11650000
      * And yes, 'Deplication' is a word.  Deplication is basically   * 11660000
      * 'data deduplication, data reduction, and delta differencing'. * 11670000
      ***************************************************************** 11680000
       5500-DEPLICATE-DELETE.                                           11690000
           MOVE ZK-ZF-KEY               TO ZF-KEY.                      11700001
           MOVE ZEROES                  TO ZF-ZEROES.                   11710001
           MOVE LENGTH OF ZF-RECORD     TO ZF-LENGTH.                   11720001
                                                                        11730001
           IF  ZK-SEGMENTS EQUAL 'Y'                                    11740001
               MOVE ONE TO ZF-SEGMENT.                                  11750001
                                                                        11760000
           EXEC CICS READ FILE(ZF-FCT)                                  11770001
                INTO(ZF-RECORD)                                         11780001
                RIDFLD(ZF-KEY-16)                                       11790001
                LENGTH(ZF-LENGTH)                                       11800001
                NOHANDLE                                                11810001
           END-EXEC.                                                    11820001
                                                                        11830000
           IF  EIBRESP EQUAL DFHRESP(NORMAL)                            11840001
               PERFORM 5600-CHECK-TTL THRU 5600-EXIT.                   11850001
                                                                        11860000
       5500-EXIT.                                                       11870001
           EXIT.                                                        11880001
                                                                        11890000
      ***************************************************************** 11900000
      * HTTP DELETE                                                   * 11910000
      * Check for expired message.                                    * 11920000
      ***************************************************************** 11930000
       5600-CHECK-TTL.                                                  11940000
           EXEC CICS ASKTIME ABSTIME(CURRENT-ABS) NOHANDLE              11950001
           END-EXEC.                                                    11960001
                                                                        11970001
           MOVE ZF-TTL                  TO TTL-SECONDS.                 11980001
           MOVE TTL-TIME                TO TTL-MILLISECONDS.            11990001
                                                                        12000000
           SUBTRACT ZF-ABS FROM CURRENT-ABS GIVING RELATIVE-TIME.       12010001
           IF  RELATIVE-TIME LESS THAN TTL-MILLISECONDS  OR             12020001
               RELATIVE-TIME EQUAL     TTL-MILLISECONDS                 12030001
               PERFORM 5700-SEND-ABS  THRU 5700-EXIT                    12040001
               PERFORM 9000-RETURN    THRU 9000-EXIT.                   12050001
                                                                        12060000
       5600-EXIT.                                                       12070001
           EXIT.                                                        12080001
                                                                        12090000
      ***************************************************************** 12100001
      * HTTP DELETE                                                   * 12110001
      * Deplicate request from the partner Data Center expiration     * 12120001
      * process.                                                      * 12121001
      * This message has not expired.                                 * 12130001
      * Send DELETE response with this record's ABSTIME.              * 12140001
      ***************************************************************** 12150001
       5700-SEND-ABS.                                                   12160001
           PERFORM 9001-ACAO          THRU 9001-EXIT.                   12170001
                                                                        12180001
           MOVE HTTP-NOT-EXPIRED        TO HTTP-201-TEXT.               12190001
           MOVE ZF-ABS                  TO HTTP-ABSTIME.                12200001
           MOVE DFHVALUE(IMMEDIATE)     TO SEND-ACTION.                 12211001
                                                                        12217001
           EXEC CICS WEB SEND                                           12218001
                FROM      (HTTP-201-TEXT)                               12219001
                FROMLENGTH(HTTP-201-LENGTH)                             12220001
                MEDIATYPE (TEXT-PLAIN)                                  12230001
                ACTION    (SEND-ACTION)                                 12240001
                STATUSCODE(HTTP-STATUS-201)                             12250001
                STATUSTEXT(HTTP-ABSTIME)                                12260001
                STATUSLEN (HTTP-ABSTIME-LENGTH)                         12270001
                SRVCONVERT                                              12280001
                NOHANDLE                                                12290001
           END-EXEC.                                                    12291001
                                                                        12300000
       5700-EXIT.                                                       12420001
           EXIT.                                                        12430001
                                                                        12440000
      ***************************************************************** 12450000
      * Get URL for replication process.                              * 12460000
      * URL must be in the following format:                          * 12470000
      * http://hostname:port                                          * 12480001
      ***************************************************************** 12490000
       8000-GET-URL.                                                    12500001
                                                                        12510001
           EXEC CICS DOCUMENT CREATE DOCTOKEN(DC-TOKEN)                 12520001
                TEMPLATE(ZECS-DC)                                       12530001
                NOHANDLE                                                12540001
           END-EXEC.                                                    12550001
                                                                        12560001
           MOVE LENGTH OF DC-CONTROL TO DC-LENGTH.                      12570001
                                                                        12580001
           IF  EIBRESP EQUAL DFHRESP(NORMAL)                            12590001
               EXEC CICS DOCUMENT RETRIEVE DOCTOKEN(DC-TOKEN)           12600001
                    INTO     (DC-CONTROL)                               12610001
                    LENGTH   (DC-LENGTH)                                12620001
                    MAXLENGTH(DC-LENGTH)                                12630001
                    DATAONLY                                            12640001
                    NOHANDLE                                            12650001
               END-EXEC.                                                12660001
                                                                        12670001
           IF  EIBRESP EQUAL DFHRESP(NORMAL)  AND                       12680001
               DC-LENGTH GREATER THAN TEN                               12690001
               SUBTRACT TWELVE FROM DC-LENGTH                           12700001
                             GIVING THE-OTHER-DC-LENGTH                 12710001
                                                                        12720001
               EXEC CICS WEB PARSE                                      12730001
                    URL(THE-OTHER-DC)                                   12740001
                    URLLENGTH(THE-OTHER-DC-LENGTH)                      12750001
                    SCHEMENAME(URL-SCHEME-NAME)                         12760001
                    HOST(URL-HOST-NAME)                                 12770001
                    HOSTLENGTH(URL-HOST-NAME-LENGTH)                    12780001
                    PORTNUMBER(URL-PORT)                                12790001
                    NOHANDLE                                            12800001
               END-EXEC.                                                12810001
                                                                        12820001
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)  OR                    12830001
               DC-LENGTH LESS THAN TEN            OR                    12840001
               DC-LENGTH EQUAL            TEN                           12850001
               MOVE ACTIVE-SINGLE                 TO DC-TYPE.           12860001
                                                                        12870001
       8000-EXIT.                                                       12880001
           EXIT.                                                        12890001
                                                                        12900000
                                                                        12910000
      ***************************************************************** 12920000
      * Open WEB connection with the other Data Center zECS.          * 12930001
      ***************************************************************** 12940000
       8100-WEB-OPEN.                                                   12950001
           IF  URL-SCHEME-NAME EQUAL 'HTTPS'                            12960001
               MOVE DFHVALUE(HTTPS)  TO URL-SCHEME                      12970001
           ELSE                                                         12980001
               MOVE DFHVALUE(HTTP)   TO URL-SCHEME.                     12990001
                                                                        13000001
           EXEC CICS WEB OPEN                                           13010001
                HOST(URL-HOST-NAME)                                     13020001
                HOSTLENGTH(URL-HOST-NAME-LENGTH)                        13030001
                PORTNUMBER(URL-PORT)                                    13040001
                SCHEME(URL-SCHEME)                                      13050001
                SESSTOKEN(SESSION-TOKEN)                                13060001
                NOHANDLE                                                13070001
           END-EXEC.                                                    13080001
                                                                        13090001
       8100-EXIT.                                                       13100001
           EXIT.                                                        13110001
                                                                        13120000
      ***************************************************************** 13130000
      * Converse with the other Data Center zECS.                     * 13140001
      * The first element of the path, which for normal processing is * 13150000
      * /resources, must be changed to /replicate.                    * 13160000
      ***************************************************************** 13170000
       8200-WEB-CONVERSE.                                               13180001
           MOVE REPLICATE TO WEB-PATH(1:10).                            13190001
                                                                        13200001
           SET ADDRESS OF CACHE-MESSAGE TO SAVE-ADDRESS.                13210001
                                                                        13220001
           IF  WEB-MEDIA-TYPE(1:04) EQUAL TEXT-ANYTHING    OR           13221001
               WEB-MEDIA-TYPE(1:15) EQUAL APPLICATION-XML               13222001
               MOVE DFHVALUE(CLICONVERT)      TO CLIENT-CONVERT         13222101
           ELSE                                                         13222201
               MOVE DFHVALUE(NOCLICONVERT)    TO CLIENT-CONVERT.        13222301
                                                                        13223001
           IF  WEB-METHOD EQUAL DFHVALUE(POST)     OR                   13230001
               WEB-METHOD EQUAL DFHVALUE(PUT)                           13240001
               IF  WEB-QUERYSTRING-LENGTH EQUAL ZEROES                  13250001
                   EXEC CICS WEB CONVERSE                               13260001
                        SESSTOKEN(SESSION-TOKEN)                        13270001
                        PATH(WEB-PATH)                                  13280001
                        PATHLENGTH(WEB-PATH-LENGTH)                     13290001
                        METHOD(WEB-METHOD)                              13300001
                        MEDIATYPE(ZF-MEDIA)                             13310001
                        FROM(CACHE-MESSAGE)                             13320001
                        FROMLENGTH(RECEIVE-LENGTH)                      13330001
                        INTO(CONVERSE-RESPONSE)                         13340001
                        TOLENGTH(CONVERSE-LENGTH)                       13350001
                        MAXLENGTH(CONVERSE-LENGTH)                      13360001
                        STATUSCODE(WEB-STATUS-CODE)                     13370001
                        STATUSLEN(WEB-STATUS-LENGTH)                    13380001
                        STATUSTEXT(WEB-STATUS-TEXT)                     13390001
                        CLIENTCONV(CLIENT-CONVERT)                      13401001
                        NOHANDLE                                        13410001
                   END-EXEC.                                            13420001
                                                                        13430001
           IF  WEB-METHOD EQUAL DFHVALUE(POST)     OR                   13440001
               WEB-METHOD EQUAL DFHVALUE(PUT)                           13450001
               IF  WEB-QUERYSTRING-LENGTH GREATER THAN ZEROES           13460001
                   EXEC CICS WEB CONVERSE                               13470001
                        SESSTOKEN(SESSION-TOKEN)                        13480001
                        PATH(WEB-PATH)                                  13490001
                        PATHLENGTH(WEB-PATH-LENGTH)                     13500001
                        METHOD(WEB-METHOD)                              13510001
                        MEDIATYPE(ZF-MEDIA)                             13520001
                        FROM(CACHE-MESSAGE)                             13530001
                        FROMLENGTH(RECEIVE-LENGTH)                      13540001
                        INTO(CONVERSE-RESPONSE)                         13550001
                        TOLENGTH(CONVERSE-LENGTH)                       13560001
                        MAXLENGTH(CONVERSE-LENGTH)                      13570001
                        STATUSCODE(WEB-STATUS-CODE)                     13580001
                        STATUSLEN(WEB-STATUS-LENGTH)                    13590001
                        STATUSTEXT(WEB-STATUS-TEXT)                     13600001
                        QUERYSTRING(WEB-QUERYSTRING)                    13610001
                        QUERYSTRLEN(WEB-QUERYSTRING-LENGTH)             13620001
                        CLIENTCONV(CLIENT-CONVERT)                      13631001
                        NOHANDLE                                        13640001
                   END-EXEC.                                            13650001
                                                                        13660001
           IF  WEB-METHOD EQUAL DFHVALUE(DELETE)                        13670001
                   EXEC CICS WEB CONVERSE                               13680001
                        SESSTOKEN(SESSION-TOKEN)                        13690001
                        PATH(WEB-PATH)                                  13700001
                        PATHLENGTH(WEB-PATH-LENGTH)                     13710001
                        METHOD(WEB-METHOD)                              13720001
                        MEDIATYPE(ZF-MEDIA)                             13730001
                        INTO(CONVERSE-RESPONSE)                         13740001
                        TOLENGTH(CONVERSE-LENGTH)                       13750001
                        MAXLENGTH(CONVERSE-LENGTH)                      13760001
                        STATUSCODE(WEB-STATUS-CODE)                     13770001
                        STATUSLEN(WEB-STATUS-LENGTH)                    13780001
                        STATUSTEXT(WEB-STATUS-TEXT)                     13790001
                        CLIENTCONV(CLIENT-CONVERT)                      13801001
                        NOHANDLE                                        13810001
                   END-EXEC.                                            13820001
                                                                        13830001
       8200-EXIT.                                                       13840001
           EXIT.                                                        13850001
                                                                        13860000
      ***************************************************************** 13870000
      * Close WEB connection with the other Data Center zECS.         * 13880001
      ***************************************************************** 13890000
       8300-WEB-CLOSE.                                                  13900001
                                                                        13910001
           EXEC CICS WEB CLOSE                                          13920001
                SESSTOKEN(SESSION-TOKEN)                                13930001
                NOHANDLE                                                13940001
           END-EXEC.                                                    13950001
                                                                        13960001
       8300-EXIT.                                                       13970001
           EXIT.                                                        13980001
                                                                        13990000
      ***************************************************************** 14000000
      * Return to CICS                                                * 14010000
      ***************************************************************** 14020000
       9000-RETURN.                                                     14030000
                                                                        14040000
           EXEC CICS RETURN                                             14050000
           END-EXEC.                                                    14060000
                                                                        14070000
       9000-EXIT.                                                       14080000
           EXIT.                                                        14090000
                                                                        14100000
                                                                        14101001
      ***************************************************************** 14101101
      * Write HTTP header                                             * 14101201
      ***************************************************************** 14101301
       9001-ACAO.                                                       14101401
           EXEC CICS WEB WRITE                                          14102001
                HTTPHEADER (HEADER-ACAO)                                14103001
                NAMELENGTH (HEADER-ACAO-LENGTH)                         14104001
                VALUE      (VALUE-ACAO)                                 14105001
                VALUELENGTH(VALUE-ACAO-LENGTH)                          14106001
                NOHANDLE                                                14107001
           END-EXEC.                                                    14108001
                                                                        14109001
       9001-EXIT.                                                       14109101
           EXIT.                                                        14109201
                                                                        14109301
      ***************************************************************** 14110000
      * FILE structure I/O error.                                     * 14120001
      ***************************************************************** 14130000
       9100-FILE-ERROR.                                                 14140001
           MOVE EIBRCODE              TO FE-RCODE.                      14141001
                                                                        14142001
           IF  EIBRESP EQUAL DFHRESP(NOSPACE)                           14150001
               MOVE NO-SPACE-MESSAGE  TO FE-NOSPACE.                    14151001
                                                                        14152001
           MOVE EIBDS                 TO FE-DS.                         14160001
           MOVE EIBRESP               TO FE-RESP.                       14161001
           MOVE EIBRESP2              TO FE-RESP2.                      14170001
           MOVE FILE-ERROR            TO TD-MESSAGE.                    14180001
           PERFORM 9900-WRITE-TD-QUEUE THRU 9900-EXIT.                  14190001
                                                                        14200000
       9100-EXIT.                                                       14210000
           EXIT.                                                        14220000
                                                                        14230000
      ***************************************************************** 14240000
      * KEY  structure I/O error                                      * 14250001
      ***************************************************************** 14260000
       9200-KEY-ERROR.                                                  14270001
           IF  EIBRESP EQUAL DFHRESP(NOSPACE)                           14271001
               MOVE NO-SPACE-MESSAGE  TO KE-NOSPACE.                    14272001
                                                                        14273001
           MOVE EIBDS                 TO KE-DS.                         14280001
           MOVE EIBRESP               TO KE-RESP.                       14290001
           MOVE EIBRESP2              TO KE-RESP2.                      14300001
           MOVE KEY-ERROR             TO TD-MESSAGE.                    14310001
           PERFORM 9900-WRITE-TD-QUEUE THRU 9900-EXIT.                  14320001
                                                                        14330000
       9200-EXIT.                                                       14340000
           EXIT.                                                        14350000
                                                                        14360000
      ***************************************************************** 14370000
      * WEB RECEIVE error                                             * 14380000
      ***************************************************************** 14390000
       9300-WEB-ERROR.                                                  14400000
           MOVE EIBRESP               TO WEB-RESP.                      14410001
           MOVE EIBRESP2              TO WEB-RESP2.                     14420001
           MOVE WEB-ERROR             TO TD-MESSAGE.                    14430001
           PERFORM 9900-WRITE-TD-QUEUE THRU 9900-EXIT.                  14440001
                                                                        14450000
       9300-EXIT.                                                       14460000
           EXIT.                                                        14470000
                                                                        14480000
      ***************************************************************** 14490000
      * HTTP status 400 messages.                                     * 14500001
      ***************************************************************** 14510000
       9400-STATUS-400.                                                 14520001
                                                                        14740001
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    14740101
                                                                        14740201
           MOVE DFHVALUE(IMMEDIATE)     TO SEND-ACTION.                 14740301
                                                                        14740401
           EXEC CICS WEB SEND                                           14741001
                FROM      (CRLF)                                        14742001
                FROMLENGTH(TWO)                                         14743001
                MEDIATYPE (TEXT-PLAIN)                                  14744001
                ACTION    (SEND-ACTION)                                 14746101
                STATUSCODE(HTTP-STATUS-400)                             14747001
                STATUSTEXT(HTTP-400-TEXT)                               14748001
                STATUSLEN (HTTP-400-LENGTH)                             14748101
                SRVCONVERT                                              14748201
                NOHANDLE                                                14748301
           END-EXEC.                                                    14749001
       9400-EXIT.                                                       14750001
           EXIT.                                                        14760001
                                                                        14770000
      ***************************************************************** 14770101
      * HTTP status 409 messages                                      * 14770201
      ***************************************************************** 14770301
       9500-STATUS-409.                                                 14770401
                                                                        14770501
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    14770601
                                                                        14770701
           MOVE DFHVALUE(IMMEDIATE)     TO SEND-ACTION.                 14770801
                                                                        14770901
           EXEC CICS WEB SEND                                           14771701
                FROM      (CRLF)                                        14771801
                FROMLENGTH(TWO)                                         14771901
                MEDIATYPE (TEXT-PLAIN)                                  14772001
                ACTION    (SEND-ACTION)                                 14772301
                STATUSCODE(HTTP-STATUS-409)                             14772401
                STATUSTEXT(HTTP-409-TEXT)                               14772501
                STATUSLEN (HTTP-409-LENGTH)                             14772601
                SRVCONVERT                                              14772801
                NOHANDLE                                                14772901
           END-EXEC.                                                    14773001
                                                                        14773101
       9500-EXIT.                                                       14773201
           EXIT.                                                        14773301
                                                                        14773401
      ***************************************************************** 14773501
      * Basic Authenticaion error.                                    * 14773601
      ***************************************************************** 14773701
       9600-AUTH-ERROR.                                                 14774001
                                                                        14779701
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    14779801
                                                                        14779901
           EXEC CICS WEB SEND                                           14780001
                FROM      (CRLF)                                        14780101
                FROMLENGTH(TWO)                                         14780201
                MEDIATYPE (TEXT-PLAIN)                                  14780301
                STATUSCODE(HTTP-STATUS-401)                             14780601
                STATUSTEXT(HTTP-AUTH-ERROR)                             14780701
                SRVCONVERT                                              14780801
                NOHANDLE                                                14780901
           END-EXEC.                                                    14781001
                                                                        14781101
       9600-EXIT.                                                       14781201
           EXIT.                                                        14781301
                                                                        14781401
      ***************************************************************** 14782001
      * Status 204 response.                                          * 14790001
      ***************************************************************** 14800000
       9700-STATUS-204.                                                 14810001
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    14811001
                                                                        14812001
           EXEC CICS DOCUMENT CREATE DOCTOKEN(DC-TOKEN)                 14820001
                NOHANDLE                                                14830001
           END-EXEC.                                                    14840001
                                                                        14850001
           MOVE DFHVALUE(IMMEDIATE)     TO SEND-ACTION.                 14860001
                                                                        14870001
           EXEC CICS WEB SEND                                           14880001
                DOCTOKEN  (DC-TOKEN)                                    14890001
                MEDIATYPE (TEXT-PLAIN)                                  14900001
                ACTION    (SEND-ACTION)                                 14930001
                STATUSCODE(HTTP-STATUS-204)                             14940001
                STATUSTEXT(HTTP-204-TEXT)                               14950001
                STATUSLEN (HTTP-204-LENGTH)                             14960001
                SRVCONVERT                                              14961001
                NOHANDLE                                                14962001
           END-EXEC.                                                    14970001
                                                                        14980001
                                                                        15040000
       9700-EXIT.                                                       15050000
           EXIT.                                                        15060000
                                                                        15070000
      ***************************************************************** 15080000
      * KEY or FILE structure I/O error.                              * 15090001
      ***************************************************************** 15100000
       9800-STATUS-507.                                                 15110001
           PERFORM 9001-ACAO         THRU 9001-EXIT.                    15120001
                                                                        15130001
           EXEC CICS WEB SEND                                           15240001
                FROM      (CRLF)                                        15241001
                FROMLENGTH(TWO)                                         15242001
                MEDIATYPE (TEXT-PLAIN)                                  15270001
                STATUSCODE(HTTP-STATUS-507)                             15300001
                STATUSTEXT(HTTP-507-TEXT)                               15310001
                STATUSLEN (HTTP-507-LENGTH)                             15320001
                SRVCONVERT                                              15321001
                NOHANDLE                                                15322001
           END-EXEC.                                                    15330001
                                                                        15340001
       9800-EXIT.                                                       15350001
           EXIT.                                                        15360001
                                                                        15370000
      ***************************************************************** 15380000
      * Write TD TD-QUEUE.                                            * 15390001
      ***************************************************************** 15400000
       9900-WRITE-TD-QUEUE.                                             15410001
           PERFORM 9950-ABS         THRU 9950-EXIT.                     15420001
           MOVE EIBTRNID              TO TD-TRANID.                     15430001
           EXEC CICS FORMATTIME ABSTIME(ZF-ABS)                         15440001
                TIME(TD-TIME)                                           15450001
                YYYYMMDD(TD-DATE)                                       15460001
                TIMESEP                                                 15470001
                DATESEP                                                 15480001
                NOHANDLE                                                15490001
           END-EXEC.                                                    15500001
                                                                        15510001
           MOVE LENGTH OF TD-RECORD   TO TD-LENGTH.                     15520001
           EXEC CICS WRITEQ TD QUEUE(TD-QUEUE)                          15530001
                FROM(TD-RECORD)                                         15540001
                LENGTH(TD-LENGTH)                                       15550001
                NOHANDLE                                                15560001
           END-EXEC.                                                    15570001
                                                                        15580000
       9900-EXIT.                                                       15590001
           EXIT.                                                        15600001
                                                                        15610000
      ***************************************************************** 15620000
      * Get Absolute time.                                            * 15630000
      ***************************************************************** 15640000
       9950-ABS.                                                        15650000
           EXEC CICS ASKTIME ABSTIME(ZF-ABS) NOHANDLE                   15660000
           END-EXEC.                                                    15670000
                                                                        15680000
       9950-EXIT.                                                       15690000
           EXIT.                                                        15700000
                                                                        15710000
      ***************************************************************** 15720001
      * Issue SYNCPOINT ROLLBACK                                      * 15730001
      ***************************************************************** 15740001
       9999-ROLLBACK.                                                   15750001
           EXEC CICS SYNCPOINT ROLLBACK NOHANDLE                        15760001
           END-EXEC.                                                    15770001
                                                                        15780001
       9999-EXIT.                                                       15790001
           EXIT.                                                        15800001
                                                                        15810001