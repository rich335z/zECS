      ***************************************************************** 03411000
      * zECS KEYx record definition.                                  * 03412000
      ***************************************************************** 03413000
       01  ZK-RECORD.                                                   03420000
           02  ZK-KEY             PIC X(255) VALUE LOW-VALUES.          03430000
           02  FILLER             PIC  X(01) VALUE LOW-VALUES.          03431000
           02  ZK-ZF-KEY.                                               03440000
               05  ZK-ZF-IDN      PIC  X(06) VALUE LOW-VALUES.          03441000
               05  ZK-ZF-NC       PIC  X(02) VALUE LOW-VALUES.          03442000
           02  ZK-SEGMENTS        PIC  X(01) VALUE SPACES.              03450000
           02  FILLER             PIC X(247) VALUE SPACES.              03460000
                                                                        03470000