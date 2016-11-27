      ***************************************************************** 03471000
      * zECS FILE record definition.                                  * 03472000
      ***************************************************************** 03473000
       01  ZF-PREFIX              PIC S9(08) VALUE 356    COMP.         03480000
                                                                        03490000
       01  ZF-RECORD.                                                   03500000
           02  ZF-KEY-16.                                               03510000
               05  ZF-KEY.                                              03520000
                 10  ZF-KEY-IDN   PIC  X(06) VALUE LOW-VALUES.          03521000
                 10  ZF-KEY-NC    PIC  X(02) VALUE LOW-VALUES.          03522000
               05  ZF-SEGMENT     PIC  9(04) VALUE ZEROES COMP.         03530000
               05  ZF-SUFFIX      PIC  9(04) VALUE ZEROES COMP.         03540000
               05  ZF-ZEROES      PIC  9(08) VALUE ZEROES COMP.         03550000
           02  ZF-ABS             PIC S9(15) VALUE ZEROES COMP-3.       03560000
           02  ZF-TTL             PIC S9(07) VALUE ZEROES COMP-3.       03570000
           02  ZF-SEGMENTS        PIC  9(04) VALUE ZEROES COMP.         03580000
           02  ZF-EXTRA           PIC  X(15).                           03590000
           02  ZF-ZK-KEY          PIC  X(255).                          03600000
           02  ZF-MEDIA           PIC  X(56).                           03610000
           02  ZF-DATA            PIC  X(32000).                        03620000
           02  FILLER             PIC  X(344).                          03630000
                                                                        03640000