//CONFIG   JOB MSGCLASS=R,NOTIFY=&SYSUID
//**********************************************************************
//* This job will modify the members in the .SOURCE and .CNTL libraries
//*
//* Steps for this job to complete successfully
//* --------------------------------------------------------------------
//* 1) Modify JOB card to meet your system installation standards
//*
//* 2) Modify the CONFIG member in the .SOURCE dataset before submitting
//*
//* 3) Change all occurrences of the following:
//*    @srclib_prfx@ to the prefix for the source libraries
//*    @source_vrsn@ to the selected version identifier
//*
//* 4) Submit the job
//**********************************************************************
//* Modify ASMZECS JCL
//**********************************************************************
//STEP01   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(ASMZECS)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ASMZECS JCL
//**********************************************************************
//STEP02    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(ASMZECS)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECS JCL
//**********************************************************************
//STEP03   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECS)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECS JCL
//**********************************************************************
//STEP04    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECS)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSN JCL
//**********************************************************************
//STEP05   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECSN)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSN JCL
//**********************************************************************
//STEP06    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECSN)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSR JCL
//**********************************************************************
//STEP07   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECSR)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSR JCL
//**********************************************************************
//STEP08    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECSR)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSS JCL
//**********************************************************************
//STEP09   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECSS)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSS JCL
//**********************************************************************
//STEP10    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(CSDZECSS)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify DEFEXPR JCL
//**********************************************************************
//STEP11   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(DEFEXPR)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace DEFEXPR JCL
//**********************************************************************
//STEP12    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(DEFEXPR)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify DEFZC## JCL
//**********************************************************************
//STEP13   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(DEFZC##)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace DEFZC## JCL
//**********************************************************************
//STEP14    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(DEFZC##)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZC##DC JCL
//**********************************************************************
//STEP15   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(ZC##DC)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZC##DC  JCL
//**********************************************************************
//STEP16    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(ZC##DC)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZC##SD JCL
//**********************************************************************
//STEP17   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(ZC##SD)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZC##SD  JCL
//**********************************************************************
//STEP18    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(ZC##SD)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZC## CSD definition source
//**********************************************************************
//STEP19   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZC##)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZC## CSD definition source
//**********************************************************************
//STEP20    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZC##)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECS CSD definition source
//**********************************************************************
//STEP21   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECS)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECS CSD definition source
//**********************************************************************
//STEP22    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECS)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSN CSD definition source
//**********************************************************************
//STEP23   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSN)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSN CSD definition source
//**********************************************************************
//STEP24    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSN)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSR CSD definition source
//**********************************************************************
//STEP25   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSR)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSR CSD definition source
//**********************************************************************
//STEP26    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSR)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSS CSD definition source
//**********************************************************************
//STEP27   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSS)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSS CSD definition source
//**********************************************************************
//STEP28    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSS)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify CSDZECSX CSD definition source
//**********************************************************************
//STEP29   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSX)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace CSDZECSX CSD definition source
//**********************************************************************
//STEP30    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.RDO(CSDZECSX)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZC##DC CSD definition source
//**********************************************************************
//STEP31   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.JCL(ZC##DC)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZC##DC CSD definition source
//**********************************************************************
//STEP32    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.JCL(ZC##DC)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZCEXPIRE IDCAMS VSAM file definition
//**********************************************************************
//STEP33   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.IDCAMS(ZCEXPIRE)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZCEXPIRE IDCAMS VSAM file definition
//**********************************************************************
//STEP34    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.IDCAMS(ZCEXPIRE)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECSFILE IDCAMS VSAM file definition
//**********************************************************************
//STEP35   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.IDCAMS(ZECSFILE)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECSFILE IDCAMS VSAM file definition
//**********************************************************************
//STEP36    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.IDCAMS(ZECSFILE)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECSKEY IDCAMS VSAM file definition
//**********************************************************************
//STEP37   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.IDCAMS(ZECSKEY)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECSKEY IDCAMS VSAM file definition
//**********************************************************************
//STEP38    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.IDCAMS(ZECSKEY)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECS000 program source
//**********************************************************************
//STEP39   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECS000)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECS000 program source
//**********************************************************************
//STEP40    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECS000)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECS001 program source
//**********************************************************************
//STEP41   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECS001)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECS001 program source
//**********************************************************************
//STEP42    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECS001)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECS002 program source
//**********************************************************************
//STEP43   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.ASM(ZECS002)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECS002 program source
//**********************************************************************
//STEP44    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.ASM(ZECS002)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECS003 program source
//**********************************************************************
//STEP45   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECS003)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECS003 program source
//**********************************************************************
//STEP46    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECS003)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECSNC program source
//**********************************************************************
//STEP47   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.ASM(ZECSNC)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECSNC program source
//**********************************************************************
//STEP48    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.ASM(ZECSNC)
//SYSIN     DD DUMMY
//**********************************************************************
//* Modify ZECSPLT program source
//**********************************************************************
//STEP49   EXEC PGM=IKJEFT1B,REGION=1024K
//SYSPRINT DD SYSOUT=*
//SYSTSPRT DD SYSOUT=*
//INPUT    DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECSPLT)
//OUTPUT   DD DISP=(NEW,PASS),DSN=&&OUTPUT,
//            UNIT=VIO,SPACE=(80,(1000,1000)),
//            DCB=(LRECL=80,RECFM=FB)
//STRINGS  DD DISP=SHR,
//            DSN=@srclib_prfx@.@source_vrsn@.TXT(CONFIG)
//SYSTSIN  DD *
 EXEC '@srclib_prfx@.@source_vrsn@.EXEC(REXXREPL)'
/*
//**********************************************************************
//* Replace ZECSPLT program source
//**********************************************************************
//STEP50    EXEC PGM=IEBGENER,REGION=1024K
//SYSPRINT  DD SYSOUT=*
//SYSUT1    DD DISP=(OLD,DELETE),DSN=&&OUTPUT
//SYSUT2    DD DISP=SHR,
//             DSN=@srclib_prfx@.@source_vrsn@.CBL(ZECSPLT)
//SYSIN     DD DUMMY
//*
//
