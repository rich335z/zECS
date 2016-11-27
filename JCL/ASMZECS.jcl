//ASMZECS  JOB @job_parms@
//**********************************************************************
//* Assemble and compile the source code
//**********************************************************************
//PROC     JCLLIB ORDER=(@proc_lib@)
//**********************************************************************
//* Compile and link ZECS000
//**********************************************************************
//ZECS000  EXEC DFHYITVL,PROGLIB=@program_lib@
//TRN.SYSIN  DD DISP=SHR,DSN=@source_lib@(ZECS000)
//*
//LKED.SYSIN DD *
   NAME ZECS000(R)
/*
//**********************************************************************
//* Compile and link ZECS001
//**********************************************************************
//ZECS001  EXEC DFHYITVL,PROGLIB=@program_lib@
//TRN.SYSIN  DD DISP=SHR,DSN=@source_lib@(ZECS001)
//*
//LKED.SYSIN DD *
   NAME ZECS001(R)
/*
//**********************************************************************
//* Assemble and link ZECS002
//**********************************************************************
//ZECS002  EXEC DFHEITAL,PROGLIB=@program_lib@
//TRN.SYSIN  DD DISP=SHR,DSN=@source_lib@(ZECS002)
//*
//LKED.SYSIN DD *
   NAME ZECS002(R)
/*
//**********************************************************************
//* Compile and link ZECS003
//**********************************************************************
//ZECS003  EXEC DFHYITVL,PROGLIB=@program_lib@
//TRN.SYSIN  DD DISP=SHR,DSN=@source_lib@(ZECS003)
//*
//LKED.SYSIN DD *
   NAME ZECS003(R)
/*
//**********************************************************************
//* Assemble and link ZNC
//**********************************************************************
//ZNC      EXEC DFHEITAL,PROGLIB=@program_lib@
//TRN.SYSIN  DD DISP=SHR,DSN=@source_lib@(ZNC)
//*
//LKED.SYSIN DD *
   NAME ZNC(R)
/*
//**********************************************************************
//* Compile and link ZPLT
//**********************************************************************
//ZPLT     EXEC DFHYITVL,PROGLIB=@program_lib@
//TRN.SYSIN  DD DISP=SHR,DSN=@source_lib@(ZPLT)
//*
//LKED.SYSIN DD *
   NAME ZPLT(R)
/*