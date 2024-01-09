!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
integer function io_BSS_Haydock(ID,iq,it,reached_treshold,mode,Af,Bf,Cf,Vnm1,Vn,Vnp1)
 !
 ! DS: at each iteration the whole vectors are rewritten
 !     The implementation should be changed taking advantage of variables of unlimited size
 !     like done in the real-time project
 !
 use pars,          ONLY:SP
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_identifier
 use BS_solvers,    ONLY:BSS_q0,Haydock_v0_mod,HAYVEC_size
 use IO_m,          ONLY:io_status,io_sec
 use IO_int,        ONLY:io_connect,io_disconnect,io_elemental,io_bulk,io_header
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 integer             :: ID,iq,it
 character(*)        :: mode
 real(SP)            :: reached_treshold,Bf(it+1),Af(it)
 complex(SP)         :: Cf(:)
 complex(SP),pointer :: Vnm1(:),Vn(:),Vnp1(:)
 !
 ! Work Space
 !
 integer  :: it_on_disk
 !
 call timing('io_BSS_Haydock',OPR='start')
 !
 io_BSS_Haydock=io_connect(desc='Haydock_Q'//trim(intc(iq))//'_'//trim(mode)//'_restart',type=2,ID=ID)
 !
 if (io_BSS_Haydock<0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_BSS_Haydock=io_header(ID,XC_KIND="K_E force",GAUGE=.TRUE.,IMPOSE_GAUGE=.TRUE.)
   if (io_BSS_Haydock<0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
   call io_elemental(ID,VAR=&
&       " BSK Identifier                   ",I0=BS_identifier,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Haydock Iteration                ",I0=it,DB_I0=it_on_disk)
   call io_elemental(ID,VAR=&
&       " Haydock reached Treshold         ",R0=reached_treshold,DB_R0=reached_treshold)
   call io_elemental(ID,VAR=&
&       " Field direction                  ",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   io_BSS_Haydock=io_status(ID)
   !
   if (io_BSS_Haydock<0) then
     reached_treshold=0._SP
     goto 1
   endif
   !
   io_BSS_Haydock=it_on_disk
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   call io_bulk(ID,'Haydock_v0_mod',VAR_SZ=(/1/))
   call io_bulk(ID,R0=Haydock_v0_mod)
   !
   ! Vnm1
   !
   call io_bulk(ID,'Vnm1',VAR_SZ=(/2,HAYVEC_size/))
   call io_bulk(ID,C1=Vnm1)
   !
   ! Vn 
   !
   call io_bulk(ID,'Vn',VAR_SZ=(/2,HAYVEC_size/))
   call io_bulk(ID,C1=Vn)
   !
   ! Af,Bf
   !
   call io_bulk(ID,'A_factors',VAR_SZ=(/it/))
   call io_bulk(ID,R1=Af)
   !
   call io_bulk(ID,'B_factors',VAR_SZ=(/it+1/))
   call io_bulk(ID,R1=Bf)
   !
   ! Cf used only for coupling
   !
   if (size(Cf)>2) then 
     call io_bulk(ID,'C_factors',VAR_SZ=(/2,it+1/))
     call io_bulk(ID,C1=Cf)
   endif
   !
   ! Vnp1 used only for coupling
   !
   if (associated(Vnp1)) then
     call io_bulk(ID,'Vnp1',VAR_SZ=(/2,HAYVEC_size/))
     call io_bulk(ID,C1=Vnp1)
   endif
   !
 end if
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_BSS_Haydock',OPR='stop')
 !
end function
