!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function io_RIM_W(ID,mode,Xw)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:RIM_W_ng,RIM_n_rand_pts,RIM_W_is_diagonal,RL_vol,RIM_RL_vol,&
&                         RIM_W,nqibz,RIM_W_E,&
&                         RIM_id_epsm1_reference,RIM_epsm1,RIM_anisotropy
 use IO_m,           ONLY:io_sec,read_is_on,io_status
 use IO_int,         ONLY:io_connect,io_disconnect,io_header,&
&                         io_elemental,io_bulk
 use frequency,      ONLY:w_samp
 !
#include <memory.h>
 !
 integer :: ID
 type(w_samp) :: Xw
 character(3) :: mode
 !
 ! Work Space
 !
 real(SP)            ::RL_vol_disk
 integer             ::RIM_W_ng_disk
 logical             ::l_PPA_E
 !
 io_RIM_W=io_connect(desc="RIM_W",type=2,ID=ID)
 !
 l_PPA_E = .false.
 if(mode == "PPA") l_PPA_E = .true.
 !
 if (io_RIM_W/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_RIM_W=io_header(ID,QPTS=.true.,R_LATT=.true.,IMPOSE_SN=.true.,CUTOFF=.true.)
   if (io_RIM_W/=0) goto 1
   !
   RL_vol_disk=RL_vol
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=10,MENU=0)
   !
   call io_elemental(ID,I0=RIM_W_ng,DB_I0=RIM_W_ng_disk,&
&       VAR=' Screening RL components          :',CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,L0=RIM_W_is_diagonal,&
&       VAR=' Screening diagonal only          :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=RIM_n_rand_pts,&
&       VAR=' RIM random points                :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=RIM_RL_vol,&
&       VAR=' RIM  RL volume             [a.u.]:')
   call io_elemental(ID,I0=RIM_id_epsm1_reference,&
&       VAR=' Eps^-1 reference component        ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R1=RIM_epsm1,&
&       VAR=' Eps^-1 components                 ',CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,R0=RIM_anisotropy,&
&       VAR=' RIM anysotropy factor             ')
   call io_elemental(ID,L0=l_PPA_E,CHECK=.true.,&
&       VAR=' Interpolation of PPA poles at q=0:')
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   io_RIM_W=io_status(ID)
   if (io_RIM_W/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if (read_is_on(ID)) then
     YAMBO_ALLOC(RIM_W,(Xw%n_freqs,nqibz,RIM_W_ng,RIM_W_ng))
     if(l_PPA_E) YAMBO_ALLOC(RIM_W_E,(RIM_W_ng,RIM_W_ng))
   endif
   !
   call io_bulk(ID,VAR="RIM_W",VAR_SZ=(/2,Xw%n_freqs,nqibz,RIM_W_ng,RIM_W_ng/))
   call io_bulk(ID,C4=RIM_W)
   !
   if(l_PPA_E) then
     call io_bulk(ID,VAR="RIM_W_E",VAR_SZ=(/shape(RIM_W_E)/))
     call io_bulk(ID,R2=RIM_W_E)
   end if
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
