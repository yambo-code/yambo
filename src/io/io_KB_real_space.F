!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
 !
 use pars,             ONLY:SP
 use stderr,           ONLY:intc
 use R_lattice,        ONLY:nkibz
 use electrons,        ONLY:n_spinor
 use pseudo,           ONLY:pp_kbv_dim_yambo
 use FFT_m,            ONLY:fft_size
 use IO_m,             ONLY:io_sec,frag_WF
 use IO_int,           ONLY:io_connect,io_disconnect,io_fragment, &
&                           io_elemental,io_bulk,io_header
 !
#include<memory.h>
 !
 integer,       intent(in)    :: ID
 integer,       intent(inout) :: kbv_real_space_table(pp_kbv_dim_yambo)
 complex(SP),   intent(inout) :: kbv_real_space(fft_size,n_spinor,pp_kbv_dim_yambo)
 !
 ! Workspace
 !
 integer :: ik,ifrag,i_sp_pol,ID_frag
 !
 ifrag=maxval(io_sec(ID,:))-1
 if(ifrag<=nkibz) then
   ik=ifrag
   i_sp_pol=1
 else
   ik=ifrag-nkibz
   i_sp_pol=2
 endif
 !
 io_KB_real_space=io_connect(desc="kb_pp_real_space",type=2,ID=ID)
 if (io_KB_real_space/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_KB_real_space=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   if (io_KB_real_space/=0) goto 1
   !
   call io_elemental(ID,VAR="PP_KBV_DIM",VAR_SZ=1,MENU=0)
   call io_elemental(ID,I0=pp_kbv_dim_yambo)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   call io_bulk(ID,VAR="PP_KBV_TABLE",VAR_SZ=(/pp_kbv_dim_yambo/))
   call io_bulk(ID,I1=kbv_real_space_table)
   !
   if (io_KB_real_space/=0) goto 1
   !
 endif
 !
 if (ifrag==0) goto 1
 !
 call io_fragment(ID,ID_frag,i_fragment=ifrag)
 !
 call io_bulk(ID_frag,VAR='PP_KB_RSPACE_IK'//trim(intc(ik))//'_SP_POL'//trim(intc(i_sp_pol)),&
 &               VAR_SZ=(/2,fft_size,n_spinor,pp_kbv_dim_yambo/))
 call io_bulk(ID_frag,C3=kbv_real_space(:,:,:))
 !
 call io_fragment_disconnect(ID,ID_frag)
 ! 
1 call io_disconnect(ID=ID)
 !
end function io_KB_real_space
