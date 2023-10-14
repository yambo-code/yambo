!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF IM
!
subroutine PP_uspp_init()
 !
 ! checks and reads USPP DB, and, in case, initialize
 ! general USPP vars
 !
 use drivers,       ONLY:l_HF_and_locXC,l_col_cut,&
&                        list_dbs,l_setup,l_rim
 use pars,          ONLY:SP,DP
 use vec_operate,   ONLY:c2a
 use D_lattice,     ONLY:nsym,dl_sop,atom_mapper
 use R_lattice,     ONLY:b,g_vec,ng_vec
 use pseudo,        ONLY:pp_is_uspp,qe_pseudo_alloc,PP_uspp_free,qe_atoms_map
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,REP
 !
 use qe_pseudo_m,   ONLY:qe_alat=>alat,qe_tpiba=>tpiba,qe_nat=>nat,qe_tau=>tau,qe_nsp=>nsp,qe_ityp=>ityp
 use qe_pseudo_m,   ONLY:nkb,qe_nsym=>nsym,d1,d2,d3
 use gvect,         ONLY:qe_eigts1=>eigts1, qe_eigts2=>eigts2, qe_eigts3=>eigts3,&
&                        qe_g=>g, qe_gg=>gg, qe_ngm=>ngm, qe_mill=>mill
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 ! Work Space 
 !
 integer  :: io_err,ID
 integer  :: gv_min,gv_max,i,ig,nfft(3)
 real(SP) :: v1(3)
 real(DP) :: qe_b(3,3)
 complex(DP), allocatable :: strf(:,:)
 !
 integer, external :: io_USPP_pwscf

 !
 ! in case, assume qe_pseudo already alloc and init
 !
 if (qe_pseudo_alloc) return
 call timing("PP_uspp_init","start")

 !
 ! perform main data IO
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_USPP_pwscf(ID)
 !
 pp_is_uspp=(io_err==0)
 !
#ifndef _USPP
 if (pp_is_uspp) call error("[PPs] Ultrasoft PP not supported")
#endif
 !
 if (.not.pp_is_uspp.or.nkb<=0) then
   pp_is_uspp=.false.
   call PP_uspp_free()
   call timing("PP_uspp_init","stop")
   return
 endif
 
 !
 ! checks
 !
 if (.not.( l_setup.or.l_col_cut.or.l_rim.or.list_dbs.or. &
            l_HF_and_locXC) ) then
    call error("[PPs] USPP not implemented for current runlevel")
 endif

 !
 ! symmetries
 !
 qe_nsym=nsym
 call d_matrix(nsym,real(dl_sop,DP),d1,d2,d3)

 !
 ! atom mapping
 !
 call atom_mapper(qe_nat,real(qe_tau*qe_alat,SP),"cc",qe_atoms_map)
 
 !
 ! init g vectors and structure factors
 !
 qe_ngm=ng_vec
 !
 YAMBO_ALLOC(qe_g,(3,qe_ngm))
 YAMBO_ALLOC(qe_gg,(qe_ngm))
 YAMBO_ALLOC(qe_mill,(3,qe_ngm))
 !
 do ig = 1, ng_vec
    !
    !qe_g(:,ig)=g_vec(ig,:)
    call c2a(b,g_vec(ig,:),v1,'ki2c')
    qe_g(:,ig)=v1/qe_tpiba
    !
    qe_gg(ig)=sum(qe_g(:,ig)**2)
    !
    call c2a(b,g_vec(ig,:),v1,'ki2a')
    qe_mill(:,ig)=nint(v1)
    !
 enddo
 !
 do i = 1, 3
   gv_min=minval(qe_mill(i,:))
   gv_max=maxval(qe_mill(i,:))
   nfft(i)=gv_max-gv_min+1
 enddo
 !
 YAMBO_ALLOC(qe_eigts1,(-nfft(1):nfft(1),qe_nat))
 YAMBO_ALLOC(qe_eigts2,(-nfft(2):nfft(2),qe_nat))
 YAMBO_ALLOC(qe_eigts3,(-nfft(3):nfft(3),qe_nat))
 YAMBO_ALLOC(strf,(qe_ngm,qe_nsp))
 !
 qe_b=transpose(b)/qe_tpiba
 !
 call struc_fact(qe_nat,qe_tau,qe_nsp,qe_ityp,qe_ngm,qe_g,qe_b,&
&                nfft(1),nfft(2),nfft(3), &
&                .false.,strf,.true.,qe_eigts1,qe_eigts2,qe_eigts3)
 YAMBO_FREE(strf)
 !
 call timing("PP_uspp_init","stop")
 return
 !
end subroutine PP_uspp_init
