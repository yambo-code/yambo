!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF IM FP DS
!
subroutine PP_uspp_init()
 !
 ! general variables needed for the pseudo-potential
 !
 use drivers,       ONLY:l_HF_and_locXC,l_col_cut,&
&                        list_dbs,l_setup,l_rim
 use pars,          ONLY:SP,DP
 use vec_operate,   ONLY:c2a
 use D_lattice,     ONLY:nsym,dl_sop,atom_mapper
 use R_lattice,     ONLY:b,g_vec,ng_vec
 use pseudo,        ONLY:pp_is_uspp,qe_pseudo_alloc,PP_uspp_free,qe_atoms_map,PP_uspp_alloc
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,REP
 !
 use qe_pseudo_m,   ONLY:qe_alat=>alat,qe_tpiba=>tpiba,qe_nat=>nat,qe_tau=>tau,qe_nsp=>nsp,qe_ityp=>ityp
 use qe_pseudo_m,   ONLY:nkb,qe_nsym=>nsym,d1,d2,d3
 use gvect,         ONLY:qe_eigts1=>eigts1, qe_eigts2=>eigts2, qe_eigts3=>eigts3,&
&                        qe_g=>g, qe_gg=>gg, qe_ngm=>ngm, qe_mill=>mill
 use vlocal,        ONLY:strf,vloc_alloc,vloc_dealloc
 use timing_m,      ONLY:timing
 use parser_m,      ONLY:parser
 !
#include<y_memory.h>
 !
 ! Work Space 
 !
 integer  :: io_err,ID,ID_vloc
 integer  :: gv_min,gv_max,i,ig,nfft(3)
 real(SP) :: v1(3)
 real(DP) :: qe_b(3,3)
 !complex(DP), allocatable :: strf(:,:)
 logical :: l_will_need_becp
 !
 call timing("PP_uspp_init","start")
 !
 ! atom mapping
 !
 call atom_mapper(qe_nat,real(qe_tau*qe_alat,SP),"cc",qe_atoms_map)
 !
 ! symmetries
 !
 qe_nsym=nsym
 call d_matrix(nsym,real(dl_sop,DP),d1,d2,d3) 
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
 !
 qe_b=transpose(b)/qe_tpiba
 !
 ! This is the part which depends on the atomic positions
 !
 call struc_fact(qe_nat,qe_tau,qe_nsp,qe_ityp,qe_ngm,qe_g,qe_b,&
&                nfft(1),nfft(2),nfft(3), &
&                .true.,strf,.true.,qe_eigts1,qe_eigts2,qe_eigts3)
 !
 !
 call timing("PP_uspp_init","stop")
 !
 return
 !
end subroutine PP_uspp_init

subroutine PP_compute_deeq()
 !
 ! This subroutine gets the D-term needed to compute the
 ! nonlocal part of the Hamiltonian together with the KB
 ! projectors.
 !
 ! Adapted from subroutine newd() in PW/src/newd.f90 in QE (ovkan=.false. case).
 !
 ! USPP part (Q function) NOT implemented.
 !
 use pars,                   ONLY:SP,DP
 ! data already stored by p2y
 use uspp,                   ONLY:deeq,deeq_nc,dvan,dvan_so
 use uspp_param,             ONLY:nh
 use ions_base,              ONLY:nat,ityp
 use lsda_mod,               ONLY:nspin
 use noncollin_module,       ONLY: noncolin
 ! qe_pseudo internal modules
 use spin_orb,               ONLY:qe_lspinorb=>lspinorb
 !
 ! Work Space
 !
 integer :: it, ia, is, nht
 ! counters atom type, atoms, spin, aux
 !
 do ia = 1, nat
   !
   it  = ityp(ia)
   nht = nh(it)
   !
   if ( qe_lspinorb ) then
     !
     deeq_nc(1:nht,1:nht,ia,1:nspin) = dvan_so(1:nht,1:nht,1:nspin,it)
     !
   elseif ( noncolin ) then
     !
     deeq_nc(1:nht,1:nht,ia,1) = dvan(1:nht,1:nht,it)
     deeq_nc(1:nht,1:nht,ia,2) = ( 0.D0, 0.D0 )
     deeq_nc(1:nht,1:nht,ia,3) = ( 0.D0, 0.D0 )
     deeq_nc(1:nht,1:nht,ia,4) = dvan(1:nht,1:nht,it)
     !
   else
     !
     do is = 1, nspin
       !
       deeq(1:nht,1:nht,ia,is) = dvan(1:nht,1:nht,it)
       !
     enddo
     !
   endif
   !
 enddo
 !
end subroutine

subroutine PP_compute_Vloc()
 !
 ! This subroutine multiplies the structure factors to the Vloc in G-space:
 !
 ! vloc_full(G) = sum_sp vloc(G,sp) * struct_factor(G,sp)
 !
 ! vloc (qe: vloc_of_g / vloc_yambo) is read from database 
 ! [use p2y -p when generating SAVE]
 !
 ! Adapted from subroutine setlocal() in PW/src/setlocal.f90 in QE
 !
 ! 2D cutoff and ESM not supported.
 !
 ! qe_pseudo internal modules
 use pars,           ONLY: DP
 use constants,      ONLY: eps8
 !use ions_base,      ONLY: zv, ntyp => nsp
 use cell_base,      ONLY: omega
 use gvect,          ONLY: igtongl, gg, qe_ngm => ngm
 use qe_pseudo_m,    ONLY: qe_nsp => nsp
 use vlocal,         ONLY: vloc_yambo, vloc_full, strf
 !use fft_base,       ONLY: dfftp
 use FFT_m,          ONLY:fft_size
 use control_flags,  ONLY: gamma_only
 !use mp_bands,       ONLY: intra_bgrp_comm
 !use mp,             ONLY: mp_sum
 !
 ! - vloc_yambo contains the G-components per atomic species
 ! - vloc_full is the actual vloc(G) including the structure factors
 !   (called 'aux' in original QE routine since they just used it for invfft)
 !
 ! Work space
 !
 INTEGER :: it, ig
 ! counter on atom types
 ! counter on g vectors
 !
 vloc_full(:) = (0.d0,0.d0)
 !
 ! Compute vloc_full
 !
 do it = 1, qe_nsp
    do ig = 1, qe_ngm ! NOTA BENE: qe_ngm = dense G-vectors (per proc. in QE) > wf_ng 
       vloc_full(ig) = vloc_full(ig) + vloc_yambo(ig,it) * strf(ig,it)
    enddo
 enddo
 !
 if (gamma_only) then
    do ig = 1, qe_ngm
       vloc_full(ig) = CONJG( vloc_full(ig) )
    enddo
 endif
 ! ... vloc_full = potential in G-space.
 !
 ! [FP] The following QE part should not be needed. Commented.
 !
 ! ... v_of_0 is (Vloc)(G=0)
 !
 !v_of_0 = 0.0_DP
 !IF (gg(1) < eps8) v_of_0 = DBLE( vloc_full(dfftp%nl(1)) )
 !
 !CALL mp_sum( v_of_0, intra_bgrp_comm )
 !
end subroutine PP_compute_Vloc
