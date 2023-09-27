!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM, FP
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
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
 use pseudo,        ONLY:pp_is_uspp,qe_pseudo_alloc,PP_uspp_free,qe_atoms_map,&
&                        l_use_p2_Vloc_becp,PP_uspp_alloc
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
#include<memory.h>
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
 integer, external :: io_USPP_pwscf
 integer, external :: io_VLOC_pwscf

 call parser('BareHfromScratch', l_will_need_becp)
 if (.not.l_will_need_becp) return
 !
 ! in case, assume qe_pseudo already alloc and init
 !
 if (qe_pseudo_alloc) return
 call timing("PP_uspp_init","start")
 ! [FP]
 if (l_use_p2_Vloc_becp) then
   call PP_uspp_alloc() ! projectors for nonlocal part
 endif
 ! [FP] 
 !
 ! perform main data IO
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_USPP_pwscf(ID)
 !
 pp_is_uspp=(io_err==0)
 !
 ! [FP] 
#ifndef _USPP
 if (pp_is_uspp.and..not.l_will_need_becp) then
   call error("[PPs] Ultrasoft PP not supported. [Is ns.uspp_pp_pwscf present but should not?]")
 endif
#endif
 !
 ! atom mapping
 !
 call atom_mapper(qe_nat,real(qe_tau*qe_alat,SP),"cc",qe_atoms_map)
 !
 if ((pp_is_uspp.and.l_will_need_becp.and..not.l_use_p2_Vloc_becp).or.&
    (.not.pp_is_uspp.or.nkb<=0)) then
 !if (.not.pp_is_uspp.or.nkb<=0) then
   ! [FP]
   pp_is_uspp=.false.
   call PP_uspp_free()
   call timing("PP_uspp_init","stop")
   return
 endif
 

 !
 ! checks
 ! [FP]
 if (.not.( l_setup.or.l_col_cut.or.l_rim.or.list_dbs.or. &
            l_HF_and_locXC.or.l_use_p2_Vloc_becp) ) then
    ! [FP]
    call error("[PPs] USPP not implemented for current runlevel")
 endif

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
 ! [FP]
 ! vlocal quantities: strf and - if needed - vloc_yambo and vloc_full
 call vloc_alloc(l_use_p2_Vloc_becp)
 ! [FP]
 !
 qe_b=transpose(b)/qe_tpiba
 !
 call struc_fact(qe_nat,qe_tau,qe_nsp,qe_ityp,qe_ngm,qe_g,qe_b,&
&                nfft(1),nfft(2),nfft(3), &
&                .true.,strf,.true.,qe_eigts1,qe_eigts2,qe_eigts3)
 !
 ! [FP]
 if (l_use_p2_Vloc_becp) then
   !
   ! Now that we have the structure factors, read and compute vloc
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID_vloc)
   io_err=io_VLOC_pwscf(ID_vloc)
   !
   call PP_compute_Vloc()
   !
   ! Compute D_ij coefficients for nonlocal part of PP 
   call PP_compute_deeq()
   !
 else
   !
   ! Dealloc strf here
   call vloc_dealloc()
   !
 endif
 ! [FP]
 !
 call timing("PP_uspp_init","stop")
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
