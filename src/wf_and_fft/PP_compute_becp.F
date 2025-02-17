!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF IM DS
!
subroutine PP_compute_becp(bands)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:n_sp_pol
 use R_lattice,     ONLY:nkibz,k_pt,b
 use interfaces,    ONLY:WF_load,WF_free
 use wave_func,     ONLY:WAVEs,wf_nc_k,wf_igk
 use hamiltonian,   ONLY:WF_G_max,WF_Go_indx
 use vec_operate,   ONLY:c2a
 use pseudo,        ONLY:becp,qe_pseudo_alloc
 use parallel_m,    ONLY:PAR_IND_WF_linear
 use qe_pseudo_m,   ONLY:vkb,tpiba
 use wvfct,         ONLY:npwx
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 integer, intent(in) :: bands(2)
 !
 integer          ::i_sp_pol,ikibz,npwk
 real(SP)         ::mndp,mxdp,xk(3)
 type(WAVEs)      ::WF
 !
 call timing('PP_compute_becp',"start")
 !
 ! checks
 !
 if (.not.qe_pseudo_alloc) call error(' [PP] qe_pseudo not alloc in PP_compute_becp')
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ikibz=1,nkibz
     !
     call WF_load(WF,WF_G_max,WF_Go_indx,bands,(/ikibz,ikibz/),(/i_sp_pol,i_sp_pol/),&
     &            title='-becp',space="C",keep_states_to_load=.true.)
     !
     !if (.not.any((/PAR_IND_WF_linear%element_2D(:,ikibz)/)) ) cycle
     !
     call c2a(b,k_pt(ikibz,:),xk,mode="ki2c")
     xk=xk/real(tpiba,SP)
     !
     npwk=wf_nc_k(ikibz)
     call init_us_2(npwk,wf_igk(1:npwk,ikibz),xk,vkb)
     !
     call PP_compute_becp_k( becp(ikibz,i_sp_pol), wf_nc_k(ikibz), bands, ikibz,i_sp_pol, WF)
     !
     call WF_free(WF) !,keep_fft=.true.)
     !
    enddo       ! i_k
 enddo          ! i_sp_pol
 !
 call timing('PP_compute_becp',"stop")
 !
end subroutine PP_compute_becp
!
!
subroutine PP_compute_becp_k(becp, npwk, bands, ikibz, i_sp_pol, WF)
 !
 ! Compute becp = < beta_j | wfc_n > matrix elements
 ! assuming init_us_2 has already been called for the given kpt.
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:WAVEs,wf_ncx
 use qe_pseudo_m,   ONLY:nkb,vkb,bec_type,is_allocated_bec_type,&
&                        allocate_bec_type,calbec
 !
#include<y_memory.h>
 !
 ! vars
 !
 type(bec_type), intent(inout) :: becp
 integer,        intent(in)    :: npwk,bands(2),ikibz,i_sp_pol
 type(WAVEs),    intent(in)    :: WF
 !
 ! Workspace
 !
 integer :: nb_max,ib,i_spinor,igs,ig,i_wf
 complex(SP), allocatable :: wf_g(:,:)
  
 !
 ! allocations
 !----------------------
 !
 YAMBO_ALLOC(wf_g,(wf_ncx*n_spinor,1))
 !
 if (.not.is_allocated_bec_type(becp)) then
   !
   call allocate_bec_type(nkb, bands(2), becp)
   !
 else
   if (becp%nbnd < nb_max) call error(" [PP] becp not properly dimensioned")
 endif

 !
 ! calbec
 !--------------------------------
 !
 if (allocated(becp%k))  becp%k=0.0
 if (allocated(becp%nc)) becp%nc=0.0
 !
 do ib = bands(1),bands(2)
   i_wf=WF%index(ib,ikibz,i_sp_pol)
   do i_spinor=1,n_spinor
     !
     igs=(i_spinor-1)*wf_ncx
     do ig = 1, wf_ncx
       wf_g(igs+ig,1)=WF%c(ig,i_spinor,i_wf)
     enddo
     !
   enddo
   !
   call calbec(npwk,vkb,wf_g,becp,ib,1)
   becp%init(ib)=.true.
   !
 enddo
 !
 YAMBO_FREE(wf_g)
 !
 return
 !
end subroutine PP_compute_becp_k
 
