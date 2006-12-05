!
! Copyright (C) 2000-2005 M. Gruening and the SELF team 
!          http://www.fisica.uniroma2.it/~self
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
 subroutine X_O_gks(Xen,Xk,X)
!============================
 use pars,          ONLY:SP,DP,pi
 use timing,        ONLY:live_timing
 use electrons,     ONLY:levels,n_spin
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,&
&                        myid,master_node,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use wave_func,     ONLY:wf,wf_ng,wf_state
 use R_lattice,     ONLY:bz_samp,qindx_S,q_pt,q_sstar,nqbz  
 use X_m,           ONLY:X_t,X_q0grad
 use collision,     ONLY:ggwinfo,collision_reset
 use com,           ONLY:depth
 use par_indexes_m, ONLY:par_indexes
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset      
 use DFT,           ONLY:X_gks_table, X_gks_nstates, damp, damp_dim
!
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
!
!ws
 type(ggwinfo) ::isc,iscp
 type(pp_indexes) ::px
 complex(DP) :: dsx(3) 
 integer iq, ib, i1,alloc_err, i_spin 
#if defined _DOUBLE
 complex(DP):: zdotc,zdotu
#else 
 complex(SP):: cdotc,cdotu
#endif
!
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
!---------------------------------------------------------------------
!
! set up and allocation
!
 call set_X_table(X,Xen,Xk)
 call set_par_nlcm(X%ngostnts)
 allocate(damp(3,damp_dim,X%ngostnts),isc%rhotw(X%ngostnts),iscp%rhotw(X%ngostnts))
 isc%qs   = 1
 isc%ngrho= X%ngostnts
 call fft_setup(isc%ngrho,maxval(qindx_S(:,:,2)),.false.)
!
 call scatterDamp(isc)
! 
! Set up the parallel environment
! 
 call par_indexes(px,(/nqbz,Xen%nbm/),(/1,X%ib(1)/))
 call live_timing('Optical strengths (GKS):',px%stps(myid+1)*X_gks_nstates)
 call pp_redux_wait
!
 do iq=1,nqbz
   isc%qs(2:)=(/q_sstar(iq,1),q_sstar(iq,2)/)
   if (isc%qs(2)/=isc%iqref) call scatterDamp(isc)
   do i1=1,X_gks_nstates
     isc%is=(/X_gks_table(i1,1),X_gks_table(i1,3),X_gks_table(i1,4),1/)
     isc%os(2:)=(/Xk%sstar(qindx_S(isc%is(2),iq,1),:),1/)
     iscp%is=(/X_gks_table(i1,2),X_gks_table(i1,3),X_gks_table(i1,4),1/)
     isc%qs(1)=qindx_S(X_gks_table(i1,3),iq,2)
     iscp%qs=isc%qs
     do ib=x%ib(1),Xen%nbm
       if (.not.px%i2p(iq,ib)) cycle
       isc%os(1)=ib
       iscp%os=isc%os
       call live_timing(steps=1)
       call scatterBamp(isc)
       iscp%rhotw=isc%rhotw
       if (any(isc%is/=iscp%is)) call scatterBamp(iscp)
       isc%rhotw=4.*pi*isc%rhotw*conjg(iscp%rhotw)*Xen%f(ib,isc%os(2),isc%os(4))
       dsx=(0._DP,0._DP)
      !NB only valid for damp%dim=1  
#if defined _DOUBLE
       dsx(1)=zdotu(X%ngostnts,isc%rhotw,1,damp(1,1,:),1)
       dsx(2)=zdotu(X%ngostnts,isc%rhotw,1,damp(2,1,:),1)
       dsx(3)=zdotu(X%ngostnts,isc%rhotw,1,damp(3,1,:),1)
#else 
       dsx(1)=cdotu(X%ngostnts,isc%rhotw,1,damp(1,1,:),1)
       dsx(2)=cdotu(X%ngostnts,isc%rhotw,1,damp(2,1,:),1)
       dsx(3)=cdotu(X%ngostnts,isc%rhotw,1,damp(3,1,:),1)
#endif
       X_q0grad(:,iscp%is(1),isc%is(1),isc%is(2),isc%is(3))=&
&       X_q0grad(:,iscp%is(1),isc%is(1),isc%is(2),isc%is(3))+dsx(:) 
     enddo
   enddo
 enddo
 call live_timing()
 deallocate(damp,isc%rhotw,iscp%rhotw)
 if (allocated(X_gks_table)) deallocate(X_gks_table)
 do i_spin=1,n_spin
   call pp_redux_wait(X_q0grad(:,:,:,:,i_spin))
 end do
 call collision_reset(isc)
 call collision_reset(iscp) 
 call pp_indexes_reset(px)
!
contains
!============================================
 subroutine set_X_table(X,Xen,Xk)
!============================================
 use pars,          ONLY:SP,pi
 use R_lattice,     ONLY:bz_samp
 use electrons,     ONLY:levels,n_spin
 use X_m,           ONLY:X_t
 use memory_m,      ONLY:mem_est
 use DFT,           ONLY:X_gks_nstates, X_gks_table
 implicit none
 type(levels),  intent(in)  :: Xen
 type(bz_samp), intent(in)  :: Xk
 type(X_t),     intent(in)  :: X
!
! ws
 integer ik, ic, iv, is, in 
!
 if (.not.allocated(X_gks_table)) then
   X_gks_nstates = n_spin*Xk%nibz*(Xen%nbm-X%ib(1)+1)*(X%ib(2)-Xen%nbf) 
   allocate(X_gks_table(X_gks_nstates,4),stat=alloc_err)
   call mem_est("GKS_X_table",(/size(X_gks_table)/),errors=(/alloc_err/))
 endif 
 in = 0
 do ik=1,Xk%nibz
   do is = 1,n_spin
     do iv=X%ib(1),Xen%nbm
       do ic=Xen%nbf+1,X%ib(2)
          in = in + 1 
          X_gks_table(in,1) = iv
          X_gks_table(in,2) = ic
          X_gks_table(in,3) = ik
          X_gks_table(in,4) = is
       enddo
     enddo
   enddo
 enddo
!
 end subroutine set_X_table
! 
 subroutine set_par_nlcm(ng)
!================================
 use xc_functionals, ONLY:GS_xc_FUNCTIONAL,XC_GKS_HF, XC_GKS_SXLDA,XC_GKS_CHSX 
 use DFT,            ONLY:which_gks, damp_dim
 implicit none
 integer,        intent(in)   ::ng
    
  select case (GS_xc_FUNCTIONAL)
  case(XC_GKS_HF)
    which_gks = 0
    damp_dim = 1
  case(XC_GKS_SXLDA)
    which_gks = 1
    damp_dim = 1
  case(XC_GKS_CHSX)
    which_gks = 2
    damp_dim = ng
  end select
 end subroutine set_par_nlcm
!
 subroutine scatterDamp(isc)
!================================
 use R_lattice,     ONLY:d3q_factor,bare_qpg,q_pt,g_vec
 use D_lattice,     ONLY:alat
 use collision,     ONLY:bare_qpg, ggwinfo
 use DFT,           ONLY: damp, damp_dim,which_gks  
 implicit none
 type(ggwinfo),intent(inout) ::isc
!ws
 integer :: ng(2),ig1,ig2,iq,ig
 real(SP):: Co
!
 Co=d3q_factor/(2.*pi)**3. ! = 1./(DL_vol*nqbz)
!-------------------------------------------------------------------------
! Calculate (2q+G+G')/(|q+G||q+G'|)**2
! needed in the evaluation of the nonlocal contribution of the oscillators. 
 !
! Note that <q_i/q^4> is odd and on a spherical region is zero (CHECK!)
! so, contrary to Gamp no contribution is coming from the q around 0.
!-------------------------------------------------------------------------
 iq=isc%qs(2)
 isc%iqref=iq
 ng=shape(damp(1,:,:))
 do ig=1,ng(1)
   do ig2=1,ng(2)
      ig1 = ig   
      if (ng(1)==1) ig1=ig2
      damp(:,ig,ig2)=Co*((2.*q_pt(iq,:)+g_vec(ig1,:)+g_vec(ig2,:))*2.*pi/alat(:))/&
&                         (bare_qpg(iq,ig1)**2.)/(bare_qpg(iq,ig2)**2.)
      if ((iq==1.and.ig1==1).or.(iq==1.and.ig2==1)) damp(:,ig,ig2)=0.
   enddo
 end do
 end subroutine scatterDamp
!
 end subroutine X_O_gks

 
