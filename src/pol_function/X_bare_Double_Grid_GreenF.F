!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine X_bare_Double_Grid_GreenF(iq,transitions,Xw,Xen,Xk,GreenF,ordering,space)
 !
 use pars,       ONLY:SP,cZERO,cONE
 use X_m,        ONLY:use_X_DbGd,integrating_over_q
 use BS,         ONLY:BSS_uses_DbGd
 use electrons,  ONLY:levels,BZ_DbGd_table,BZ_DbGd_nkpt,spin_occ,BZ_DbGd_nbands,&
                      BZ_DbGd_ipol_weight,BZ_DbGd_kpts
 use R_lattice,  ONLY:qindx_X,bz_samp,q_pt,nqibz
 use frequency,  ONLY:w_samp
 use vec_operate,ONLY:k2bz,iku_v_norm
 !
 implicit none
 integer      :: iq,transitions(4)
 type(w_samp) :: Xw
 type(levels) :: Xen
 type(bz_samp):: Xk
 complex(SP)  :: GreenF(Xw%n_freqs)
 character(*) :: ordering,space
 !
 !Work Space
 !
 logical      ::X_os_run,K_inv_run 
 integer      ::i1,i2,iw,ikbz,ikpbz,iv,ic,i_spin,ik_rand,ikp_rand,ikbz_rand,ikpbz_rand,ik,ikp
 real(SP)     ::ffac,cg_ffac,f_h,weight1,weight2,W_,q_pt_shift(3),q_norm_ref,qsize_fac
 complex(SP)  ::Z_,pole
 complex(SP),external      ::X_simple_GreenF
 !
 integer, allocatable      ::BZ_DbGd_iq_npt(:)
 logical, allocatable      ::use_kpt(:,:)
 !
 !
 X_os_run =trim(space)=="G"
 K_inv_run=trim(space)=="eh"
 !
 GreenF  = cZERO
 !
 ! Transitions
 !
 ikbz   = transitions(1)     
 ikpbz  = qindx_X(iq,ikbz,1) 
 ik     = Xk%sstar(ikbz,1)
 ikp    = Xk%sstar(ikpbz,1)
 iv     = transitions(2)
 ic     = transitions(3)
 i_spin = transitions(4)
 !
 allocate(BZ_DbGd_iq_npt( BZ_DbGd_nkpt(ikbz) ) )
 allocate(use_kpt( BZ_DbGd_nkpt(ikbz), BZ_DbGd_nkpt(ikpbz) ) )
 !
 !
 W_=0._SP
 Z_=cONE
 if (associated(Xen%W)) W_=abs(Xen%W(ic,ik,i_spin))+abs(Xen%W(iv,ikp,i_spin))
 if (associated(Xen%Z)) Z_=Xen%Z(ic,ik,i_spin)*Xen%Z(iv,ikp,i_spin)
 !
 if ((.not.use_X_DbGd.or.ic>BZ_DbGd_nbands).and.(.not.BSS_uses_DbGd)) then
   !
   ! Using QP corrected states
   !
   pole=cmplx(Xen%E(ic,ik,i_spin)-Xen%E(iv,ikp,i_spin),-W_)
   !
   do iw=1,Xw%n_freqs
     GreenF(iw)=X_simple_GreenF(Xw%p(iw),pole,ordering)*Z_
   enddo
   !
   return
   !
 endif
 !
 ! The GreenF is multiplied by the occupations on the coarse grid.
 ! Thus, to correctly renormalize, I have to:
 ! - divide   by the occupations on the coarse grid
 ! - multiply by the occupations on the fine   grid
 !
 ! In X_os.F I need the structure
 ! f_eh= Xen%f(iv,ikp,i_spin)*(spin_occ-Xen%f(ic,ik ,i_spin)) (see X_eh_setup.F)
 !
 if( X_os_run  ) cg_ffac = Xen%f(iv,ikp,i_spin)*( spin_occ-Xen%f(ic,ik ,i_spin) )
 !
 ! In K_inversion.F I need the structure
 ! f_eh= Ken%f(iv,ikp,i_sp)            -Ken%f(ic,ik ,i_sp))   (see K_tranisitions_setup.F)
 !
 if( K_inv_run ) cg_ffac = Xen%f(iv,ikp,i_spin)           -Xen%f(ic,ik ,i_spin)
 !
 !
 ! [DAVIDE]
 ! Define which kpts I will use in a consistent way between q=0 and q/=0
 ! For q/=0 this change makes the code much faster as it avoids the use of all 
 ! double-grid kpts in the two spheres (k and kp) which would smear out too much
 ! the absorption.
 ! For q==0 the approach can be used to properly describe the q->0 limit
 ! This part of the code should be moved away from this subroutine as here the 
 ! same procedure is repeated for each v -> c transition, which is a waste of CPU time.
 !
 q_norm_ref=100000._SP
 do i1=2,nqibz
    q_norm_ref=min(q_norm_ref,iku_v_norm(q_pt(i1,:)))
 enddo
 !
 qsize_fac=2._SP
 if(.not.integrating_over_q) qsize_fac=qsize_fac*10._SP
 !
 q_norm_ref=q_norm_ref/qsize_fac
 !
 BZ_DbGd_iq_npt=0
 use_kpt=.false.
 do i1=1,BZ_DbGd_nkpt(ikbz)
   ikbz_rand = BZ_DbGd_table(ikbz,i1,2)
   do i2=1,BZ_DbGd_nkpt(ikpbz)
     ikpbz_rand = BZ_DbGd_table(ikpbz,i2,2)
     q_pt_shift(:)=BZ_DbGd_kpts(ikbz_rand,:)-BZ_DbGd_kpts(ikpbz_rand,:)-q_pt(iq,:)
     if ( iku_v_norm(q_pt_shift) > q_norm_ref ) cycle
     BZ_DbGd_iq_npt(i1)=BZ_DbGd_iq_npt(i1)+1
     use_kpt(i1,i2)=.true.
   enddo
 enddo
 !
 do i1=1,BZ_DbGd_nkpt(ikbz)
   !
   weight1   = BZ_DbGd_ipol_weight(i1)/real(BZ_DbGd_nkpt(ikbz),SP)
   ik_rand   = BZ_DbGd_table(ikbz,i1,1)
   ikbz_rand = BZ_DbGd_table(ikbz,i1,2)
   !
   if( X_os_run  ) f_h = spin_occ-Xen%f_DbGd(ic,ik_rand,i_spin)
   if( K_inv_run ) f_h =          Xen%f_DbGd(ic,ik_rand,i_spin)
   !
   do i2=1,BZ_DbGd_nkpt(ikpbz)
     !
     if(.not.use_kpt(i1,i2)) cycle
     !
     weight2    = BZ_DbGd_ipol_weight(i2)/real(BZ_DbGd_iq_npt(i1),SP)
     ikp_rand   = BZ_DbGd_table(ikpbz,i2,1)
     ikpbz_rand = BZ_DbGd_table(ikpbz,i2,2)
     !
     if( X_os_run  ) ffac=(Xen%f_DbGd(iv,ikp_rand,i_spin)*f_h)/cg_ffac
     if( K_inv_run ) ffac=(Xen%f_DbGd(iv,ikp_rand,i_spin)-f_h)/cg_ffac
     !
     ffac=ffac*weight1*weight2*real(Z_)
     pole=cmplx(Xen%E_DbGd(ic,ik_rand,i_spin)-Xen%E_DbGd(iv,ikp_rand,i_spin),-W_)
     !
     do iw=1,Xw%n_freqs
       GreenF(iw)=GreenF(iw)+X_simple_GreenF(Xw%p(iw),pole,ordering)*ffac
     enddo
     !
   enddo
   !
 enddo
 !
 deallocate(use_kpt,BZ_DbGd_iq_npt)
 !
end subroutine
