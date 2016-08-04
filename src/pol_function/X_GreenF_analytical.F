!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine X_GreenF_analytical(iq,transitions,Xw,Xen,Xk,GreenF,ordering,space)
 !
 ! Compute the GreenF in frequency space starting from it analytical
 ! expression as the Fourier Transform of a Lorentzian
 !
 use pars,       ONLY:SP,cZERO,cONE
 use X_m,        ONLY:use_X_DbGd,X_is_evaluated_for_all_Qs
 use BS,         ONLY:BSS_uses_DbGd
 use electrons,  ONLY:levels,spin_occ
 use R_lattice,  ONLY:qindx_X,bz_samp,q_pt,nqibz
 use frequency,  ONLY:w_samp
 use vec_operate,ONLY:iku_v_norm
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
 logical      :: X_os_run,K_inv_run,use_DbGd
 integer      :: i1,i2,iw,ikbz,ikpbz,iv,ic,i_spin,ik_rand,ikp_rand,ikbz_rand,ikpbz_rand,ik,ikp
 real(SP)     :: ffac,cg_ffac,f_h,weight1,weight2,W_,q_pt_shift(3),q_norm_ref,qsize_fac,&
&                kpt_i1(3),kpt_i2(3)
 complex(SP)  :: Z_,pole
 !
 complex(SP),external      ::Lorentzian_FT
 !
 integer, allocatable      ::BZ_DbGd_iq_npt(:)
 logical, allocatable      ::use_kpt(:,:)
 !
 GreenF  = cZERO
 !
 X_os_run =trim(space)=="G"
 K_inv_run=trim(space)=="eh"
 !
 ! Transitions
 !
 ikbz   = transitions(1)
 iv     = transitions(2)
 ic     = transitions(3)
 i_spin = transitions(4)
 ! 
 ikpbz  = qindx_X(iq,ikbz,1) 
 ik     = Xk%sstar(ikbz,1)
 ikp    = Xk%sstar(ikpbz,1)
 !
 W_=0._SP
 Z_=cONE
 if (associated(Xen%W)) W_=abs(Xen%W(ic,ik,i_spin))+abs(Xen%W(iv,ikp,i_spin))
 if (associated(Xen%Z)) Z_=Xen%Z(ic,ik,i_spin)*Xen%Z(iv,ikp,i_spin)
 !
 use_DbGd= (X_os_run .and.use_X_DbGd.and.ic>Xen%nb_FineGd) .or. &
&          (K_inv_run.and.BSS_uses_DbGd)
 !
 ! Case without DbGd
 !===================
 !
 ! In K_inversion_Lo the occupations and the Z_factor are not included.
 ! Thus I have to multiply by them the Green function
 !
 if ( use_DbGd ) then
    !
    pole=cmplx(Xen%E(ic,ik,i_spin)-Xen%E(iv,ikp,i_spin),-W_,SP)
    !
    if( K_inv_run ) ffac = ( Xen%f(iv,ikp,i_spin) - Xen%f(ic,ik ,i_spin) )/spin_occ*real(Z_)
    if( X_os_run  ) ffac = 1._SP
    !
    do iw=1,Xw%n_freqs
      GreenF(iw)=Lorentzian_FT(Xw%p(iw),pole,ordering)*ffac
    enddo
    !
    return
    !
 endif
 !
 ! Case with DbGd
 !================
 !
 ! In X_os the GreenF is multiplied by the occupations on the coarse grid.
 ! Thus, to correctly renormalize, I have to:
 ! - divide   by the occupations on the coarse grid
 ! - multiply by the occupations on the fine   grid
 !
 if( K_inv_run ) cg_ffac = spin_occ/real(Z_)
 if( X_os_run  ) cg_ffac = Xen%f(iv,ikp,i_spin)*( spin_occ-Xen%f(ic,ik ,i_spin) )
 !
 allocate(BZ_DbGd_iq_npt( Xk%nbz_mapped(ikbz) ) )
 allocate(use_kpt( 0:Xk%nbz_mapped(ikbz), 0:Xk%nbz_mapped(ikpbz) ) )
 !
 call tmp_double_grid()
 !
 ! Coarse grid points are included for i1 and i2 =0
 ! Fine   grid points are included for i1 and i2 >0
 !
 do i1=0,Xk%nbz_mapped(ikbz)
   !
   if(i1==0) then
     weight1   = 1._SP/real(Xk%nbz_mapped(ikbz)+1,SP)
     ffac      = Xen%f_FineGd(ic,ik,i_spin)
   else
     weight1   = 1._SP/real(Xk%nbz_mapped(ikbz)+1,SP)
     ikbz_rand = Xk%bz_DbGd_map(ikbz,i1)
     ik_rand   = Xk%sstar_FineGd(ikbz_rand)
     ffac      = Xen%f_FineGd(ic,ik_rand,i_spin)
   endif
   !
   if( X_os_run  ) f_h = spin_occ-ffac
   if( K_inv_run ) f_h =          ffac
   !
   do i2=0,Xk%nbz_mapped(ikpbz)
     !
     if(.not.use_kpt(i1,i2)) cycle
     !
     weight2    = 1._SP/real(BZ_DbGd_iq_npt(i1),SP)
     ikpbz_rand = Xk%bz_DbGd_map(ikpbz,i2)
     ikp_rand   = Xk%sstar_FineGd(ikpbz_rand)
     !
     if( X_os_run  ) ffac=(Xen%f_FineGd(iv,ikp_rand,i_spin)*f_h)/cg_ffac
     if( K_inv_run ) ffac=(Xen%f_FineGd(iv,ikp_rand,i_spin)-f_h)/cg_ffac
     !
     ffac=ffac*weight1*weight2
     pole=cmplx(Xen%E_FineGd(ic,ik_rand,i_spin)-Xen%E_FineGd(iv,ikp_rand,i_spin),-W_)
     !
     do iw=1,Xw%n_freqs
       GreenF(iw)=GreenF(iw)+Lorentzian_FT(Xw%p(iw),pole,ordering)*ffac
     enddo
     !
   enddo
   !
 enddo
 !
 deallocate(use_kpt,BZ_DbGd_iq_npt)
 !
 contains
   !
   subroutine tmp_double_grid()
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
     if(.not.X_is_evaluated_for_all_Qs) qsize_fac=qsize_fac*10._SP
     !
     q_norm_ref=q_norm_ref/qsize_fac
     !
     BZ_DbGd_iq_npt=0
     use_kpt=.false.
     do i1=0,Xk%nbz_mapped(ikbz)
       if(i1==0) kpt_i1=Xk%ptbz(ikbz,:)
       if(i1> 0) kpt_i1=Xk%ptbz_FineGd(Xk%bz_DbGd_map(ikbz,i1),:)
       do i2=1,Xk%nbz_mapped(ikpbz)
         if(i2==0) kpt_i2=Xk%ptbz(ikpbz,:)
         if(i2>0 ) kpt_i2=Xk%ptbz_FineGd(Xk%bz_DbGd_map(ikpbz,i2),:)
         q_pt_shift(:)=kpt_i1-kpt_i2-q_pt(iq,:)
         if ( iku_v_norm(q_pt_shift) > q_norm_ref ) cycle
         BZ_DbGd_iq_npt(i1)=BZ_DbGd_iq_npt(i1)+1
         use_kpt(i1,i2)=.true.
       enddo
     enddo
     !
   end subroutine
   !
end subroutine
