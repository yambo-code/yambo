!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine X_GreenF_analytical(iq,transitions,Xw,Xen,Xk,GreenF,ordering,space,no_occupations)
 !
 ! Compute the GreenF in frequency space starting from it analytical
 ! expression as the Fourier Transform of a Lorentzian
 !
 ! In the K case, the anti-res part of Lo uses a w+\Omega (and not -\Omega) simply
 ! because this routine is always called using the i_res \Omega. Indeed 
 !
 !  \Omega_{anti-res} = -\Omega_{res}

 !
 use pars,       ONLY:SP,cZERO,cONE
 use X_m,        ONLY:use_X_DbGd,X_is_integrated,&
&                     X_t,l_X_terminator,X_terminator_E,X_term_E
 use BS_solvers, ONLY:BSS_uses_DbGd
 use electrons,  ONLY:levels,spin_occ
 use R_lattice,  ONLY:qindx_X,bz_samp,nqibz,smallest_q_norm
 use frequency,  ONLY:w_samp
 use vec_operate,ONLY:iku_v_norm
 !
#include<memory.h>
 !
 integer      :: iq,transitions(4)
 type(w_samp) :: Xw
 type(levels) :: Xen
 type(bz_samp):: Xk
 complex(SP)  :: GreenF(Xw%n_freqs)
 character(*) :: ordering,space
 logical      :: no_occupations
 !
 ! Work Space
 !
 logical      :: X_os_run,K_inv_run,use_DbGd
 integer      :: ikbz_FineGd,ikpbz_FineGd,iw,ikbz,ikpbz,iv,ic,i_spin,ik_FineGd,ikp_FineGd,ik,ikp,&
&                low_ik,upp_ik,low_ikp,upp_ikp
 real(SP)     :: ffac,cg_ffac,f_e,f_h,weight,W_,q_pt_shift(3),q_norm_ref,qsize_fac,k_pt_loc(3),kp_pt_loc(3),En1,En2
 complex(SP)  :: Z_,pole,pole_X_term
 !
 complex(SP),external      ::Lorentzian_FT
 !
 integer, allocatable      ::nkpt_DbGd_scatt(:)
 logical, allocatable      ::use_kpt_DbGd(:,:)
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
 if (allocated(Xen%W)) W_=abs(Xen%W(ic,ik,i_spin))+abs(Xen%W(iv,ikp,i_spin))
 if (allocated(Xen%Z)) Z_=Xen%Z(ic,ik,i_spin)*Xen%Z(iv,ikp,i_spin)
 !
 use_DbGd= (X_os_run .and.use_X_DbGd.and.ic<=Xen%nb_FineGd) .or. &
&          (K_inv_run.and.BSS_uses_DbGd)
 !
 !
 ! Case without DbGd
 !===================
 !
 ! In K_inversion_Lo the occupations and the Z_factor are not included.
 ! Thus I have to multiply by them the Green function
 !
 if ( .not.use_DbGd ) then
   !
   pole=cmplx(Xen%E(ic,ik,i_spin)-Xen%E(iv,ikp,i_spin),-W_,KIND=SP)
   !
   pole_X_term=0.0_SP
   if(l_X_terminator) pole_X_term= cmplx(X_term_E-Xen%E(iv,ikp,i_spin),-W_,KIND=SP)
   !
   if( X_os_run  )      ffac = 1._SP
   if( K_inv_run )      ffac = ( Xen%f(iv,ikp,i_spin) - Xen%f(ic,ik ,i_spin) )/spin_occ*real(Z_)
   if( no_occupations ) ffac = 1._SP
   !
   do iw=1,Xw%n_freqs
     !
     GreenF(iw)= Lorentzian_FT(Xw%p(iw),pole,ordering)*ffac
     !
     if (l_X_terminator.and.ic>Xen%nbm) then
       GreenF(iw)= GreenF(iw) -Lorentzian_FT(Xw%p(iw),pole_X_term,ordering)*ffac  
     endif
     !
     if (l_X_terminator.and.ic<=Xen%nbm) then
       GreenF(iw)= -Lorentzian_FT(Xw%p(iw),pole_X_term,ordering)*ffac 
     endif
     !
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
 if( X_os_run  ) cg_ffac = Xen%f(iv,ikp,i_spin)*( spin_occ-Xen%f(ic,ik ,i_spin) )
 if( K_inv_run ) cg_ffac = spin_occ/real(Z_)
 !
 low_ik=Xk%bz_DbGd_map(1,ikbz)-1
 upp_ik=Xk%bz_DbGd_map(2,ikbz)
 low_ikp=Xk%bz_DbGd_map(1,ikpbz)-1
 upp_ikp=Xk%bz_DbGd_map(2,ikpbz)
 allocate( nkpt_DbGd_scatt(low_ik:upp_ik) )
 YAMBO_ALLOC(use_kpt_DbGd,(low_ikp:upp_ikp,low_ik:upp_ik))
 !
 if(.not.allocated(Xk%ptbz)) call k_ibz2bz(Xk,'i',.true.)
 !
 call define_use_kpt()
 !
 ! In the K case, the anti-res part of Lo uses a w+\Omega (and not -\Omega) simply
 ! because this routine is always called using the i_res \Omega. Indeed 
 !
 !  \Omega_{anti-res} = -\Omega_{res}
 !
 ! Coarse grid points are included for ikbz_FineGd= Xk%bz_DbGd_map(1,ikbz)-1
 ! Fine   grid points are included for ikbz_FineGd>=Xk%bz_DbGd_map(1,ikbz)
 !
 do ikbz_FineGd=Xk%bz_DbGd_map(1,ikbz)-1,Xk%bz_DbGd_map(2,ikbz)
   !
   weight = 1._SP/real(Xk%nbz_mapped(ikbz)+1,SP)/real(nkpt_DbGd_scatt(ikbz_FineGd),SP)
   !
   if(ikbz_FineGd<Xk%bz_DbGd_map(1,ikbz)) then
     En1       = Xen%E(ic,ik,i_spin)
     ffac      = Xen%f(ic,ik,i_spin)
   else
     ik_FineGd = Xk%sstar_FineGd(ikbz_FineGd)
     En1       = Xen%E_FineGd(ic,ik_FineGd,i_spin)
     ffac      = Xen%f_FineGd(ic,ik_FineGd,i_spin)
   endif
   !
   if( X_os_run  ) f_h = spin_occ-ffac
   if( K_inv_run ) f_h =          ffac
   !
   do ikpbz_FineGd=Xk%bz_DbGd_map(1,ikpbz)-1,Xk%bz_DbGd_map(2,ikpbz)
     !
     if(.not.use_kpt_DbGd(ikpbz_FineGd,ikbz_FineGd)) cycle
     !
     if(ikpbz_FineGd<Xk%bz_DbGd_map(1,ikpbz)) then
       En2       = Xen%E(iv,ikp,i_spin)
       f_e       = Xen%f(iv,ikp,i_spin)
     else
       ikp_FineGd= Xk%sstar_FineGd(ikpbz_FineGd)
       En2       = Xen%E_FineGd(iv,ikp_FineGd,i_spin)
       f_e       = Xen%f_FineGd(iv,ikp_FineGd,i_spin)
     endif
     !
     pole=cmplx(En1-En2,-W_)
     !
     if(l_X_terminator) pole_X_term=cmplx(X_term_E-En2,-W_,KIND=SP)
     !
     if( X_os_run       ) ffac=(f_e*f_h)/cg_ffac*weight
     if( K_inv_run      ) ffac=(f_e-f_h)/cg_ffac*weight
     if( no_occupations ) ffac = 1._SP
     !
     do iw=1,Xw%n_freqs
       !
       GreenF(iw)=GreenF(iw)+Lorentzian_FT(Xw%p(iw),pole,ordering)*ffac
       !
       if (l_X_terminator.and.ic>Xen%nbm) then 
         GreenF(iw)= GreenF(iw) -Lorentzian_FT(Xw%p(iw),pole_X_term,ordering)*ffac
       endif
       !
       if (l_X_terminator.and.ic<=Xen%nbm) then
         GreenF(iw)= -Lorentzian_FT(Xw%p(iw),pole_X_term,ordering)*ffac
       endif      
       !
     enddo
     !
   enddo
   !
 enddo
 !
 YAMBO_FREE(use_kpt_DbGd)
 deallocate(nkpt_DbGd_scatt)
 !
 contains
   !
   subroutine define_use_kpt()
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
     !
     qsize_fac=2._SP
     if(.not.X_is_integrated) qsize_fac=qsize_fac*10._SP
     !
     q_norm_ref=smallest_q_norm/qsize_fac
     !
     nkpt_DbGd_scatt=0
     use_kpt_DbGd=.false.
     do ikbz_FineGd=Xk%bz_DbGd_map(1,ikbz)-1,Xk%bz_DbGd_map(2,ikbz)
       if(ikbz_FineGd< Xk%bz_DbGd_map(1,ikbz)) k_pt_loc=Xk%ptbz(ikbz,:)
       if(ikbz_FineGd>=Xk%bz_DbGd_map(1,ikbz)) k_pt_loc=Xk%ptbz_FineGd(ikbz_FineGd,:)
       do ikpbz_FineGd=Xk%bz_DbGd_map(1,ikpbz)-1,Xk%bz_DbGd_map(2,ikpbz)
         if(ikpbz_FineGd< Xk%bz_DbGd_map(1,ikpbz)) kp_pt_loc=Xk%ptbz(ikpbz,:)
         if(ikpbz_FineGd>=Xk%bz_DbGd_map(1,ikpbz)) kp_pt_loc=Xk%ptbz_FineGd(ikpbz_FineGd,:)
         q_pt_shift(:)=(k_pt_loc-kp_pt_loc)-(Xk%ptbz(ikbz,:)-Xk%ptbz(ikpbz,:))
         if ( iku_v_norm(q_pt_shift) > q_norm_ref ) cycle
         nkpt_DbGd_scatt(ikbz_FineGd)=nkpt_DbGd_scatt(ikbz_FineGd)+1
         use_kpt_DbGd(ikpbz_FineGd,ikbz_FineGd)=.true.
       enddo
     enddo
     !
   end subroutine
   !
end subroutine
