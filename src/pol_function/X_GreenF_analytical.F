!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars,       ONLY:SP,cZERO,cONE
 use X_m,        ONLY:use_X_DbGd,X_t,l_X_terminator,X_term_E,X_DbGd_nkpt_at_q,X_DbGd_nkpt_at_q_table
 use BS_solvers, ONLY:BSS_uses_DbGd
 use electrons,  ONLY:levels,BZ_DbGd_table,spin_occ,BZ_DbGd_nbands,&
                      BZ_DbGd_weight,BZ_DbGd_kpts
 use R_lattice,  ONLY:qindx_X,bz_samp
 use frequency,  ONLY:w_samp
 use parallel_m, ONLY:PAR_Xk_bz_index
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
 logical      :: intra_band
 !
 !Work Space
 !
 logical      :: X_os_run,K_inv_run 
 integer      :: iw,ikbz,ikpbz,iv,ic,i_spin,ik_rand,ikp_rand,ik,ikp,ikbz_mem,i_r,ik_r,ikp_r
 real(SP)     :: ffac,cg_ffac,f_h,weight1,weight2,W_
 complex(SP)  :: Z_,pole,pole_X_term
 !
 complex(SP),external      ::Lorentzian_FT
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
 pole=cmplx(Xen%E(ic,ik,i_spin)-Xen%E(iv,ikp,i_spin),-W_,KIND=SP)
 !
 pole_X_term=0.0_SP
 if(l_X_terminator) then 
   pole_X_term= cmplx(X_term_E-Xen%E(iv,ikp,i_spin),-W_,KIND=SP) 
 endif
 !
 ! Case without DbGd
 !===================
 !
 ! In K_inversion_Lo the occupations and the Z_factor are not included.
 ! Thus I have to multiply by them the Green function
 !
 if( K_inv_run      ) ffac = ( Xen%f(iv,ikp,i_spin) - Xen%f(ic,ik,i_spin) )/spin_occ*real(Z_)
 if( X_os_run       ) ffac = 1._SP
 if( no_occupations ) ffac = 1._SP
 !
 if (X_os_run .and.(.not.use_X_DbGd.or.ic>BZ_DbGd_nbands).or. &
&    K_inv_run.and.(.not.BSS_uses_DbGd)                       ) then
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
 if( K_inv_run ) cg_ffac = spin_occ/real(Z_)
 if( X_os_run  ) cg_ffac = Xen%f(iv,ikp,i_spin)*( spin_occ-Xen%f(ic,ik ,i_spin) )
 !
 ! 4/8/2016. AM. Note that, in the K case, the anti-res part of Lo uses a w+\Omega (and not -\Omega) simply
 ! because this routine is always called using the i_res \Omega. Indeed 
 !
 intra_band = .FALSE.
 if( ic <= Xen%nbf ) intra_band = .TRUE.
 !
 ! 1/6/2020 CA When Terminator is on, there are v->v' transition and therefore occupation factor
 ! cg_ffac goes to zeros and gives NaN. Fixed by introducing a flag for intra-band transitions
 !
 !  \Omega_{anti-res} = -\Omega_{res}
 !
 ikbz_mem  = PAR_Xk_bz_index(ikbz)
 !
 do i_r=1,X_DbGd_nkpt_at_q(ikbz_mem)
   !
   ik_r =X_DbGd_nkpt_at_q_table(ikbz_mem, i_r,1)
   ikp_r=X_DbGd_nkpt_at_q_table(ikbz_mem, i_r,2)
   ik_rand  = BZ_DbGd_table(ikbz, ik_r,1)
   ikp_rand = BZ_DbGd_table(ikpbz,ikp_r,1)
   !
   weight1   = BZ_DbGd_weight(ik_r)/real(X_DbGd_nkpt_at_q(ikbz_mem))
   !
   if( X_os_run  ) f_h = spin_occ-Xen%f_DbGd(ic,ik_rand,i_spin)
   if( K_inv_run ) f_h =          Xen%f_DbGd(ic,ik_rand,i_spin)
   !
   weight2    = BZ_DbGd_weight(ikp_r)
   !
   if( X_os_run       ) ffac=(Xen%f_DbGd(iv,ikp_rand,i_spin)*f_h)/cg_ffac
   if( K_inv_run      ) ffac=(Xen%f_DbGd(iv,ikp_rand,i_spin)-f_h)/cg_ffac
   if( no_occupations .or. intra_band ) ffac = 1._SP
   !
   ffac=ffac*weight1*weight2
   pole=cmplx(Xen%E_DbGd(ic,ik_rand,i_spin)-Xen%E_DbGd(iv,ikp_rand,i_spin),-W_,KIND=SP)
   !
   if(l_X_terminator) then 
     pole_X_term=cmplx(X_term_E-Xen%E_DbGd(iv,ikp_rand,i_spin),-W_,KIND=SP) 
   endif
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
end subroutine
