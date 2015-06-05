!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine X_bare_Double_Grid_GreenF(iq,transitions,Xw,Xen,Xk,GreenF,ordering)
 !
 use pars,       ONLY:SP,cZERO,cONE
 use X_m,        ONLY:use_X_DbGd
 use BS,         ONLY:BSS_uses_DbGd
 use electrons,  ONLY:levels,BZ_DbGd_table,BZ_DbGd_nkpt,spin_occ,BZ_DbGd_nbands,&
                      BZ_DbGd_ipol_weight
 use R_lattice,  ONLY:qindx_X,bz_samp
 use frequency,  ONLY:w_samp
 ! GPL_EXCLUDE_START
#if defined _ELPH
 use units,      ONLY:pi
#endif
 ! GPL_EXCLUDE_END
 !
 implicit none
 integer      :: iq,transitions(4)
 type(w_samp) :: Xw
 type(levels) :: Xen
 type(bz_samp):: Xk
 complex(SP)  :: GreenF(Xw%n_freqs)
 character(*) :: ordering
 !
 !Work Space
 !
 integer      ::i1,i2,iw,ikbz,ikpbz,iv,ic,i_spin,ik_rand,ikp_rand,ik,ikp
 real(SP)     ::ffac,cg_ffac,f_e,f_h,weight1,weight2,W_
 complex(SP)  ::Z_,pole
 complex(SP),external      ::X_simple_GreenF
 !
 ! GPL_EXCLUDE_START
#if defined _ELPH
 complex(SP), allocatable  ::Green_F_convoluted(:)
 real(SP)                  ::W_step
#endif
 ! GPL_EXCLUDE_END
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
 ! GPL_EXCLUDE_START
#if defined _ELPH
 !
 ! Green Functions convolution
 !
 if (associated(Xen%GreenF)) then 
   ! 
   allocate(Green_F_convoluted(Xen%GreenF_n_E_steps))
   !
   W_step=real( Xen%GreenF_W(ic,ik,i_spin,2) - Xen%GreenF_W(ic,ik,i_spin,1))
   !
   call Convolve(real(Xen%GreenF(ic,ik,i_spin,:)), real(Xen%GreenF(iv,ikp,i_spin,:)),&
&                Green_F_convoluted,Xen%GreenF_n_E_steps,W_step,real( Xen%GreenF_W(ic,ik,i_spin,1)) )
   !
   ! Green_F_convoluted is now integrated to Zc*Zv*pi*pi. As I want the spectral function only
   ! to pass to the KK transformation I need to rescale by pi^2
   !
   Green_F_convoluted=Green_F_convoluted/pi**2.
   !
   GreenF=cZERO
   !
   ! Resonant Part
   !
   if (trim(ordering)=='r'.or.trim(ordering)=='t'.or.trim(ordering)=='c') then
     call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,            Xw%p,                          Xw%n_freqs, cZERO)
   endif
   !
   if (trim(ordering)=='c'.or.trim(ordering)=='ca') then
     call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,           -Xw%p,                          Xw%n_freqs, cZERO)
   endif
   !
   if (trim(ordering)=='t'.or.trim(ordering)=='a') then
     call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,     -conjg(Xw%p),                         Xw%n_freqs, cZERO)
   endif
   !
   deallocate(Green_F_convoluted)
   !
   return
   !
 endif
 !
#endif
 ! GPL_EXCLUDE_END
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
 ! In the X_o loop the GreenF is multiplied by 
 !
 !  cg_ffac/spin_occ/real(Xk%nbz)/DL_vol
 !
 ! so that I have to divide/multiply to correctly renormalize 
 !
 ! From X_os.F
 !
 if( iq/=1  .or. .not.(ordering=="sr".or.ordering=="sa") ) then
   f_e =          Xen%f(iv,ikp,i_spin)
   f_h = spin_occ-Xen%f(ic,ik ,i_spin) 
   cg_ffac = f_e*f_h          
 endif
 !
 ! From K_inversion.F
 !
 if( iq==1 .and.      (ordering=="sr".or.ordering=="sa") ) cg_ffac = spin_occ**2._SP  
 !
 do i1=1,BZ_DbGd_nkpt(ikbz)
   !
   ik_rand = BZ_DbGd_table(ikbz,i1)
   !
   f_h     = spin_occ-Xen%f_DbGd(ic,ik_rand,i_spin)
   weight1 = BZ_DbGd_ipol_weight(i1)/real(BZ_DbGd_nkpt(ikbz),SP)
   !
   do i2=1,BZ_DbGd_nkpt(ikpbz)
     !
     ikp_rand=BZ_DbGd_table(ikpbz,i2) 
     !
     f_e     = Xen%f_DbGd(iv,ikp_rand,i_spin)
     weight2 = BZ_DbGd_ipol_weight(i2)/real(BZ_DbGd_nkpt(ikpbz),SP)
     !
     ffac=real(Z_)*f_e*f_h*weight1*weight2/cg_ffac
     !
     pole=cmplx(Xen%E_DbGd(ic,ik_rand,i_spin)-Xen%E_DbGd(iv,ikp_rand,i_spin),W_)
     !
     do iw=1,Xw%n_freqs
       GreenF(iw)=GreenF(iw)+X_simple_GreenF(Xw%p(iw),pole,ordering)*ffac
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine
