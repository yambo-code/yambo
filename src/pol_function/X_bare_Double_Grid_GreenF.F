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
 use X_m,        ONLY:use_X_DbGd
 use BS,         ONLY:BSS_uses_DbGd
 use electrons,  ONLY:levels,BZ_DbGd_table,BZ_DbGd_nkpt,spin_occ,BZ_DbGd_nbands,&
                      BZ_DbGd_ipol_weight,BZ_DbGd_kpts
 use R_lattice,  ONLY:qindx_X,bz_samp,q_pt,nqibz
 use frequency,  ONLY:w_samp
 use vec_operate,ONLY:k2bz,iku_v_norm
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
 ! GPL_EXCLUDE_START
#if defined _ELPH
 complex(SP), allocatable  ::Green_F_convoluted(:)
 real(SP)                  ::W_step
#endif
 ! GPL_EXCLUDE_END
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
   Green_F_convoluted=Green_F_convoluted/pi**2
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
 ! For q/=0 all KPTs.
 ! For q==0 only KPTs with i1==i2
 !
 BZ_DbGd_iq_npt=0
 use_kpt=.false.
 do i1=1,BZ_DbGd_nkpt(ikbz)
   do i2=1,BZ_DbGd_nkpt(ikpbz)
     if ( iq==1 .and. i1/=i2 ) cycle
     BZ_DbGd_iq_npt(i1)=BZ_DbGd_iq_npt(i1)+1
     use_kpt(i1,i2)=.true.
   enddo
 enddo
 !
 do i1=1,BZ_DbGd_nkpt(ikbz)
   !
   weight1   = BZ_DbGd_ipol_weight(i1)/real(BZ_DbGd_nkpt(ikbz),SP)
   ik_rand   = BZ_DbGd_table(ikbz,i1)
   !
   if( X_os_run  ) f_h = spin_occ-Xen%f_DbGd(ic,ik_rand,i_spin)
   if( K_inv_run ) f_h =          Xen%f_DbGd(ic,ik_rand,i_spin)
   !
   do i2=1,BZ_DbGd_nkpt(ikpbz)
     !
     if(.not.use_kpt(i1,i2)) cycle
     !
     weight2    = BZ_DbGd_ipol_weight(i2)/real(BZ_DbGd_iq_npt(i1),SP)
     ikp_rand   = BZ_DbGd_table(ikpbz,i2)
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
