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
subroutine X_full_GreenF(iq,transitions,Xw,Xen,Xk,GreenF,ordering,space)
 !
 use pars,       ONLY:SP,cZERO,cONE
 use electrons,  ONLY:levels
 use R_lattice,  ONLY:qindx_X,bz_samp
 use frequency,  ONLY:w_samp
 ! GPL_EXCLUDE_START
#if defined _ELPH
 use units,      ONLY:pi
 use com,        ONLY:error
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
 logical      :: X_os_run,K_inv_run 
 integer      :: ikbz,ikpbz,iv,ic,i_spin,ik,ikp
 !
 ! GPL_EXCLUDE_START
 !
#if defined _ELPH
 complex(SP), allocatable  ::Green_F_convoluted(:)
 real(SP)                  ::W_step
#endif
 !
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
 ! GPL_EXCLUDE_START
 !
#if defined _ELPH
 !
 if (.not.associated(Xen%GreenF)) call error(' Green function not associated')
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
 !
 ! GPL_EXCLUDE_END
 !
end subroutine
