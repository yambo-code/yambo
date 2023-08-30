!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine X_GreenF_convoluted(iq,transitions,Xw,Xen,Xk,GreenF,ordering)
 !
 use pars,       ONLY:SP,cZERO
 use electrons,  ONLY:levels
 use R_lattice,  ONLY:qindx_X,bz_samp
 use frequency,  ONLY:w_samp
 use units,      ONLY:pi
 !
 implicit none
 integer      :: iq,transitions(4)
 type(w_samp) :: Xw
 type(levels) :: Xen
 type(bz_samp):: Xk
 complex(SP)  :: GreenF(Xw%n_freqs)
 character(*) :: ordering
 !
 ! Work Space
 !
 integer      :: ikbz,ikpbz,iv,ic,i_spin,ik,ikp
 !
 complex(SP), allocatable  :: Green_F_convoluted(:)
 real(SP)                  :: W_step
 !
 if (.not.allocated(Xen%GreenF)) call error(' Green function not allocated')
 !
 GreenF  = cZERO
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
 ! Green Functions convolution
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
 ! Resonant Part
 !
 if (trim(ordering)=='r'.or.trim(ordering)=='T'.or.trim(ordering)=='R') then
   call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,            Xw%p,                          Xw%n_freqs, cZERO)
 endif
 !
 ! Anti-Resonant Part
 !
 if (trim(ordering)=='R'.or.trim(ordering)=='Ra') then
   call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,           -Xw%p,                          Xw%n_freqs, cZERO)
 endif
 !
 if (trim(ordering)=='T'.or.trim(ordering)=='Ta') then
   call Kramers_Kronig(Green_F_convoluted,real(Xen%GreenF_W(iv,ikp,i_spin,:)),Xen%GreenF_n_E_steps,&
&                        GreenF,     -conjg(Xw%p),                         Xw%n_freqs, cZERO)
 endif
 !
 deallocate(Green_F_convoluted)
 !
 return
 !
end subroutine
