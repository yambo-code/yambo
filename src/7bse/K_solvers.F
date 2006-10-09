!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine K_solvers(iq,Ken,Xk,q,X,W_bss)
!=========================================
 use pars,           ONLY:SP,pi
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_t,X_epsilon,X_alloc,X_rhoq0,q0_defmod,&
&                         alpha_dim,eps_2_alpha
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BSS_rhoq0,&
&                         BS_bands,BSS_q0,BS_eh_table,BSS_mode,BS_eh_E,&
&                         BSS_n_descs,BSS_description,BSS_of_title,BS_K_coupling
 use com,            ONLY:msg
 use drivers,        ONLY:l_bs_fxc
 use frequency,      ONLY:w_samp
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:d3k_factor,bz_samp
 use vec_operate,    ONLY:v_module
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,NONE
 use TDDFT,          ONLY:FXC_description,FXC_n_descs
 implicit none
 type(levels) ::Ken
 type(bz_samp)::Xk,q
 type(X_t)    ::X
 type(w_samp) ::W_bss
 integer      ::iq
!ws
 integer           ::ik,iv,ic,i1,X_ib_save(2)
 integer           ::ioBS_err,ID
 integer, external ::ioBS
!
 call section('*','BSE solver(s)')
!---------------------------------
!
!=======
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)
 ioBS_err=ioBS(iq,X,ID)
 if (ioBS_err/=0) return
!
! Titles & Descriptions
!=======================
 BSS_of_title(1:3)=(/'E/ev[1]','e/Im[2]','e/Re[3]'/)
!
! Polarizability ?
!------------------
 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization   [au]:',eps_2_alpha)
   call msg('rn','Polarizability dimension      :',trim(alpha_dim))
   BSS_of_title(4:5)=(/'ao/Im[4]','ao/Re[5]'/) 
   if (index(BSS_mode,'h')/=0) BSS_of_title(6:7)=(/'e`/Im[6]','e`/Re[7]'/)
 else
   BSS_of_title(4:5)=(/'ao/Im[4]','ao/Re[5]'/) 
   if (index(BSS_mode,'h')/=0) BSS_of_title(6:7)=(/'a`/Im[6]','a`/Re[7]'/)
 endif
!
! Eps_0
!========
 allocate(X_epsilon(4,BSS_n_freqs))
 X_epsilon=(0.,0.)
!
!Save the X bands 
 X_ib_save=X%ib
 X%ib=BS_bands
 if (BS_K_coupling) then
   allocate(BSS_rhoq0(2*BS_K_dim)) 
   call mem_est("BSS_rhoq0",(/2*BS_K_dim/))
 else
   allocate(BSS_rhoq0(BS_K_dim)) 
   call mem_est("BSS_rhoq0",(/BS_K_dim/))
 endif
 if (iq==1) then
   X%q0=BSS_q0
   call X_O_strengths(Ken,Xk,X,BSS_q0)  
   do i1=1,BS_K_dim
     ik=BS_eh_table(i1,1)
     iv=BS_eh_table(i1,2)
     ic=BS_eh_table(i1,3)
!
! X_rhoq0(c,v,k) is iq . <v | r |c> while I need 
!
!   iq . <c|r|v> = - conjg( iq . <v | r |c> )
!
     BSS_rhoq0(i1)=-conjg(X_rhoq0(ic,iv,ik,1))
!
! minus comes from the occupation factor
!
     if (BS_K_coupling) BSS_rhoq0(BS_K_dim+i1)=X_rhoq0(ic,iv,ik,1)
!
     X_epsilon(3,:)=X_epsilon(3,:)-&
&                   BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))/(W_bss%p(:)-BS_eh_E(i1))
     if (BS_K_coupling) then
       X_epsilon(3,:)=X_epsilon(3,:)+&
&                     BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))/(conjg(W_bss%p(:))+BS_eh_E(i1))
     endif
   enddo
   X_epsilon(3,:)=X_epsilon(3,:)*d3k_factor/pi**2./q0_defmod**2.+1.
 endif
 X%ib=X_ib_save

! The TDDFT way 
!===============
 if (index(BSS_mode,'t')/=0) then
   FXC_description(FXC_n_descs+1:FXC_n_descs+BSS_n_descs-2)=BSS_description(3:BSS_n_descs)
   FXC_n_descs=FXC_n_descs+BSS_n_descs-2
   call tddft_BSK_disk_2_FXC(iq,Xk,W_bss) 
   l_bs_fxc=.true.
 endif
!
! Solvers
!=========
 if (index(BSS_mode,'h')/=0) then
   call K_dump_to_o_file(iq,-1)
   call K_Haydock(iq,W_bss)
 endif
 if (index(BSS_mode,'d')/=0) call  K_diagonalization(iq,W_bss)
! if (index(bss%mode,'i')/=0) 
!
! CLEAN
!=======
!
 deallocate(BSS_rhoq0,X_epsilon)
 call mem_est("BSS_rhoq0")
 if (iq==1) call X_alloc('OptOsc')

 end subroutine
