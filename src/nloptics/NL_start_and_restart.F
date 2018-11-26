!
!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,CA
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
subroutine NL_start_and_restart(E,k,Dip)
 !
 ! This subroutine initialize some variables and 
 ! restart from a previous NL calculation
 !
 ! [1] Initialize the Vbands and potentials
 ! [2] Build Ho
 ! [3] RESTART from a previous run
 ! [4] Initialize the bare Hartree and XC
 ! [5] Initialize the fields
 !
 use pars,           ONLY:cZERO,rZERO,cONE,SP,LP
 use com,            ONLY:msg,error
 use electrons,      ONLY:levels,n_sp_pol
 use real_time,      ONLY:rho_reference,RT_P
 use R_lattice,      ONLY:bz_samp,k_map
 use X_m,            ONLY:X_t
 use DIPOLES,        ONLY:DIP_P,DIPOLE_t
 use hamiltonian,    ONLY:V_hartree_sc,V_xc_sc,H_nl_sc,rho_n
 use X_m,            ONLY:X_t 
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use QP_m,           ONLY:QP_nk
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use fields,         ONLY:A_tot,A_ext,A_vecpot_reset
 use parallel_m,     ONLY:master_cpu
 use nl_optics,      ONLY:Correlation,IPA,E_full,full_rho,V_bands,TDDFT,n_freqs_todo,NL_LRC_alpha,Alpha_ED,NL_bands, &
&                         V_xc_0,NL_initial_P,l_use_DIPOLES,n_frequencies,freqs_done,JGM,E_xc_0,I_relax,l_use_DIPOLES, &
&                         VAL_BANDS,NL_J,l_eval_CURRENT
 use IO_m,           ONLY:OP_RD_CL,io_control,NONE,file_is_present,VERIFY
 use parallel_m,     ONLY:PAR_IND_freqs,PP_redux_wait
 use stderr,         ONLY:intc
 use IO_m,           ONLY:IO_and_Messaging_switch
 use interfaces,     ONLY:NL_average_operator
 !
#include<memory.h>
 !
 type(levels)      :: E
 type(bz_samp)     :: k
 type(DIPOLE_t)    :: Dip
 !type(X_t)         :: X
 !
 ! Working space
 !
 integer :: i1,ib,i_f
 integer :: ID,io_NonLinear
 integer, external::io_NL
 !
 !
 ! Initialize the Gf and potentials
 !==================================
 rho_reference     =rZERO
 if(allocated(full_rho))        full_rho    =rZERO
 if(allocated(V_xc_sc))         V_xc_sc     =rZERO
 if(allocated(V_hartree_sc))    V_hartree_sc=rZERO
 H_nl_sc           =cZERO
 I_relax           =cZERO
 V_bands           =cZERO
 !
 if(NL_bands(2)<=E%nbf.or.NL_bands(1)>E%nbf) &
     & call error(" Wrong bands range!")
 !
 forall(i1=1:E%nbf) V_bands(i1,i1,:,:)=cONE
 !
 if(Correlation/=IPA) then
   !  
   call NL_build_valence_bands(E,V_bands,VAL_BANDS,1)
   call el_density_vbands(E,k,full_rho,VAL_BANDS)
   !
   call NL_build_valence_bands(E,V_bands,VAL_BANDS,NL_bands(1))
   call el_density_vbands(E,k,rho_reference,VAL_BANDS)
   !
   rho_n=full_rho
   !
 endif
 !
 ! Fields
 !=====================
 call A_vecpot_reset(A_tot)
 call A_vecpot_reset(A_ext)
 !
 if(Correlation==TDDFT) then
   !
   ! Calculate V_xc
   !
   call IO_and_Messaging_switch("-report -log -io_in -io_out",CONDITION=master_cpu)
   call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1)
   call IO_and_Messaging_switch("+report +log +io_in +io_out")
   V_xc_0=V_xc
   !       
 endif
 !
 if(Correlation==JGM) then
   RT_P=cZERO
   call EXC_macroscopic_JGM(E,RT_P,rho_n,E_xc_0,V_xc_0)
   call msg('sr',' JGM initial alpha :',NL_LRC_alpha)
 endif
 !
 ! Apply QP Corrections
 ! ====================
 do ib=NL_bands(1),NL_bands(2)
   E_full(ib,1:QP_nk,1:n_sp_pol)=E%E(ib,1:QP_nk,1:n_sp_pol)
 enddo
 !
 do ib=1,NL_bands(1)-1
   E_full(ib,1:QP_nk,1:n_sp_pol)=E_full(ib,1:QP_nk,1:n_sp_pol) &
& -E_full(NL_bands(1),1:QP_nk,1:n_sp_pol)+E%E(NL_bands(1),1:QP_nk,1:n_sp_pol)
 enddo
 !
 ! Evaluate Initial Polarization
 ! ====================================
 !
 if(.not.l_use_DIPOLES) then
   if(.not.allocated(k_map%k_map_dir)) call k_map_nearest_by_dir(k,k_map)
   if(.not.allocated(k%k_table))       call k_build_up_BZ_tables(k)
   call POLARIZATION_output(E,k,Dip)
 endif
 !
 if(l_use_DIPOLES) then
   call DIP_polarization(E,k,V_bands,NL_initial_P)
 else
   ! This subroutine update also the Sm1_plus,Sm1_minus and All_S_det matrices
   call Berry_Polarization_NEQ(E,k,V_bands,NL_initial_P)
   !
 endif
 !
 if(l_eval_CURRENT) then
   call NL_average_operator(V_VALUE=NL_J(4:6),V_MATRIX=-DIP_P,V_bands=V_bands,k=k,en=E,TRANS=.TRUE.,nK_mem=k%nibz)
   call NL_Berry_current(E,k,k_map,V_bands,NL_J(1:3))
 endif
 !
 call msg('r','')
 call msg('r',' * * * Non-linear dynamics parameters * * * ')
 call msg('r','============================================')
 call msg('r ',' Initial Polarization      :',real(NL_initial_P))
 if(l_eval_CURRENT) then
   call msg('r ',' Initial Current         :',real(NL_J(1:3)))
   call msg('r ',' Initial <v>=<[Hloc,r]>  :',real(NL_J(4:6)))
 endif
 !
 ! Restart
 ! ====================================
 YAMBO_ALLOC(freqs_done,(n_frequencies))
 !
 freqs_done=.false.
 !
 if(master_cpu) then
   !      
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_Nonlinear=io_NL(ID)
   !
   if(io_Nonlinear==0) then
     do i_f=1,n_frequencies
        if(file_is_present('Nonlinear_fragment_'//intc(i_f))) freqs_done(i_f)=.true.
     enddo                                                                    
    endif
    !
 endif
 !
 call PP_redux_wait(freqs_done,imode=3)
 !
 n_freqs_todo=n_frequencies-count(freqs_done)
 if(n_freqs_todo/=n_frequencies) call msg('s','RESTART, remaining frequencies: ',n_freqs_todo)
 !
end subroutine NL_start_and_restart

