!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP AC
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
!> @brief Calculate phonon-assisted absorption and emission (see PRL 122, 187401 )
!!
subroutine EXCPH_optics(E,k,q,X,wv)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,cZERO,schlen,rZERO
 use vec_operate,    ONLY:iku_v_2D_norm,c2a
 use electrons,      ONLY:levels,nel_cond,E_reset
 use R_lattice,      ONLY:bz_samp,bare_qpg,d3k_factor
 use D_lattice,      ONLY:Boltz_Temp
 use X_m,            ONLY:X_t
 use com,            ONLY:msg,of_open_close
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states,L_kind_in,L_kind_out,BS_E,BS_all_E,BS_E_in,   &
&                         BS_R_left,BS_R_right,BS_R,BSE_alloc,BSE_free,EXC_occ,alphaQ,q_DbGd,  &
&                         RESIDUALS_and_EIGENVALUES_free, RESIDUALS_and_EIGENVALUES_alloc,EXC_E_DbGd,  &
&                         l_DbGd_PH_only,l_DbGd_WEIGHTs,l_no_matrix_elements,Sat_alloc,Sat_free, &
&                         min_EXC_E_DbGd,min_pos_E_DbGd
 use ELPH,           ONLY:use_PH_DbGd,ph_modes
 use BS_solvers,     ONLY:BSS_eh_table,BSS_alloc,BSS_free
 use BS,             ONLY:BSE_prop,BS_dip_size
 use frequency,      ONLY:w_samp
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 type(w_samp)  :: wv 
 !
 ! Work space
 !
 complex(SP), allocatable :: PL_ii(:,:),Epsilon_ii(:,:)
 integer, parameter  :: n_output=7
 real(SP)            :: output_data(n_output),E_fermi_h,E_fermi_e,v(3)
 character(schlen)   :: headers(n_output)
 integer             :: iw,iq,i_alpha,max_filling,EXC_n_sum,i_beta
 integer, allocatable:: EXC_indexes(:)
 character(schlen)   :: dbgd_string
 character(schlen) :: q_point 
 type(levels)        :: E_QF
 logical :: l_sat_from_Hpert
 !
 call section('*','EXCPH-Optics')
 !
 ! Load excitonic energies 
 !
 if(L_kind_in/=L_kind_out) &
&     call warning(' Lkind_in /= Lkind_out this can generate inconsistencies in the luminescence! ')
 !
 call EXCPH_load_L(1,X,'check','Lout')
 !
 ! Allocations 
 !
 call RESIDUALS_and_EIGENVALUES_alloc(q)
 !
 call Sat_alloc()
 !
 ! Load excitonic energies and residuals
 !
 BSE_prop='abs'
 BS_dip_size=1
 !
 call K_observables()
 !
 call BSS_alloc()
 !
 call BSE_alloc(k,q)
 !
 call EXCPH_load_L(1,X,'load residuals','Lin')
 BS_E_in(:)=BS_E(EXCPH_states(1):EXCPH_states(2)) ! L_in can be different from L_out 
 !
 ! Recalculate excitonic dipole if required
 !
 call EXC_dipole(k,E,q,X)
 !
 ! Excitonic occupation
 !
 if(nel_cond/=rZERO) then
   call msg('rs','Excitonic quasi-fermi distribution interpolation')
   call E_reset(E_QF)
   call OCCUPATIONS_Quasi_Fermi(E,k,E_QF,E_fermi_h,E_fermi_e)
   EXC_n_sum=EXCPH_sum(2)-EXCPH_sum(1)+1
   YAMBO_ALLOC(EXC_occ,(EXC_n_sum,q%nibz))
   YAMBO_ALLOC(EXC_indexes,(EXC_n_sum))
   if(.not.allocated(q%k_table))  call k_build_up_BZ_tables(q)   
   if(EXCPH_sum(1)/=1) call error("This part of the code does not work for ELPhExcSum(1)/=1")
   do i_beta=EXCPH_sum(1),EXCPH_sum(2)
      EXC_indexes(i_beta)=i_beta 
   enddo
   if(Boltz_Temp/=rZERO) call warning('Excitonic Occupation, EXCTemp ignored, use ElecTemp!')
 endif
 !
 do iq=1,q%nibz
   if(nel_cond/=rZERO) then
     call EXCPH_load_L(iq,X,'load','Lout')
     call EXC_occupations(E_QF,k,q,iq,EXC_n_sum,EXC_indexes,EXC_occ(:,iq))
   else
     call EXCPH_load_L(iq,X,'eigenvalues','Lout')
   endif
     BS_all_E(:,iq)=real(BS_E(:)) ! I suppose the BSE eigenvalues are real
 enddo
 !
 call parser('NoMatrxEl',l_no_matrix_elements)
 if(l_no_matrix_elements) call warning('No exc-ph matrix elements, just ph-assisted dos')
 !  
 ! Turn on double-grid if presents
 call EXCPH_double_grid_setup(q)
 !
 dbgd_string=''
 !
 call msg('rs','Double-grid                  :',use_PH_DbGd)
 !
 if(use_PH_DbGd) then
   dbgd_string='DbGd'
   max_filling=maxval(q%FGbz%N_mapped)+1  ! max number of tilde q around each q-point include the q itself
   !
   ! Check double-grid flags
   !
   call parser('DbGdWEIGHTs',l_DbGd_WEIGHTs)
   call parser('DbGdOnlyPh', l_DbGd_PH_only)
   !
   call msg('rs','Double-grid for satellites   :',l_DbGd_WEIGHTs)
   call msg('rs','Double-grid only for phonons :',l_DbGd_PH_only) 
   !
 endif
 !
 ! Frequencies and damping setup
 call FREQUENCIES_setup(wv)
 YAMBO_ALLOC(Epsilon_ii,(wv%n_freqs,3))
 YAMBO_ALLOC(PL_ii,(wv%n_freqs,3))
 !
 ! Band structure distortion
 !
 if(alphaQ/=rZERO) then
   call msg('rs','Excitonic bands 2D stretching :',alphaQ)
   do iq=1,q%nibz
      ! In a general case here we should use iku_v_norm
      ! we modified to the iku_v_2D_norm for the AB-hBN case
      ! one can restore the original one
      BS_all_E(:,iq)=BS_all_E(:,iq)+alphaQ*iku_v_2D_norm(q%pt(iq,:3))
   enddo
   if(use_PH_DbGd) then
      do iq=1,q_DbGd%nibz
         EXC_E_DbGd(:,iq)=EXC_E_DbGd(:,iq)+alphaQ*iku_v_2D_norm(q_DbGd%pt(iq,:3))
      enddo
       min_EXC_E_DbGd=minval(EXC_E_DbGd(1,:))
       min_pos_E_DbGd=minloc(EXC_E_DbGd(1,:))
       call c2a(v_in=q_DbGd%pt(min_pos_E_DbGd,:),v_out=v,mode="ki2a")
       call msg('rs','NEW!!! Minimum excitonic energy with DbGd: ',min_EXC_E_DbGd*HA2EV,'eV')
       write(q_point,'(3f12.6,a)') v(:),' [rlu] '
       call msg('rs','NEW!!!! Minimum q-point in the DbGrid : '//q_point)
   endif
 endif
 !
 call parser('SatFromHPert',l_sat_from_Hpert)
 if (l_sat_from_Hpert) then
   !
   ! This option only computes phonon-assisted emission satellites
   ! using perturbation-theory formula from excitonic Hamiltonian
   ! (i.e. |sum_in BS_R (in) G (in,out)|^2 instead of sum_in BS_R(in)**2! G(in,out)**2 )
   !
   call warning(' Calculating only modified satellites for PL')
   call EXCPH_X_phass_from_Hpert(wv,q,PL_ii)
   !
 else
   !
   ! Calculate X(w) including phonon-assisted transitions as
   ! derived from exciton-phonon self-energy
   !
   call EXCPH_X_phass(wv,q,Epsilon_ii,PL_ii)
   !
 endif
 !
! call X_setup_file_names(1,'bse ','phass',dbgd_string) 
 !
 call print_output()
 !
 ! Deallocations
 !
 call RESIDUALS_and_EIGENVALUES_free()
 call Sat_free()
 call BSS_free()
 YAMBO_FREE(Epsilon_ii)
 YAMBO_FREE(PL_ii)
 !
 call FREQUENCIES_reset(wv,"all")
 !
 contains
   !
   subroutine print_output()
!     use X_output,      ONLY:X_write_messages_before_headers,X_write_q_plus_G
     implicit none
     !
     character(schlen) :: eps_file_name,pl_file_name
     !
     eps_file_name='eps_bse_ph_ass'
     pl_file_name ='pl_bse_ph_ass'
     if(use_PH_DbGd) then
       eps_file_name='eps_bse_ph_ass_dbgd'
       pl_file_name ='pl_bse_ph_ass_dbgd'
     endif
     !
     call of_open_close(eps_file_name,'ot')
     call of_open_close(pl_file_name,'ot')
     !
!     call X_write_messages_before_headers(1,.FALSE.,.FALSE.,'r ')
     !
!     call X_write_q_plus_G(1,(/0._SP,0._SP,0._SP/),1)
     !
     call msg('o eps_bse pl_bse','# EXCITON PHONON')    
     call msg('o eps_bse pl_bse','# Exciton_states    :',EXCPH_states,INDENT=0)    
     call msg('o eps_bse pl_bse','# EXciton sum       :',EXCPH_sum,INDENT=0)    
     if(use_PH_DbGd) then
       call msg('o eps_bse pl_bse','# Double-grid for satellites   :',l_DbGd_WEIGHTs,INDENT=0)    
       call msg('o eps_bse pl_bse','# Double-grid only for phonons :',l_DbGd_PH_only,INDENT=0)    
     endif
     call msg('o eps_bse pl_bse','#')
     headers(1)='E[eV]'
     headers(2:n_output)=(/'Im(eps_ph )','Re(eps_ph )','Im(eps_nos)','Re(eps_nos)','Re(eps_sat)','Im(eps_sat)'/)
     call msg('o eps_bse','#',headers,INDENT=0,USE_TABS=.TRUE.)
     !
     headers(2:n_output)=(/'Im(pl_ph )','Re(pl_ph )','Im(pl_nos)','Re(pl_nos)','Re(pl_sat)','Im(pl_sat)'/)
     call msg('o pl_bse','#',headers,INDENT=0,USE_TABS=.TRUE.)
     call msg('o eps_bse pl_bse','#')
     !
     ! Write output on file
     !
     do iw=1,wv%n_freqs
       !
       ! Eps
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1)), &
&             aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),aimag(Epsilon_ii(iw,3)),real(Epsilon_ii(iw,3))/)
       call msg('o eps_bse','',output_data,INDENT=-2,USE_TABS=.true.)
       !
       ! Luminescence
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(PL_ii(iw,1)),real(PL_ii(iw,1)),          &
&               aimag(PL_ii(iw,2)),real(PL_ii(iw,2)),aimag(PL_ii(iw,3)),real(PL_ii(iw,3))/)
       call msg('o pl_bse','',output_data,INDENT=-2,USE_TABS=.true.)
       !
     enddo
     !
     ! Close files
     !
     call of_open_close(eps_file_name)
     call of_open_close(pl_file_name)
     !
   end subroutine print_output
   !
end subroutine
