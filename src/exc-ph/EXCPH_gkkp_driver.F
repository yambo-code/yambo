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
!> @brief Calculate exciton-phonon matrix elements and satellites renormalizations
!!
!! @param[in]       E                quasi-particle index
!! @param[in]       k                q-index in the BZ
!! @param[in]       q                q-index in distributed array
!! @param[in]       X                phonon-branch index
!!
!! @param[out]      EXCPH_gkkp                  Exciton-phonon matrix elements
!! @param[out]      EXCPH_gkkp_sq               Exciton-phonon matrix elements squared
!! @param[out]      EXCPH_Renorm                Excitonn renormalization factor R
!! @param[out]      EXCPH_Renorm_PH_abs         Excitonn renormalization factor R_PH_abs for absorbed phonons
!! @param[out]      BS_Sat_E                    Satellite energy differences
!! @param[out]      BS_Sat_E_PH_abs             Satellite energy differences for absorbed phonons
!! @param[out]      BS_Sat_WEIGHT               Satellite weight
!! @param[out]      BS_Sat_WEIGHT_PH_abs        Satellite weight for absorbed phonons
!
subroutine EXCPH_gkkp_driver(E,k,q,X)
 !
 use pars,           ONLY:SP,cZERO,rZERO,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,qindx_B_load
 use X_m,            ONLY:X_t
 use BS_solvers,     ONLY:BS_mat,BSS_eh_table,BS_H_dim,BSS_n_eig
 use BS,             ONLY:BS_H_dim,BS_bands,L_kind,BS_res_ares_n_mat,BS_K_dim
 use ELPH,           ONLY:elph_branches
 use IO_m,           ONLY:NONE,OP_APP_CL,OP_RD_CL,DUMP,OP_RD,RD_CL_IF_END,OP_WR_CL,deliver_IO_error_message, &
&                         REP,VERIFY,WR_CL
 use IO_int,         ONLY:io_control
 use EXCPH,          ONLY:BS_mat_in,BSS_eh_table_in,EXCPH_gkkp,EXCPH_gkkp_sq,BS_E_in,BS_E,BSS_eh_table_m1_in,&
&                         EXCPH_states,EXCPH_sum,EXCPH_kind,BSS_eh_table_m1,k_plus_q_table,Xi, &
&                         BS_Sat_E,BS_Sat_E_PH_abs,BS_Sat_WEIGHT,BS_Sat_WEIGHT_PH_abs,Sat_alloc,Sat_free, &
&                         EXCPH_gkkp_alloc,EXCPH_gkkp_free,EXCPH_Renorm,EXCPH_Renorm_PH_abs
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 use com,            ONLY:msg
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 !
 ! Work Space
 !
 integer, external :: io_ELPH,io_EXCPH_gkkp
 integer  :: ID_gkkp,ID_exc_gkkp,io_err
 integer  :: il,iq,i1
 real(SP), allocatable  :: max_Sat_weight(:)
 integer,  allocatable  :: max_Sat_pos(:,:)
 !
 ! Symmetries check
 ! =================
 !
 if(q%nibz/=q%nbz) call error("Symmetries not implemented yet!!! ")
 !
 call section('*','Excitonic gkkp')
 !======================================
 !
 ! Check if Lout and Lin are present
 ! and read their kind
 ! 
 call EXCPH_load_L(1,X,'check','Lout')
 call EXCPH_load_L(1,X,'check','Lin')
 !
 ! Check if exciton-phonon matrix elements have been already calculated
 ! and are consistent with the Lin and Lout specified in input
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'all')
 !
 if(io_err==0) return
 if(io_err/=0) call warning("[EXCPH] Exc-ph matrix elements not correct or missing. To be computed")
 !
 call timing('EXCPH_gkkp',OPR='start')
 !
 call EXCPH_load_L(1,X,'check','Lin') ! At present the Lin can be only gamma iq=1
 !
 call k_build_up_BZ_tables(k)
 call k_build_up_BZ_tables(q)
 !
 call BSE_alloc()
 !
 call EXCPH_load_L(1,X,'load','Lin')
 !
 ! Alloage EXCPH_gkkp
 !
 call EXCPH_gkkp_alloc()
 YAMBO_ALLOC(max_Sat_weight,(EXCPH_states(1):EXCPH_states(2)))
 YAMBO_ALLOC(max_Sat_pos,(EXCPH_states(1):EXCPH_states(2),3))
 YAMBO_ALLOC(EXCPH_Renorm,(EXCPH_states(2)))
 YAMBO_ALLOC(EXCPH_Renorm_PH_abs,(EXCPH_states(2)))
 !
 max_Sat_weight=rZERO
 max_Sat_pos   =0
 !
 ! K plus q table
 !
 call build_k_plus_q_table()
 !
 ! BSS_eh_table_m1
 !
 call build_inverse_eh_table()
 !
 ! Save the Lin in information
 ! 
 BS_mat_in         =BS_mat(:,EXCPH_states(1):EXCPH_states(2))
 BSS_eh_table_in   =BSS_eh_table(EXCPH_states(1):EXCPH_states(2),:)
 BSS_eh_table_m1_in=BSS_eh_table_m1
 BS_E_in           =BS_E
 !
 ! Check if gkkp_expanded is present
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID_gkkp)
 io_err=io_ELPH(ID_gkkp,'gkkp_expanded') ! For now we have to work in the full BZ by turning all symmetries off.
 call deliver_IO_error_message(io_err,"GKKP")
 !
 if (io_err/=0) call error('El-Ph database not found')
 !
 ! Write headers
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),ID=ID_exc_gkkp)
 io_err=io_EXCPH_gkkp(ID_exc_gkkp,'all')
 !
 call Sat_alloc()
 !
 ! Main q-loop
 !
 call live_timing('Excitonic Gkkp',q%nibz*(elph_branches(2)-elph_branches(1)+1))
 !
 EXCPH_Renorm       =rZERO
 EXCPH_Renorm_PH_abs=rZERO
 !
 do iq=1,q%nibz
   ! 
   if(iq>1) call EXCPH_load_L(iq,X,'check','Lout')
   call EXCPH_load_L(iq,X,'load','Lout')
   !
   EXCPH_gkkp          =cZERO
   EXCPH_gkkp_sq       =rZERO
   !
   BS_Sat_E            =rZERO
   BS_Sat_E_PH_abs     =rZERO
   BS_Sat_WEIGHT       =rZERO
   BS_Sat_WEIGHT_PH_abs=rZERO
   !
   do il=elph_branches(1),elph_branches(2)
     !
     call  EXCPH_gkkp_eval(iq,il,E,k,q,X,ID_gkkp)
     !
     call live_timing(steps=1)
     !
   enddo
   !
   ! Add the WEIGHTs factor to calculate the renormalization factors
   !
   call  EXCPH_eval_sat(iq,q)
   !
   ! Find the maximum weight of the satellites
   ! 
   do i1=EXCPH_states(1),EXCPH_states(2)
     if(max_Sat_weight(i1)<maxval(abs(BS_Sat_WEIGHT(:,:,i1)))) then
        max_Sat_weight(i1)=maxval(abs(BS_Sat_WEIGHT(:,:,i1)))
        max_Sat_pos(i1,1) =iq
        max_Sat_pos(i1,2:3) =maxloc(abs(BS_Sat_WEIGHT(:,:,i1)))
     endif
   enddo
   !
   ! I/O
   !=====
   call io_control(ACTION=OP_APP_CL,SEC=(/iq+1/),ID=ID_exc_gkkp)
   io_err=io_EXCPH_gkkp(ID_exc_gkkp,'all')
   !
 enddo
 !
 call live_timing( )
 !
 call BSE_free()
 call EXCPH_gkkp_free()
 !
 ! Report all relevant data of the exciton-phonon coupling
 !
 call EXCPH_gkkp_report()
 YAMBO_FREE(max_Sat_weight)
 YAMBO_FREE(max_Sat_pos)
 YAMBO_FREE(EXCPH_Renorm)
 YAMBO_FREE(EXCPH_Renorm_PH_abs)
 ! 
 call Sat_free()
 !
 call timing('EXCPH_gkkp',OPR='stop')
 !
 return
 !
 contains
   !
   subroutine BSE_alloc()
     implicit none
     !      
     ! pre-allocs...
     !
     YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
     YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,3+n_sp_pol-1))
     YAMBO_ALLOC(BSS_eh_table_m1,(k%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2)))
     YAMBO_ALLOC(BS_E,(BSS_n_eig))
     YAMBO_ALLOC(k_plus_q_table,(k%nbz,q%nbz))
     !
     YAMBO_ALLOC(BS_E_in,(BSS_n_eig))
     YAMBO_ALLOC(BS_mat_in,(BS_H_dim,BSS_n_eig))
     YAMBO_ALLOC(BSS_eh_table_in,(BS_H_dim,3+n_sp_pol-1))
     YAMBO_ALLOC(BSS_eh_table_m1_in,(k%nbz,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2)))
     !
     BS_mat=cZERO
     BS_E=cZERO
     !
     k_plus_q_table  = 0
     BSS_eh_table    = 0
     BSS_eh_table_m1 = 0
     !
     BS_mat_in=cZERO
     BS_E_in  =cZERO
     BSS_eh_table_in     = 0
     BSS_eh_table_m1_in  = 0
     !
   end subroutine BSE_alloc
   !
   subroutine BSE_free
     implicit none
     YAMBO_FREE(BS_mat)
     YAMBO_FREE(BSS_eh_table)
     YAMBO_FREE(BSS_eh_table_m1)
     YAMBO_FREE(BS_E_in)
     YAMBO_FREE(BS_mat_in)
     YAMBO_FREE(BSS_eh_table_in)
     YAMBO_FREE(BSS_eh_table_m1_in)
     YAMBO_FREE(BS_E)
     YAMBO_FREE(k_plus_q_table)
   end subroutine BSE_free
   !
   subroutine build_k_plus_q_table()
     !      
     integer, external ::qindx_B_init,qindx_B_close 
     integer :: qindx_ID,qindx_ID_frag,qindx_tmp(2)
     integer :: ik_bz,iq_bz,ip_bz
     !
     call live_timing('K plus q table',k%nbz)
     qindx_tmp=qindx_B_init(qindx_ID,qindx_ID_frag)
     do ik_bz=1,k%nbz
       call live_timing(steps=1)
       IQ_bz_loop: do iq_bz=1,q%nbz
         do ip_bz=1,k%nbz
           qindx_tmp=qindx_B_load(ip_bz,ik_bz,qindx_ID_frag)
           if (qindx_tmp(1)==iq_bz) then
             k_plus_q_table(ik_bz,iq_bz)=ip_bz
             cycle IQ_bz_loop
           endif
         enddo
       enddo IQ_bz_loop
     enddo
     qindx_tmp=qindx_B_close(qindx_ID,qindx_ID_frag)
     call live_timing( )
     !
   end subroutine build_k_plus_q_table
   !
   subroutine build_inverse_eh_table()
     !
     integer :: iv,ic,i_H,ik_bz
     !     
     call live_timing('BSE inverse eh-table',BS_H_dim)
     do i_H=1,BS_H_dim
       !
       ik_bz = BSS_eh_table(i_H,1)
       iv    = BSS_eh_table(i_H,2)
       ic    = BSS_eh_table(i_H,3)
       !
       BSS_eh_table_m1(ik_bz,iv,ic)=i_H
       call live_timing(steps=1)
       !
     enddo
     call live_timing( )
     !
   end subroutine build_inverse_eh_table
   !
   subroutine EXCPH_gkkp_report()
     use stderr,        ONLY:intc
     use EXCPH,         ONLY:EXCPH_Renorm_PH_abs,EXCPH_Renorm
     use units,         ONLY:HA2EV
     use ELPH,          ONLY:ph_freqs_sq
     implicit none
     !
     ! work space
     !
     character(schlen) :: message
     real(SP)          :: ph_E
     integer           :: iq_bz
     !
     call msg('sr',' Excitons renormalizations  (without Bose factor): ')
     do i1=EXCPH_states(1),EXCPH_states(2)
        write(message,*) 'Exciton '//trim(intc(i1))//': R =',EXCPH_Renorm(i1),'    R_abs =',EXCPH_Renorm_PH_abs(i1)
        call msg('sr',message)
     enddo
     call msg('sr',' Max satellite weight: ')
     do i1=EXCPH_states(1),EXCPH_states(2)
        iq_bz = q%k_table(max_Sat_pos(i1,1), 1)
        ph_E=sqrt(abs(ph_freqs_sq(iq_bz,max_Sat_pos(i1,2))))
        write(message,*) 'Exciton '//trim(intc(i1))//' max(W_sat)=',max_Sat_weight(i1)
        call msg('sr',message)
        write(message,*) '   for iq = '//intc(max_Sat_pos(i1,1))//' il = '//intc(max_Sat_pos(i1,2))//'  ib = ' &
&                                     //intc(max_Sat_pos(i1,3)),' w(iq,il) = ',ph_E*HA2EV*1000._SP,'meV'
        call msg('sr',message)
     enddo
     !
   end subroutine EXCPH_gkkp_report
   !
end subroutine
