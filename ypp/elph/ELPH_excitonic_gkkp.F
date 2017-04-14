!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine ELPH_excitonic_gkkp(Xk,lambda_s,n_lambda)
 !
 use pars,                ONLY:SP,schlen,cZERO
 use units,               ONLY:HA2EV,HA2FSm1
 use BS_solvers,          ONLY:BS_mat,BSS_eh_table
 use BS,                  ONLY:BS_K_dim,BS_nT_at_k
 use R_lattice,           ONLY:bz_samp
 use IO_m,                ONLY:io_control,OP_RD,DUMP,RD_CL_IF_END
 use ELPH,                ONLY:elph_nDBS_used,ph_modes,elph_global_free,&
&                              elph_use_q_grid,ph_freqs_sq,elph_gkkp
 use parallel_m,          ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,myid
 use interfaces,          ONLY:PARALLEL_index
 use LIVE_t,              ONLY:live_timing
 use functions,           ONLY:Fermi_fnc_derivative
 use com,                 ONLY:msg,of_open_close
 use stderr,              ONLY:intc
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 integer       :: lambda_s(n_lambda),n_lambda
 !
 ! Work Space
 !
 integer                   ::neh1,ic1,iv1,ik_bz1,ik_ibz,ID,io_err,iq,ikk,&
&                            neh2,ic2,iv2,ik_bz2,i_lambda,lambda,i_l
 complex(SP), allocatable  ::diagonal_xhi(:,:,:)
 real(SP),    allocatable  ::ph_freqs(:)
 real(SP)                  ::gkkp_fs
 integer, external         ::io_ELPH
 type(PP_indexes)          ::px
 character(schlen)         ::ch
 !
 call section("+","Excitonic gkkp factors")
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=io_ELPH(ID,'gkkp')
 if (io_err/=0.or.elph_use_q_grid) return
 !
 YAMBO_ALLOC(diagonal_xhi,(elph_nDBS_used,ph_modes,n_lambda))
 YAMBO_ALLOC(ph_freqs,(ph_modes))
 diagonal_xhi = cZERO
 !
 ! Fill locally BS_blk_dim
 !
 YAMBO_ALLOC(BS_nT_at_k,(Xk%nibz))
 BS_nT_at_k=0
 do neh1=1,BS_K_dim
   ik_ibz=Xk%sstar(BSS_eh_table(neh1,1),1)
   BS_nT_at_k(ik_ibz)= BS_nT_at_k(ik_ibz)+1
 enddo
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/elph_nDBS_used,Xk%nibz/))
 call PP_redux_wait
 !
 do iq=1,elph_nDBS_used
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=io_ELPH(ID,'gkkp')
   !
   if (iq==1) call live_timing('GKKP factors',px%n_of_elements(myid+1))
   if (iq==1) cycle
   !
   do i_l=1,ph_modes
     ph_freqs(i_l)=sqrt(ph_freqs_sq(iq,i_l))
   enddo
   !
   do ik_ibz=1,Xk%nibz
     !
     ikk=sum(BS_nT_at_k(:ik_ibz-1))
     !
     do i_lambda=1,n_lambda
       !
       lambda=lambda_s(i_lambda)
       !
       do neh1=1,BS_nT_at_k(ik_ibz)
          !
          ik_bz1= BSS_eh_table(ikk+neh1,1)
          iv1   = BSS_eh_table(ikk+neh1,2)
          ic1   = BSS_eh_table(ikk+neh1,3)
          !
          do neh2=1,BS_nT_at_k(ik_ibz)
            !
            ik_bz2= BSS_eh_table(ikk+neh2,1)
            iv2   = BSS_eh_table(ikk+neh2,2)
            ic2   = BSS_eh_table(ikk+neh2,3)
            !
            if (ik_bz1/=ik_bz2) cycle
            !
            if (iv1==iv2) then
              !
              do i_l=1,ph_modes
                diagonal_xhi(iq,i_l,i_lambda)= diagonal_xhi(iq,i_l,i_lambda)- &
&                           conjg(BS_mat(lambda,ikk+neh2))*BS_mat(lambda,ikk+neh1) * &
&                           elph_gkkp(ik_bz1,i_l,ic2,ic1)/sqrt(2.*ph_freqs(i_l))
              enddo
              !
            endif
            !
            if (ic1==ic2) then
              !
              do i_l=1,ph_modes
                diagonal_xhi(iq,i_l,i_lambda)= diagonal_xhi(iq,i_l,i_lambda)+ &
&                           conjg(BS_mat(lambda,ikk+neh2))*BS_mat(lambda,ikk+neh1) * &
&                           elph_gkkp(ik_bz1,i_l,iv2,iv1)/sqrt(2.*ph_freqs(i_l))
              enddo
              !
            endif
            !
          enddo
          !
        enddo
        !
     enddo
     !
     call live_timing(steps=1)
     !
   enddo
   !
 enddo
 !
 call live_timing()
 !
 call PP_redux_wait(diagonal_xhi)
 call PP_indexes_reset(px)
 !
 ch='gkkpF_'//trim(intc(lambda_s(1)))
 call of_open_close(trim(ch),'ot')
 call msg('o gkkp','#',(/'E      [meV]', '|gkkp| [meV]','|gkkp|  [fs]'/),INDENT=0,USE_TABS=.true.)  
 call msg('o gkkp','#')
 !
 do iq=1,elph_nDBS_used
   !
   do i_l=1,ph_modes
     !
     ph_freqs(i_l)=sqrt(ph_freqs_sq(iq,i_l))
     !
     gkkp_fs=0.
     if (abs(diagonal_xhi(iq,i_l,1))>0.) gkkp_fs=1./(abs(diagonal_xhi(iq,i_l,1))*HA2EV*HA2FSm1) 
     if (gkkp_fs==0.) cycle
     !
     call msg('o gkkp','',(/ph_freqs(i_l)*HA2EV*1000.,&
&                           abs(diagonal_xhi(iq,i_l,1))*HA2EV*1000.,&
&                           gkkp_fs/),USE_TABS=.true.)
     !
     call live_timing(steps=1)
     !
   enddo
   !
 enddo
 !
 call of_open_close(trim(ch))
 !
 YAMBO_FREE(diagonal_xhi)
 YAMBO_FREE(ph_freqs)
 YAMBO_FREE(BS_nT_at_k)
 call elph_global_free()
 !
end subroutine
