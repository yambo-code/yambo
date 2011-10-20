!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine QP_Green_Function(qp,E,GFdb_read_err)
 !
 ! This routine calcualtes a modified definition of the GF that
 ! yields an always positive spectral function. This is because the 
 ! Im(G) can be easily post-processed.
 !
 ! So regardless of the temperature the G defined here is
 !
 ! G = conjg( G_causal )
 !
 use pars,          ONLY:SP,pi
 use com,           ONLY:msg
 use units,         ONLY:HA2EV
 use QP_m,          ONLY:QP_t,QP_Sc,QP_Vnl_xc,QP_Vxc,QP_table,&
&                        QP_n_states,use_GreenF_to_eval_QP,QP_Sc_steps,QP_G_dr,&
&                        QP_dSc_delta,QP_G_Zoom_treshold,&
&                        QP_G_amplitude_integral,QP_G_zoom_er,use_GreenF_Zoom
 use electrons,     ONLY:spin,levels
 use frequency,     ONLY:w_samp,W_reset
 implicit none
 !
 type(levels)     ::E
 type(QP_t)       ::qp
 integer          ::GFdb_read_err
 !
 ! Work Space
 !
 type(w_samp)     ::W
 integer          ::iqp,ib,ik,i_spin,iw,iw_start,iw_end
 real(SP)         ::Eo,Amp_Peak,old_min_max_step(2),new_min_max_step(2),W_step
 !
 real(SP), external :: RIntegrate
 !
 ! The Green's function
 !
 call W_reset(W)
 W%n                =QP_Sc_steps
 W%dr               =QP_G_dr
 old_min_max_step   =(/1000.,0./)
 new_min_max_step   =(/1000.,0./)
 !
 do iqp=1,QP_n_states
   !
   ! Frequency range
   !
   if (GFdb_read_err<0) then
     call FREQUENCIES_Green_Function(iqp,W,E%E,.FALSE.)
     qp%GreenF_W(iqp,:)=W%p(:)
   endif
   !
   ! Frequency step
   !
   W_step=real(qp%GreenF_W(iqp,2)-qp%GreenF_W(iqp,1))
   if (W_step<old_min_max_step(1)) old_min_max_step(1)=W_step
   if (W_step>old_min_max_step(2)) old_min_max_step(2)=W_step
   !
   ! Bare energy
   !
   ib    =QP_table(iqp,1)
   ik    =QP_table(iqp,3)
   i_spin=spin(QP_table(iqp,:))
   !
   Eo=E%E(ib,ik,i_spin) 
   if (associated(E%Eo)) Eo=E%Eo(ib,ik,i_spin)
   !
   if (GFdb_read_err<0) then
     if ( allocated(QP_Vnl_xc) ) then
       qp%S_total(iqp,:)=QP_Sc(iqp,:)+QP_Vnl_xc(iqp)-QP_Vxc(iqp)
     else
       qp%S_total(iqp,:)=QP_Sc(iqp,:)
     endif
   endif
   !
   do iw=1,W%n(1) 
     !
     qp%GreenF(iqp,iw)=1./(real(qp%GreenF_W(iqp,iw))-Eo-qp%S_total(iqp,iw))
     !
   enddo
   !
   ! Spectral Function Integral
   !
   QP_G_amplitude_integral(iqp)=RIntegrate(aimag(qp%GreenF(iqp,:)),real(qp%GreenF_W(iqp,:)),W%n(1))/pi
   !
   ! Search for a finer energy range 
   !
   if (use_GreenF_Zoom.and.GFdb_read_err==0) then
     !
     Amp_Peak=maxval(aimag(qp%GreenF(iqp,:)))
     iw_start=0
     do iw=1,W%n(1)
       if (iw_start==0.and.aimag(qp%GreenF(iqp,iw))>Amp_Peak*QP_G_Zoom_treshold/100.) iw_start=iw
       if (iw_start/=0) exit
     enddo
     iw_end=0
     do iw=W%n(1),1,-1
       if (iw_end==0.and.aimag(qp%GreenF(iqp,iw))>Amp_Peak*QP_G_Zoom_treshold/100.) iw_end=iw
       if (iw_end/=0) exit
     enddo
     QP_G_zoom_er(iqp,:)=(/real(qp%GreenF_W(iqp,iw_start)),real(qp%GreenF_W(iqp,iw_end))/)
     qp%GreenF(iqp,:)=(0.,0.)
     !
     ! Zoomed Frequency step
     !
     W_step=(QP_G_zoom_er(iqp,2)-QP_G_zoom_er(iqp,1))/QP_Sc_steps
     if (W_step<new_min_max_step(1)) new_min_max_step(1)=W_step
     if (W_step>new_min_max_step(2)) new_min_max_step(2)=W_step
     !
     call FREQUENCIES_Green_Function(iqp,W,E%E,.FALSE.)
     qp%GreenF_W(iqp,:)=W%p(:)
     !
   endif
   !
   if (.not.use_GreenF_to_eval_QP) cycle
   !
   ! Extraction of the QP properties directly from the Green's function
   !====================================================================
   !
   call QPartilize(W%n(1),qp%GreenF(iqp,:),real(qp%GreenF_W(iqp,:)),qp%E(iqp),qp%Z(iqp),QP_dSc_delta) 
   !
 enddo
 !
 if ( use_GreenF_Zoom .and. GFdb_read_err==0) then
   call msg('nr','[Green zoom] Frequency step              (current)   [ev]:',old_min_max_step*HA2EV)
   call msg('r', '[Green zoom]                                (zoom)   [ev]:',new_min_max_step*HA2EV)
 endif
 !
end subroutine

