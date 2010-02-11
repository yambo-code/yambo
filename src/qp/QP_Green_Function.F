!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
 use pars,          ONLY:SP,pi
 use com,           ONLY:msg
 use units,         ONLY:HARTREE
 use QP_m,          ONLY:QP_t,QP_Sc,QP_Vnl_xc,QP_Vxc,QP_table,&
&                        QP_n_states,use_GreenF_to_eval_QP,QP_Sc_steps,QP_G_dr,&
&                        QP_dSc_delta,GF_is_causal,QP_G_Zoom_treshold,&
&                        QP_G_amplitude_integral,QP_G_zoom_er,use_GreenF_Zoom,QP_G_er
 use electrons,     ONLY:spin,levels,spin_occ
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
 complex(SP)      ::QP_Go,f_energy
 real(SP)         ::Eo,Amp_Peak,old_min_max_step(2),new_min_max_step(2),W_step
 logical          ::band_is_metallic,apply_damping
 !
 real(SP), external :: Integrate
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
     call FREQUENCIES_Green_Function(iqp,W,E%E)
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
   band_is_metallic=ib>E%nbf.and.ib<=E%nbm
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
   apply_damping= maxval(abs( aimag(qp%S_total(iqp,:) ) )) < 0.001/HARTREE
   !
   do iw=1,W%n(1) 
     !
     !  Davide: 15/04/2009 moved conjg from the second term of the Green to
     !          the first according to the QP expression for the T-ordered Green
     !          G=\frac{1}{\omega-\epsilon_i+i\eta sign(\epsilon-\mu)}         
     !
     f_energy=conjg(qp%GreenF_W(iqp,iw))
     if (GF_is_causal)  f_energy=qp%GreenF_W(iqp,iw)
     !
     QP_Go=           E%f(ib,ik,i_spin)/(f_energy-Eo)/spin_occ+&
&          (spin_occ-E%f(ib,ik,i_spin))/(qp%GreenF_W(iqp,iw) -Eo)/spin_occ  
     !
     if ( band_is_metallic .or. apply_damping ) then
       qp%GreenF(iqp,iw)=QP_Go/(1.-QP_Go*qp%S_total(iqp,iw))  
     else
       !
       ! Andrea (20/7/2009): If the imaginary part of the self-energy
       ! is smaller than the broadening the sign of the spectral function
       ! may be wrong. So, as the SE is already broaded during the evaluation,
       ! in case the band/k is not metallic I use an analytic QP_Go with 
       ! zero damping.
       !
       qp%GreenF(iqp,iw)=1./(real(qp%GreenF_W(iqp,iw))-Eo-qp%S_total(iqp,iw)) 
     endif
     !
   enddo
   !
   ! Spectral Function Integral
   !
   QP_G_amplitude_integral(iqp)=Integrate(aimag(qp%GreenF(iqp,:)),real(qp%GreenF_W(iqp,:)),W%n(1))/pi
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
     call FREQUENCIES_Green_Function(iqp,W,E%E)
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
   call msg('nr','[Green zoom] Frequency step              (current)   [ev]:',old_min_max_step*HARTREE)
   call msg('r', '[Green zoom]                                (zoom)   [ev]:',new_min_max_step*HARTREE)
 endif
 !
end subroutine

