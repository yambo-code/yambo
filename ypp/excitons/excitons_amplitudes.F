!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine excitons_amplitudes(Xk,Xen,BS_E_degs,lambda_s,n_lambda,iq)
 !
 use pars,          ONLY:SP,schlen,pi,cI
 use units,         ONLY:HA2EV
 use R_lattice,     ONLY:bz_samp,qindx_X
 use electrons,     ONLY:levels,n_sp_pol,spin
 use stderr,        ONLY:intc,real2ch
 use YPP,           ONLY:min_weight
 use BS_solvers,    ONLY:BS_mat,BSS_eh_table,BSS_n_descs,BSS_description,BSS_n_eig
 use BS,            ONLY:BS_H_dim
 use com,           ONLY:msg,of_open_close
 use vec_operate,   ONLY:sort
 !
 implicit none
 !
 type(bz_samp)    :: Xk
 type(levels)     :: Xen
 integer          :: BS_E_degs(BSS_n_eig),lambda_s(n_lambda),n_lambda,iq
 !
 integer          :: j1,j2,i_l,i_l_grp,i_lambda,neh,i_spin,iv,ic,ikbz,ikibz,is,n_lambda_deg,S_indx(BS_H_dim)
 integer          :: ikpbz,ikp,isp
 real(SP)         :: A_weight(BS_H_dim),K_weight(Xk%nibz),value,dip_factor,norm_
 character(schlen):: titles(9),ch_dummy(2)
 !
 integer, parameter   :: amp_steps=1000
 integer              :: amp_n_trans
 real(SP)             :: amp_range(2),amp_damping,amp_I(amp_steps),amp_trans(BS_H_dim,2)
 complex(SP)          :: amp_E(amp_steps) 
 !
 call section('=','Amplitude and weights')
 !==================================================
 !
 !call parser('DipWeight',l_weight_with_dipoles)
 !!
 !if(l_weight_with_dipoles) then
 !  call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 !  io_err=io_DIPOLES(Dip,io_ID)
 !endif
 !
 call msg('s','Processing '//trim(intc(n_lambda))//' states')
 !
 do i_lambda=1,n_lambda
   !
   A_weight   =0._SP
   !
   i_l_grp=lambda_s(i_lambda)
   !
   n_lambda_deg=count(BS_E_degs==BS_E_degs(i_l_grp))
   !
   if (n_lambda_deg>1) call msg('s',':: State '//trim(intc(i_l_grp))//' Merged with states '//&
&                                     trim(intc(BS_E_degs(i_l_grp)))//' -> '//&
&                                     trim(intc(BS_E_degs(i_l_grp)+n_lambda_deg-1)))
   !
   do neh = 1,BS_H_dim
     !
     dip_factor=1._SP
     !
     !if(l_weight_with_dipoles) then
     !  ikbz  = BSS_eh_table(neh,1)
     !  iv    = BSS_eh_table(neh,2)
     !  ic    = BSS_eh_table(neh,3)
     !  i_spin= spin(BSS_eh_table(neh,:))
     !  dip_factor=conjg(DIP_projected(ic,iv,ikbz,i_spin))*DIP_projected(ic,iv,ikbz,i_spin)
     !endif
     !
     do i_l=BS_E_degs(i_l_grp),BS_E_degs(i_l_grp)+n_lambda_deg-1
       A_weight(neh)=A_weight(neh)+BS_mat(neh,i_l)*conjg(BS_mat(neh,i_l))*dip_factor
     enddo
     !
   enddo
   !
   norm_ = sqrt(dot_product(A_weight,A_weight))
   !if(l_weight_with_dipoles.and.norm_>0._SP) A_weight=A_weight/norm_
   !
   ! Sort the weights
   !
   call sort(arrin=A_weight,indx=S_indx)
   !
   ! report on file the weights and the amplitude
   ! of the excitonic state...
   ! 
   ! ... first open the file.
   ! 
   ch_dummy(1)='exc_qpt'//trim(intc(iq))//'_weights_at_'//trim(intc(i_l_grp))
   ch_dummy(2)='exc_qpt'//trim(intc(iq))//'_amplitude_at_'//trim(intc(i_l_grp))
   call of_open_close(ch_dummy(1),'ot')
   call of_open_close(ch_dummy(2),'ot')
   
!
!Note that BSS_description is empty as io_BS it is not called anymore 
!
   do j1=1,BSS_n_descs
     call msg('o weight amp',"#",trim(BSS_description(j1)),INDENT=0)
   enddo
   call msg('o weight amp',"#")
   call msg('o weight amp',&
&   '# Electron-Hole pairs that contributes to Excitonic State '//&
&   trim(intc(i_l_grp))//' for iq='//trim(intc(iq))//' more than '// trim(real2ch(min_weight*100)) //'%')
   call msg('o weight amp','#')
   !
   ! First summarize the total weight vs K-points
   !
   K_weight=0._SP
   do neh = 1,BS_H_dim
     ikbz  = BSS_eh_table(S_indx(neh),1)
     ikibz = Xk%sstar(ikbz,1)
     K_weight(ikibz)=K_weight(ikibz)+A_weight(neh)
   enddo
   K_weight=K_weight/maxval(K_weight)
   titles(1:4)=(/'             ','K-point [iku]','             ','Weight       '/)
   call msg('o weight','#',titles(:4),INDENT=0,USE_TABS=.true.)
   do ikibz=1,Xk%nibz
     if (K_weight(ikibz)>min_weight) then
       call msg('o weight','# ',(/Xk%pt(ikibz,:),K_weight(ikibz)/),INDENT=0,USE_TABS=.true.)
     endif
   enddo
   !
   ! Then report the detailed list of transitions & calculate the amplitude
   !
   call msg('o weight','#','',INDENT=0,USE_TABS=.true.)
   titles(1:6)=(/'Band_V  ','Band_C  ','Kv-q ibz','Symm_kv ', 'Kc ibz  ', 'Symm_kc '/)
   if(n_sp_pol==1) then
     titles(7:8)=(/'Weight','Energy'/)
     call msg('o weight','#',titles(:8),INDENT=0,USE_TABS=.true.)
   else
     titles(7:9)=(/'Spin  ','Weight','Energy'/)
     call msg('o weight','#',titles(:7),INDENT=0,USE_TABS=.true.)
   endif
   titles(1:2)=(/'E    [eV]','Amplitude'/)
   call msg('o amp','#',titles(:2),INDENT=0,USE_TABS=.true.)
   call msg('o weight amp','#','',INDENT=0,USE_TABS=.true.)
   !
   amp_n_trans=0
   !
   do neh = BS_H_dim,1,-1
     !
     if (A_weight(neh)/maxval(A_weight)<min_weight) cycle
     !
     ikbz = BSS_eh_table(S_indx(neh),1)
     iv = BSS_eh_table(S_indx(neh),2)
     ic = BSS_eh_table(S_indx(neh),3)
     i_spin= spin(BSS_eh_table(S_indx(neh),:))
     !
     ikibz = Xk%sstar(ikbz,1)
     is = Xk%sstar(ikbz,2)
     !
     ikpbz = qindx_X(iq,ikbz,1)
     ikp  = Xk%sstar(ikpbz,1)
     isp = Xk%sstar(ikpbz,2)
     !
     amp_n_trans=amp_n_trans+1
     amp_trans(amp_n_trans,1)=Xen%E(ic,ikibz,i_spin)-Xen%E(iv,ikp,i_spin)
     amp_trans(amp_n_trans,2)=A_weight(neh)
     !
     if (A_weight(neh)<min_weight) cycle
     !
     if(n_sp_pol==1) then
       call msg('o weight','',(/real(iv,SP),real(ic,SP),real(ikp,SP),real(isp,SP),& 
&                              real(ikibz,SP),real(is,SP),&
&                             A_weight(neh),amp_trans(amp_n_trans,1)*HA2EV/),&
&            INDENT=-2,USE_TABS=.FALSE.)
     else
       if(i_spin==1) value= 1._SP
       if(i_spin==2) value=-1._SP
       call msg('o weight','',(/real(iv,SP),real(ic,SP),real(ikibz,SP),real(is,SP),&
                              real(ikp,SP),real(isp,SP),&
&                           value,A_weight(neh),amp_trans(amp_n_trans,1)*HA2EV/),&
&                 INDENT=-2,USE_TABS=.FALSE.)
     endif
   enddo
   !
   ! Excitonic Amplitude
   !
   amp_range=(/minval(amp_trans(:amp_n_trans,1))-0.5_SP/HA2EV,&
&              maxval(amp_trans(:amp_n_trans,1))+0.5_SP/HA2EV/)
   amp_damping=(amp_range(2)-amp_range(1))/100._SP
   amp_I=0._SP
   do j2=1,amp_steps
     amp_E(j2)=amp_range(1)+(j2-1)*(amp_range(2)-amp_range(1))/real(amp_steps,SP)+cI*amp_damping
     do j1=1,amp_n_trans
       amp_I(j2)=amp_I(j2)+amp_damping/pi*amp_trans(j1,2)/( (real(amp_E(j2),SP)-amp_trans(j1,1))**2+amp_damping**2)
     enddo
   enddo
   amp_I=amp_I/maxval(amp_I)
   do j1=1,amp_steps
     call msg('o amp','',(/real(amp_E(j1),SP)*HA2EV,amp_I(j1)/),INDENT=-2,USE_TABS=.FALSE.)
   enddo
   !
   call of_open_close(ch_dummy(1))
   call of_open_close(ch_dummy(2))
   !
 enddo
 !
 return
 !
end subroutine excitons_amplitudes
