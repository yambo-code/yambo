!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine excitons_amplitudes(Xk,Xen,q,BS_E_degs,iq)
 !
 use pars,           ONLY:SP,schlen,pi,cI
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,qindx_X
 use electrons,      ONLY:levels,n_sp_pol,spin
 use stderr,         ONLY:intc,real2ch
 use OUTPUT,         ONLY:OUTPUT_driver
 use YPPm,           ONLY:EXCITON_weight_treshold,EXCITONS_user_indexes,EXCITONS_n_user_states,DIPs,&
&                         DIPs_direction,l_dipoles,BSiq
 use BS_solvers,     ONLY:BS_mat,BSS_eh_table,BSS_desc,BSS_n_eig
 use DIPOLES,        ONLY:DIP_rotated,DIP_alloc,DIPOLES_reset
 use BS,             ONLY:BS_H_dim,BS_bands,BSqpts
 use com,            ONLY:msg
 use vec_operate,    ONLY:sort,v_norm
 use interfaces,     ONLY:DESC_write
 use parallel_int,   ONLY:PARALLEL_global_indexes,PP_redux_wait
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP
 !
 implicit none
 !
 type(bz_samp)    :: Xk,q
 type(levels)     :: Xen
 integer          :: BS_E_degs(BSS_n_eig),iq
 !
 ! Workspace
 integer          :: j1,j2,i_l,i_l_grp,i_lambda,neh,i_spin,iv,ic,ikbz,ikibz,is,n_lambda_deg,S_indx(BS_H_dim),&
&                    ikpbz,ikp,isp,io_err
 real(SP)         :: A_weight(BS_H_dim),K_weight(Xk%nibz),dip_factor,norm_
 character(schlen):: titles(9),o_file(2)
 integer, parameter   :: amp_steps=1000
 integer              :: amp_n_trans
 real(SP)             :: amp_range(2),amp_damping,amp_I(amp_steps),amp_trans(BS_H_dim,2)
 complex(SP)          :: amp_E(amp_steps) 
 !
 call section('=','Amplitude and weights')
 !========================================
 !
 ! Dipoles
 !---------
 if (l_dipoles) then
   !
   ! ... Dimensions
   DIPs%ib    =BS_bands
   DIPs%ib_lim=(/maxval(Xen%nbm),minval(Xen%nbf)+1/)
   !
   ! ... PARALLEL setup
   call PARALLEL_global_indexes(Xen,Xk,q," ",RESET=.TRUE.)
   call PARALLEL_global_indexes(Xen,Xk,q,"DIPOLES",Dip=DIPs,Dip_limits_pre_defined=.TRUE.)
   !
   ! ... IO
   call DIPOLE_IO(Xk,Xen,DIPs,'read ',io_err,'DIP')
   !
   DIPs_direction=BSqpts(:,BSiq)/v_norm(BSqpts(:,BSiq))
   !
 endif
 !
 call msg('s','Processing '//trim(intc(EXCITONS_n_user_states))//' states')
 !
 do i_lambda=1,EXCITONS_n_user_states
   !
   A_weight   =0._SP
   !
   i_l_grp=EXCITONS_user_indexes(i_lambda)
   !
   n_lambda_deg=count(BS_E_degs==BS_E_degs(i_l_grp))
   !
   if (n_lambda_deg>1) call msg('s',' State '//trim(intc(i_l_grp))//' Merged with states '//&
&                                     trim(intc(BS_E_degs(i_l_grp)))//' -> '//&
&                                     trim(intc(BS_E_degs(i_l_grp)+n_lambda_deg-1)))
   !
   do neh = 1,BS_H_dim
     !
     dip_factor=1._SP
     !
     if (l_dipoles) then
       ikbz  = BSS_eh_table(neh,1)
       ikibz = Xk%sstar(ikbz,1)
       iv    = BSS_eh_table(neh,2)
       ic    = BSS_eh_table(neh,3)
       i_spin= spin(BSS_eh_table(neh,:))
       if (.not.PAR_IND_DIPk_ibz%element_1D(ikibz)) cycle
       if (.not.PAR_IND_CON_BANDS_DIP%element_1D(ic)) cycle
       if (.not.PAR_IND_VAL_BANDS_DIP%element_1D(iv)) cycle
       dip_factor=abs(dot_product(DIPs_direction,DIP_rotated(ic,iv,ikbz,i_spin,"DIP_iR",Xk)))**2.
     endif
     !
     do i_l=BS_E_degs(i_l_grp),BS_E_degs(i_l_grp)+n_lambda_deg-1
       A_weight(neh)=A_weight(neh)+BS_mat(neh,i_l)*conjg(BS_mat(neh,i_l))*dip_factor
     enddo
     !
   enddo
   !
   if (l_dipoles) call PP_redux_wait( A_weight )
   !
   norm_ = sqrt(dot_product(A_weight,A_weight))
   if(l_dipoles.and.norm_>0._SP) A_weight=A_weight/norm_
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
   o_file(1)='exc_qpt'//trim(intc(iq))//'_weights_at_'//trim(intc(i_l_grp))
   if (l_dipoles) o_file(1)='exc_qpt'//trim(intc(iq))//'_weights_and_dipoles_at_'//trim(intc(i_l_grp))
   o_file(2)='exc_qpt'//trim(intc(iq))//'_amplitude_at_'//trim(intc(i_l_grp))
   !
   do j1=1,2
     call OUTPUT_driver(o_file(j1),action="reset")
     call OUTPUT_driver(o_file(j1),action="open")
     call DESC_write('o '//o_file(j1),'#',BSS_desc)
   enddo
   !
   ! Note that BSS_description is empty as io_BS it is not called anymore 
   call msg('o weight',"#")
   call msg('o weight','# Electron-Hole pairs that contribute to Excitonic State '//&
&           trim(intc(i_l_grp))//' for iq='//trim(intc(iq))//' more than '// trim(real2ch(EXCITON_weight_treshold*100)) //'%')
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
     if (K_weight(ikibz)>EXCITON_weight_treshold) then
       call msg('o weight','# ',(/Xk%pt(ikibz,:),K_weight(ikibz)/),INDENT=0,USE_TABS=.true.)
     endif
   enddo
   call msg('o weight','#','',INDENT=0,USE_TABS=.true.)
   !
   amp_n_trans=0
   !
   do neh = BS_H_dim,1,-1
     !
     if (A_weight(neh)/maxval(A_weight)<EXCITON_weight_treshold) cycle
     !
     ikbz = BSS_eh_table(S_indx(neh),1)
     iv = BSS_eh_table(S_indx(neh),2)
     ic = BSS_eh_table(S_indx(neh),3)
     i_spin= spin(BSS_eh_table(S_indx(neh),:))
     if (i_spin==1) j1= 1
     if (i_spin==2) j1=-1
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
     if (A_weight(neh)<EXCITON_weight_treshold) cycle
     !
     call OUTPUT_driver('weight',TITLES=(/"Band_V","Band_C"/),I_VALUES=(/iv,ic/))
     call OUTPUT_driver('weight',TITLES=(/"Kv-q ibz","Symm_kv "/),I_VALUES=(/ikp,isp/))
     call OUTPUT_driver('weight',TITLES=(/"Kc q ibz","Symm_kc "/),I_VALUES=(/ikibz,is/))
     if (n_sp_pol>1) call OUTPUT_driver('weight',TITLES=(/"Spin"/),I_VALUES=(/j1/))
     call OUTPUT_driver('weight',TITLES=(/"Weight"/),R_VALUES=(/A_weight(neh)/))
     call OUTPUT_driver('weight',TITLES=(/"Energy"/),R_VALUES=(/amp_trans(amp_n_trans,1)/),UNIT="eV")
     call OUTPUT_driver("weight",action="write")
     !
   enddo
   !
   ! Excitonic Amplitude
   !
   amp_range=(/minval(amp_trans(:amp_n_trans,1))-0.5_SP/HA2EV,maxval(amp_trans(:amp_n_trans,1))+0.5_SP/HA2EV/)
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
     call OUTPUT_driver('_amp',TITLES=(/"Energy"/),R_VALUES=(/real(amp_E(j1),SP)/),UNIT="eV")
     call OUTPUT_driver('_amp',TITLES=(/"Amplitude"/),R_VALUES=(/amp_I(j1)/))
     call OUTPUT_driver("_amp",action="write")
   enddo
   !
   do j1=1,2
     call OUTPUT_driver(o_file(j1),action="close")
   enddo
   !
 enddo
 !
 if (l_dipoles) then
   call DIP_alloc('DIP_iR')
   call DIP_alloc('DIP_P')
   call DIP_alloc('DIP_v')
   call DIPOLES_reset(DIPs)
 endif
 !
 return
 !
end subroutine excitons_amplitudes
