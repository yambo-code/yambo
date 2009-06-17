!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
integer function io_SC_components(what,E_sc,ID)
 !
 ! what can be E WF V J G
 !
 use pars,           ONLY:SP
 use units,          ONLY:HARTREE
 use drivers,        ONLY:l_sc_run
 use R_lattice,      ONLY:nkibz
 use D_lattice,      ONLY:atoms_string
 use stderr,         ONLY:intc
 use QP_m,           ONLY:SC_band_mixing
 use SC,             ONLY:SC_bands,it_now,rho_convergence,E_convergence,SC_potential,&
&                         SC_R,V_xc_sc,local_V,H_nl_sc,SC_kind,&
&                         SC_xc_functional,V_hartree_sc,NSC_shot,SC_fft_size,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size
 use IO_m,           ONLY:io_connect,io_disconnect,io_header,io_elemental,io_status,&
&                         io_sec,io_bulk,io_fragment,read_is_on,write_is_on,&
&                         manage_RD_WR_CL_IF_END
#if defined _RT
 use real_time,      ONLY:l_rt_efield,EF_q0,l_fast_exchange,l_RPA,RT_step,Tot_steps, &
&                         Thermal_steps,Integrator_name,l_qp_analyze,DiagH_Iter, &
&                         LifeTime,l_always_relax,Ext_Field_name,Ext_Field_Int,  &
&                         Ext_Field_freq,RT_resonant,RT_no_intraband,l_no_diamagnetic,&
&                         T_initial,j_current,A_norm,G_lesser
 use IO_m,           ONLY:io_restart,io_check_restart
 use QP_m,           ONLY:QP_nk
#endif                                
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use electrons,      ONLY:levels,n_sp_pol,n_spin
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_CHOSEX,SC_EXXC
 implicit none
 !
 integer               :: ID
 type(levels)          :: E_sc
 character(*)          :: what
 !
 ! Work Space
 !
 real(SP), allocatable :: R_H_disk(:,:,:),E_disk(:,:,:)
 integer               :: ik,bands_to_load,ib,SC_kind_disk,io_status_save,i_spin,i1
 logical               :: l_wf,l_V,l_E,l_IMPOSE_SN,CHECK_
#if defined _RT		
 logical               :: l_J,l_G
 integer               :: it_IO
 real(SP)              :: current_disk(3),vec_pot_disk(2)
#endif
 !
 l_E =what=="E"
 l_wf=what=="WF"
 l_V =what=="V"
#if defined _RT		
 l_J =what=="J"
 l_G =what=="G"
#endif
 !
 ! When the db.scV is used for single non-SC shot the number
 ! of bands and SC_mixing can be not respected. 
 !
 CHECK_=.TRUE.
 if (NSC_shot) CHECK_=.FALSE.
 !
 if (l_wf) io_SC_components=io_connect(desc="scWFs",type=2,ID=ID)
 if (l_E)  io_SC_components=io_connect(desc="scE",type=2,ID=ID)
 if (l_V)  io_SC_components=io_connect(desc="scV",type=2,ID=ID)
#if defined _RT
 if (l_J)  io_SC_components=io_connect(desc="rtJ",type=2,ID=ID)
 if (l_G)  io_SC_components=io_connect(desc="rtG",type=2,ID=ID)
#endif
 !
 if (io_SC_components/=0) goto 1
 !
 ik = maxval(io_sec(ID,:))-1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   l_IMPOSE_SN=.not.l_V
   !
   io_SC_components=io_header(ID,IMPOSE_SN=l_IMPOSE_SN)
   !
   if (io_SC_components/=0) goto 1
   !
   call io_elemental(ID,VAR='ATOMS_STRING',CH0="",VAR_SZ=1)
   call io_elemental(ID,CH0=atoms_string,&
&       VAR=' Unit cell atoms                 :',CHECK=CHECK_,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='XC_FUNCTIONAL',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=SC_potential,&
&       VAR=' xc Functional                   :',CHECK=CHECK_,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="XC_KIND",VAR_SZ=2,MENU=0)
   call io_elemental(ID,I0=SC_kind,DB_I0=SC_kind_disk)
   call io_elemental(ID,I0=SC_xc_functional)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   io_status_save=io_status(ID)
   call io_elemental(ID,VAR="LOCAL_ITER_BANDS_MIX",VAR_SZ=4,MENU=0) 
   call io_elemental(ID,L0=Local_V,&
&       VAR=' The potential is local          :')
#if defined _RT
   if(.not.l_G.and..not.l_J) then
#endif
   call io_elemental(ID,I0=it_now,DB_I0=it_now,&
&       VAR=' Iteration                       :')
#if defined _RT
   endif
#endif
   !
   if (l_sc_run) then
     call io_elemental(ID,I0=SC_bands,&
&         VAR=' Total number of bands           :',CHECK=CHECK_,OP=(/"=="/))
   else
     call io_elemental(ID,I0=SC_bands,&
&         VAR=' Total number of bands           :',CHECK=.true.,OP=(/"<="/))
   endif
   !
   call io_elemental(ID,R0=SC_band_mixing,&
&       VAR=' Bands mixing                    :',CHECK=CHECK_,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (any((/SC_kind_disk==SC_EXX,SC_kind_disk==SC_EXXC,SC_kind_disk==SC_HF,&
&            SC_kind_disk==SC_CHOSEX/))) then
     call io_elemental(ID,VAR="SIGMAx_G",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=SC_ng_Sx,&
&         VAR=' Sx RL components                :',CHECK=CHECK_,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   if ( SC_kind_disk==SC_CHOSEX   ) then
     call io_elemental(ID,VAR="X_bands_size",VAR_SZ=3,MENU=0) 
     call io_elemental(ID,I1=SC_X_bands,&
&         VAR=' Response function bands         :',CHECK=CHECK_,OP=(/"==","=="/))
     call io_elemental(ID,I0=SC_X_size,&
&         VAR='                   size          :',CHECK=CHECK_,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
   if (io_status_save==0.and.io_status(ID)<0) then
     io_status(ID)=-2
     if (NSC_shot) io_status(ID)=0
   endif
   !
   if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_CHOSEX/))) then
     call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0) 
     call io_elemental(ID,I0=SC_fft_size,&
&         VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
#if defined _RT
   !
   if(any((/l_J,l_G/))) then
     call io_elemental(ID,VAR="RT_DYNAMICS",VAR_SZ=6,MENU=0) 
     call io_elemental(ID,L0=l_fast_exchange,VAR=' Use fast exchange  :')
     call io_elemental(ID,L0=l_RPA,VAR=' RPA optics in real time  :')
     call io_elemental(ID,&
&       VAR=" external field intensity          :",R0=RT_step,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID, &
&       VAR=" total number of steps          :",I0=Tot_steps,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID, &
&       VAR=" number of thermalization steps :",I0=Thermal_steps,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,CH0=Integrator_name, &
&         VAR=' integrator                   :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     call io_elemental(ID,VAR="RT_OTHERS",VAR_SZ=4,MENU=0) 
     call io_elemental(ID,L0=l_qp_analyze,VAR=' analize quasi-particles  :')
     call io_elemental(ID,I0=DiagH_Iter,VAR='diagonalization interval   :')
     call io_elemental(ID,R0=LifeTime,VAR=' quasi-particle life time  :')
     call io_elemental(ID,L0=l_always_relax,VAR=' relaxation during thermalization  :')
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     call io_elemental(ID,VAR="EXTERNAL_FIELD",VAR_SZ=8,MENU=0) 
     call io_elemental(ID,L0=l_rt_efield,VAR=' External Field is present :')
     call io_elemental(ID,CH0=Ext_Field_name, &
&         VAR=' external field                  :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,R1=EF_q0,CHECK=.true., &
&       VAR=" electric field versor             :",OP=(/"==","==","=="/))
     call io_elemental(ID,&
&       VAR=" external field intensity          :",R0=Ext_Field_Int,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,&
&       VAR=" external field frequency          :",R0=Ext_Field_freq,CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,L0=RT_resonant,    VAR=' Resonant external field    :')
     call io_elemental(ID,L0=RT_no_intraband,VAR=' No intraband transitions   :')
     call io_elemental(ID,L0=l_no_diamagnetic,VAR=' No diamagnetic term        :')
     call io_elemental(ID,R0=T_initial,VAR=' initial time for the external field :')
     call io_elemental(ID,VAR="",VAR_SZ=0)
     !
   endif
   !
#endif
   !
#if defined _RT
   if(any((/l_E,l_V,l_wf/))) then
#endif
   !
   call io_elemental(ID,VAR="RHOCONV_ECONV",VAR_SZ=2,MENU=0) 
   call io_elemental(ID,R0=rho_convergence)
   call io_elemental(ID,R0=E_convergence)
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
#if defined _RT
   endif
#endif
   !
   io_SC_components=io_status(ID)
   if (io_SC_components/=0) goto 1
   !
 endif
 ! 
#if defined _RT
 !
 ! Time to store
 !
 do i1=1,size(io_sec(ID,:))
   if(io_sec(ID,i1)==0.or.io_sec(ID,i1)==1) cycle
   it_IO=io_sec(ID,i1)-1
 enddo 
 !
 if(any((/l_E,l_V,l_wf/))) then
   call manage_RD_WR_CL_IF_END(ID,ik,nkibz)
 else
   call manage_RD_WR_CL_IF_END(ID,it_now,Tot_steps+Thermal_steps)
 endif  
#else
 call manage_RD_WR_CL_IF_END(ID,ik,nkibz)
#endif
 !
 ! Potential
 !===========

 !
 if (l_V.and.local_V.and.ik==0.and.allocated(V_xc_sc)) then
   !
   call io_bulk(ID,VAR="V_xc_sc",VAR_SZ=(/SC_fft_size,n_spin/))
   call io_bulk(ID,R2=V_xc_sc)
   !
   call io_bulk(ID,VAR="V_hartree",VAR_SZ=(/SC_fft_size,n_sp_pol/))
   call io_bulk(ID,R2=V_hartree_sc)
   !
 endif
 !
 if ( l_E.and.associated(E_sc%E)) then
   !
   ! Energies
   !==========
   !
   call io_bulk(ID,VAR="E_sc",VAR_SZ=(/SC_bands,nkibz,n_sp_pol/))
   if (write_is_on(ID)) then
     call io_bulk(ID,R3=E_sc%E(:SC_bands,:,:))
   else
     allocate(E_disk(SC_bands,nkibz,n_sp_pol))
     call io_bulk(ID,R3=E_disk)
     !
     bands_to_load=size(E_sc%E,1)
     !
     if (bands_to_load>SC_bands) then
       !
       ! For either the H_sc and WFs I first check to 
       ! have enough bands (in wf_load and X_O_strengths).
       ! For the energies instead I need to shift the levels > SC_bands (if any).
       !
       forall (ib=SC_bands+1:bands_to_load) E_sc%E(ib,:,:)=E_sc%E(ib,:,:)-E_sc%E(SC_bands,:,:)
       E_sc%E(:SC_bands,:,:)=E_disk(:SC_bands,:,:)
       forall (ib=SC_bands+1:bands_to_load) E_sc%E(ib,:,:)=E_sc%E(ib,:,:)+E_sc%E(SC_bands,:,:)
       !
     else
       E_sc%E(:bands_to_load,:,:)=E_disk(:bands_to_load,:,:)
     endif
     deallocate(E_disk)
   endif
   !
 endif
 !
#if defined _RT
 if(any((/l_E,l_V,l_wf/))) then
#endif
 !
 if (ik==0) goto 1
 !
 ! Fragmentation
 !===============
 !
 call io_fragment(ID,i_pointer=ik)
 !
#if defined _RT
 endif
#endif
 !
 if (l_V.and..not.local_V) then
   !
   ! H_sc
   !======
   !
   allocate(R_H_disk(SC_bands,SC_bands,2))
   !
   do i_spin=1,n_sp_pol
     !
     call io_bulk(ID,VAR="H_nl_sc_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
     !
     if (write_is_on(ID)) call mat_c2r(H_nl_sc(:,:,i_spin),R_H_disk)
     call io_bulk(ID,R3=R_H_disk)
     if (read_is_on(ID))  call mat_r2c(R_H_disk,H_nl_sc(:,:,i_spin))
     !
   enddo
   !
   deallocate(R_H_disk)
   !
 endif
 !
 if ( l_WF ) then
   !
   ! Rotation matrix 
   !=================
   !
   allocate(R_H_disk(SC_bands,SC_bands,2))
   !
   do i_spin=1,n_sp_pol
     call io_bulk(ID,VAR="Rotation_Matrix_K"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
     !
     if (write_is_on(ID)) call mat_c2r(SC_R(:,:,ik,i_spin),R_H_disk)
     call io_bulk(ID,R3=R_H_disk)
     if (read_is_on(ID))  call mat_r2c(R_H_disk,SC_R(:,:,ik,i_spin))
     !
   enddo
   deallocate(R_H_disk)
 endif
 !
#if defined _RT
 !
 if ( l_G ) then
   !
   ! lesser Green Function
   !=================
   !
   call io_check_restart("rtG",ID)
   io_SC_components=io_restart(ID)
   !
   if (io_SC_components/=0) goto 1  
   !
   if(any(io_sec(ID,:)/=1)) then
     !
     allocate(R_H_disk(SC_bands,SC_bands,2))
     !
!    do i_spin=1,n_sp_pol
     i_spin=1
     !
     do ik=1,QP_nk
       !
       call io_bulk(ID,VAR="Lesser_Green_Function"//trim(intc(ik))//"_SPIN"//trim(intc(i_spin)),&
&                 VAR_SZ=(/SC_bands,SC_bands,2/))
       !
       if (write_is_on(ID)) call mat_c2r(G_lesser(:,:,ik),R_H_disk)
       call io_bulk(ID,R3=R_H_disk)
       if (read_is_on(ID))  call mat_r2c(R_H_disk,G_lesser(:,:,ik))
       !
     enddo
     !
!    enddo
     deallocate(R_H_disk)
     !
     call io_check_restart("rtG",ID,now=it_IO,todo=Thermal_steps+Tot_Steps)  
     io_SC_components=io_restart(ID)    
     !
   endif
   !
 endif
 !
 if ( l_J ) then
   !
   ! Current and Vector Potential
   !=================
   !
   if(any(io_sec(ID,:)/=1)) then
     !   
     call io_bulk(ID,VAR="Induced_current",VAR_SZ=(/3/))
     if (write_is_on(ID))    current_disk(:)=real(j_current(:))
     call io_bulk(ID,R1=current_disk)
     if (read_is_on(ID)) j_current(:)=cmplx(current_disk(:),0.)
     !
     call io_bulk(ID,VAR="Vector_potential",VAR_SZ=(/2/))
     if (write_is_on(ID)) then
       vec_pot_disk(1)=real(A_norm)
       vec_pot_disk(2)=aimag(A_norm)
     endif
     call io_bulk(ID,R1=vec_pot_disk)
     if (read_is_on(ID)) then
       A_norm = cmplx(vec_pot_disk(1),vec_pot_disk(2))
     endif
     !
   endif
   !
 endif       
 !
#endif
 !
1 call io_disconnect(ID=ID)
 !
end function
