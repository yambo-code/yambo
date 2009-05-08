!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_driver(en,Xen,X,k,xk,q)
 !
 use pars,           ONLY:SP,DP,lchlen,schlen
 use stderr,         ONLY:set_real_printed_length
 use units,          ONLY:HARTREE,FS2AUT
 use com,            ONLY:msg,of_open_close,error,warning
 use vec_operate,    ONLY:normalize_v
 use stderr,         ONLY:intc
 use timing,         ONLY:live_time_msg,live_timing 
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels,el_density,spin_occ,n_spin,E_duplicate 
 use X_m,            ONLY:X_t,X_poles,X_poles_tab,Eval_P_and_P2_only,X_alloc
 use wave_func,      ONLY:wf,wf_load,wf_ng,WF_free
 use FFT_m,          ONLY:fft_size
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 use matrix_operate, ONLY:mat_dia_inv,INV,USE_LK,DIAGO
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_table,QP_n_states,QP_Vxc
 use SC,             ONLY:SC_potential,SC_bands,SC_fft_size,V_hartree_sc,Ho
 use real_time,      ONLY:EF_q0,Thermal_steps,Tot_steps,Ext_Field_name,RT_step,G_lesser,           &
&                         G_lesser_old,l_rt_efield,n_poles,RT_alloc,RT_free,rho,l_keep_vxc,        &
&                         Integrator_Name,l_rt_exc,RT_no_intraband,RT_resonant,V_hartree_0,V_xc_0, &
&                         Thermal_time,RT_phase_int,DiagH_Iter,H_t,EXACT,RK2EXACT,Ho_pot,A_t

 use D_lattice,      ONLY:Tel
 use xc_functionals, ONLY:V_xc

 implicit none
 !
 ! Notice that "en" never changes, it contains
 ! the bare occupation and bare energies
 !
 type(levels)  :: en
 type(levels)  :: Xen
 !
 type(bz_samp) :: k,Xk,q
 type(X_t):: X(4)
 ! 
 ! Work Space
 !
 logical, external        :: XC_switch
 integer                  :: it,idt,i1,i2,ib,ibp
 character(schlen)        :: dumb_ch,headings(10),Int_Type
 character(lchlen)        :: file_name(2),qp_fname
 integer                  :: WF_G_max,WF_Go_indx
 integer                  :: i_pole,iv,ic,ik,i_sp
 integer                  :: X_eh_setup
 real(SP)                 :: minmax_ehe(2)
 real(SP)                 :: values(9)
 real(SP)                 :: Occ_Natural(SC_bands,en%nk)
 complex(SP)              :: TmpMatrix(SC_bands,SC_bands)
 complex(SP)              :: V_tmp1(SC_bands),V_tmp2(SC_bands)
 real(SP)                 :: RT_time  ! The real time on the diagonal RTime = it*RT_step   
 complex(SP)              :: Trace_Gf,cdotu
 complex(SP)              :: E_kin,E_ion,E_h
 real(SP)                 :: E_xc
 real(SP)                 :: j_current(3)
 !
 logical                  :: EpsAnalyze  ! Analyde current response
 real(DP),    allocatable :: j_current_store(:,:)
 complex(DP), allocatable :: A_t_store(:)
 !
 !
#if defined _DOUBLE
 complex(DP)              :: zdotc
#else
 complex(SP)              :: cdotc
#endif
 !
 call parser('RTResonant',RT_resonant)
 call parser('RTNoIntraband',RT_no_intraband)
 call parser('PhaseInt',RT_phase_int)
 call parser('EpsAnalyze',EpsAnalyze)
 call parser('KeepVxc',l_keep_vxc)
 !
 call set_real_printed_length(f_length=15,g_length=15)  
 !
 WF_G_max=wf_ng
 WF_Go_indx=1  
 SC_fft_size=fft_size
 !
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 !
 if(Thermal_steps/=0) then
   !
   Thermal_time=real(Thermal_steps)*RT_step
   call section('*','Adiabatic Switching')
   call msg('nr','Thermalization Time :',Thermal_Time/FS2AUT)
   !
 else
   !      
   Thermal_time=0._SP
   !
 endif
 !
 l_rt_efield=.false.
 if(Ext_Field_Name/='NONE'.and.Ext_Field_Name/='none') then
   !      
   l_rt_efield=.true.
   call section('*','External Field')
   !
 else
   !      
   A_t=(0._SP,0._SP)
   if(EpsAnalyze) then
     call warning(' No External Field  EpsAnalyze set to .false.')      
     EpsAnalyze=.false.
   endif
   !
 endif
 !
 if(EpsAnalyze) then
   !
   allocate(j_current_store(Tot_steps,3))
   call mem_est("J_current",(/size(j_current_store)/),(/SP/))
   allocate(A_t_store(Tot_steps))
   call mem_est("A_t",(/size(A_t_store)/),(/2*SP/))
   !
 endif
 !                                              
 if(RT_phase_int.and.(trim(Integrator_Name)==EXACT.or.trim(Integrator_Name)==RK2EXACT)) &
& call error(" You cannot use EXACT integrator with PhaseInt!")
 !
 call msg('nr','[RT] Integrator:'//Integrator_Name)
 !
 l_rt_exc=XC_switch()
 !
 X(1)%ib(2)=SC_bands
 X(1)%ng   =wf_ng
 Eval_P_and_P2_only=.true.
 call Dipole_driver(Xen, Xk, X(1), X(1)%q0)
 !
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),space='R',title=' ')  
 !
 QP_nb=SC_bands
 QP_nk=k%nibz
 !
 call RT_alloc(Xen)
 !
 call mem_est("QP_state",(/QP_n_states*QP_nk/))
 allocate(QP_state(QP_nb,QP_nk))
 !
 QP_state=.TRUE.
 !
 call QP_state_table_setup(Xen)
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 !
 file_name(1)='dynamics'
 call of_open_close(file_name(1),'ot') 
 headings(1)="Time"
 headings(2)="E_kin"
 headings(3)="E_ion"
 if(l_rt_exc) then
   headings(4)="E_Hartree"
   headings(5)="E_xc"
 else
   headings(4)="Eo_pot"
   headings(5)="none"
 endif
 headings(6)="N" 
 call msg('o '//file_name(1),'#',headings(1:6),INDENT=-2,USE_TABS=.TRUE.)
 !
 ! Initialize the Gf and potentials
 !
 ! G_lesser(T=0,T=0)=Go and G_lesser_old(T=0,T=0)=Go
 !
 G_lesser=(0._SP,0._SP)
 G_lesser_old =(0._SP,0._SP)
 ! 
 forall(i1=1:SC_bands,ik=1:Xen%nk)
   G_lesser(i1,i1,ik)=(0._SP,1._SP)*Xen%f(i1,ik,1)
 end forall
 !
 G_lesser_old=G_lesser
 !
 ! I set this values in such a way to sum on all transitions
 !
 Xen%nbm=SC_bands
 Xen%nbf=0 
 Xen%f(:,:,:)=spin_occ/2._SP
 !
 ! Set Values in such a way to have transition between all the
 ! bands between 0 and SC_bands
 !
 X%ib(1)=1
 X%ib(2)=SC_bands 
 EF_q0(:)=normalize_v(EF_q0)
 !
 n_poles=X_eh_setup(-1,X,Xen,k,minmax_ehe)
 allocate(X_poles_tab(n_poles,4))
 call mem_est("X_poles_tab",(/size(X_poles_tab)/))
 n_poles=X_eh_setup(1,X,Xen,Xk,minmax_ehe)
 !
 if(l_rt_efield) then
   !      
   file_name(2)='response'
   call of_open_close(file_name(2),'ot') 
   headings(1)='Time'
   headings(2)='Re(Ext_Field)'
   headings(3)='Img(Ext_Field)'
   headings(4)='j_x'
   headings(5)='j_y'
   headings(6)='j_x'
   !
   call msg('o '//file_name(2),'#',headings(1:6),INDENT=-2,USE_TABS=.TRUE.)
   !
   if(RT_resonant.and..not.RT_no_intraband) then
     RT_no_intraband=.true.
     dumb_ch='[RT] Intraband transitions excluded because Resonant Efield !' 
     call live_time_msg("n",dumb_ch,"","%s")
   endif
   !
 endif
 !
 ! Update occupations
 !
 forall(i1=1:SC_bands,ik=1:Xen%nk) 
   Xen%f(i1,ik,1)=-(0._SP,1._SP)*G_lesser(i1,i1,ik)
 end forall
 !
 call el_density(Xen,Xk,rho,G_lesser=G_lesser)
 !
 call DFT_Vxc(Xen,k)
 !
 V_xc_0=V_xc 
 !
 ! Fill the initial Hamiltonian Ho
 ! Ho is the non-interacting kinetic energy 
 ! as defined in SC_driver.F
 !
 Ho=(0.,0.)
 !
 call V_Hartree(rho,V_hartree_0)
 !
 do ik=1,QP_nk
   call V_to_H(ik,-V_hartree_0,Ho(:,:,ik,1),wf)
 enddo
 !
 do i1 = 1, QP_n_states
   !      
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   !
   if (ib==ibp) Ho(ib,ibp,ik,1) = Ho(ib,ibp,ik,1) + en%E(ib,ik,1)
   !
   Ho(ib,ibp,ik,1) = Ho(ib,ibp,ik,1) - QP_Vxc(i1)
   !
   if (ib==ibp) Ho(ib,ibp,ik,1) = real(Ho(ib,ibp,ik,1))
   !
   Ho(ibp,ib,ik,1) = conjg(Ho(ib,ibp,ik,1))
   !
 enddo
 !
 if(RT_phase_int.or..not.l_rt_exc) then 
   !
   Ho_pot=(0._SP,0._SP)
   !
   do ik=1,QP_nk
     call V_to_H(ik,V_hartree_0+V_xc_0,Ho_pot(:,:,ik),wf)
   enddo
   !
 endif
 !
 if(DiagH_Iter/=0) then
   !      
   dumb_ch='[RT] Hamiltonianan Diagonalization '
   call live_time_msg("n",dumb_ch,"","%s")
   call report_energies(Xen%E(:SC_bands,:,:),k,Xk,SC_bands,(/1,Xen%nk/),'E  ',.TRUE.)
   ! 
 endif
 !
 ! Real zero of the time                   
 !
 if(Thermal_steps/=0) call live_timing('[RT] Thermalization :',Thermal_steps)
 !
 do it=1,Thermal_steps+Tot_steps
    !
    RT_time=(it-Thermal_steps)*RT_step
    !
    if(it==Thermal_steps+1) call live_timing('[RT] Iteration :',Tot_steps)
    !
    call live_timing(steps=1)
    !
    if(it==Thermal_steps) call live_timing
    !
    ! Copy G_lesser in G_old before start collisin
    !
    ! Simmetrization
    !
    do i1=1,SC_bands
      do i2=i1+1,SC_bands
        G_lesser_old(i1,i2,:)=G_lesser(i1,i2,:)
        G_lesser_old(i2,i1,:)=-conjg(G_lesser(i1,i2,:))
      enddo
      G_lesser_old(i1,i1,:)=(0._SP,1._SP)*imag(G_lesser(i1,i1,:))
    enddo
    !
    call RT_Collision(G_lesser_old,en,Xen,k,q,RT_time)
    !
    call RT_Energy(Xen,k,E_kin,E_ion,E_h,E_xc,Trace_Gf)
    !
    ! Now print the Energy and other stuff
    !
    values(1)=RT_time/FS2AUT
    values(2)=real(E_kin)
    values(3)=real(E_ion)
    values(4)=real(E_h)
    values(5)=E_xc ! calculated in xc_lda_driver()
    values(6)=real(Trace_Gf)
    call msg('o dynamics','',values(1:6),USE_TABS=.TRUE.)
    !
    if(l_rt_efield) then
      !
      call RT_current(j_current,Xen,xK,A_t,EF_q0,G_lesser_old)
      !
      values(2)=real(A_t)
      values(3)=aimag(A_t)
      values(4:6)=j_current(1:3)
      !
      call msg('o '//file_name(2),'',values(1:6),USE_TABS=.TRUE.)
      !
      if(EpsAnalyze.and.RT_time>0) then
        A_t_store(it-Thermal_steps)=A_t
        j_current_store(it-Thermal_steps,:)=j_current(:)
      endif
      !
    endif
    !
    call RT_Integrator(G_lesser,G_lesser_old,RT_step,en,Xen,k,q,Integrator_name,RT_time)
    !
    if(DiagH_Iter/=0) then
      if(mod(it,DiagH_Iter)==0) then
      !      
      dumb_ch='[RT] Hamiltonianan Diagonalization '
      call live_time_msg("n",dumb_ch,"","%s")
      !
      call msg('nr','[RT] Iterations               :',it)
      call msg('r' ,'[RT] Real Time                :',RT_time/FS2AUT)
      !
      ! Diagonalize H_nl_sc 
      !=====================
      !
      do ik=1,en%nk
        !
        ! Find the natural orbitals
        !
        TmpMatrix=H_t(:,:,ik) 
        call mat_dia_inv(DIAGO,USE_LK,TmpMatrix,E_real=Xen%E(1:SC_bands,ik,1))
        !
        do ib=1,SC_bands
          call cgemv('n',SC_bands,SC_bands,-(0._SP,1._SP),G_lesser(:,:,ik),SC_bands,TmpMatrix(:,ib),1,(0._SP,0._SP),V_tmp1,1)
#if defined _DOUBLE
          Occ_Natural(ib,ik)=real(zdotc(SC_bands,TmpMatrix(:,ib),1,V_tmp1,1))
#else 
          Occ_Natural(ib,ik)=real(cdotc(SC_bands,TmpMatrix(:,ib),1,V_tmp1,1))
#endif   
        enddo
        !
      enddo
      !
      call fermi_level(Xen,k,Tel,0)
      !
      call report_energies(Xen%E(:SC_bands,:,:)-Xen%Efermi(1),k,Xk,SC_bands,(/1,Xen%nk/),'E  ',.FALSE.)
      !
      ! Now write the occupation
      !
      do ik=1,Xen%nk
        do i1=1,SC_bands,8 
          !      
          call msg('r','n   ',(/(Occ_Natural(ib,ik),ib=i1,min(i1+7,SC_bands))/))
          !
        enddo
      enddo
      !
      endif
    endif
    !
 enddo
 !
 call of_open_close(file_name(1))
 if(l_rt_efield) call of_open_close(file_name(2))
 !
 if(DiagH_Iter/=0) call of_open_close(qp_fname)
 !
 if(EpsAnalyze) then
   call RT_eps_analyze(j_current_store,A_t_store)
   deallocate(j_current_store)
   call mem_est("J_current")
   deallocate(A_t_store)
   call mem_est("A_t")
 endif
 !
 call RT_free()
 !
 call X_alloc("P_square")
 !
 deallocate(QP_state)
 call mem_est("QP_state")
 !  
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
 deallocate(X_poles_tab)
 call mem_est("X_poles_tab")
 !
end subroutine RT_driver
