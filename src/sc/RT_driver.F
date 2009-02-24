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
subroutine RT_driver(en,Xen,X,k,xk)
 !
 use pars,           ONLY:SP,lchlen,schlen
 use units,          ONLY:HARTREE,FS2AUT
 use com,            ONLY:msg,of_open_close
 use vec_operate,    ONLY:normalize_v
 use stderr,         ONLY:intc,real2ch
 use timing,         ONLY:live_time_msg
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels,el_density 
 use X_m,            ONLY:X_t,X_alloc,X_rhoq0,X_poles,X_poles_tab
 use wave_func,      ONLY:wf,wf_load,wf_ng
 use FFT_m,          ONLY:fft_size
 use R_lattice,      ONLY:bz_samp
 use parser_m,       ONLY:parser
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_table,QP_n_states,QP_Vxc
 use SC,             ONLY:SC_potential,SC_bands,SC_fft_size,V_hartree_sc
 use real_time,      ONLY:EF_q0,Thermal_steps,Tot_steps,Ext_Field_name,RT_step,G_lesser, &
&                         Go_Phases,Ef_rhoq0,G_l_old,l_rt_efield,n_poles,Ext_Field_Int,  &
&                         RT_alloc,RT_free,rho,Thermal_eta,Integrator_name,l_rt_exc,     &
&                         RT_no_intraband,RT_resonant,V_hartree_0,V_xc_0,Thermal_time,   &
&                         RT_no_phase_int,Ho,E_bare

 use xc_functionals, ONLY:V_xc

 implicit none
 type(levels)  :: en,Xen
 type(bz_samp) :: k,Xk
 type(X_t):: X(4)
 ! 
 ! Work Space
 !
 logical, external        :: XC_switch
 integer                  :: it,idt,i1,i2,ib,ibp
 character(schlen)        :: dumb_ch,energy_ch,Int_Type
 character(lchlen)        :: file_name
 integer                  :: WF_G_max,WF_Go_indx
 integer                  :: i_pole,iv,ic,ik,i_sp
 integer                  :: X_eh_setup
 real(SP)                 :: minmax_ehe(2)
 real(SP)                 :: values(4)
 real(SP)                 :: RT_time  ! The real time on the diagonal RTime = it*RT_step   
 complex(SP)              :: Trace_Gf
 complex(SP)              :: Ko,Vh
 !
 call parser('RTResonant',RT_resonant)
 call parser('RTNoIntraband',RT_no_intraband)
 call parser('RTNoPhaseInt',RT_no_phase_int)
 !
 WF_G_max=wf_ng
 WF_Go_indx=1  
 SC_fft_size=fft_size
 !
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),title=' ')  
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 if(Thermal_eta/=0.0) call section('*','Adiabatic Switching: '//trim(intc(int(Thermal_eta/RT_step))))
 !
 l_rt_efield=.false.
 if(Ext_Field_Name/='NONE'.and.Ext_Field_Int/=0.0) l_rt_efield=.true.
 !
 ! Allocation quasi-particle indices
 !
 QP_nb=SC_bands
 QP_nk=k%nibz
 !
 if (allocated(QP_state)) deallocate(QP_state)
 allocate(QP_state(QP_nb,QP_nk))
 !
 QP_state=.TRUE.
 !
 call QP_state_table_setup(en)
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 !
 l_rt_exc=XC_switch()
 !
 call RT_alloc(en)
 !
 ! Reference energies
 !
 E_bare=en%E(:SC_bands,:,1)
 !
 file_name='dynamics'
 call of_open_close(file_name,'ot') 
 energy_ch='#Time            Ekin        E_Hartree    N' 
 call msg('o dynamics',energy_ch)
 !
 ! Initialize the Gf and potentials
 !
 if(.not.RT_no_phase_int) then
   !
   ! Go_Phases(n,k)= exp(- i \Delta T * E_{n,k} )
   !
   forall(i1=1:SC_bands,ik=1:en%nk)
     Go_Phases(i1,ik)=exp(-(0._SP,1._SP)*RT_step*E_bare(i1,ik))
   end forall
   !
 endif
 !
 ! G_lesser(T=0,T=0)=Go and G_l_old(T=0,T=0)=Go
 !
 G_lesser=(0._SP,0._SP)
 G_l_old =(0._SP,0._SP)
 ! 
 forall(i1=1:SC_bands,ik=1:en%nk)
      G_lesser(i1,i1,ik)=(0._SP,1._SP)*en%f(i1,ik,1)
 end forall
 !
 G_l_old=G_lesser
 !
 ! I set this values in such a way to sum on all transitions
 !
 en%nbm=SC_bands
 en%nbf=0 
 en%f(:,:,:)=1.0_SP
 !
 ! Set Values in such a way to have transition between all the
 ! bands between 0 and SC_bands
 !
 X%ib(1)=1
 X%ib(2)=SC_bands 
 EF_q0(:)=normalize_v(EF_q0)
 !
 n_poles=X_eh_setup(-1,X,en,k,minmax_ehe)
 allocate(X_poles_tab(n_poles,4))
 call mem_est("X_poles_tab",(/size(X_poles_tab)/))
 n_poles=X_eh_setup(1,X,en,Xk,minmax_ehe)
 !
 if(l_rt_efield) then
   !      
   call X_O_strengths(en, k, X, EF_q0)
   !
   Ef_rhoq0=(1._SP,0._SP)
   !
   if(RT_resonant.and..not.RT_no_intraband) then
     RT_no_intraband=.true.
     dumb_ch='[RT] Intraband transitions excluded because Resonant Efield !' 
     call live_time_msg("n",dumb_ch,"","%s")
   endif
   !
   do i_pole=1,n_poles
      i1=X_poles_tab(i_pole,1)
      iv=X_poles_tab(i_pole,2)
      ic=X_poles_tab(i_pole,3)
      i_sp=1
      ik=k%sstar(i1,1)
      Ef_rhoq0(iv,ic,ik)=X_rhoq0(iv,ic,ik,i_sp)
      Ef_rhoq0(ic,iv,ik)=conjg(X_rhoq0(iv,ic,ik,i_sp))
   enddo
   !
   ! Remove intraband elements if NoIntraband=.false.
   !
   if(RT_no_intraband) then
     forall(i1=1:SC_Bands,ik=1:en%nk) &
&      Ef_rhoq0(i1,i1,ik)=(0._SP,0._SP)
   endif
   !
   call X_alloc('OptOsc')
   !
 endif
 !
 ! Update occupations
 !
 forall(i1=1:SC_bands,ik=1:en%nk) 
   en%f(i1,ik,1)=-(0._SP,1._SP)*G_lesser(i1,i1,ik)
 end forall
 !
 call el_density(en,Xk,rho,G_lesser=G_lesser)
 call DFT_Vxc(en,k)
 V_xc_0=V_xc 
 !
 ! Fill the initial Hamiltonian Ho
 !
 call V_Hartree(rho,V_hartree_0)
 do ik=1,QP_nk
   call V_to_H(ik,-V_hartree_0,Ho(:,:,ik),wf)
 enddo
 !
 ! Ho is the non-interacting kinetic energy 
 !    as defined in SC_driver.F
 !
 Ho(:,:,:)=(0._SP,0._SP)
 !
 do i1 = 1, QP_n_states
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   !
   if (ib==ibp) Ho(ib,ibp,ik) = Ho(ib,ibp,ik) + E_bare(ib,ik)
   !
   Ho(ib,ibp,ik) = Ho(ib,ibp,ik) - QP_Vxc(i1)
   !
   if (ib==ibp) Ho(ib,ibp,ik) = real(Ho(ib,ibp,ik))
   !
   Ho(ibp,ib,ik) = Ho(ib,ibp,ik) !????????????????
   !
 enddo
 !
 ! Real zero of the time                   
 !
 Thermal_time=real(Thermal_steps)*RT_step
 !
 do it=1,Thermal_steps+Tot_steps
    !
    RT_time=(it-Thermal_steps)*RT_step
    !
    if(RT_time>0._SP) then
      !      
      dumb_ch='[RT] Iteration '//trim(intc(it-Thermal_steps))
      !
    else
      !      
      dumb_ch='[RT] Thermalization step '//trim(intc(it))
      !
    endif
    !
    call live_time_msg("n",dumb_ch,"","%s")
    !
    ! Simmetrization
    !
!    do i1=1,SC_bands
!      do i2=i1+1,SC_bands
!        G_l_old(i1,i2,:)=G_lesser(i1,i2,:)
!        G_l_old(i2,i1,:)=-conjg(G_lesser(i1,i2,:))
!      enddo
!      G_l_old(i1,i1,:)=(0._SP,1._SP)*imag(G_lesser(i1,i1,:))
!   enddo
    !
    call RT_Collision(G_l_old,en,k,RT_time)
    !
    call RT_Integrator(G_lesser,G_l_old,RT_step,en,k,Integrator_name,RT_time)
    !
    call Eval_Energy(en,k,Ko,Vh,Trace_Gf)
    ! 
    ! Now print the Energy and other stuff
    !
    values(1)=RT_time/FS2AUT
    values(2)=real(Ko)
    values(3)=real(Vh)
    values(4)=real(Trace_Gf)
    call msg('o dynamics','',values(:),INDENT=-2,USE_TABS=.TRUE.)
    !
 enddo
 !
 call of_open_close(file_name)
 !
 call RT_free()
 !
end subroutine RT_driver
