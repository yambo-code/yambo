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
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HARTREE
 use vec_operate,    ONLY:normalize_v
 use stderr,         ONLY:intc,real2ch
 use timing,         ONLY:live_time_msg
 use memory_m,       ONLY:mem_est
 use drivers,        ONLY:l_sc_exx,l_sc_chosex,l_sc_hartree,l_sc_pz,l_sc_lda_x,&
&                         l_sc_hf,l_sc_exxc
 use electrons,      ONLY:levels,el_density 
 use X_m,            ONLY:X_t,X_alloc,X_rhoq0,X_poles,X_poles_tab
 use wave_func,      ONLY:wf,wf_load,wf_ng
 use FFT_m,          ONLY:fft_size
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_state,QP_nb,QP_nk,QP_table,QP_n_states
 use SC,             ONLY:SC_potential,SC_bands,V_xc_sc,SC_fft_size,V_hartree_sc
 use real_time,      ONLY:EF_q0,Thermal_steps,Tot_steps,Ext_Field_name,RTime,& 
&                         RT_step,Gf_diagonal,n_steps_mem,Ekin,Ec,G_lesser, &
&                         Trace_Gf,Go_Phases,Ef_rhoq0,G_l_old,l_rt_efield,&
&                         n_poles,Ext_Field_Int,l_rt_efield,RT_alloc,RT_free, &
&                         Sigma_s,rho,Thermal_eta

 implicit none
 type(levels)  :: en,Xen
 type(bz_samp) :: k,Xk
 type(X_t):: X(4)
 ! 
 ! Work Space
 !
 logical, external        :: XC_switch
 integer                  :: it,idt,i1
 character(schlen)        :: dumb_ch,energy_ch,Int_Type
 integer                  :: WF_G_max,WF_Go_indx
 integer                  :: i_pole,iv,ic,ik,i_sp
 integer                  :: X_eh_setup
 real(SP)                 :: minmax_ehe(2)
 real(SP)                 :: Time_0
 !
 ! Switch to the user defined potential (if appropriate)
 !
 !     
 WF_G_max=wf_ng
 WF_Go_indx=1  
 !
 call wf_load(WF_G_max,WF_Go_indx,(/1,SC_bands/),(/1,k%nibz/),title=' ')  
 call section('*',trim(SC_potential)//' Real-Time Dynamics')
 if(Thermal_eta/=0.0) call section('*',' Adiabatic Switching')
 !
 Gf_diagonal=.false.
 if(n_steps_mem==1.or..not.l_sc_chosex) Gf_diagonal=.true.
 !
 l_rt_efield=.false.
 if(Ext_Field_Name/='NONE'.and.Ext_Field_Int/=0.0) l_rt_efield=.true.
 !
 QP_nb=SC_bands
 QP_nk=k%nibz
 !
 if (allocated(QP_state)) deallocate(QP_state)
 allocate(QP_state(QP_nb,QP_nk))
 QP_state=.TRUE.
 !
 call QP_state_table_setup(en)
 !
 call RT_alloc(en)
 !
 ! I set this values in such a way to sum on all transitions
 !
 en%nbm=SC_bands
 en%nbf=0 
 !
 if(l_rt_efield) then 
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
   n_poles=X_eh_setup(1,X,Xen,Xk,minmax_ehe)
   call X_O_strengths(en, k, X, EF_q0)
   !
   Ef_rhoq0=(1._SP,0._SP)
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
   call X_alloc('OptOsc')
   !
 endif
 !
 ! Initialize the Gf and potentials
 !
 ! Go_Phases(n,k)= exp(- i \Delta T * E_{n,k} )
 !
 do ik=1,en%nk
   Go_Phases(1:SC_bands,ik)=exp(-(0._SP,1._SP)*RT_step*en%E(1:SC_bands,ik,1))
 enddo
 !
 ! G_lesser(T=0,T=0)=Go and G_l_old(T=0,T=0)=Go
 !
 G_lesser=(0._SP,0._SP)
 G_l_old =(0._SP,0._SP)
 !
 do iv=1,SC_bands
      G_lesser(iv,iv,:)=(0._SP,1._SP)*en%f(iv,:,1)
 enddo
 !
 G_l_old=G_lesser
 !
 call Eval_Energy(en,k)
 !
 forall(iv=1:SC_bands,ik=1:en%nk) &
&   en%f(iv,ik,1)=-(0._SP,1._SP)*G_lesser(iv,iv,ik)
 !
 call el_density(en,Xk,rho,G_lesser=G_lesser)
 !
 ! Real zero of the time
 !
 Time_0=-Thermal_steps*RT_step
 !
 do it=0,Thermal_steps+Tot_steps
    !
    RTime=(it-Thermal_steps)*RT_step
    !
    if(it>Thermal_steps) then
      !      
      dumb_ch='[RT] Iteration '//trim(intc(it-Thermal_steps))
      !
    else
      !      
      dumb_ch='[RT] Thermalization step '//trim(intc(it))
      !
    endif
    !
    ! Just print the Energy
    !
    call live_time_msg("n",dumb_ch,"","%s")
    energy_ch='[RT] Ekin '//trim(real2ch(real(Ekin)))//'  Ec '//trim(real2ch(real(Ec)))// &
&             '  Etot '//trim(real2ch(real(Ec+Ekin))) 
    call live_time_msg("n",energy_ch,"","%s")
    !
    ! Check if the number of particles is conserved  n_el=Tr(G_lesser(T,T))
    !
    Trace_Gf=(0._SP,0._SP)
    do i1=1,SC_bands
      Trace_Gf=Trace_Gf-(0._SP,1._SP)*sum(G_lesser(i1,i1,:)*k%weights(:))
    enddo
    !
    energy_ch='[RT] Check Trace of G_lesser: '//trim(real2ch(real(Trace_Gf)))
    call live_time_msg("n",energy_ch,"","%s")
    !
    if(l_sc_hartree) call V_Hartree(rho,V_hartree_sc)
    !
    do ik=1,k%nibz
      !
      Sigma_s(:,:)=(0._SP,0._SP)
      !
      if(l_rt_efield.and.it>Thermal_steps)  call Ext_Field(ik,Sigma_s)
      !
      if(l_sc_hartree) call V_to_H(ik,V_hartree_sc,Sigma_s,wf)
      !
      ! Adiabatic Switching
      !
      if(Thermal_eta/=0.0) &
&       Sigma_s=Sigma_s*(1._SP-exp(-(RTime-Time_0)*Thermal_eta))
      !
      !
      ! Prepare I1_lesser and I2_lesser
      !
      call RT_Collision(ik,it,en)
      !
      Int_Type='diagonal'
      call RT_Integrator(ik,it,en,Int_Type)
      !
    enddo
    !
    ! Update occupations
    !
    forall(iv=1:SC_bands,ik=1:en%nk) &
&      en%f(iv,ik,1)=-(0._SP,1._SP)*G_lesser(iv,iv,ik)
    !
    call el_density(en,Xk,rho,G_lesser=G_lesser)
    !
    call Eval_Energy(en,k)
    !
 enddo
 !
 call RT_free()
 !
end subroutine RT_driver
