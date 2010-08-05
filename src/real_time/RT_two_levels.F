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
subroutine RT_two_levels(G_lesser)
 !
 use pars,           ONLY:SP,pi,cI
 use units,          ONLY:FS2AUT
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use D_lattice,      ONLY:DL_vol
 use units,          ONLY:HARTREE
 use com,            ONLY:msg
 use real_time,      ONLY:Wo,Rabi_freq_pump,W_pump,Phase_LifeTime,&
&                         QP_LifeTime,RT_step,l_NE_with_fields,i_time,P,D,delta,Rabi_nn,Rabi_freq_probe
 use fields,         ONLY:Efield
 !
 implicit none           
 complex(SP),       intent(in) :: G_lesser(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer       :: i_f
 logical       :: P_and_P
 real(SP)      :: T,W_step,Wo_here,W_range
 integer, parameter :: N_W_delta_search=100000
 complex(SP)        :: Poles_func(N_W_delta_search),Poles_func_deriv(N_W_delta_search),W_func(N_W_delta_search),&
&                      G(2,2),Gamma_mat(2,2),Gamma_delta_11
 complex(SP), save  :: W_bar,Beta_wbar,R_wbar,dGinfty,W_gamma,Beta_gamma,R_gamma,&
&                      So,S_wo,S_wbar,S_minus_wbar_star,S_gamma,eta1_plus,eta1_minus,p1_plus,p1_minus,p0
 !
 if(.not.l_NE_with_fields.or.i_time<0) return
 !
 P_and_P=Efield(2)%ef_name/="none"
 !
 select case(trim(Efield(1)%ef_name))
   case('RES')
     Wo_here=Wo+W_pump
   case('ANTIRES')
     Wo_here=Wo-W_pump
   case('DELTA')
     Wo_here=Wo
 end select
 !
 T= (i_time-1)*RT_step
 !
 W_range=10./HARTREE
 if (P_and_P) W_range=2./HARTREE
 !
 if (i_time == 1) then
   W_step=W_range/real(N_W_delta_search)
   do i_f=1,N_W_delta_search-1
     W_func(i_f)=-0.1/HARTREE+W_step*i_f
     if (P_and_P) then
       Poles_func(i_f)=DELTA_Pump_F(-W_func(i_f))
       Poles_func_deriv(i_f)=(DELTA_Pump_F(-W_func(i_f)+W_step)-DELTA_Pump_F(-W_func(i_f)))/W_step
       write (100,'(3f20.10)')  real(W_func(i_f))*HARTREE,DELTA_Pump_F(-W_func(i_f))
     else
       Poles_func(i_f)=DELTA_Pump_F(W_func(i_f))
       Poles_func_deriv(i_f)=(DELTA_Pump_F(W_func(i_f)+W_step)-DELTA_Pump_F(W_func(i_f)))/W_step
       write (100,'(3f20.10)')  real(W_func(i_f))*HARTREE,DELTA_Pump_F(W_func(i_f))
     endif
     if (i_f==1) cycle
     if ( (real(Poles_func(i_f-1))<0..and.real(Poles_func(i_f))>0. ) ) then
       W_bar = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_wbar= Poles_func_deriv(i_f-1)
     endif
     if ( (real(Poles_func(i_f-1))>0..and.real(Poles_func(i_f))<0. ) ) then
       W_bar = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_wbar= Poles_func_deriv(i_f-1)
     endif
     if ( abs(real(W_func(i_f-1)))<epsilon(1._SP) ) then
       W_gamma = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_gamma= Poles_func_deriv(i_f-1)
     endif
   enddo
   !
   call msg('s','[2 LEVELS] W bar           [eV   ]',real(W_bar)*HARTREE)
   !
 endif
 stop
 !
 Gamma_mat(1,1)=abs(Rabi_freq_pump)**2/(Wo_here**2.+1./Phase_LifeTime**2.)*QP_LifeTime/Phase_LifeTime
 dGinfty=-2.*cI/(Gamma_mat(1,1)+1.)
 ! 
 R_wbar=2*abs(Rabi_freq_pump)**2/W_bar/Beta_wbar* ( W_bar+cI/Phase_LifeTime )
 R_gamma=2*abs(Rabi_freq_pump)**2/W_gamma/Beta_gamma*( W_gamma+cI/Phase_LifeTime )
 !
 So  =cI*dGinfty/(Wo_here+cI/Phase_LifeTime)
 S_wo=R_wbar/(W_bar+Wo_here+cI/Phase_LifeTime)
 S_wbar=R_wbar/(W_bar+Wo_here+cI/Phase_LifeTime)
 S_minus_wbar_star=conjg(R_wbar)/(-conjg(W_bar)+Wo_here+cI/Phase_LifeTime)
 S_gamma=R_gamma/(Wo_here+W_gamma+cI/Phase_LifeTime)
 !
 G(1,2)=exp_by_sigma_12(T)-exp_by_sigma_12(0.)*exp(cI*Wo_here*T-T/Phase_LifeTime)
 select case(trim(Efield(1)%ef_name))
   case('RES')
     G(1,2)=G(1,2)*exp(-cI*W_pump*T)
   case('ANTIRES')
     G(1,2)=G(1,2)*exp( cI*W_pump*T)
 end select
 G(2,1)=-conjg(G(1,2))
 !
 ! P&P
 !
 if (.not.P_and_P) goto 1001
 !
 Gamma_mat(1,2)=Rabi_nn(1,2)/( Wo-W_pump-delta+cI/Phase_LifeTime )/( Wo-W_pump-cI/Phase_LifeTime )*QP_LifeTime/Phase_LifeTime
 Gamma_mat(2,1)=conjg( Gamma_mat(1,2) )
 Gamma_mat(2,2)=Rabi_nn(2,2)/((Wo-W_pump-delta)**2.+1./Phase_LifeTime**2.)*QP_LifeTime/Phase_LifeTime
 Gamma_delta_11=abs(Rabi_freq_pump)**2/((Wo-W_pump)**2.-(delta-cI/Phase_LifeTime)**2.)*QP_LifeTime/Phase_LifeTime
 !
 eta1_plus=cI*Gamma_mat(1,2)*(delta*Phase_LifeTime-2.*cI)/(delta*QP_LifeTime-cI)
 eta1_minus=-conjg(eta1_plus)
 call open_it(101,"dG_theo_LLR")
 call write_it(101,-cI*(eta1_plus*exp(cI*delta*T)+eta1_minus*exp(-cI*delta*T)-2.*cI*(1.-Gamma_mat(1,1)-Gamma_mat(2,2))))
 call open_it(102,"G12_theo_LLR")
 call write_it(102,G12_LRR(T,Rabi_freq_pump,W_pump)+G12_LRR(T,Rabi_freq_probe,W_pump+delta))
 !
 eta1_plus=-dGinfty/2.*Gamma_mat(1,2)*(delta*Phase_LifeTime-2.*cI)/(delta*QP_LifeTime-cI-Gamma_delta_11*(delta*QP_LifeTime-cI))
 eta1_minus=-conjg(eta1_plus)
 call open_it(103,"dG_theo")
 call write_it(103,-cI*(eta1_plus*exp(cI*delta*T)+eta1_minus*exp(-cI*delta*T)+dGinfty))
 !
 p1_plus= (eta1_plus*Rabi_freq_pump/2.+dGinfty*Rabi_freq_probe)/2./(Wo+cI/Phase_LifeTime-W_pump-delta)
 p1_minus=eta1_minus*Rabi_freq_pump/2./(Wo+cI/Phase_LifeTime-W_pump+delta)
 p0= dGinfty*Rabi_freq_pump/2./(Wo+cI/Phase_LifeTime-W_pump)
 call open_it(104,"G12_theo")
 call write_it(104,(p0+p1_plus*exp(cI*delta*T)+p1_minus*exp(-cI*delta*T))*exp(cI*W_pump*T))
 !
 goto 1002
 !
1001 continue
 ! 
 call open_it(101,"dG_theo")
 call write_it(101,-cI*(-2.*cI*real(R_wbar*exp(-cI*W_bar*T))+dGinfty-cI*R_gamma*exp(-cI*W_gamma*T) ))
 call open_it(102,"G12_theo")
 call write_it(102,G(1,2))
 call open_it(103,"Polarization_theo")
 call write_it(103, -cI*(D*G(2,1)+conjg(D)*G(1,2))/DL_vol)
 !
1002 continue
 !
 call open_it(201,"dG_num")
 call write_it(201,-cI*(G_lesser(2,2,1)-G_lesser(1,1,1)))
 call open_it(202,"Polarization_num")
 call write_it(202, -cI*(D*G_lesser(2,1,1)+conjg(D)*G_lesser(1,2,1))/DL_vol)
! call open_it(203,"Current_num")
! call write_it(203, -cI*(-1)*(P*G_lesser(2,1,1)+conjg(P)*G_lesser(1,2,1))/DL_vol)
 call open_it(205,"G12_num")
 call write_it(205,G_lesser(1,2,1))
 !
 contains
   !
   subroutine write_it(unit_,what)
     use units,       ONLY:FS2AUT
     use pars,        ONLY:pi
     integer     ::unit_
     complex(SP) ::what
     write (unit_,'(10g20.10)') (i_time-1)*RT_step/FS2AUT,what
   end subroutine
   !
   subroutine open_it(unit_,where_)
     use IO_m,      ONLY:rename_file,cp_file
     use units,     ONLY:FS2AUT
     use stderr,    ONLY:intc,real2ch,string_remove
     use real_time, ONLY:NE_steps,QP_LifeTime
     integer      ::unit_,ierr,i1
     character(*)   ::where_
     character(200) ::file_name,local
     if (i_time/=1) return
!     write (file_name,'(a,a,f10.5,a,f3.1,2(a,f5.2),a)')  where_,"_Rabi_",abs(Rabi_freq)*HARTREE,"_Wph_",W_photon*HARTREE,&
!&       "_Lrad_",QP_LifeTime/FS2AUT,"_Lphase_",Phase_LifeTime/FS2AUT,".dat"
     file_name=trim(where_)
     do  i1=1,100
       local = trim(string_remove(file_name,' '))
       file_name=local
     enddo
!     if (P_and_P) local="PP_"//trim(file_name)
     file_name=local
     open (unit=unit_,file=trim(file_name),form='formatted')
   end subroutine
   !
   complex(SP) function DELTA_Pump_F(W)
    complex(SP) :: W
    DELTA_Pump_F=(W+cI/QP_LifeTime)*(W+Wo_here+cI/Phase_LifeTime)*(W-Wo_here+cI/Phase_LifeTime)-(W+cI/Phase_LifeTime)*abs(Rabi_freq_pump)**2.
   end function
   !
   complex(SP) function exp_by_sigma_12(Time)
    real(SP)    :: Time
    exp_by_sigma_12=-cI*Rabi_freq_pump/2.*(&
&       So&
&      +S_gamma*exp( -cI*W_gamma*Time)&
&      +S_wbar*exp( -cI*W_bar*Time)&
&      +S_minus_wbar_star*exp( cI*conjg(W_bar)*Time)&
&       )
   end function
   !
   complex(SP) function G12_LRR(Time,Rabi,W)
    real(SP)    :: Time,W
    complex(SP) :: Rabi
    G12_LRR=-cI*Rabi/(Wo+cI/Phase_LifeTime-W)*exp(cI*W*Time)
   end function
   !
end subroutine RT_two_levels
