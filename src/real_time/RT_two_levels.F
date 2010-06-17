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
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT,FS2AUT
 use SC,             ONLY:SC_bands
! use real_time,      ONLY:l_RT_no_diamagnetic,i_time,l_NE_with_fields,j_current, &
!&                         gauge_field,RT_average_operator,RT_step
 use QP_m,           ONLY:QP_nk
 use units,          ONLY:HARTREE
 use pars,           ONLY:cI
 use real_time,      ONLY:Wo,Rabi_freq,W_photon,Phase_LifeTime,RT_is_dumped,P,W_detuning,&
&                         QP_LifeTime,RT_step,l_NE_with_fields,i_time
 !
 implicit none           
 complex(SP),       intent(in) :: G_lesser(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer       :: i_r
 real(SP)      :: T,Rabi_test,Gamma_,delta_G_SS,Current_SS
 !
 ! j(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 !
 ! j_current(t) = int dr j(r,t)
 !
 if(.not.l_NE_with_fields.or.i_time<0) return
 !
 T= (i_time-1)*RT_step
 !
 Gamma_= abs(Rabi_freq)**2/(W_Detuning**2.+1./Phase_LifeTime**2.)*QP_LifeTime/Phase_LifeTime
 !
 if (i_time == 1) then
   open (unit=300,name='Weights_RES.dat',form='formatted')
   open (unit=301,name='Weights_DELTA.dat',form='formatted')
   do i_r=1,1000
     Rabi_test=0.+i_r*2./1000/HARTREE
     Gamma_= abs(Rabi_test)**2/(W_Detuning**2.+1./Phase_LifeTime**2.)*QP_LifeTime/Phase_LifeTime
     delta_G_SS=-2./(Gamma_+1.)
     Current_SS=2.*abs(Rabi_test/(Wo+W_photon+cI/Phase_LifeTime)/(Gamma_+1.))
     write (300,*) Rabi_Test*HARTREE,delta_G_SS,Current_SS
     Current_SS=2.*abs(Rabi_test/(W_photon+cI/Phase_LifeTime)/(Gamma_+1.))
     write (301,*) Rabi_Test*HARTREE,delta_G_SS,Current_SS
   enddo
 endif
 !
 call open_it(103,"G12_delta")
 call open_it(104,"G12_res")
 call open_it(109,"G12_antires")
 if (Phase_LifeTime>1.E-5) then
    call write_it(103, -cI*Rabi_freq/(Wo+cI/Phase_LifeTime)*( 1- exp( cI*Wo*T -T/Phase_LifeTime ) ) )
    call write_it(104, Rabi_freq/(Wo+W_photon+cI/Phase_LifeTime)*( exp(-cI*W_photon*T)- exp(cI*Wo*T-T/Phase_LifeTime) ) ) 
    call write_it(109,-Rabi_freq/(Wo-W_photon+cI/Phase_LifeTime)*( exp( cI*W_photon*T)- exp(cI*Wo*T-T/Phase_LifeTime) ) ) 
 else
   call write_it(103, -cI*Rabi_freq/Wo*( 1- exp( cI*Wo*T ) ) )
   call write_it(104, Rabi_freq/(Wo+W_photon)*( exp(-cI*W_photon*T)- exp(cI*Wo*T) ) ) 
   call write_it(109,-Rabi_freq/(Wo-W_photon)*( exp( cI*W_photon*T)- exp(cI*Wo*T) ) ) 
 endif
 !
 call open_it(105,"G22")
 call write_it(105,-cI*G_lesser(2,2,1))
 call open_it(106,"G11")
 call write_it(106,-cI*G_lesser(1,1,1))
 call open_it(107,"G22+G11")
 call write_it(107,-cI*(G_lesser(1,1,1)+G_lesser(2,2,1)))
 call open_it(108,"G22-G11")
 call write_it(108,-cI*(G_lesser(2,2,1)-G_lesser(1,1,1)))
 call open_it(100,"G21")
 call write_it(100,G_lesser(2,1,1))
 call open_it(101,"G12")
 call write_it(101,G_lesser(1,2,1))
 !
 call open_it(110,"Current")
 call write_it(110, -cI*(P*G_lesser(2,1,1)+conjg(P)*G_lesser(1,2,1)))
 !
 contains
   !
   subroutine write_it(unit_,what)
     use units,       ONLY:FS2AUT
     use real_time,   ONLY:Wo
     use pars,        ONLY:pi
     integer     ::unit_
     complex(SP) ::what
     write (unit_,'(10g20.10)') (i_time-1)*RT_step/FS2AUT,what
     !write (unit_,'(10g20.10)') (i_time-1)*RT_step/pi*W_photon,what
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
     write (file_name,'(a,a,f10.5,a,f3.1,2(a,f5.2),a)')  where_,"_Rabi_",abs(Rabi_freq)*HARTREE,"_Wph_",W_photon*HARTREE,&
&       "_Lrad_",QP_LifeTime/FS2AUT,"_Lphase_",Phase_LifeTime/FS2AUT,".dat"
     do  i1=1,100
       local = trim(string_remove(file_name,' '))
       file_name=local
     enddo
     open (unit=unit_,name=trim(file_name),form='formatted')
   end subroutine
   !
end subroutine RT_two_levels
