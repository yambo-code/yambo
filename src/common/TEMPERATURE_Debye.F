!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
subroutine TEMPERATURE_Debye(delta_E)
 !
 use pars,       ONLY:SP,pi
 use units,      ONLY:HA2EV
 use D_lattice,  ONLY:n_atoms,Bose_Temp
 use com,        ONLY:error
 use ELPH,       ONLY:W_debye
 !
 implicit none
 !
 real(SP), intent(in) :: delta_E              ! Delta_E is in Hartree
 ! 
 ! Paramenters
 integer,  parameter :: n_max=20000           ! Max steps
 real(SP), parameter :: T_step=0.05_SP        ! 0.05 K each T_step --> 1000 K range for C_v
 real(SP), parameter :: R_const=8.617E-5_SP   ! [eV/(K*atom)], this is actually the Boltzmann constant
 !
 ! Functions
 real(SP) :: Cv_Debye(n_max)
 real(SP) :: f_int(n_max)
 real(SP) :: f_x_Ti
 !
 ! Variables
 real(SP) :: T_in, T_out
 real(SP) :: delta_E_int
 real(SP) :: T_D, Ti
 real(SP) :: xi(n_max), x_max, x_step, x_Ti
 real(SP) :: m
 integer  :: i1,i2,i_max,i_shift
 !
 ! pol_fit
 integer, parameter :: fit_dim=5
 integer            :: order=3
 real(SP)           :: X(fit_dim),Y(fit_dim),coeff(4),eps=0._SP
 !
 ! Initialization
 Cv_Debye=0._SP
 f_int=0._SP
 T_D=W_debye*HA2EV/R_const
 !
 !write(*,*) T_D,W_debye*HA2EV/R_const
 !
 ! Temperature in Kelvin here
 T_in=Bose_temp*HA2EV/R_const
 !
 if(T_in>=1._SP)  x_max=T_D/T_in
 if(T_in< 1._SP)  x_max=T_D/1._SP
 x_step=x_max/real(n_max-2,SP)
 !
 ! f_int needed for the Debye function
 !write(*,*) '  '
 !write(*,*) '#F(x) Debye'
 do i1=2,n_max
   xi(i1)=x_step*real(i1-1,SP)
 enddo
 do i1=2,n_max
   f_int(i1)=f_int(i1-1)+xi(i1)**4._SP*exp(xi(i1))/(exp(xi(i1))-1)**2._SP*x_step
   if(f_int(i1)==f_int(i1-1)) exit
 enddo
 if(i1<n_max) f_int(i1+1:)=f_int(i1)
 !
 !do i1=1,n_max
 !  write(*,*) xi(i1),f_int(i1) !,xi(i1)**3._SP/3._SP,4._SP*pi**4._SP/15._SP
 !enddo
 !
 ! The Debye Cv function
 !write(*,*) '  '
 !write(*,*) '#Cv(T) Debye'
 i_max=n_max
 do i1=1,n_max-2
   Ti=T_in+real(i1-1,SP)*T_step
   if(Ti==0._SP) cycle
   x_Ti=T_D/Ti
   do i2=i_max,1,-1
     if(xi(i2)<x_Ti) exit 
   enddo 
   i_max=i2
   if(i2>(fit_dim-5)) i_shift=(fit_dim-5)
   if(i2>(fit_dim-4)) i_shift=(fit_dim-4)
   if(i2>(fit_dim-3)) i_shift=(fit_dim-3)
   if(i2>(n_max-2))   i_shift=(fit_dim-2)
   if(i2>(n_max-1))   i_shift=(fit_dim-1)
   X=xi(i2-i_shift:i2-i_shift+4)
   Y=f_int(i2-i_shift:i2-i_shift+4)
   coeff=0._SP
   call pol_fit(fit_dim,X,Y,order,coeff,eps,0.d0)
   f_x_Ti=coeff(1)+coeff(2)*x_Ti+coeff(3)*x_Ti**2._SP+coeff(4)*x_Ti**3._SP
   Cv_Debye(i1)=9._SP*R_const*(Ti/T_D)**3._SP*f_x_Ti/HA2EV
   !write(*,*) Ti,Cv_Debye(i1) !, &
   !&             9._SP*R_const*(Ti/T_D)**3._SP*(4._SP*pi**4._SP/15._SP)/HA2EV,&
   !&             3._SP*R_const/HA2EV
 enddo
 !
 ! The integral to obtain T_out
 !write(*,*) '  '
 !write(*,*) '#E(T) Debye'
 delta_E_int=0
 do i1=1,n_max-3
   !
   Ti=T_in+real(i1-1,SP)*T_step
   delta_E_int=delta_E_int+(Cv_Debye(i1)+Cv_Debye(i1+1))/2._SP*T_step*n_atoms
   !write(*,*) Ti,delta_E_int
   !
   if( delta_E_int>delta_E ) exit
 enddo
 !
 if( i1==n_max-1) call error('T_out not found')
 !
 T_out=Ti
 !write(*,*) '  '
 !write(*,*) '# T_in, T_out, delta_E= ',T_in,T_out,delta_E
 !
 Bose_Temp=T_out/HA2EV*R_const
 !
end subroutine
