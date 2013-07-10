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
subroutine TEMPERATURE_Debye(delta_E,T_in,T_out)
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV
 use D_lattice,  ONLY:n_atoms
 use com,        ONLY:error
 !
 implicit none
 !
 real(SP), intent(in) :: delta_E
 real(SP), intent(in) :: T_in
 real(SP), intent(out):: T_out
 ! 
 ! Paramenters
 integer,  parameter :: n_max=10000           ! Max steps
 real(SP), parameter :: T_step=1._SP          ! 1 K each T_step
 real(SP), parameter :: R_const=8.617*10.E-5  ! [eV/(K*atom)]
 !
 ! Functions
 real(SP) :: Cv_Debye(n_max)
 real(SP) :: f_int(n_max)
 real(SP) :: f_x_Ti
 !
 ! Variables
 real(SP) :: delta_E_int
 real(SP) :: T_D, Ti
 real(SP) :: xi(n_max), x_max, x_step, x_Ti
 real(SP) :: m
 integer  :: i1,i2,i_max
 !
 ! pol_fit
 integer, parameter :: fit_dim=5
 integer            :: order=3
 real(SP)           :: X(fit_dim),Y(fit_dim),coeff(4),eps=0._SP
 !
 ! Initialization
 Cv_Debye=0._SP
 f_int=0._SP
 T_D=640._SP                   ! Silicon (K)
 !
 x_max=T_D/T_in
 x_step=x_max/real(n_max-2,SP)
 write(*,*) '# T_in= ',T_in
 write(*,*) '# T_D = ',T_D
 write(*,*) '  '
 !
 ! f_int needed for the Debye function
 write(*,*) '#F(x) Debye'
 do i1=2,n_max
   xi(i1)=x_step*real(i1-1,SP)
   f_int(i1)=f_int(i1-1)+xi(i1)**4._SP*exp(xi(i1))/(exp(xi(i1))-1)**2._SP*x_step
   write(*,*) xi(i1),f_int(i1),xi(i1)**3._SP/3._SP
 enddo
 !
 ! The Debye Cv function
 write(*,*) '  '
 write(*,*) '#Cv(T) Debye'
 i_max=n_max
 do i1=1,n_max-2
   Ti=T_in+real(i1-1,SP)*T_step
   x_Ti=T_D/Ti
   if(x_Ti<0.1) then
     f_x_Ti=x_Ti**3._SP/3._SP
   else
     do i2=i_max,1,-1
       if(xi(i2)<x_Ti) exit 
     enddo 
     i_max=i2
     if(x_Ti<1.) then
       X=xi(i2-(fit_dim-3):i2+2)
       Y=f_int(i2-(fit_dim-3):i2+2)
       coeff=0._SP
       call pol_fit(fit_dim,X,Y,order,coeff,eps,0.d0)
       f_x_Ti=coeff(1)+coeff(2)*x_Ti+coeff(3)*x_Ti**2._SP+coeff(4)*x_Ti**3._SP
       !write(*,*) coeff
     else
       m=(f_int(i2+1)-f_int(i2))/x_step
       f_x_Ti=f_int(i2)+m*(x_Ti-xi(i2))
     endif     
   endif
   Cv_Debye(i1)=9._SP*R_const*(Ti/T_D)**3._SP*f_x_Ti/HA2EV
   if(Cv_Debye(i1)>3._SP*R_const/HA2EV) Cv_Debye(i1)=3._SP*R_const/HA2EV
   write(*,*) Ti,Cv_Debye(i1),3._SP*R_const/HA2EV
 enddo
 !
 ! The integral to obtain T_out
 write(*,*) '  '
 write(*,*) '#E(T) Debye'
 delta_E_int=0
 do i1=1,n_max-3
   !
   Ti=T_in+real(i1-1,SP)*T_step
   delta_E_int=delta_E_int+(Cv_Debye(i1)+Cv_Debye(i1+1))/2._SP*T_step*n_atoms
   write(*,*) Ti,delta_E_int
   !
   if( delta_E_int>delta_E ) exit
 enddo
 !
 if( i1==n_max-1) call error('T_out not found')
 !
 T_out=Ti
 write(*,*) '  '
 write(*,*) '# T_out= ',T_out
 !
end subroutine
