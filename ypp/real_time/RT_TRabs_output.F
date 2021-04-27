!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_TRabs_output(Xen,Xk,Xq)
 !
 use pars,            ONLY:SP,pi
 use units,           ONLY:AUT2FS,HA2EV,SPEED_OF_LIGHT
 use R_lattice,       ONLY:bz_samp
 use D_lattice,       ONLY:alat
 use electrons,       ONLY:levels
 use RT_control,      ONLY:RT_output
 use YPP_real_time,   ONLY:RT_time,TRabsWHAT
 use X_m,             ONLY:Epsilon_ii
 use frequency,       ONLY:w_samp
 use stderr,          ONLY:STRING_match
 !
#include<memory.h>
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,Xq
 !
 ! Work space
 !
 type(w_samp)      :: W_bss
 !
 ! Old...
 !
 integer              :: i_T,i_freq
 real(SP)             :: output(4)
 !
 real(SP),   allocatable :: X_absorption_coeff(:,:)
 complex(SP),allocatable :: X_reflectance(:,:)
 !
 if(STRING_match(TRabsWHAT,"r")) then
   ! Reflectance is assumed for a 3D system
   ! r=(sqrt(eps)-1)/(sqrt(eps)+1)
   X_reflectance(:,1)=(sqrt(Epsilon_ii(:,1))-1._SP)/(sqrt(Epsilon_ii(:,1))+1._SP)
   ! der r/der eps = 1/(sqrt(eps)+1)**2/ sqrt(eps)
   X_reflectance(:,2)=Epsilon_ii(:,2)/(sqrt(Epsilon_ii(:,1))+1._SP)**2._SP/sqrt(Epsilon_ii(:,1)) 
 endif
 !
 if(STRING_match(TRabsWHAT,"t")) then
   ! 3D systems
   ! alpha= 2/c aimag(sqrt(eps))
   !X_absorption_coeff(:,1)=1._SP-exp(-real(W_bss%p(:))/SPEED_OF_LIGHT*aimag(sqrt(Epsilon_ii(:,1)))*thickness)
   !X_absorption_coeff(:,2)=
   ! MAU Nano-letters: 2D systems
   X_absorption_coeff(:,1)=real(W_bss%p(:))/SPEED_OF_LIGHT*aimag(Epsilon_ii(:,1))*alat(3)
   X_absorption_coeff(:,2)=real(W_bss%p(:))/SPEED_OF_LIGHT*aimag(Epsilon_ii(:,2))*alat(3)
 endif
 !
 do i_freq=1,W_bss%n_freqs
   output(4)=real(Epsilon_ii(i_freq,1))
   output(3)=aimag(Epsilon_ii(i_freq,1))
   output(2)=aimag(Epsilon_ii(i_freq,2))
   output(1)=real(Epsilon_ii(i_freq,2))
   call RT_output("trans_eps_real",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
   call RT_output("trans_eps_imag",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
   if(i_T==1) call RT_output("eps_eq",VALUEs=(/output(3:4)/),E=real(W_bss%p(i_freq))*HA2EV)
   if(STRING_match(TRabsWHAT,"r")) then
     !  R=|r|^2
     output(3)=X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,1))
     !output(2)=X_reflectance(i_freq,2)*conjg(X_reflectance(i_freq,2))-output(3)
     output(2)=X_reflectance(i_freq,2)*conjg(X_reflectance(i_freq,1))+X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,2))
     output(1)=output(2)/output(3)
     call RT_output("trans_refl_rel",VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     call RT_output("trans_refl_abs",VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     if(i_T==1) call RT_output("refl_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV)
   endif
   ! I need to check some factor
   if(STRING_match(TRabsWHAT,"t")) then
     ! 3D systems
     !output(3)=1._SP-X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,1))-X_absorption_coeff(i_freq,1)
     !output(2)=-X_reflectance(i_freq,1)*conjg(X_reflectance(i_freq,2))-conjg(X_reflectance(i_freq,1))*X_reflectance(i_freq,2) &
     !         -X_absorption_coeff(i_freq,2)
     ! 2D systems (reflectance is assumed to be zero)
     output(3)=1._SP-X_absorption_coeff(i_freq,1)
     output(2)=-X_absorption_coeff(i_freq,2)
     output(1)=output(2)/output(3)
     call RT_output("trans_transm_rel",    VALUEs=(/output(1)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     call RT_output("trans_transm_abs",    VALUEs=(/output(2)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     if(i_T==1) call RT_output("transm_eq",VALUEs=(/output(3)/),E=real(W_bss%p(i_freq))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
   endif
 enddo
 !
 call RT_output("space trans")
 !
end subroutine RT_TRabs_output
