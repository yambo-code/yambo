!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_INTERPOLATED_step(G_new_k,G_old_k,I1_k,I2_k,ik,Time,deltaT)
 !
 use pars,           ONLY:SP,cI,cZERO
 use rt_ctl,         ONLY:it_start,it_dyn
 use real_time,      ONLY:I_MEM_steps,RT_step,MEM_index,I_coh_SAVE,RT_bands
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: Time,deltaT
 complex(SP), intent(in)  :: G_old_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: G_new_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work Space
 !
 integer            :: i_n,i_np,i_t,i_t_mem,i_f,nP(2),i_o
 real(SP)           :: Times_SAVE(I_MEM_steps),I_real(I_MEM_steps),I_aimag(I_MEM_steps),EPS(2),&
&                      P_real(I_MEM_steps),P_aimag(I_MEM_steps),T_o
 real(SP),parameter :: treshold=1.E-8
 !
 do i_n=RT_bands(1),RT_bands(2)
   do i_np=RT_bands(1),RT_bands(2)
     !
     i_t_mem = MEM_index(it_dyn,I_MEM_steps)
     I_coh_SAVE(i_n,i_np,ik,i_t_mem) = I1_k(i_n,i_np)-I2_k(i_n,i_np)
     !
     if (it_dyn-it_start+1<2*I_MEM_steps.or.abs(I_coh_SAVE(i_n,i_np,ik,i_t_mem))<treshold.or.i_n==i_np) then
       !
       G_new_k(i_n,i_np)=G_old_k(i_n,i_np)-cI*deltaT*(I1_k(i_n,i_np)-I2_k(i_n,i_np))
       !
     else
       !
       do i_f=1,I_MEM_steps
         i_t = it_dyn-I_MEM_steps+i_f
         i_t_mem = MEM_index(i_t,I_MEM_steps)
         Times_SAVE(i_f)=(i_t-1)*RT_step
         I_real(i_f) =real(I_coh_SAVE(i_n,i_np,ik,i_t_mem))
         I_aimag(i_f)=aimag(I_coh_SAVE(i_n,i_np,ik,i_t_mem))
       enddo
       !
       T_o=Times_SAVE(1)
       !
       nP(1)=0
       P_real(1)=I_real(I_MEM_steps)
       if (sum(abs(I_real))>treshold) then
         nP(1)=max(1,I_MEM_steps-2)
         call pol_fit(I_MEM_steps,Times_SAVE(:I_MEM_steps)-T_o,I_real(:I_MEM_steps),nP(1),P_real(:nP(1)+1),EPS(1),0.d0)
       endif
       !
       nP(2)=0
       P_aimag(1)=I_aimag(I_MEM_steps)
       if (sum(abs(I_aimag))>treshold) then
         nP(2)=max(1,I_MEM_steps-2)
         call pol_fit(I_MEM_steps,Times_SAVE(:I_MEM_steps)-T_o,I_aimag(:I_MEM_steps),nP(2),P_aimag(:nP(2)+1),EPS(2),0.d0)
       endif
       !
       G_new_k(i_n,i_np)=G_old_k(i_n,i_np)+deltaG(Time,deltaT)
       !
     endif
     !
   enddo
 enddo
 !
 contains
   !
   complex(SP) function deltaG(T,dT)
     integer     :: i_o
     real(SP)    :: T,dT
     deltaG=cZERO
     do i_o=1,nP(1)+1
       deltaG=deltaG-cI*P_real(i_o)/real(i_o)*( (T+dT-T_o)**i_o-(T-T_o)**i_o )
     enddo
     do i_o=1,nP(2)+1
       deltaG=deltaG  + P_aimag(i_o)/real(i_o)*( (T+dT-T_o)**i_o-(T-T_o)**i_o )
     enddo
   end function
   !
   complex(SP) function I(T)
     integer     :: i_o
     real(SP)    :: T
     I=cZERO
     do i_o=1,nP(1)+1
       I=I+(T-T_o)**(i_o-1)*P_real(i_o)
     enddo
     do i_o=1,nP(2)+1
       I=I+cI*(T-T_o)**(i_o-1)*P_aimag(i_o)
     enddo
   end function
   !
end subroutine RT_INTERPOLATED_step
