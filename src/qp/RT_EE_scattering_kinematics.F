!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_EE_scattering_kinematics(k,q,i_p,i_Rp,i_q,i_Rq,i_t_bz)
 !
 !      __/__ i_p,i_Rp (in)
 !    ./  \
 !    |\__\__ i_t_bz (out)
 !    |   /
 !   \|/
 !    | i_q,i_Rq (in)
 !  
 ! I need to evaluate the ingredients of the scattering:  
 !  
 !   Rp p - Rq q = Rt t
 !  
 ! in different steps:
 !  
 ! Rp p - Rq q = Rp ( p - Rp^-1 Rq q)
 !    
 ! Rp^-1    ->  i_Rpm1=sop_inv(i_Rp)
 !    
 ! Rp^-1 Rq ->  i_Rpm1_times_Rq=sop_tab(i_Rpm1,i_Rq)
 !    
 ! Rp^-1 Rq q - > iqbz=q%s_table(i_q,i_Rpm1_times_Rq) (q%s_table_mode="k")
 !    
 ! ( p - Rp^-1 Rq q) -> i_u =qindx_S(i_p,iqbz,1)    
 !                      i_Ru=k%sstar(i_u,2)
 !    
 ! itbz=k%s_table(i_u,sop_tab(i_Rp,i_Ru))
 !
 ! i_t =k%sstar(itbz,1)
 ! i_Rt=k%sstar(itbz,2)
 !    
 use D_lattice,       ONLY:sop_tab,sop_inv
 use R_lattice,       ONLY:bz_samp,qindx_S
 !
 implicit none
 !
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: i_p,i_Rp,i_q,i_Rq
 integer,      intent(out) :: i_t_bz
 !
 ! Work Space ...
 !
 integer               :: i_Rpm1,i_Rpm1_times_Rq,iqbz,iubz,i_u,i_Ru
 !
 ! Rp^-1    ->  i_Rpm1=sop_inv(i_Rp)
 !
 i_Rpm1=sop_inv(i_Rp)
 !    
 ! Rp^-1 Rq ->  i_Rpm1_times_Rq=sop_tab(i_Rpm1,i_Rq)
 !
 i_Rpm1_times_Rq=sop_tab(i_Rpm1,i_Rq)
 !    
 ! Rp^-1 Rq q - > iqbz=q%s_table(i_q,i_Rpm1_times_Rq) (q%s_table_mode="k")
 !    
 iqbz=q%s_table(i_q,i_Rpm1_times_Rq)
 !    
 ! ( p - Rp^-1 Rq q) -> i_u =qindx_S(i_p,iqbz,1)    
 !
 iubz =qindx_S(i_p,iqbz,1)
 !
 !                      i_Ru=k%sstar(i_u,2)
 i_u =k%sstar(iubz,1)
 i_Ru=k%sstar(iubz,2)
 !    
 i_t_bz=k%s_table(i_u,sop_tab(i_Rp,i_Ru))
 !
end subroutine RT_EE_scattering_kinematics
