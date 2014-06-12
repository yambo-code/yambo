! 
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function K_T_group_index(I)
 !
 ! Return the transition group corresponding to index I 
 ! when each k is divided in N cpu's. I runs in BS_K_dim
 !
 use R_lattice,    ONLY:nXkbz
 use BS,           ONLY:BS_nT_at_k,BS_nT_grps_at_k
 use parallel_m,   ONLY:PAR_COM_eh_INDEX
 !
 implicit none
 !
 integer :: I
 !
 ! Workspace
 !
 integer :: J,ik_I,I_mean,i_g,ik,K,N
 !
 K_T_group_index=0
 ik_I=1
 J   =I
 N   =PAR_COM_eh_INDEX%n_CPU
 !
 do ik=1,nXkbz 
   if (J>BS_nT_at_k(ik)) then
     J=J-BS_nT_at_k(ik)
   else if (J==BS_nT_at_k(ik)) then
     J=0
     ik_I=ik
     goto 1
   else if (J<BS_nT_at_k(ik)) then
     ik_I=ik
     goto 1
   endif
 enddo
 !
1 continue
 !if (PAR_COM_eh_INDEX%CPU_id==0) write (*,*) "I J (1)",I,J, ik_I,BS_nT_grps_at_k(:ik_I)
 !
 if (J==0) then
   K_T_group_index=sum( BS_nT_grps_at_k(:ik_I) )
   return
 else
   K_T_group_index=sum( BS_nT_grps_at_k(:ik_I-1) ) +1
 endif
 !
 K=BS_nT_at_k(ik_I) 
 !
 I_mean=BS_nT_at_k(ik)/N
 !
 !if (PAR_COM_eh_INDEX%CPU_id==0) write (*,*) "I J (1a)",I,J,K_T_group_index,I_mean
 !
 if (I_mean>0) then
   do i_g=1,N
     if (J>I_mean) then
       J=J-I_mean
       K=K-I_mean
       K_T_group_index=K_T_group_index+1
     endif
   enddo
 endif
 !if (PAR_COM_eh_INDEX%CPU_id==0) write (*,*) "I J K (2)",I,J,K,K_T_group_index
 !
 if (I_mean>0.and.K>=I_mean) return
 !
 K_T_group_index=K_T_group_index+J-1
 !
end function
