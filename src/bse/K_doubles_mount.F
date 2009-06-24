!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
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
subroutine K_doubles_mount(omega)
 !
 ! In this subrioutine the complete second order kernel is
 ! constructed for a specific frequency, starting from the BS_mat_doubles
 ! coupling term which has been printed by the subroutine K_doubles.F
 ! throught ioBS_doubles.F into the file
 ! ndb.BS_C_doubles = C_{sd}^{i}    with i=1,2
 !
 !  K_{ss'}= 1/2 \sum_{ij} \sum_d
 !       \frac{C^{i}_{sd}C^{j}*_{ds'}\}{\omega-E_d} 
 !
 !  s={c,v}    d={c1,c2,v1,v2}   E_d={e_{c1}+e_{c2}-e_{v1}+e_{v2}}
 !
 use pars,          ONLY:SP
 use BS,            ONLY:BS_K_dim,BS_mat
 use BS_doubles,    ONLY:BS_mat_doubles,BS_doubles_E,BS_res_K_doubles, &
&                        BS_doubles_table,BS_K_doubles_dim,&
&                        static_approx,BS_mat_tmp,&
! DOUBLES CORR PROJECT
&                        doubles_corr
 !
 implicit none
 ! Kernel construction
 integer             :: i_doubles,icv1,icv2,icin,ncin
 complex             :: omega
 !
 !
 if(.not.BS_res_K_doubles) return
 !
 ! DEBUG <
 open(unit=3455, file='Kernel_double.txt', status='unknown')
 write(3455,*) BS_doubles_E(:)*27.2116
 ! DEBUG >
 !
 ncin=2
 if(doubles_corr) ncin=1
 !
 do icin=1,ncin
   BS_mat_tmp(:,:,2)=(0.,0.)
   forall(i_doubles=1:BS_K_doubles_dim) BS_mat_doubles(:,i_doubles,ncin+1)=&
&     BS_mat_doubles(:,i_doubles,icin)/(real(omega)-BS_doubles_E(i_doubles))
   call cgemm('n','c',BS_K_dim,BS_K_dim,BS_K_doubles_dim,(1._SP,0._SP),&
&    BS_mat_doubles(:,:,ncin+1),BS_K_dim,BS_mat_doubles(:,:,icin),BS_K_dim,&
&    (0._SP,0._SP),BS_mat_tmp(:,:,2),BS_K_dim)    
   BS_mat(:,:)=BS_mat(:,:)+BS_mat_tmp(:,:,2)
 enddo
 !
 forall(icv1=1:BS_K_dim) BS_mat(icv1,icv1)=real(BS_mat(icv1,icv1))
 !
 ! DEBUG <
 write(3455,*) BS_mat(:,:)
 close(unit=3455)
 ! DEBUG >
 !
end subroutine
