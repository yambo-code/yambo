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
subroutine K_doubles_mount(iw,W)
 !
 ! In this subrioutine the complete second order kernel is
 ! constructed starting from the BS_mat_doubles coupling term
 ! which has been printed by the subroutine K_doubles throught
 ! ioBS.F into the file ndb.BS_C_doubles = C_{sd}^{i}  i=1,8
 !
 !  K_{ss'}= \sum_d \sum_{ij}
 !       \frac{C^{i}_{sd}C^{j}*_{ds'}\Chi^{spin}_{ij}}{\omega-E_d} 
 !
 !  s={c,v}    d={c1,c2,v1,v2}   E_d={e_{c1}+e_{c2}-e_{v1}+e_{v2}}
 !
 use BS,            ONLY:BS_K_dim,BS_mat
 use BS_doubles,    ONLY:BS_mat_doubles,BS_doubles_E,BS_res_K_doubles, &
&                        BS_doubles_table,BS_K_doubles_dim
 use frequency,    ONLY:w_samp
 !
 ! Kernel construction
 integer           :: i_doubles,icv1,icv2,icin,jcin
 integer           :: spin_matrix(8,8)
 integer           :: iw
 complex           :: kernel_doubles,diagrams_doubles(16),k_loop
 !
 type(w_samp) :: W
 !
 !
 if(.not.BS_res_K_doubles) return
 !
 !
 spin_matrix(1,:)=(/2,1,1,2,2,1,1,2/)
 spin_matrix(2,:)=(/1,2,2,1,1,2,2,1/)
 spin_matrix(3,:)=(/1,2,2,1,1,2,2,1/)
 spin_matrix(4,:)=(/2,1,1,2,2,1,1,2/)
 spin_matrix(5,:)=(/2,1,1,2,2,1,1,2/)
 spin_matrix(6,:)=(/1,2,2,1,1,2,2,1/)
 spin_matrix(7,:)=(/1,2,2,1,1,2,2,1/)
 spin_matrix(8,:)=(/2,1,1,2,2,1,1,2/)
 !
 ! DEBUG <
 open(unit=3455, file='Kernel_double.txt', status='unknown')
 write(3455,*) real(1/(W%p(:)-BS_doubles_E(1)))
 write(3455,*) W%p(:)
 write(3455,*) BS_doubles_E(:)*27.2
 ! DEBUG >
 do icv1=1,BS_K_dim
   do icv2=1,BS_K_dim
     kernel_doubles=(0.,0.)
     diagrams_doubles(:)=(0.,0.)
     do icin=1,8
       do jcin=1,8
         k_loop=(0.,0.)
         do i_doubles=1,BS_K_doubles_dim
           k_loop=k_loop+&
&          BS_mat_doubles(icv1,i_doubles,icin)*conjg(BS_mat_doubles(icv2,i_doubles,jcin))/&
&          (W%p(iw)-BS_doubles_E(i_doubles))
!&          (-BS_doubles_E(i_doubles))
         enddo	
         k_loop=k_loop*spin_matrix(icin,jcin)*0.5
!         kernel_doubles=kernel_doubles+k_loop
!         if (iw==100) write(*,*) icv1,icv2,kernel_doubles,icin,jcin
         if (icin<=4 .and. icin==jcin  ) diagrams_doubles(1)=diagrams_doubles(1)+k_loop        !S: bubble up   (=Tc)
         if (icin>4  .and. icin==jcin  ) diagrams_doubles(2)=diagrams_doubles(2)+k_loop        !S: bubble down (=Tv)
         if (icin<=4 .and. icin==jcin-4) diagrams_doubles(3)=diagrams_doubles(3)+k_loop        !K: bubble Tc
         if (icin>4  .and. icin==jcin+4) diagrams_doubles(4)=diagrams_doubles(4)+k_loop        !K: bubble Tv
         if ( (icin==1 .and. jcin==2) .or. &
&             (icin==2 .and. jcin==1) .or. &                                                   !S: vertex up
&             (icin==4 .and. jcin==3) .or. &
&             (icin==3 .and. jcin==4)  ) diagrams_doubles(5)=diagrams_doubles(5)+k_loop        
         if ( (icin==1 .and. jcin==3) .or. &
&             (icin==3 .and. jcin==1) .or. &                                                   !K: oriz.ladder
&             (icin==4 .and. jcin==2) .or. &                                                   !   cross interactions
&             (icin==2 .and. jcin==4)  ) diagrams_doubles(6)=diagrams_doubles(6)+k_loop        !   Tc
         if ( (icin==1 .and. jcin==4) .or. &
&             (icin==4 .and. jcin==1) .or. &                                                   !K: vertical ladder
&             (icin==2 .and. jcin==3) .or. &                                                   !   Tc
&             (icin==3 .and. jcin==2)  ) diagrams_doubles(7)=diagrams_doubles(7)+k_loop        
         if ( (icin==5 .and. jcin==6) .or. &
&             (icin==6 .and. jcin==5) .or. &                                                   !K: oriz. ladder
&             (icin==8 .and. jcin==7) .or. &                                                   !   cross interactions
&             (icin==7 .and. jcin==8)  ) diagrams_doubles(8)=diagrams_doubles(8)+k_loop        !   Tv
         if ( (icin==5 .and. jcin==7) .or. &
&             (icin==7 .and. jcin==5) .or. &                                                   !S: vertex down
&             (icin==8 .and. jcin==6) .or. &
&             (icin==6 .and. jcin==8) ) diagrams_doubles(9)=diagrams_doubles(9)+k_loop        
         if ( (icin==5 .and. jcin==8) .or. &
&             (icin==8 .and. jcin==5) .or. &                                                   !K: vertical ladder
&             (icin==6 .and. jcin==7) .or. &                                                   !   Tv
&             (icin==7 .and. jcin==6)  ) diagrams_doubles(10)=diagrams_doubles(10)+k_loop         
         if ( (icin==1 .and. jcin==6) .or. &
&             (icin==2 .and. jcin==5) .or. &                                                   !K: vertex up Tc
&             (icin==4 .and. jcin==7) .or. &                                                   
&             (icin==3 .and. jcin==8)  ) diagrams_doubles(11)=diagrams_doubles(11)+k_loop        
         if ( (icin==1 .and. jcin==7) .or. &
&             (icin==3 .and. jcin==5) .or. &                                                   !K: vertex down Tc
&             (icin==4 .and. jcin==6) .or. &                                                      
&             (icin==2 .and. jcin==8)  ) diagrams_doubles(12)=diagrams_doubles(12)+k_loop           
         if ( (icin==1 .and. jcin==8) .or. &
&             (icin==4 .and. jcin==5) .or. &                                                   !K: vertical ladder
&             (icin==2 .and. jcin==7) .or. &                                                   !   cross interaction
&             (icin==3 .and. jcin==6)  ) diagrams_doubles(13)=diagrams_doubles(13)+k_loop      !   Tc
         if ( (icin==5 .and. jcin==2) .or. &
&             (icin==6 .and. jcin==1) .or. &                                                   !K: vertex up Tv
&             (icin==8 .and. jcin==3) .or. &                                                      
&             (icin==7 .and. jcin==4)  ) diagrams_doubles(14)=diagrams_doubles(14)+k_loop      
         if ( (icin==5 .and. jcin==3) .or. &
&             (icin==7 .and. jcin==1) .or. &                                                   !K: vertex down Tv
&             (icin==8 .and. jcin==2) .or. &                                                   
&             (icin==6 .and. jcin==4)  ) diagrams_doubles(15)=diagrams_doubles(15)+k_loop        
         if ( (icin==5 .and. jcin==4) .or. &
&             (icin==8 .and. jcin==1) .or. &                                                   !K: vertical ladder
&             (icin==6 .and. jcin==3) .or. &                                                   !   cross interactions
&             (icin==7 .and. jcin==2)  ) diagrams_doubles(16)=diagrams_doubles(16)+k_loop      !   Tv
       enddo
     enddo
     do icin=1,16 
       kernel_doubles=kernel_doubles+diagrams_doubles(icin)
     enddo
     ! DEBUG <
     !if(iw==100) write(3455,*) icv1,icv2,abs(kernel_doubles),real(diagrams_doubles(:)),aimag(diagrams_doubles(:))
     ! DEBUG >
     BS_mat(icv1,icv2)=BS_mat(icv1,icv2)+kernel_doubles
   enddo
 enddo
 !
 ! DEBUG <
 close(unit=3455)
 ! DEBUG >
end subroutine
