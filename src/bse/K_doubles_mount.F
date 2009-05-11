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
 ! ndb.BS_C_doubles = C_{sd}^{i}    with i=1,8
 !
 !  K_{ss'}= 1/2 \sum_{ij} \sum_d
 !       \frac{C^{i}_{sd}C^{j}*_{ds'}\Chi^{spin}_{ij}}{\omega-E_d} 
 !
 !  s={c,v}    d={c1,c2,v1,v2}   E_d={e_{c1}+e_{c2}-e_{v1}+e_{v2}}
 !
 use BS,            ONLY:BS_K_dim,BS_mat
 use BS_doubles,    ONLY:BS_mat_doubles,BS_doubles_E,BS_res_K_doubles, &
&                        BS_doubles_table,BS_K_doubles_dim,static_approx
 !
 ! Kernel construction
 integer           :: i_doubles,icv1,icv2,icin,jcin
 integer           :: spin_matrix(8,8)
 complex           :: kernel_doubles,k_loop,omega
 ! TESTING POURPOSE < (see line 105)
 ! complex           :: diagrams_doubles(16)
 ! TESTING POURPOSE >
 !
 !
 if(.not.BS_res_K_doubles) return
 !
 ! This is needed to respect the spin singlet simmetry.
 ! Some diagrams brings a factor two, some others not
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
 ! open(unit=3455, file='Kernel_double.txt', status='unknown')
 ! write(3455,*) real(1/(W%p(:)-BS_doubles_E(1)))
 ! write(3455,*) W%p(:)
 ! write(3455,*) BS_doubles_E(:)*27.2116
 ! DEBUG >
 !
 ! *********************
 ! ***** Main loop *****
 ! *********************
 do icv1=1,BS_K_dim
   do icv2=1,BS_K_dim
     kernel_doubles=(0.,0.)
     ! TESTING POURPOSE < (see line 95)
     !diagrams_doubles(:)=(0.,0.)
     ! TESTING POURPOSE >
     do icin=1,8
       do jcin=1,8
         k_loop=(0.,0.)
         do i_doubles=1,BS_K_doubles_dim
           ! STATIC APPROX <
           !if(iw/=-1.and.static_approx(i_doubles)==1) cycle
           !if(iw==-1.and.static_approx(i_doubles)==0) cycle
           !if(iw==-1.and.static_approx(i_doubles)==1) then
           !  k_loop=k_loop+&
&          !  BS_mat_doubles(icv1,i_doubles,icin)*conjg(BS_mat_doubles(icv2,i_doubles,jcin))/&
&          !  (-BS_doubles_E(i_doubles))
           !endif
           !if(iw/=-1.and.static_approx(i_doubles)==0) then
             k_loop=k_loop+&
&            BS_mat_doubles(icv1,i_doubles,icin)*conjg(BS_mat_doubles(icv2,i_doubles,jcin))/&
&            (omega-BS_doubles_E(i_doubles))
           !endif
           ! STATIC APPROX >
         enddo	
         ! Here we put the correct factors for spin.
         ! There is a 0.5 as second order brings a factor 1/2
         k_loop=k_loop*spin_matrix(icin,jcin)
         !
         kernel_doubles=kernel_doubles+k_loop
       enddo
     enddo
         ! TESTING POURPOSE <
         ! In the following line the second order in TDA is decomposed in its 16 diagrams
         ! For testing pourpose only. In this way it's possible to switch off and on the
         ! only specific diagrams.
         ! If you want to do that comment the last 3 lines before this comment and
         ! uncomment all the following untill the next TESTING POURPOSE >
!         if (icin<=4 .and. icin==jcin  ) diagrams_doubles(1)=diagrams_doubles(1)+k_loop        !S: bubble up   ( Tc)    +1
!         if (icin>4  .and. icin==jcin  ) diagrams_doubles(2)=diagrams_doubles(2)+k_loop        !S: bubble down ( Tv)    +2
!         if (icin<=4 .and. icin==jcin-4) diagrams_doubles(3)=diagrams_doubles(3)+k_loop        !K: bubble Tc            +3
!         if (icin>4  .and. icin==jcin+4) diagrams_doubles(4)=diagrams_doubles(4)+k_loop        !K: bubble Tv            +4
!         if ( (icin==1 .and. jcin==2) .or. &
!&             (icin==2 .and. jcin==1) .or. &                                                   !S: vertex up            -5
!&             (icin==4 .and. jcin==3) .or. &                                                   
!&             (icin==3 .and. jcin==4)  ) diagrams_doubles(5)=diagrams_doubles(5)+k_loop        
!         if ( (icin==1 .and. jcin==3) .or. &
!&             (icin==3 .and. jcin==1) .or. &                                                   !K: oriz.ladder          -6
!&             (icin==4 .and. jcin==2) .or. &                                                   !   cross interactions
!&             (icin==2 .and. jcin==4)  ) diagrams_doubles(6)=diagrams_doubles(6)+k_loop        !   Tc
!         if ( (icin==1 .and. jcin==4) .or. &
!&             (icin==4 .and. jcin==1) .or. &                                                   !K: vertical ladder      +7
!&             (icin==2 .and. jcin==3) .or. &                                                   !   cross interactions
!&             (icin==3 .and. jcin==2)  ) diagrams_doubles(7)=diagrams_doubles(7)+k_loop        !   Tc
!         if ( (icin==5 .and. jcin==6) .or. &
!&             (icin==6 .and. jcin==5) .or. &                                                   !K: oriz. ladder         -8
!&             (icin==8 .and. jcin==7) .or. &                                                   !   cross interactions
!&             (icin==7 .and. jcin==8)  ) diagrams_doubles(8)=diagrams_doubles(8)+k_loop        !   Tv
!         if ( (icin==5 .and. jcin==7) .or. &
!&             (icin==7 .and. jcin==5) .or. &                                                   !S: vertex down          -9
!&             (icin==8 .and. jcin==6) .or. &
!&             (icin==6 .and. jcin==8) ) diagrams_doubles(9)=diagrams_doubles(9)+k_loop        
!         if ( (icin==5 .and. jcin==8) .or. &
!&             (icin==8 .and. jcin==5) .or. &                                                   !K: vertical ladder      +10
!&             (icin==6 .and. jcin==7) .or. &                                                   !   cross interactions
!&             (icin==7 .and. jcin==6)  ) diagrams_doubles(10)=diagrams_doubles(10)+k_loop      !   Tv
!         if ( (icin==1 .and. jcin==6) .or. &
!&             (icin==2 .and. jcin==5) .or. &                                                   !K: vertex up Tc         -11
!&             (icin==4 .and. jcin==7) .or. &                                                   
!&             (icin==3 .and. jcin==8)  ) diagrams_doubles(11)=diagrams_doubles(11)+k_loop        
!         if ( (icin==1 .and. jcin==7) .or. &
!&             (icin==3 .and. jcin==5) .or. &                                                   !K: vertex down Tc       -12
!&             (icin==4 .and. jcin==6) .or. &                                                      
!&             (icin==2 .and. jcin==8)  ) diagrams_doubles(12)=diagrams_doubles(12)+k_loop           
!         if ( (icin==1 .and. jcin==8) .or. &
!&             (icin==4 .and. jcin==5) .or. &                                                   !K: vertical ladder      +13
!&             (icin==2 .and. jcin==7) .or. &                                                   !   Tc
!&             (icin==3 .and. jcin==6)  ) diagrams_doubles(13)=diagrams_doubles(13)+k_loop      
!         if ( (icin==5 .and. jcin==2) .or. &
!&             (icin==6 .and. jcin==1) .or. &                                                   !K: vertex up Tv         -14
!&             (icin==8 .and. jcin==3) .or. &                                                      
!&             (icin==7 .and. jcin==4)  ) diagrams_doubles(14)=diagrams_doubles(14)+k_loop      
!         if ( (icin==5 .and. jcin==3) .or. &
!&             (icin==7 .and. jcin==1) .or. &                                                   !K: vertex down Tv       -15
!&             (icin==8 .and. jcin==2) .or. &                                                   
!&             (icin==6 .and. jcin==4)  ) diagrams_doubles(15)=diagrams_doubles(15)+k_loop        
!         if ( (icin==5 .and. jcin==4) .or. &
!&             (icin==8 .and. jcin==1) .or. &                                                   !K: vertical ladder      +16
!&             (icin==6 .and. jcin==3) .or. &                                                   !   Tv
!&             (icin==7 .and. jcin==2)  ) diagrams_doubles(16)=diagrams_doubles(16)+k_loop      
!       enddo
!     enddo
     ! Here it's possible to choose which 2nd order diagrams you want to consider.
     ! i.e. if you loop only an icin=1,2 only the self energy bubbles are considered
     ! A loop from 1 to 16 consider the whole kernel
!     do icin=1,16 
!       kernel_doubles=kernel_doubles+diagrams_doubles(icin)
!     enddo
     ! TESTING POURPOSE >
     BS_mat(icv1,icv2)=BS_mat(icv1,icv2)+real(kernel_doubles)
   enddo
 enddo
 !
 ! DEBUG <
 ! close(unit=3455)
 ! DEBUG >
end subroutine
