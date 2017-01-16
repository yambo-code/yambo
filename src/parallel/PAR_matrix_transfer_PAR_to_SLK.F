!
!        Copyright (C) 2000-2017 the YAMBO team
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
SUBROUTINE PAR_matrix_transfer_PAR_to_SLK( mode, M_par, M_slk, i_MAT )
 !
 ! M_par (M_par%COMM) ->M_slk (SLK WORLD)
 !
 ! M_par can be mode "X up/dn" or "X". In the first case the upper/lower triangles are treated differently.
 !
 use linear_algebra, ONLY:PAR_matrix,SLK_matrix_transfer_side_crop
 use parallel_m,     ONLY:PP_redux_wait,myid
 use SLK_m,          ONLY:SLK_POOL,n_pools
 !
 implicit none
 !
 character(*)     :: mode
 type(PAR_matrix) :: M_par,M_slk
 integer          :: i_MAT
 !
 ! Work Space
 !
 integer :: SLK_coordinates(SLK_POOL%n_CPU,4),M_coordinates(M_par%COMM%n_CPU,4),i_c1,i_c2,i_pool,&
&           SLK_cpu_id_in_the_world(SLK_POOL%n_CPU),CROP(4)
 logical :: l_UP,l_DN
 !
 l_UP=index(mode,"up")>0
 l_DN=index(mode,"dn")>0
 !
 write (*,*) 'SLK',myid+1,M_slk%rows,M_slk%cols
 write (*,*) 'PAR',myid+1,M_par%rows,M_par%cols
 !
 ! SLK
 !
 SLK_coordinates                      =0
 SLK_coordinates(SLK_POOL%CPU_id+1,:) =(/M_slk%rows(1),M_slk%rows(2),M_slk%cols(1),M_slk%cols(2)/)
 call PP_redux_wait(SLK_coordinates ,COMM=SLK_POOL%INTRA_comm)
 !
 ! PAR
 M_coordinates                        =0
 M_coordinates(M_par%COMM%CPU_id+1,:) =(/M_par%rows(1),M_par%rows(2),M_par%cols(1),M_par%cols(2)/)
 call PP_redux_wait(M_coordinates ,COMM=M_par%COMM%COMM)
 !
 do i_pool=1,n_pools
   !
   SLK_cpu_id_in_the_world = 0
   if (SLK_POOL%ID==i_pool-1) SLK_cpu_id_in_the_world(SLK_POOL%CPU_id+1)=myid+1
   !
   call PP_redux_wait(SLK_cpu_id_in_the_world,COMM=M_par%COMM%COMM)
   !
   ! Here I define the list of "local buffers" to exchange. They are the minimal overlaps of the
   ! SLK and PAR sub-matrices.
   !
   do i_c1=1,SLK_POOL%n_CPU
     !
     ! i_c1 is the BLACS cpu
     !
     do i_c2=1,M_par%COMM%n_CPU
       !
       ! i_c2 is the X cpu
       !
       call SLK_matrix_transfer_side_crop(SLK_coordinates(i_c1,1:2), M_coordinates(i_c2,1:2), CROP(1:2) )
       call SLK_matrix_transfer_side_crop(SLK_coordinates(i_c1,3:4), M_coordinates(i_c2,3:4), CROP(3:4) )
 write (*,*) i_c2,'->',i_c1,CROP
       !if (.not.any(K_coo==0)) call elemental_transfer('N')
       !call PP_redux_wait( )
       !
     enddo
     !
   enddo
   !
 enddo
 stop
 !
end SUBROUTINE
