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
SUBROUTINE MATRIX_PAR_to_from_PAR( M_par, mode, M_slk, i_M_par, i_M_slk )
 !
 ! M_par <-> M_slk
 !
 ! mode can be ">" or "<". 
 !
 ! In the case of a XUP/XDN "PAR" kinds the UP/DN triangles are treated differently.
 !
 use pars,           ONLY:SP
 use linear_algebra, ONLY:PAR_matrix,SLK_matrix_transfer_side_crop
 use parallel_m,     ONLY:PP_redux_wait,myid,PAR_COM_X_WORLD
 use SLK_m,          ONLY:SLK_POOL,n_pools
 use D_lattice,      ONLY:i_space_inv
 !
 implicit none
 !
 character(1)     :: mode
 type(PAR_matrix) :: M_par,M_slk
 integer          :: i_M_par,i_M_slk
 !
 ! Work Space
 !
 integer :: SLK_coordinates(SLK_POOL%n_CPU,4),M_coordinates(M_par%INTER_comm%n_CPU,4),i_c_slk,i_c_par,i_pool,&
&           SLK_cpu_id_in_the_world(SLK_POOL%n_CPU),CROP(4)
 logical :: l_UP,l_DN,l_SYMM
 !
 l_UP  =index(mode,"up")>0
 l_DN  =index(mode,"dn")>0
 l_SYMM=.not.l_UP.and..not.l_DN.and.index(mode,"X")>0
 !
 ! SLK
 !
 SLK_coordinates                      =0
 SLK_coordinates(SLK_POOL%CPU_id+1,:) =(/M_slk%rows(1),M_slk%rows(2),M_slk%cols(1),M_slk%cols(2)/)
 call PP_redux_wait(SLK_coordinates ,COMM=SLK_POOL%INTRA_comm)
 !
 ! PAR
 M_coordinates                        =0
 M_coordinates(M_par%INTER_comm%CPU_id+1,:) =(/M_par%rows(1),M_par%rows(2),M_par%cols(1),M_par%cols(2)/)
 call PP_redux_wait(M_coordinates ,COMM=M_par%INTER_comm%COMM)
 !
 do i_pool=1,n_pools
   !
   SLK_cpu_id_in_the_world = 0
   if (SLK_POOL%ID==i_pool-1) SLK_cpu_id_in_the_world(SLK_POOL%CPU_id+1)=myid+1
   !
   call PP_redux_wait(SLK_cpu_id_in_the_world,COMM=M_par%INTER_comm%COMM)
   !
   ! Here I define the list of "local buffers" to exchange. They are the minimal overlaps of the
   ! SLK and PAR sub-matrices.
   !
   do i_c_slk=1,SLK_POOL%n_CPU
     !
     ! i_c_slk is the BLACS cpu
     !
     do i_c_par=1,M_par%INTER_comm%n_CPU
       !
       ! i_c_par is the X/PAR cpu
       !
       ! a. Upper part (X) or entire matrix block (PAR)
       !
       ! ------
       ! -ooooo
       ! -ooooo
       ! ------
       ! ------
       ! ------
       !
       if (l_UP.or.l_SYMM) then
         call SLK_matrix_transfer_side_crop(SLK_coordinates(i_c_slk,1:2), M_coordinates(i_c_par,1:2), CROP(1:2) )
         call SLK_matrix_transfer_side_crop(SLK_coordinates(i_c_slk,3:4), M_coordinates(i_c_par,3:4), CROP(3:4) )
         if (.not.any(CROP==0)) then
           call elemental_transfer('N')
           call elemental_transfer('N')
         endif
       endif
       !
       ! a. Lower part
       !
       ! ------
       ! -oo---
       ! -oo---
       ! -oo---
       ! -oo---
       ! -oo---
       !
       if (l_DN.or.l_SYMM) then
         call SLK_matrix_transfer_side_crop(SLK_coordinates(i_c_slk,1:2), M_coordinates(i_c_par,3:4), CROP(1:2) )
         call SLK_matrix_transfer_side_crop(SLK_coordinates(i_c_slk,3:4), M_coordinates(i_c_par,1:2), CROP(3:4) )
         if (.not.any(CROP==0)) then
           if (l_DN) call elemental_transfer('N')
           if (l_SYMM) then
             if (i_space_inv==0) call elemental_transfer('C')
             if (i_space_inv==1) call elemental_transfer('T')
           endif
         endif
       endif
       !
       call PP_redux_wait(COMM=PAR_COM_X_WORLD%COMM)
       !
     enddo
     !
   enddo
   !
 enddo
 !
 contains
   !
   subroutine elemental_transfer(mode)
     !
     use parallel_m, ONLY:PP_send_and_receive
     character(*) :: mode
     !
     integer      :: BUFF_size,i_r,i_c,SLK_blk_coo(4)
     complex(SP)  :: M_cropped(CROP(1):CROP(2),CROP(3):CROP(4))
     !
     if (myid/=i_c_par-1.and.myid/=SLK_cpu_id_in_the_world(i_c_slk)-1) return
     !
     SLK_blk_coo(1:2) = CROP(1:2)-M_slk%rows(1)+1
     SLK_blk_coo(3:4) = CROP(3:4)-M_slk%cols(1)+1
     !
     BUFF_size    = (CROP(2)-CROP(1)+1)*(CROP(4)-CROP(3)+1)
     !
!     if (target_world=="SLK") then
       !
       if (myid==i_c_par-1) then
         !
         do i_r=CROP(1),CROP(2)
           do i_c=CROP(3),CROP(4)
             if (trim(mode)=="N") M_cropped(i_r,i_c)=M_par%blc_3d(i_r,i_c,i_M_par)
             if (trim(mode)=="T") M_cropped(i_r,i_c)=M_par%blc_3d(i_c,i_r,i_M_par)
             if (trim(mode)=="C") M_cropped(i_r,i_c)=conjg(M_par%blc_3d(i_c,i_r,i_M_par))
           enddo
         enddo
         !
         if (i_c_par==SLK_cpu_id_in_the_world(i_c_slk)) then
           M_slk%blc(SLK_blk_coo(1):SLK_blk_coo(2),SLK_blk_coo(3):SLK_blk_coo(4))=M_cropped
         else
           call PP_send_and_receive("send",M_cropped,SLK_cpu_id_in_the_world(i_c_slk)-1)
         endif
         !
         return
         !
!       endif
       !
       if (myid==SLK_cpu_id_in_the_world(i_c_slk)-1) then
         call PP_send_and_receive("receive",M_slk%blc(SLK_blk_coo(1):SLK_blk_coo(2),&
&                                                     SLK_blk_coo(3):SLK_blk_coo(4)),i_c_par-1)
       endif
       !
     else
       !
       if (myid==SLK_cpu_id_in_the_world(i_c_slk)-1) then
         !
         if (i_c_par==SLK_cpu_id_in_the_world(i_c_slk)) then
           M_par%blc_3d(CROP(1):CROP(2),CROP(3):CROP(4),i_M_par)=&
&                                   M_slk%blc(SLK_blk_coo(1):SLK_blk_coo(2),SLK_blk_coo(3):SLK_blk_coo(4))
         else
           call PP_send_and_receive("send",M_slk%blc(SLK_blk_coo(1):SLK_blk_coo(2),&
&                                                    SLK_blk_coo(3):SLK_blk_coo(4)),i_c_par-1)
         endif
         !
         return
         !
       endif
       !
       if (myid==i_c_par-1) then
         call PP_send_and_receive("receive",M_par%blc_3d(CROP(1):CROP(2),&
&                                                        CROP(3):CROP(4),i_M_par),SLK_cpu_id_in_the_world(i_c_slk)-1)
       endif
     endif
     !
   end subroutine
   !
end SUBROUTINE
