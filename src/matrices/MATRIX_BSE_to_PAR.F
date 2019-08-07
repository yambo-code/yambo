!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine MATRIX_BSE_to_PAR( M_bse , M_slk )
 !
 ! M_bse (WORLD) ->M_slk (SLK WORLD)
 !
 use pars,           ONLY:SP
 use SLK_m,          ONLY:SLK_POOL,n_pools
 use BS,             ONLY:BS_block,n_BS_blks,BS_K_dim,BS_K_coupling
 use parallel_m,     ONLY:PP_redux_wait,ncpu,myid
 use matrix,         ONLY:PAR_matrix
 !
 implicit none
 !
 type(PAR_matrix) :: M_slk
 type(BS_block)   :: M_bse(n_BS_blks)
 !
 ! Work Space
 !
 integer :: i_B,i_c1,i_c2,GLOBAL_n_BS_blks(ncpu),&
&           SLK_coordinates(SLK_POOL%n_CPU,4),K_coo(4),MAX_n_BS_blks,i_r,i_c,&
&           i_r_,i_c_,SLK_cpu_id_in_the_world(SLK_POOL%n_CPU),i_pool
 integer, allocatable  :: BSE_components(:,:,:)
 !
 ! SLK
 !
 SLK_coordinates                      =0
 SLK_coordinates(SLK_POOL%CPU_id+1,:) =(/M_slk%rows(1),M_slk%rows(2),M_slk%cols(1),M_slk%cols(2)/)
 call PP_redux_wait(SLK_coordinates ,COMM=SLK_POOL%INTRA_comm)
 !
 ! BSE
 !
 GLOBAL_n_BS_blks         =0
 GLOBAL_n_BS_blks(myid+1) =n_BS_blks
 call PP_redux_wait(GLOBAL_n_BS_blks)
 MAX_n_BS_blks  =maxval(GLOBAL_n_BS_blks)
 allocate(BSE_components(ncpu,MAX_n_BS_blks,7)) ! 1-4 rows,cols / 5-6 iTs / 7 Mode
 BSE_components =0
 !
 do i_B=1,n_BS_blks
   !
   if (M_bse(i_B)%mode=="R") BSE_components(myid+1,i_B,7)=1
   if (M_bse(i_B)%mode=="A") BSE_components(myid+1,i_B,7)=2
   !
   BSE_components(myid+1,i_B,5:6)=(/M_bse(i_B)%iT_k,M_bse(i_B)%iT_p/)
   !
   BSE_components(myid+1,i_B, :4)=(/M_bse(i_B)%coordinate(1),M_bse(i_B)%coordinate(1)+M_bse(i_B)%size(1)-1,&
&                                   M_bse(i_B)%coordinate(2),M_bse(i_B)%coordinate(2)+M_bse(i_B)%size(2)-1/)
   !
 enddo
 !  
 call PP_redux_wait(BSE_components)
 !
 do i_pool=1,n_pools
   !
   SLK_cpu_id_in_the_world  =0
   if (SLK_POOL%ID==i_pool-1) SLK_cpu_id_in_the_world(SLK_POOL%CPU_id+1)=myid+1
   !
   call PP_redux_wait(SLK_cpu_id_in_the_world)
   !
   ! Here I define the list of "local buffers" to exchange. They are the minimal overlaps of the
   ! SLK and BSE sub-matrices.
   !
   do i_c1=1,SLK_POOL%n_CPU
     !
     ! i_c1 is the BLACS cpu
     !
     do i_c2=1,ncpu
       !
       ! i_c2 is the BS cpu
       !
       do i_B=1,GLOBAL_n_BS_blks(i_c2)
         !
         ! Here I crop the part of the BS block that fits into the BLACS structure. I crop the two sides of the
         ! specific K block.
         !
         if (BSE_components(i_c2,i_B,7)==1) then
           !
           !  Res.   Cpl.
           !
           !  - X -  - - -  
           !  - - -  - - - Res
           !  - - -  - - -  
           !  - - -  - - -  
           !  - - -  - - - Cpl
           !  - - -  - - -  
           !
           call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,1:2), K_coo(1:2) )
           call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,3:4), K_coo(3:4) )
           if (.not.any(K_coo==0)) call elemental_transfer('N')
           call PP_redux_wait( )
           !
           if (BS_K_coupling) then
             !
             !  Res.   Cpl.
             !
             !  - - -  - - -  
             !  - - -  - - - Res
             !  - - -  - - -  
             !  - - -  - X -  
             !  - - -  - - - Cpl
             !  - - -  - - -  
             !
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,1:2)+BS_K_dim, K_coo(1:2) )
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,3:4)+BS_K_dim, K_coo(3:4) )
             if (.not.any(K_coo==0)) call elemental_transfer('-C')
             call PP_redux_wait( )
             !
           endif
           !
           if (BSE_components(i_c2,i_B,5)/=BSE_components(i_c2,i_B,6)) then
             !
             !  Res.   Cpl.
             !
             !  - - -  - - -  
             !  X - -  - - - Res
             !  - - -  - - -  
             !  - - -  - - -  
             !  - - -  - - - Cpl
             !  - - -  - - -  
             !
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,3:4), K_coo(1:2) )
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,1:2), K_coo(3:4) )
             if (.not.any(K_coo==0)) call elemental_transfer('H')
             call PP_redux_wait( )
             !
             if (BS_K_coupling) then
               !
               !  Res.   Cpl.
               !
               !  - - -  - - -  
               !  - - -  - - - Res
               !  - - -  - - -  
               !  - - -  - - -  
               !  - - -  X - - Cpl
               !  - - -  - - -  
               !
               call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), &
&                                                 BSE_components(i_c2,i_B,3:4)+BS_K_dim, K_coo(1:2) )
               call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), &
&                                                 BSE_components(i_c2,i_B,1:2)+BS_K_dim, K_coo(3:4) )
               if (.not.any(K_coo==0)) call elemental_transfer('-T')
               call PP_redux_wait( )
               !
             endif
             !
           endif
           !
         else if (BSE_components(i_c2,i_B,7)==2) then
           !
           !  Res.   Cpl.
           !
           !  - - -  - X -  
           !  - - -  - - - Res
           !  - - -  - - -  
           !  - - -  - - -  
           !  - - -  - - - Cpl
           !  - - -  - - -  
           !
           call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,1:2), K_coo(1:2) )
           call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,3:4)+BS_K_dim, K_coo(3:4) )
           if (.not.any(K_coo==0)) call elemental_transfer('N')
           !
           if (BS_K_coupling) then
             !
             !  Res.   Cpl.
             !
             !  - - -  - - -  
             !  - - -  - - - Res
             !  - - -  - - -  
             !  - X -  - - -  
             !  - - -  - - - Cpl
             !  - - -  - - -  
             !
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,1:2)+BS_K_dim, K_coo(1:2) )
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,3:4), K_coo(3:4) )
             if (.not.any(K_coo==0)) call elemental_transfer('-C')
             !
           endif
           !
           if (BSE_components(i_c2,i_B,5)/=BSE_components(i_c2,i_B,6)) then
             !
             !  Res.   Cpl.
             !
             !  - - -  - - -  
             !  - - -  X - - Res
             !  - - -  - - -  
             !  - - -  - - -  
             !  - - -  - - - Cpl
             !  - - -  - - -  
             !
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,3:4), K_coo(1:2) )
             call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,1:2)+BS_K_dim, K_coo(3:4) )
             if (.not.any(K_coo==0)) call elemental_transfer('T')
             !
             if (BS_K_coupling) then
               !
               !  Res.   Cpl.
               !
               !  - - -  - - -  
               !  - - -  - - - Res
               !  - - -  - - -  
               !  - - -  - - -  
               !  X - -  - - - Cpl
               !  - - -  - - -  
               !
               call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,1:2), BSE_components(i_c2,i_B,3:4)+BS_K_dim, K_coo(1:2) )
               call MATRIX_transfer_side_crop(SLK_coordinates(i_c1,3:4), BSE_components(i_c2,i_B,1:2), K_coo(3:4) )
               if (.not.any(K_coo==0)) call elemental_transfer('-H')
               !
             endif
           endif
           !
           call PP_redux_wait( )
           !
         endif
         !
       enddo
       !
     enddo
     !
   enddo
   !
 enddo
 !
 deallocate(BSE_components)
 !
 contains
   !
   subroutine elemental_transfer(mode)
     !
     use parallel_m, ONLY:PP_send_and_receive
     character(*) :: mode
     complex(SP)  :: M_tmp(K_coo(1):K_coo(2),K_coo(3):K_coo(4))
     !
     integer      :: i_r_in_BLK,i_c_in_BLK
     !
     if (myid/=i_c2-1.and.myid/=SLK_cpu_id_in_the_world(i_c1)-1) return
     !
     if (myid==i_c2-1) then
       !
       do i_r=K_coo(1),K_coo(2)
         do i_c=K_coo(3),K_coo(4)
           !
           if (mode=="N".or.mode=="C".or.mode=="-C") then
             i_r_in_BLK=i_r-BSE_components(i_c2,i_B,1)+1
             i_c_in_BLK=i_c-BSE_components(i_c2,i_B,3)+1
             if (i_r>BS_K_dim) i_r_in_BLK=i_r_in_BLK-BS_K_dim
             if (i_c>BS_K_dim) i_c_in_BLK=i_c_in_BLK-BS_K_dim
           endif
           !
           if (mode=="H".or.mode=="T".or.mode=="-H".or.mode=="-T") then
             i_r_in_BLK=i_c-BSE_components(i_c2,i_B,1)+1
             i_c_in_BLK=i_r-BSE_components(i_c2,i_B,3)+1
             if (i_c>BS_K_dim) i_r_in_BLK=i_r_in_BLK-BS_K_dim
             if (i_r>BS_K_dim) i_c_in_BLK=i_c_in_BLK-BS_K_dim
           endif
           !
           if (mode=="N".or.mode=="T") then
             M_tmp(i_r,i_c)=M_bse(i_B)%mat(i_r_in_BLK,i_c_in_BLK)
           else if (mode=="H".or.mode=="C") then
             M_tmp(i_r,i_c)=conjg(M_bse(i_B)%mat(i_r_in_BLK,i_c_in_BLK))
           else if (mode=="-H".or.mode=="-C") then
             M_tmp(i_r,i_c)=-conjg(M_bse(i_B)%mat(i_r_in_BLK,i_c_in_BLK))
           else if (mode=="-T") then
             M_tmp(i_r,i_c)=-M_bse(i_B)%mat(i_r_in_BLK,i_c_in_BLK)
           endif
           !
           if (i_r==i_c) M_tmp(i_r,i_c)=real( M_tmp(i_r,i_c) )
           !
         enddo
       enddo
       !
       if (i_c2==SLK_cpu_id_in_the_world(i_c1)) then
         M_slk%blc(K_coo(1):K_coo(2),K_coo(3):K_coo(4),M_slk%I)=M_tmp
       else
         call PP_send_and_receive("send",M_tmp,SLK_cpu_id_in_the_world(i_c1)-1)
       endif
       !
       return
       !
     endif
     !
     if (myid==SLK_cpu_id_in_the_world(i_c1)-1) then
       call PP_send_and_receive("receive",M_slk%blc(K_coo(1):K_coo(2),K_coo(3):K_coo(4),M_slk%I),i_c2-1)
     endif
     !
   end subroutine
   ! 
end subroutine
