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
subroutine MATRIX_PAR_to_from_PAR( BLC_s, COMM_s, ROWS_s, COLS_s,&
&                                  BLC_r, COMM_r, ROWS_r, COLS_r )
 !
 !  BLC_s => BLC_r
 !
 ! using the MPI comm's
 !
 !  COMM_s,COMM_r
 !
 ! M_snd > M_rcv
 !
 use pars,           ONLY:SP
 use linear_algebra, ONLY:I_do_RCV,I_do_SND,TRANSFER_world,PAR_to_PAR_cpu,PAR_to_PAR_comm
 use parallel_m,     ONLY:PP_redux_wait,myid,MPI_comm,COMM_reset,CREATE_the_COMM,COMM_copy
 !
 implicit none
 !
 integer          :: ROWS_s(2),COLS_s(2)
 integer          :: ROWS_r(2),COLS_r(2)
 complex(SP)      :: BLC_s(ROWS_s(2)-ROWS_s(1)+1,COLS_s(2)-COLS_s(1)+1)
 complex(SP)      :: BLC_r(ROWS_r(2)-ROWS_r(1)+1,COLS_r(2)-COLS_r(1)+1)
 type(MPI_comm)   :: COMM_s,COMM_r
 !
 ! Work Space
 !
 integer :: i_rcv,i_snd,CROP(4),i_snd_world,i_rcv_world,i_cpu
 integer :: RCV_corners(TRANSFER_world%n_CPU,4),SND_corners(TRANSFER_world%n_CPU,4),&
&           ACTIVE_cpus_snd(TRANSFER_world%n_CPU),ACTIVE_cpus_rcv(TRANSFER_world%n_CPU),&
&           CPU_in_the_world(TRANSFER_world%n_CPU),LOCAL_PAR_to_PAR_cpu(TRANSFER_world%n_CPU,2)
 logical :: BUILD_the_COMM=.TRUE.
 !
 ! Create a local MAP
 !
 LOCAL_PAR_to_PAR_cpu=0
 do i_cpu=1,TRANSFER_world%n_CPU
   if (i_cpu-1==TRANSFER_world%CPU_id) then
     LOCAL_PAR_to_PAR_cpu(i_cpu,1)=myid+1
     if (I_do_SND) LOCAL_PAR_to_PAR_cpu(i_cpu,2)=+1
     if (I_do_RCV) LOCAL_PAR_to_PAR_cpu(i_cpu,2)=-1
   endif
 enddo
 call PP_redux_wait(LOCAL_PAR_to_PAR_cpu,COMM=TRANSFER_world%COMM)
 ! 
 ! Compare with the saved MAP
 !
 if (allocated(PAR_to_PAR_cpu)) then
   if (size(LOCAL_PAR_to_PAR_cpu)==size(PAR_to_PAR_cpu)) then
     BUILD_the_COMM=any(LOCAL_PAR_to_PAR_cpu/=PAR_to_PAR_cpu)
   endif
   if (BUILD_the_COMM) deallocate(PAR_to_PAR_cpu)
 endif
 !
 ! Local WORLD
 !
 if (BUILD_the_COMM) then
   call COMM_reset(PAR_to_PAR_comm)
   PAR_to_PAR_comm%my_CHAIN=myid
   do i_snd=1,COMM_s%n_CPU
     if (COMM_s%CPU_id==i_snd-1.and.I_do_SND) PAR_to_PAR_comm%my_CHAIN=-1
   enddo
   do i_rcv=1,COMM_r%n_CPU
     if (COMM_r%CPU_id==i_rcv-1.and.I_do_RCV) PAR_to_PAR_comm%my_CHAIN=-1
   enddo
   if (TRANSFER_world%n_CPU >1) call CREATE_the_COMM(TRANSFER_world%COMM,PAR_to_PAR_comm,myid)
   if (TRANSFER_world%n_CPU==1) call COMM_copy(TRANSFER_world,PAR_to_PAR_comm)
   !
   allocate(PAR_to_PAR_cpu(TRANSFER_world%n_CPU,2))
   PAR_to_PAR_cpu=LOCAL_PAR_to_PAR_cpu
   !
 endif
 ! 
 if (.not.I_do_SND.and..not.I_do_RCV) return
 !
 CPU_in_the_world=0
 ACTIVE_cpus_snd =0
 ACTIVE_cpus_rcv =0
 do i_cpu=1,PAR_to_PAR_comm%n_CPU
   if (PAR_to_PAR_comm%CPU_id==i_cpu-1) then
     CPU_in_the_world(i_cpu)=myid
     if (I_do_SND) ACTIVE_cpus_snd(i_cpu)=1
     if (I_do_RCV) ACTIVE_cpus_rcv(i_cpu)=1
   endif
 enddo
 call PP_redux_wait(CPU_in_the_world,COMM=PAR_to_PAR_comm%COMM)
 call PP_redux_wait(ACTIVE_cpus_snd,COMM=PAR_to_PAR_comm%COMM)
 call PP_redux_wait(ACTIVE_cpus_rcv,COMM=PAR_to_PAR_comm%COMM)
 !
 ! CORNERS
 !
 SND_corners                              =0
 SND_corners(PAR_to_PAR_comm%CPU_id+1,:) =(/ROWS_s(1),ROWS_s(2),COLS_s(1),COLS_s(2)/)
 RCV_corners                              =0
 RCV_corners(PAR_to_PAR_comm%CPU_id+1,:) =(/ROWS_r(1),ROWS_r(2),COLS_r(1),COLS_r(2)/)
 call PP_redux_wait(SND_corners         ,COMM=PAR_to_PAR_comm%COMM)
 call PP_redux_wait(RCV_corners         ,COMM=PAR_to_PAR_comm%COMM)
 !
 ! Here I define the list of "local buffers" to exchange. They are the minimal overlaps of the
 ! two PAR matrices.
 !
 do i_snd=1,PAR_to_PAR_comm%n_CPU
   !
   if (ACTIVE_cpus_snd(i_snd)==0) cycle
   !
   do i_rcv=1,PAR_to_PAR_comm%n_CPU
     !
     if (ACTIVE_cpus_rcv(i_rcv)==0) cycle
     !
     i_snd_world=CPU_in_the_world(i_snd)
     i_rcv_world=CPU_in_the_world(i_rcv)
     !
     call MATRIX_transfer_side_crop(RCV_corners(i_rcv,1:2), SND_corners(i_snd,1:2), CROP(1:2) )
     call MATRIX_transfer_side_crop(RCV_corners(i_rcv,3:4), SND_corners(i_snd,3:4), CROP(3:4) )
     if (.not.any(CROP==0)) call elemental_transfer( )
     !
   enddo
   !
 enddo
 !
 return
 !
 contains
   !
   subroutine elemental_transfer( )
     !
     use parallel_m, ONLY:PP_send_and_receive
     integer            ::CROP_s(4),CROP_r(4)
     !
     if (myid/=i_snd_world.and.myid/=i_rcv_world) return
     !
     CROP_s(1:2)=CROP(1:2)-ROWS_s(1)+1
     CROP_s(3:4)=CROP(3:4)-COLS_s(1)+1
     CROP_r(1:2)=CROP(1:2)-ROWS_r(1)+1
     CROP_r(3:4)=CROP(3:4)-COLS_r(1)+1
     !
     if (myid==i_snd_world.and.i_snd_world==i_rcv_world) then
       BLC_r(CROP_r(1):CROP_r(2),CROP_r(3):CROP_r(4))=BLC_s(CROP_s(1):CROP_s(2),CROP_s(3):CROP_s(4))
     else if (myid==i_snd_world) then
       call PP_send_and_receive("send",BLC_s(CROP_s(1):CROP_s(2),CROP_s(3):CROP_s(4)),i_rcv_world)
     else if (myid==i_rcv_world) then
       call PP_send_and_receive("receive",BLC_r(CROP_r(1):CROP_r(2),CROP_r(3):CROP_r(4)),i_snd_world)
     endif
     !
   end subroutine
   !
end subroutine
