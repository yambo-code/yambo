!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine MATRIX_PAR_to_from_PAR( BLC_s, ROWS_s, COLS_s,&
&                                  BLC_r, ROWS_r, COLS_r )
 !
 use pars,           ONLY:SP
 use matrix,         ONLY:I_do_RCV,I_do_SND,TRANSFER_world,N_PAR_to_PAR_comms,&
&                         PAR_to_PAR_comm_SAVE,PAR_to_PAR_comm_NAME,PAR_to_PAR_comm_SAVE_NAME,N_MAX_PAR_to_PAR_comms
 use parallel_m,     ONLY:PP_redux_wait,myid,COMM_reset,CREATE_the_COMM,COMM_copy,MPI_comm,ncpu
 !
 implicit none
 !
 integer,  intent(IN)       :: ROWS_s(2),COLS_s(2)
 integer,  intent(IN)       :: ROWS_r(2),COLS_r(2)
 complex(SP), intent(IN)    :: BLC_s(ROWS_s(2)-ROWS_s(1)+1,COLS_s(2)-COLS_s(1)+1)
 complex(SP), intent(INOUT) :: BLC_r(ROWS_r(2)-ROWS_r(1)+1,COLS_r(2)-COLS_r(1)+1)
 !
 ! Work Space
 !
 integer :: i_rcv,i_snd,CROP(4),i_snd_world,i_rcv_world,i_cpu,&
&           RCV_corners(TRANSFER_world%n_CPU,4),SND_corners(TRANSFER_world%n_CPU,4),&
&           ACTIVE_cpus_snd(TRANSFER_world%n_CPU),ACTIVE_cpus_rcv(TRANSFER_world%n_CPU),&
&           CPU_in_the_world(TRANSFER_world%n_CPU),i_COMM_found,i_c,i_COMM_free
 !
 ! Local WORLD
 !
 type(MPI_comm) :: COMM
 !
 i_COMM_found =0
 i_COMM_free  =0
 if (trim(PAR_to_PAR_comm_NAME)/="NONE") then
   do i_c=1,N_PAR_to_PAR_comms
     if (len_trim(PAR_to_PAR_comm_SAVE_NAME(i_c))==0.and.i_COMM_free==0) i_COMM_free=i_c
     if (PAR_to_PAR_comm_SAVE_NAME(i_c)==PAR_to_PAR_comm_NAME          ) i_COMM_found=i_c
   enddo
   if (i_COMM_found==0.and.i_COMM_free==0) i_COMM_free=N_PAR_to_PAR_comms+1
   if (i_COMM_free>N_MAX_PAR_to_PAR_comms) i_COMM_free=1
 endif
 !
 if (i_COMM_found==0) then
   call COMM_reset(COMM)
   COMM%my_CHAIN=myid
   if (I_do_SND.or.I_do_RCV) COMM%my_CHAIN=ncpu+1
   if (TRANSFER_world%n_CPU >1) call CREATE_the_COMM(TRANSFER_world%COMM,COMM,myid)
   if (TRANSFER_world%n_CPU==1) call COMM_copy(TRANSFER_world,COMM)
   if (i_COMM_free>0) then
     call COMM_copy(COMM,PAR_to_PAR_comm_SAVE(i_COMM_free))
     PAR_to_PAR_comm_SAVE_NAME(i_COMM_free)=PAR_to_PAR_comm_NAME
     if (i_COMM_free>N_PAR_to_PAR_comms) N_PAR_to_PAR_comms=i_COMM_free
   endif
 else
   call COMM_copy(PAR_to_PAR_comm_SAVE(i_COMM_found),COMM)
 endif
 ! 
 if (.not.I_do_SND.and..not.I_do_RCV) return
 !
 CPU_in_the_world=0
 ACTIVE_cpus_snd =0
 ACTIVE_cpus_rcv =0
 do i_cpu=1,COMM%n_CPU
   if (COMM%CPU_id==i_cpu-1) then
     CPU_in_the_world(i_cpu)=myid
     if (I_do_SND) ACTIVE_cpus_snd(i_cpu)=1
     if (I_do_RCV) ACTIVE_cpus_rcv(i_cpu)=1
   endif
 enddo
 call PP_redux_wait(CPU_in_the_world,COMM=COMM%COMM)
 call PP_redux_wait(ACTIVE_cpus_snd,COMM=COMM%COMM)
 call PP_redux_wait(ACTIVE_cpus_rcv,COMM=COMM%COMM)
 !
 ! CORNERS
 !
 SND_corners                              =0
 SND_corners(COMM%CPU_id+1,:) =(/ROWS_s(1),ROWS_s(2),COLS_s(1),COLS_s(2)/)
 RCV_corners                              =0
 RCV_corners(COMM%CPU_id+1,:) =(/ROWS_r(1),ROWS_r(2),COLS_r(1),COLS_r(2)/)
 call PP_redux_wait(SND_corners         ,COMM=COMM%COMM)
 call PP_redux_wait(RCV_corners         ,COMM=COMM%COMM)
 !
 ! Here I define the list of "local buffers" to exchange. They are the minimal overlaps of the
 ! two PAR matrices.
 !
 do i_snd=1,COMM%n_CPU
   !
   if (ACTIVE_cpus_snd(i_snd)==0) cycle
   !
   do i_rcv=1,COMM%n_CPU
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
