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
SUBROUTINE MATRIX_PAR_to_from_PAR( M_snd, M_rcv )
 !
 ! M_snd > M_rcv
 !
 use linear_algebra, ONLY:PAR_matrix,I_do_RCV,I_do_SND,TRANSFER_world
 use parallel_m,     ONLY:PP_redux_wait,myid,MPI_comm,COMM_reset,CREATE_the_COMM,ncpu
 !
 implicit none
 !
 type(PAR_matrix) :: M_snd,M_rcv
 !
 ! Work Space
 !
 integer :: i_rcv,i_snd,CROP(4),i_snd_world,i_rcv_world,i_cpu
 integer :: RCV_corners(ncpu,4),SND_corners(ncpu,4),ACTIVE_cpus_snd(ncpu),&
&           ACTIVE_cpus_rcv(ncpu),CPU_in_the_world(ncpu)
 ! 
 ! Local global MPI comm
 !
 type(MPI_comm) :: COMM
 !
 ! Local WORLD
 !
 call COMM_reset(COMM)
 COMM%my_CHAIN=myid
 do i_snd=1,M_snd%INTER_comm%n_CPU
   if (M_snd%INTER_comm%CPU_id==i_snd-1.and.I_do_SND) COMM%my_CHAIN=-1
 enddo
 do i_rcv=1,M_rcv%INTER_comm%n_CPU
   if (M_rcv%INTER_comm%CPU_id==i_rcv-1.and.I_do_RCV) COMM%my_CHAIN=-1
 enddo
 call CREATE_the_COMM(TRANSFER_world%COMM,COMM,myid)
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
 !write (*,*) "ID=",myid,"C=",CPU_in_the_world(:COMM%n_CPU)
 !write (*,*) "ID=",myid,"A_S=",ACTIVE_cpus_snd(:COMM%n_CPU)
 !write (*,*) "ID=",myid,"A_R=",ACTIVE_cpus_rcv(:COMM%n_CPU)
 !
 ! CORNERS
 !
 SND_corners                              =0
 SND_corners(COMM%CPU_id+1,:) =(/M_snd%rows(1),M_snd%rows(2),M_snd%cols(1),M_snd%cols(2)/)
 RCV_corners                              =0
 RCV_corners(COMM%CPU_id+1,:) =(/M_rcv%rows(1),M_rcv%rows(2),M_rcv%cols(1),M_rcv%cols(2)/)
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
 call COMM_reset(COMM)
 !
 !write (*,*) "FINISHED PAR_to_from_PAR"
 !
 return
 !
 contains
   !
   subroutine elemental_transfer( )
     !
     use parallel_m, ONLY:PP_send_and_receive
     !
     if (myid/=i_snd_world.and.myid/=i_rcv_world) return
     !
     if (myid==i_snd_world.and.i_snd_world==i_rcv_world) then
       !write (*,*) i_snd_world,M_snd%I,"<=>",i_rcv_world,M_rcv%I,CROP
       M_rcv%blc(CROP(1):CROP(2),CROP(3):CROP(4),M_rcv%I)=M_snd%blc(CROP(1):CROP(2),CROP(3):CROP(4),M_snd%I)
     else if (myid==i_snd_world) then
       !write (*,*) i_snd_world,M_snd%I,"=>",i_rcv_world,M_rcv%I,CROP
       call PP_send_and_receive("send",M_snd%blc(CROP(1):CROP(2),CROP(3):CROP(4),M_snd%I),i_rcv_world)
     else if (myid==i_rcv_world) then
       !write (*,*) i_rcv_world,M_rcv%I,"<=",i_snd_world,M_snd%I,CROP
       call PP_send_and_receive("receive",M_rcv%blc(CROP(1):CROP(2),CROP(3):CROP(4),M_rcv%I),i_snd_world)
     endif
     !
   end subroutine
   !
end SUBROUTINE
