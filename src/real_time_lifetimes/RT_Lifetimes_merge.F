!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_Lifetimes_merge(what,LIFE_h,LIFE_e)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PAR_COM_PLASMA_INDEX,PAR_COM_Q_INDEX,PP_redux_wait
 use R_lattice,      ONLY:nkibz
 use real_time,      ONLY:RT_nk,RT_bands,RT_ibz_coo
 use timing_m,       ONLY:timing
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Xk_ibz_A2A,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,myid
 !
 implicit none
 !
 character(*)            :: what
 real(SP), intent(inout) :: LIFE_h(RT_bands(1):RT_bands(2),RT_nk)
 real(SP), intent(inout) :: LIFE_e(RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: NC,ik,do_I_snd(nkibz),do_I_rcv(nkibz)
 !
 NC=1
 if (what=="elph".or.what=="bose") NC=PAR_COM_PLASMA_INDEX%n_CPU
 if (what=="elgm")                 NC=PAR_COM_Q_INDEX%n_CPU
 !
 if (NC>1) then
   LIFE_e(:,:)=LIFE_e(:,:)/NC
   LIFE_h(:,:)=LIFE_h(:,:)/NC
 endif
 !
 call timing('RT life merge (intra-k)',OPR='start')
 !
 do_I_snd=0
 do_I_rcv=0
 !
 K_loop_1: do ik=1,nkibz
   !
   if (.not.PAR_IND_Xk_ibz%element_1D(ik)) then
     do_I_rcv(ik)=myid+1
     cycle K_loop_1
   endif
   !
   do_I_snd(ik)=myid+1
   !
   if (PAR_COM_Xk_ibz_A2A%n_CPU>1) then
     call PP_redux_wait(LIFE_e(:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)),COMM=PAR_COM_Xk_ibz_A2A%COMM)
     call PP_redux_wait(LIFE_h(:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)),COMM=PAR_COM_Xk_ibz_A2A%COMM)
   endif
   !
 enddo K_loop_1
 !
 call PP_redux_wait(do_I_snd)
 call PP_redux_wait(do_I_rcv)
 !
 call timing('RT life merge (intra-k)',OPR='stop')
 !
 call timing('RT life merge (extra-k)',OPR='start')
 !
 !write (*,*) "MYID:",myid," PAR:",PAR_IND_Xk_ibz%element_1D
 !if (myid==0) then
 !  write (*,*) "SND:",do_I_snd
 !  write (*,*) "RCV:",do_I_rcv
 !endif
 !
 !K_loop_2: do ik=1,nkibz
 !  !
 !  do i_c_snd=1,ncpu
 !    if (do_I_snd(ik)/=i_c_snd) cycle
 !    do i_c_rcv=1,ncpu
 !      if (do_I_rcv(ik)/=i_c_rcv) cycle
 !      write (*,*) ik,i_c_snd,"=>",i_c_rcv
 !      call PP_send_and_receive("send",LIFE_e(:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)),i_c_rcv-1)
 !      call PP_send_and_receive("receive",LIFE_e(:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)),i_c_snd-1)
 !      call PP_send_and_receive("send",LIFE_h(:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)),i_c_rcv-1)
 !      call PP_send_and_receive("receive",LIFE_h(:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2)),i_c_snd-1)
 !    enddo
 !  enddo
 !  !
 !enddo K_loop_2
 !
 !write (*,*) do_I_send
 !do while(.true.)
 !enddo
 !
 call PP_redux_wait(LIFE_e,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 call PP_redux_wait(LIFE_h,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
 call timing('RT life merge (extra-k)',OPR='stop')
 !
end subroutine RT_Lifetimes_merge
