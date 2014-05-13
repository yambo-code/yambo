!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine PARALLEL_structure()
 !
 use parallel_m,  ONLY:ncpu,myid,n_MPI,INTER_CHAIN,n_chains,INTRA_CHAIN
 implicit none
 !
 ! Work Space 
 !
 integer     ::i_err,ID(ncpu+1),i_cpu,ID_save(ncpu+1),i_chain,n_groups
#if defined _MPI
 include 'mpif.h'
#endif
 !
 if (ncpu==1) then
   INTRA_CHAIN(1)%n_CPU=1
   INTER_CHAIN(1)%n_CPU=1
 endif
 !
#if defined _MPI
 !
 if (ncpu==1) return
 !
 ID=0
 ID_save=0
 !
 n_groups=ncpu/2
 n_MPI=n_groups*2
 !
 ! 2^n_chains = N  =>  n_chains=log_2(N)
 !
 n_chains=log(real(ncpu))/log(2.)
 !
 ! ID = (POOL_id, CHAIN_id(1),...., CHAIN_id(n_chains))
 !
 do i_cpu=1,ncpu
   !
   ID(1)=ID(1)+1 
   ID(2)=ID(2)+1 
   !
   do i_chain=1,n_chains
     ID(2+i_chain)=ID(2+i_chain)+1 ! CHAINS
   enddo
   !
   if (myid+1==i_cpu) then
     !
     do i_chain=1,n_chains
       INTRA_CHAIN(i_chain)%chain_order=i_chain
       INTRA_CHAIN(i_chain)%my_CHAIN=ID(2+i_chain)
       INTRA_CHAIN(i_chain)%n_CPU=ncpu/2**(i_chain)
     enddo
     !
   endif
   !
   do i_chain=1,n_chains
    if (mod(i_cpu,2*n_groups/(ncpu/2**i_chain))==0) ID(2+i_chain)=0
   enddo
   !
   if (mod(i_cpu,2)==0) then
     ID_save(2)=ID(2)
   else
     ID(2)=ID_save(2)
   endif
   !
 enddo
 !
 ! Communicators
 !
 do i_chain=1,n_chains
   !
   call MPI_COMM_SPLIT(mpi_comm_world,INTRA_CHAIN(i_chain)%my_CHAIN,myid,INTRA_CHAIN(i_chain)%COMM,i_err)
   call MPI_COMM_RANK(INTRA_CHAIN(i_chain)%COMM,INTRA_CHAIN(i_chain)%CPU_id,i_err)
   !
   INTER_CHAIN(i_chain)%my_CHAIN=INTRA_CHAIN(i_chain)%my_CHAIN
   INTER_CHAIN(i_chain)%n_CPU=2**(i_chain)
   INTER_CHAIN(i_chain)%chain_order=i_chain
   !
   call MPI_COMM_SPLIT(mpi_comm_world,INTRA_CHAIN(i_chain)%CPU_id,myid,INTER_CHAIN(i_chain)%COMM,i_err)
   call MPI_COMM_RANK(INTER_CHAIN(i_chain)%COMM,INTER_CHAIN(i_chain)%CPU_id,i_err)
   !
 enddo
 !
#endif
 !
end subroutine
