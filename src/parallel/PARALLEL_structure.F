!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine PARALLEL_structure(n_elements,ROLE)
 !
 use parallel_m,  ONLY:ncpu,myid,INTER_CHAIN,nchains_group,n_groups,INTRA_CHAIN,&
&                      PARALLEL_CPU_role,PARALLEL_CPU_used,PARALLEL_n_structures_active,&
&                      ncpu_chain,mpi_comm_world,CREATE_the_COMM
 !
 implicit none
 !
 integer        :: n_elements 
 character(*)   :: ROLE(n_elements)
 !
 ! Work Space 
 !
 integer     ::ID(n_elements),i_cpu,i_chain,i_chain_lim(2),chain_now,i_order,&
&              tmp_comm_inter,tmp_comm_intra,i_structure
 !
 if (ncpu==1) then
   INTRA_CHAIN(1)%n_CPU=1
   INTER_CHAIN(1)%n_CPU=1
   return
 endif
 !
#if defined _MPI
 !
 n_groups=n_groups+1
 chain_now=n_groups*2-1
 if(chain_now>1) then
   nchains_group(chain_now)  =nchains_group(chain_now-1)+1
   nchains_group(chain_now+1)=nchains_group(chain_now-1)+n_elements
 else
   nchains_group(chain_now)  =1
   nchains_group(chain_now+1)=n_elements
 endif
 !
 i_chain_lim(1:2)=nchains_group(chain_now:chain_now+1)
 !
 do i_chain=i_chain_lim(1),i_chain_lim(2)
   i_order=i_chain-i_chain_lim(1)+1
   !
   ncpu_chain(i_chain)=1
   do i_structure = 1, PARALLEL_n_structures_active
     if ( trim(PARALLEL_CPU_role(i_structure)) == trim(ROLE(i_order)) ) ncpu_chain(i_chain)=PARALLEL_CPU_used(i_structure)
   enddo
   !
 enddo
 !
 ! I need to define: ID,chain_order,my_chain=ID(1+i_chain),n_CPU
 !
 ID=0
 !
 do i_cpu=1,ncpu
   !
   ID(:)=ID(:)+1 ! CHAINS
   !
   if (myid+1==i_cpu) then
     !
     do i_chain=i_chain_lim(1),i_chain_lim(2)
       i_order=i_chain-i_chain_lim(1)+1
       INTRA_CHAIN(i_chain)%chain_order=i_order
       INTRA_CHAIN(i_chain)%my_CHAIN=ID(i_order)
       INTRA_CHAIN(i_chain)%n_CPU=product(ncpu_chain(i_chain_lim(1):i_chain))
     enddo
     !
   endif
   !
   do i_chain=i_chain_lim(1),i_chain_lim(2)
     i_order=i_chain-i_chain_lim(1)+1
     if ( mod( i_cpu,ncpu/product(ncpu_chain(i_chain_lim(1):i_chain)) )==0) ID(i_order)=0
   enddo
   !
 enddo
 !
 ! INTRA Communicators
 !
 tmp_comm_intra = mpi_comm_world
 !
 do i_chain=i_chain_lim(2),i_chain_lim(1),-1
   i_order=i_chain-nchains_group(chain_now)+1
   !
   call CREATE_the_COMM(tmp_comm_intra,INTRA_CHAIN(i_chain),myid)
   tmp_comm_intra = INTRA_CHAIN(i_chain)%COMM
   !
 enddo
 !
 ! INTER Communicators
 !
 tmp_comm_inter = mpi_comm_world
 !
 do i_chain=i_chain_lim(1),i_chain_lim(2)
   !
   INTER_CHAIN(i_chain)%my_CHAIN   =INTRA_CHAIN(i_chain)%CPU_id
   INTER_CHAIN(i_chain)%n_CPU      =ncpu/product(ncpu_chain(i_chain_lim(1):i_chain))
   INTER_CHAIN(i_chain)%chain_order=i_order
   !
   call CREATE_the_COMM(tmp_comm_inter,INTER_CHAIN(i_chain),myid)
   tmp_comm_inter = INTER_CHAIN(i_chain)%COMM
   !
 enddo
 !
#endif
 !
end subroutine
