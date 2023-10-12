!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine K_stored_in_a_big_matrix(i_BS_mat,iq,what)
 !
 ! Here I fill the kernel (coupling included) in an entire BIG matrix to be
 ! used for diagonalization and/or invertion
 !
 use pars,           ONLY:SP,cI,cZERO,cONE
 use parallel_int,   ONLY:PP_redux_wait
 use BS,             ONLY:BS_K_coupling,n_BS_blks,BS_res_ares_n_mat,&
&                         l_BS_ares_from_res,BS_blks_free
 use BS_solvers,     ONLY:BS_mat,BS_blk,BS_H_dim,BS_K_dim,&
&                         BSS_perturbative_width,run_inversion,run_Haydock,&
&                         BSS_eh_E,BSS_eh_W
 use timing_m,       ONLY:timing
 !
#include <memory.h>
 !
 integer,      intent(in) :: i_BS_mat,iq
 character(*), intent(in) :: what
 !
 ! Work Space
 !
 complex(SP) :: Mij
 integer     :: i_Tk,i_Tp,i_r,i_c,H_pos(2),H_shift(2),i_B,BS_mat_dim
!****************************** SAVIO NEW **********************************
 integer(8),parameter :: max_int_kind4=2147483647
 integer(8)           :: BS_mat_dim_kind8
 logical              :: go_to_kind8
!***************************************************************************
 !
 if (allocated(BS_mat)) return
 !
 call timing('BSE matrix filling',OPR='start')
 !
 if (     BS_K_coupling) BS_mat_dim=BS_H_dim
 if (.not.BS_K_coupling) BS_mat_dim=BS_K_dim(i_BS_mat)
 !
 YAMBO_ALLOC(BS_mat,(BS_mat_dim,BS_mat_dim))
 BS_mat    = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_res_ares_n_mat==2) cycle
   !
   H_shift=0
   if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
   if(BS_blk(i_B)%mode=="A") H_shift(:)=BS_K_dim(1)
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-1
     !
     do i_c=1,BS_blk(i_B)%size(2)
       !
       H_pos(2)=BS_blk(i_B)%coordinate(2)+i_c-1
       !
       ! Then the upper triangle of each block and direct symmetrization
       !
       if (H_pos(1)+H_shift(1)>H_pos(2)+H_shift(2)) cycle
       if (l_BS_ares_from_res.and.H_pos(1)>H_pos(2)) cycle
       !
       Mij=BS_blk(i_B)%mat(i_r,i_c)
       !
       ! Add energies to the diagonal
       !
       if(H_pos(1)+H_shift(1)==H_pos(2)+H_shift(2)) then
         Mij=real(Mij)+BSS_eh_E(H_pos(1)+H_shift(1))*cONE
         if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
           Mij=Mij+cI*BSS_eh_W(H_pos(1)+H_shift(1))
         endif
       endif
       !
       select case(BS_blk(i_B)%mode)
       case("R")
           BS_mat(H_pos(1)            ,H_pos(2)            )=       Mij
           ! The resonant block is hermitial
           BS_mat(H_pos(2)            ,H_pos(1)            )= conjg(Mij)
           if (l_BS_ares_from_res.and.BS_K_coupling) then
             ! The anti-resonant block is A=-R*
             BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)+BS_K_dim(1))=-conjg(Mij)
             ! The anti-resonant block is hermitian
             BS_mat(H_pos(2)+BS_K_dim(1),H_pos(1)+BS_K_dim(1))=      -Mij
           endif
       case("C")
           BS_mat(H_pos(1)            ,H_pos(2)+BS_K_dim(1))=       Mij
           ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian
           BS_mat(H_pos(2)+BS_K_dim(1),H_pos(1)            )=-conjg(Mij)
           if (l_BS_ares_from_res) then
             ! The coupling block and the anti-coupling block are symmetric
             BS_mat(H_pos(2)            ,H_pos(1)+BS_K_dim(1))=       Mij
             BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)            )=-conjg(Mij)
           endif
       case("A")
         ! The anti-resonant block is hermitial
         if(BS_res_ares_n_mat==1) then
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)+BS_K_dim(1))=       Mij
           BS_mat(H_pos(2)+BS_K_dim(1),H_pos(1)+BS_K_dim(1))= conjg(Mij)
         else
           BS_mat(H_pos(1)            ,H_pos(2)            )=       Mij
           BS_mat(H_pos(2)            ,H_pos(1)            )= conjg(Mij)
         endif
       case("Q")
           BS_mat(H_pos(1)+BS_K_dim(1),H_pos(2)            )=       Mij
           ! Coupling from anti-coupling: the whole BSE matrix is Pseudo-HErmitian
           BS_mat(H_pos(2)            ,H_pos(1)+BS_K_dim(1))=-conjg(Mij)
       end select
       !
     enddo
     !
   enddo
   !
 enddo
 !
 call timing('BSE matrix filling',OPR='stop')
 !
 ! ALL 2 ALL
 !===========
 !
 ! BS_mat lives in the entire world.
 !
 call timing('BSE matrix (REDUX)',OPR='start')
! call PP_redux_wait(BS_mat)
!****************************** SAVIO NEW **********************************
 ! 
 ! ------------------ Check for possible integer overflow ------------------
 ! When BS_mat_dim is bigger than 32768 its square can give rise to an
 ! integer overflow error, i.e. its square is bigger than the maximum
 ! integer number that can be stored in a space reserved or a 32 bit
 ! integer data type. [this is integer or integer(4)]. Ideally, we could
 ! use new variables defined with integer(8), but the argument 'count' in
 ! the 'mpi_allreduce' call is only defined as integer(4), and the compiler
 ! will give an error message. The easier solution to this problem is
 ! splitting the two-particle Hamiltonian in several block matrices, whose
 ! dimensionality does not give rise to an integer overflow error.
 ! 
 go_to_kind8=.false.
 BS_mat_dim_kind8=int(BS_mat_dim,8)*int(BS_mat_dim,8)
 if (BS_mat_dim_kind8 > max_int_kind4) go_to_kind8=.true.
 ! 
 if (go_to_kind8) then
   call PP_redux_wait_kind8(BS_mat,BS_mat_dim)
 else
   call PP_redux_wait(BS_mat)
 endif
!***************************************************************************
 call timing('BSE matrix (REDUX)',OPR='stop')
 !
 ! After the folding the %mat's are (in some cases) not needed anymore 
 !
 if (run_Haydock) return
 if (run_inversion.and.what=="DIAGO") return
 !
 call BS_blks_free(i_BS_mat)
 !
end subroutine K_stored_in_a_big_matrix
!****************************** SAVIO NEW **********************************
subroutine PP_redux_wait_kind8(BS_mat,BS_mat_dim)
 ! 
 use pars,           ONLY:SP,DP
 use parallel_m,     ONLY:n_nodes,ncpu
 use parallel_int,   ONLY:PP_redux_wait
 ! 
#include <memory.h>
 ! 
 integer                 :: BS_mat_dim
 complex(SP)             :: BS_mat(BS_mat_dim,BS_mat_dim)
 ! 
 ! Work space
 ! 
 integer                 :: ncpu_per_node,rest,nblocks,n_partitions_space
 integer                 :: i_partition,i_partition_a,i_partition_b
 integer                 :: ii,ii2,ia,ib,dim_a,dim_b,factor,BS_mat_mem
 integer,allocatable     :: BB_mat_dim(:),BB_mat_pos(:)
 real(SP)                :: rnblocks     
 real(SP),parameter      :: mem_per_node=250._SP
 real(SP),parameter      :: byte2gb=1073741824._SP
 complex(SP),allocatable :: BB_mat(:,:)
 ! 
 ncpu_per_node=ncpu/n_nodes(1)
 rest=mod(ncpu,n_nodes(1))
 if (rest /= 0) ncpu_per_node=ncpu_per_node+1
 ! 
 factor=1
 if (SP==DP) factor=2
 BS_mat_mem=8._SP*factor*real(BS_mat_dim)**2/byte2gb
 ! 
 ! The number of block matrices used in the BS_mat splitting also
 ! depends on their size, i.e. on the condition
 ! 
 !                    m(BS_mat) + m(BB_mat) <= m                   [a]
 ! 
 ! where m is the total memory available on the node, m(BS_mat) is
 ! the memory required to allocate the BS_mat array, and where m(BB_mat)
 ! is the memory required to allocate the block matrix BB_mat.
 ! 
 ! Knowing that
 ! 
 ! a) the total memory available on the node can be also defined as
 !    
 !                       m = M(node)/ncpu_per_node
 !    
 !    with M(node) the total memory one can have access to on the node and
 !    ncpu_per_node the number of MPI tasks used per node;
 ! b) the memory m(BB_mat) can be defined as a fraction of m(BS_mat), i.e. 
 !    
 !                     m(BB_mat) = m(BS_mat)/nblocks
 !    
 ! one can easily find the condition
 ! 
 !                         [      m(BS_mat)*ncpu_per_node      ]
 !           nblocks >= Int| --------------------------------- |   [b]
 !                         [ M(node) - m(BS_mat)*ncpu_per_node ]
 ! 
 ! This condition is useful in case of a very large BS_mat matrix, case in
 ! which one needs to split it in very small blocks in order to satisfy
 ! to the condition [a]. In the above condition [b] Int indicates that
 ! function rounding up to the next integer value. In Fortran the function
 ! int takes only the integer part (e.g. int(0.6) gives 0), so that we add 
 ! 1 to the condition [b] to round up to the next integer value.
 ! 
 ! Note that, based on the algorithm used in this routine, the minimum
 ! splitting required to avoid an integer overflow error must use nblocks=4.
 ! 
 rnblocks=ncpu_per_node*BS_mat_mem/(mem_per_node-ncpu_per_node*BS_mat_mem)+1
 nblocks=max(int(rnblocks),49)
 ! 
 ! The number of blocks BB_mat computed above is just indicative. Such a number
 ! must also satisfy to the condition 
 ! 
 !        n_partitions_space = sqrt[nblocks]   "must be an integer" 
 ! 
 ! where n_partitions_space is the space states partitioning number, i.e. the 
 ! number of groups in which the space of the states |k,v,c> is divided. We are
 ! partitioning the square matrix BS_mat in nblocks and the easier algorithm
 ! requires an equal division of both the dimensions. In the following the
 ! routine looks for the closest nblocks number that satisfies the above
 ! condition
 ! 
 do ii=2,100
   ii2=ii*ii
   if (ii2 >= nblocks) then
     nblocks=ii2
     n_partitions_space=ii
     exit
   endif
 enddo
 ! 
 YAMBO_ALLOC(BB_mat_dim,(n_partitions_space))
 YAMBO_ALLOC(BB_mat_pos,(n_partitions_space))
 ! 
 rest=mod(BS_mat_dim,n_partitions_space)
 if (n_partitions_space > 1) then
   BB_mat_dim(1:(n_partitions_space-1))=int(BS_mat_dim/n_partitions_space)
   BB_mat_dim(n_partitions_space)=int(BS_mat_dim/n_partitions_space)+rest
 else
   BB_mat_dim(1)=BS_mat_dim
 endif
 ! 
 BB_mat_pos(1)=1
 do i_partition=2,n_partitions_space
   BB_mat_pos(i_partition)=BB_mat_pos(i_partition-1)+BB_mat_dim(i_partition-1)
 enddo
 ! 
 do i_partition_a=1,n_partitions_space
   do i_partition_b=1,n_partitions_space
     ! 
     dim_a=BB_mat_dim(i_partition_a)
     dim_b=BB_mat_dim(i_partition_b)
     YAMBO_ALLOC(BB_mat,(dim_a,dim_b))
     ! 
     ia=BB_mat_pos(i_partition_a)
     ib=BB_mat_pos(i_partition_b)
     BB_mat(1:dim_a,1:dim_b)=BS_mat(ia:ia+dim_a-1,ib:ib+dim_b-1)
     ! 
     call PP_redux_wait(BB_mat)
     ! 
     BS_mat(ia:ia+dim_a-1,ib:ib+dim_b-1)=BB_mat(1:dim_a,1:dim_b)
     ! 
     YAMBO_FREE(BB_mat)
     !
   enddo
 enddo
 ! 
 YAMBO_FREE(BB_mat_dim)
 YAMBO_FREE(BB_mat_pos)
 ! 
end subroutine PP_redux_wait_kind8
!***************************************************************************
