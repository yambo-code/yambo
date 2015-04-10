!
!        Copyright (C) 2000-2015 the YAMBO team
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
module parallel_m
 !
 use pars,       ONLY:SP,DP,schlen,lchlen
 !
 implicit none
 !
#if defined _MPI
 include 'mpif.h'
#else
 integer            :: mpi_comm_world=0
#endif
 !
 integer            :: myid
 integer            :: ncpu
 integer            :: n_nodes  = 1
 integer            :: n_MPI    = 1
#if defined _OPENMPI
 integer, parameter :: comm_default_value  = -1
#else
 integer, parameter :: comm_default_value  = 0
#endif
 !
 ! Logicals
 !
 logical            :: l_par_X_T,l_par_X_G_q0,l_par_X_G_all_q,&
&                      l_par_X_G_finite_q,l_par_SE,l_par_RT
 !
 ! In a parallel run only the head CPU in the q and b chains can do 
 ! (b,k) loops
 !
 logical           :: HEAD_QP_cpu=.TRUE.
 logical           :: HEAD_k_cpu =.TRUE.
 !
 ! Communicators
 !
 integer, parameter :: MAX_N_OF_CHAINS=100
 integer            :: n_chains = 1
 !
 ! MPI intra-groups Communicators
 !
 type MPI_comm 
   integer  :: COMM      
   integer  :: CPU_id    
   integer  :: my_CHAIN    ! equivalent to CPU_id+1 in INTER_CHAIN
   integer  :: chain_name  ! INTRA, INTER, CHILD
   integer  :: chain_order ! this corresponds to the order in the global power of 2 hierarchy
   integer  :: n_CPU     
 end type MPI_comm
 ! 
 ! CHAINS 
 !========
 type(MPI_comm),SAVE   :: INTRA_CHAIN(MAX_N_OF_CHAINS) ! COMMUNICATOR among CPUs of the same chain
 type(MPI_comm),SAVE   :: INTER_CHAIN(MAX_N_OF_CHAINS) ! COMMUNICATOR among same CPU (same ID) of different CHAINS 
 type(MPI_comm),SAVE   :: CHILD_CHAIN(MAX_N_OF_CHAINS) ! COMMUNICATOR among CPUs (same ID) of different CHAINS enclosed in 
                                                  ! the above INTER_CHAIN cpu's'
 !
 ! CPU's
 !=======
 type CPU_stru 
   integer              :: N_chains  =1
   integer              :: CPU(MAX_N_OF_CHAINS) = 1
   character(4)         :: ROLE(MAX_N_OF_CHAINS)= " "
   character(schlen)    :: CPU_string  = " "
   character(schlen)    :: ROLE_string = " "
   character(schlen)    :: Long_Description  = " "
   character(schlen)    :: Short_Description = " "
   integer              :: nCPU_diagonalization =0
   integer              :: nCPU_inversion       =0
 end type CPU_stru
 !
 !      if (i_s==1) possible_fields='(k,c,v)'   ! X_q_0
 !      if (i_s==2) possible_fields='(q,k,c,v)' ! X_finite_q
 !      if (i_s==3) possible_fields='(q,k,c,v)' ! X_all_q
 !      if (i_s==4) possible_fields='(k,eh,t)'  ! BS
 !      if (i_s==5) possible_fields='(q,qp,b)'  ! SE
 !      if (i_s==6) possible_fields='(k,b,q,qp)'! RT
 !
 integer,parameter   :: n_CPU_str_max=20
 type(CPU_stru),SAVE :: CPU_structure(n_CPU_str_max)
 !
 !... Running values ...
 !
 integer          :: PARALLEL_CPU_used(MAX_N_OF_CHAINS) = 1
 character(4)     :: PARALLEL_CPU_role(MAX_N_OF_CHAINS) = " "
 integer          :: PARALLEL_n_structures_active = 0
 integer          :: PARALLEL_cpu_mat_inversion = 1
 integer          :: PARALLEL_cpu_mat_diagonalization = 1
 !
 !... Logging CPUs ...
 !
 integer          :: n_log_CPUs = 0
 !
 ! MPI operations
 !
 integer, parameter :: p_sum=1
 integer, parameter :: p_prod=2
 !
 integer, parameter :: max_n_of_cpus=10000
 !
 ! Logicals
 !
 logical            :: IO_write_default(max_n_of_cpus)
 logical            :: master_cpu
 logical            :: l_open_MP
 !
 ! PP indexes
 !
 type PP_indexes 
   logical, pointer :: element_1D(:)     => null()
   logical, pointer :: element_2D(:,:)   => null()
   integer, pointer :: n_of_elements(:)  => null()
   integer, pointer :: weight_1D(:)      => null()
   integer, pointer :: first_of_1D(:)    => null()
 end type PP_indexes
 !
 ! Number of Bands to load
 !=========================
 !
 ! When the PARALLEL_global_index define a distribution common to HF,GW and e-p
 ! it defines a global number of bands to load that overwrites the local values
 !
 integer            :: n_WF_bands_to_load
 !
 ! Number of Response functions
 !==============================
 !
 !1:Xo 2:em1s 3:em1d 4:pp 5:bse
 !
 integer, parameter :: n_parallel_X_types=5
 !
 ! Specific PP indexes ...
 !========================
 !
 ! ... linear algebra
 type(PP_indexes),SAVE :: PAR_IND_INV
 type(PP_indexes),SAVE :: PAR_IND_DIAGO
 !
 ! ... BZ sampling
 type(PP_indexes),SAVE :: PAR_IND_Q
 type(PP_indexes),SAVE :: PAR_IND_Q_bz
 type(PP_indexes),SAVE :: PAR_IND_Kk_ibz
 type(PP_indexes),SAVE :: PAR_IND_Xk_ibz
 type(PP_indexes),SAVE :: PAR_IND_Xk_bz
 type(PP_indexes),SAVE :: PAR_IND_G_k
 !
 ! ... linear response
 type(PP_indexes),SAVE :: PAR_IND_VAL_BANDS_X(n_parallel_X_types)
 type(PP_indexes),SAVE :: PAR_IND_CON_BANDS_X(n_parallel_X_types)
 type(PP_indexes),SAVE :: PAR_IND_DIPk_ibz
 !
 ! ... QP
 type(PP_indexes),SAVE :: PAR_IND_QP
 !
 ! ... Plasma
 type(PP_indexes),SAVE :: PAR_IND_Plasma
 !
 ! ... G bands
 type(PP_indexes),SAVE :: PAR_IND_G_b
 type(PP_indexes),SAVE :: PAR_IND_B_mat
 type(PP_indexes),SAVE :: PAR_IND_Bp_mat
 type(PP_indexes),SAVE :: PAR_IND_B_mat_ordered
 !
 ! ... WF
 type(PP_indexes),SAVE :: PAR_IND_WF_b
 type(PP_indexes),SAVE :: PAR_IND_WF_k
 type(PP_indexes),SAVE :: PAR_IND_WF_b_and_k
 type(PP_indexes),SAVE :: PAR_IND_WF_linear
 !
 ! ... RL vectors
 type(PP_indexes),SAVE :: PAR_IND_RL
 !
 ! ... Transitions
 type(PP_indexes),allocatable,SAVE :: PAR_IND_eh(:)
 type(PP_indexes)            ,SAVE :: PAR_IND_T_groups
 type(PP_indexes)            ,SAVE :: PAR_IND_T_all
 type(PP_indexes)            ,SAVE :: PAR_IND_T_ordered
 !
 ! Specific MPI ID's ...
 !======================
 !
 ! ... linear algebra
 integer            :: PAR_IND_INV_ID
 integer            :: PAR_IND_DIAGO_ID
 !
 ! ... QP
 integer            :: PAR_IND_QP_ID
 !
 ! ... PLASMA
 integer            :: PAR_IND_Plasma_ID
 !
 ! ... G bands
 integer            :: PAR_IND_G_b_ID
 !
 ! ... WF
 integer            :: PAR_IND_WF_b_ID
 integer            :: PAR_IND_WF_k_ID
 !
 ! ... BZ
 integer            :: PAR_IND_Q_ID
 integer            :: PAR_IND_Kk_ibz_ID
 integer            :: PAR_IND_Xk_ibz_ID
 integer            :: PAR_IND_Xk_bz_ID
 integer            :: PAR_IND_G_k_ID
 !
 ! ... linear response & BSK
 integer            :: PAR_IND_VAL_BANDS_X_ID(n_parallel_X_types)
 integer            :: PAR_IND_CON_BANDS_X_ID(n_parallel_X_types)
 !
 ! ... RT
 integer            :: PAR_IND_B_mat_ID
 integer            :: PAR_IND_Bp_mat_ID
 !
 ! ... BSK
 integer            :: PAR_IND_eh_ID
 !
 ! ... RL
 integer            :: PAR_IND_RL_ID
 !
 ! Specific MPI COMMUNICATORS...
 !==============================
 !
 ! ... linear algebra
 type(MPI_comm),SAVE :: PAR_COM_WORLD
 !
 ! ... linear algebra
 type(MPI_comm),SAVE :: PAR_COM_INV_INDEX
 type(MPI_comm),SAVE :: PAR_COM_INV
 type(MPI_comm),SAVE :: PAR_COM_DIAGO_INDEX
 type(MPI_comm),SAVE :: PAR_COM_DIAGO
 !
 ! ... RL vectors
 integer            :: PAR_nRL
 integer,allocatable:: PAR_RL_index(:)
 !
 ! ... QP
 type(MPI_comm),SAVE :: PAR_COM_QP_INDEX
 type(MPI_comm),SAVE :: PAR_COM_QP_A2A
 !
 ! ... Plasma
 type(MPI_comm),SAVE :: PAR_COM_Plasma_INDEX
 !
 ! ... G bands
 type(MPI_comm),SAVE :: PAR_COM_G_b_INDEX
 type(MPI_comm),SAVE :: PAR_COM_G_b_A2A
 !
 ! ... WF
 type(MPI_comm),SAVE :: PAR_COM_WF_k_A2A
 type(MPI_comm),SAVE :: PAR_COM_WF_k_INDEX
 type(MPI_comm),SAVE :: PAR_COM_WF_b_INDEX
 !
 ! ... BZ
 type(MPI_comm),SAVE :: PAR_COM_Q_INDEX
 type(MPI_comm),SAVE :: PAR_COM_Q_A2A
 type(MPI_comm),SAVE :: PAR_COM_Xk_ibz_INDEX
 type(MPI_comm),SAVE :: PAR_COM_Xk_ibz_A2A
 type(MPI_comm),SAVE :: PAR_COM_Xk_bz_INDEX
 type(MPI_comm),SAVE :: PAR_COM_Xk_bz_A2A
 !
 ! ... and derived variables
 integer            :: PAR_nPlasma
 integer,allocatable:: PAR_Plasma_index(:)
 integer            :: PAR_nQP
 integer,allocatable:: PAR_QP_index(:)
 integer            :: PAR_n_B_mat_elements
 integer,allocatable:: PAR_B_mat_index(:,:)
 integer            :: PAR_n_Bp_mat_elements
 integer,allocatable:: PAR_Bp_mat_index(:,:)
 integer            :: PAR_nQ
 integer,allocatable:: PAR_Q_index(:)
 integer            :: PAR_nQ_bz
 integer,allocatable:: PAR_Q_bz_index(:)
 integer            :: PAR_Kk_nibz
 integer            :: PAR_Xk_nibz
 integer,allocatable:: PAR_Xk_ibz_index(:)
 integer            :: PAR_DIPk_nibz
 integer,allocatable:: PAR_DIPk_ibz_index(:)
 integer            :: PAR_Xk_nbz
 integer,allocatable:: PAR_Xk_bz_index(:)
 integer            :: PAR_BS_nT_col_grps 
 integer,allocatable:: PAR_BS_T_grps_index(:)
 integer            :: PAR_nG_bands 
 integer,allocatable:: PAR_G_bands_index(:)
 !
 ! ... linear response & BSK
 type(MPI_comm),SAVE :: PAR_COM_VAL_INDEX(n_parallel_X_types)
 type(MPI_comm),SAVE :: PAR_COM_CON_INDEX(n_parallel_X_types)
 type(MPI_comm),SAVE :: PAR_COM_q_for_Xo
 type(MPI_comm),SAVE :: PAR_COM_k_for_P
 !
 ! ... BSK
 type(MPI_comm),SAVE :: PAR_COM_eh_A2A
 type(MPI_comm),SAVE :: PAR_COM_eh_INDEX
 type(MPI_comm),SAVE :: PAR_COM_T_INDEX
 !
 ! ... density
 type(MPI_comm),SAVE :: PAR_COM_density
 !
 ! WorkSpace
 !
 integer            :: i_err
 integer, private   :: local_type
 !
 interface PP_redux_wait
   module procedure l1share,i1share,i18share,i2share,i3share,&
&                   r0share,r1share,r2share,r3share,&
&                   c0share,c1share,c2share,c3share,c4share,ch0share,PARALLEL_wait
#if ! defined _DOUBLE
   module procedure d0share,d1share,d2share
#endif
 end interface PP_redux_wait
 !
 interface pp_bcast
   module procedure c0bcast,c1bcast,c2bcast
 end interface pp_bcast
 !
 contains
   !
   character(lchlen) function PARALLEL_message(i_s)
     use stderr, ONLY:intc
     use openmp, ONLY:n_threads_X,n_threads_SE,n_threads_RT,n_threads_DIP,n_threads_K,n_threads
     integer :: i_s
     !
     PARALLEL_message=" "
     !
#if !defined _MPI && !defined _OPENMP && !defined _OPENMPI
     return
#endif
     if (i_s>0) then
       if (len_trim(CPU_structure(i_s)%CPU_string)==0) return
     endif
     !
     if (i_s==0) then
       PARALLEL_message=trim(intc(ncpu))//"(CPU)"
       if (n_threads>0) PARALLEL_message=trim(PARALLEL_message)//"-"//trim(intc(n_threads))//"(threads)"
       if (n_threads_X>0) PARALLEL_message=trim(PARALLEL_message)//"-"//trim(intc(n_threads_X))//"(threads@X)"
       if (n_threads_DIP>0) PARALLEL_message=trim(PARALLEL_message)//"-"//trim(intc(n_threads_DIP))//"(threads@DIP)"
       if (n_threads_SE>0) PARALLEL_message=trim(PARALLEL_message)//"-"//trim(intc(n_threads_SE))//"(threads@SE)"
       if (n_threads_RT>0) PARALLEL_message=trim(PARALLEL_message)//"-"//trim(intc(n_threads_RT))//"(threads@RT)"
       if (n_threads_K>0) PARALLEL_message=trim(PARALLEL_message)//"-"//trim(intc(n_threads_K))//"(threads@K)"
     else
       PARALLEL_message=trim(CPU_structure(i_s)%Short_Description)//"(environment)-"//&
&                       trim(CPU_structure(i_s)%CPU_string)//"(CPUs)-"//&
&                       trim(CPU_structure(i_s)%ROLE_string)//"(ROLEs)"
     endif
     !
   end function
   !
   subroutine CPU_str_reset()
     CPU_structure(1)%Long_Description="Response_G_space_Zero_Momentum"
     CPU_structure(1)%Short_Description="X_q_0"
     CPU_structure(2)%Long_Description="Response_G_space_Finite_Momentum"
     CPU_structure(2)%Short_Description="X_finite_q"
     CPU_structure(3)%Long_Description="Response_G_space"
     CPU_structure(3)%Short_Description="X_all_q"
     CPU_structure(4)%Long_Description="Response_T_space"
     CPU_structure(4)%Short_Description="BS"
     CPU_structure(5)%Long_Description="Self_Energy"
     CPU_structure(5)%Short_Description="SE"
     CPU_structure(6)%Long_Description="Real_Time"
     CPU_structure(6)%Short_Description="RT"
   end subroutine
   !
   subroutine COMM_reset(COMM)
     type(MPI_comm):: COMM
     COMM%n_CPU      =1
     COMM%COMM       = comm_default_value
     COMM%my_CHAIN   =1
     COMM%chain_order=n_chains
     COMM%CPU_ID     =0
   end subroutine
   !
   subroutine COMM_copy(COMM_in,COMM_out)
     type(MPI_comm):: COMM_in,COMM_out
     COMM_out%n_CPU      =COMM_in%n_CPU
     COMM_out%my_CHAIN   =COMM_in%my_CHAIN
     COMM_out%chain_order=COMM_in%chain_order
     COMM_out%COMM       =COMM_in%COMM
     COMM_out%CPU_ID     =COMM_in%CPU_ID
   end subroutine
   !
   subroutine PAR_INDEX_copy(IND_in,IND_out)
     type(PP_indexes):: IND_in,IND_out
     integer :: dim_
     if (associated(IND_in%n_of_elements)) then
       dim_=size(IND_in%n_of_elements)
       allocate(IND_out%n_of_elements(dim_))
       IND_out%n_of_elements=IND_in%n_of_elements
     endif
     if (associated(IND_in%element_1D)) then
       dim_=size(IND_in%element_1D)
       allocate(IND_out%element_1D(dim_))
       IND_out%element_1D=IND_in%element_1D
     endif
     if (associated(IND_in%weight_1D)) then
       dim_=size(IND_in%weight_1D)
       allocate(IND_out%weight_1D(dim_))
       IND_out%weight_1D=IND_in%weight_1D
     endif
     if (associated(IND_in%first_of_1D)) then
       dim_=size(IND_in%first_of_1D)
       allocate(IND_out%first_of_1D(dim_))
       IND_out%first_of_1D=IND_in%first_of_1D
     endif
   end subroutine
   !
   subroutine MPI_close
     implicit none
#if defined _MPI
     if (ncpu>1) then
       call mpi_barrier(mpi_comm_world,i_err)
       call mpi_finalize(i_err)
     endif
#endif
     stop
   end subroutine
   !
   subroutine PP_indexes_reset(ip)
     type(PP_indexes)::ip
     if(associated(ip%element_1D))    deallocate(ip%element_1D)
     if(associated(ip%element_2D))    deallocate(ip%element_2D)
     if(associated(ip%weight_1D))     deallocate(ip%weight_1D)
     if(associated(ip%n_of_elements)) deallocate(ip%n_of_elements)
     if(associated(ip%first_of_1D))   deallocate(ip%first_of_1D)
     nullify(ip%element_1D,ip%element_2D,ip%n_of_elements,ip%weight_1D,ip%first_of_1D)
   end subroutine
   !
   subroutine PARALLEL_wait(COMM)
     integer, optional :: COMM
#if defined _MPI
     integer :: local_COMM
     if (ncpu==1) return
     !
     local_COMM=mpi_comm_world
     if (present(COMM)) then
       local_COMM=COMM
     endif
     if (local_COMM==comm_default_value) local_COMM=mpi_comm_world
     call mpi_barrier(local_COMM,i_err)
#endif
   end subroutine
   !
   subroutine l1share(array,imode,COMM)
     logical   :: array(:)
     integer, optional :: imode,COMM
#if defined _MPI
     integer ::omode,LOCAL_COMM
     integer ::dimensions(1),dimension ! Work Space
     logical,allocatable::larray(:)    ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     call mpi_barrier(LOCAL_COMM,i_err)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=.FALSE.
     call mpi_allreduce(array(1),larray,dimension,mpi_logical,omode,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine i1share(array,imode,COMM)
     integer(4):: array(:)
     integer, optional :: imode,COMM
#if defined _MPI
     integer ::omode,LOCAL_COMM
     integer ::dimensions(1),dimension ! Work Space
     integer,allocatable::larray(:)    ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     call mpi_barrier(LOCAL_COMM,i_err)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0
     call mpi_allreduce(array(1),larray,dimension,mpi_integer,omode,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine i18share(array,imode,COMM)
     integer(8)        :: array(:)
     integer, optional :: imode,COMM
#if defined _MPI
     integer :: omode,LOCAL_COMM
     integer::dimensions(1),dimension  !Work Space
     integer(8),allocatable::larray(:) !Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     call mpi_barrier(LOCAL_COMM,i_err)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0
     call mpi_allreduce(array(1),larray,dimension,mpi_integer8,omode,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine i2share(array,COMM)
     integer :: array(:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(2),dimension,LOCAL_COMM  ! Work Space
     integer,allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0
     call mpi_allreduce(array(1,1),larray,dimension,mpi_integer,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine i3share(array,COMM)
     integer:: array(:,:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(3),dimension,LOCAL_COMM  ! Work Space
     integer,allocatable::larray(:) ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0
     call mpi_allreduce(array(1,1,1),larray,dimension,mpi_integer,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine ch0share(chval,COMM)
     character(lchlen) :: chval
     integer, optional :: COMM
#if defined _MPI
     integer          :: LOCAL_COMM   ! Work Space
     character(lchlen):: local_chval  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     local_chval=' '
     call mpi_allreduce(chval,local_chval,lchlen,MPI_CHARACTER,mpi_sum,LOCAL_COMM,i_err)
     chval=local_chval
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine r0share(rval,imode,COMM)
     real(SP)          :: rval
     integer, optional :: imode,COMM
#if defined _MPI
     integer :: omode,LOCAL_COMM  ! Work Space
     real(SP):: local_rval  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_REAL
     if (SP==DP) local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     local_rval=0.
     call mpi_allreduce(rval,local_rval,1,local_type,omode,LOCAL_COMM,i_err)
     rval=local_rval
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine r1share(array,COMM)
     real(SP) :: array(:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(1),dimension,LOCAL_COMM ! Work Space
     real(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_REAL
     if (SP==DP) local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine r2share(array,COMM)
     real(SP) :: array(:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(2),dimension,LOCAL_COMM  ! Work Space
     real(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_REAL
     if (SP==DP) local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1,1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine r3share(array,COMM)
     real(SP):: array(:,:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(3),dimension,LOCAL_COMM  ! Work Space
     real(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_REAL
     if (SP==DP) local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1,1,1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine d0share(rval,imode,COMM)
     real(DP)          :: rval
     integer, optional :: imode,COMM
#if defined _MPI
     integer :: omode,LOCAL_COMM  ! Work Space
     real(DP):: local_rval  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     local_rval=0.
     call mpi_allreduce(rval,local_rval,1,local_type,omode,LOCAL_COMM,i_err)
     rval=local_rval
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine d1share(array,COMM)
     real(DP) :: array(:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(1),dimension,LOCAL_COMM ! Work Space
     real(DP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine d2share(array,COMM)
     real(DP) :: array(:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(2),dimension,LOCAL_COMM  ! Work Space
     real(DP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_DOUBLE_PRECISION
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1,1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c0share(cval,imode,COMM)
     complex(SP)       :: cval
     integer, optional :: imode,COMM
#if defined _MPI
     integer :: omode,LOCAL_COMM  ! Work Space
     complex(SP):: local_cval  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     local_cval=0.
     call mpi_allreduce(cval,local_cval,1,local_type,omode,LOCAL_COMM,i_err)
     cval=local_cval
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c1share(array,COMM)
     complex(SP):: array(:)
     integer, optional :: COMM
#if defined _MPI
     integer  :: LOCAL_COMM
     complex(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     allocate(larray(size(array)))
     larray=(0.,0.)
     call mpi_allreduce(array(1),larray,size(array),local_type,mpi_sum,LOCAL_COMM,i_err)
     array=larray
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c2share(array,COMM)
     complex(SP):: array(:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(2),dimension,LOCAL_COMM  ! Work Space
     complex(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=(0.,0.)
     call mpi_allreduce(array(1,1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c3share(array,COMM)
     complex(SP):: array(:,:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(3),dimension,LOCAL_COMM  ! Work Space
     complex(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1,1,1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c4share(array,COMM)
     complex(SP):: array(:,:,:,:)
     integer, optional :: COMM
#if defined _MPI
     integer::dimensions(4),dimension,LOCAL_COMM  ! Work Space
     complex(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     if (LOCAL_COMM==comm_default_value) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(LOCAL_COMM,i_err)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0.
     call mpi_allreduce(array(1,1,1,1),larray,dimension,local_type,mpi_sum,LOCAL_COMM,i_err)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c0bcast(cval,node,COMM)
     complex(SP):: cval
     integer, intent(in) :: node
     integer, optional :: COMM
#if defined _MPI
     integer::LOCAL_COMM  ! Work Space  
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_bcast(cval,1,local_type,node,LOCAL_COMM,i_err)
#endif
   end subroutine
   !
   subroutine c1bcast(array,node,COMM)
     complex(SP):: array(:)
     integer, intent(in) :: node
     integer, optional :: COMM
#if defined _MPI
     integer::LOCAL_COMM  ! Work Space  
     if (ncpu==1) return
     !
     if (present(COMM)) then
       LOCAL_COMM=COMM
     else
       LOCAL_COMM=mpi_comm_world
     endif
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_bcast(array(1),size(array),local_type, node,LOCAL_COMM, i_err)
#endif
   end subroutine
   !
   subroutine c2bcast(array,node)
     complex(SP):: array(:,:)
     integer, intent(in) :: node
#if defined _MPI
     if (ncpu==1) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_bcast(array(1,1),size(array),local_type, node,mpi_comm_world , i_err)
#endif
   end subroutine
   !
end module parallel_m
