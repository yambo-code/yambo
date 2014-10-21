!
!*********************************************
   MODULE para_module
!*********************************************
   IMPLICIT NONE
   SAVE

#ifdef __MPI
   include 'mpif.h'
#endif

   !
   ! basic variables 
   !  
   integer :: ionode_id = 0
   logical :: ionode = .TRUE.

   !
   ! ... World group (all processors)
   !
   INTEGER :: mpime = 0  ! processor index (starts from 0 to nproc-1)
   INTEGER :: root  = 0  ! index of the root processor
   INTEGER :: nproc = 1  ! number of processors
   INTEGER :: world_comm = 0  ! communicator
   INTEGER :: nproc_file = 1  ! saved on file

   !
   ! ... Pool groups (processors within a pool of k-points)
   !
   INTEGER :: npool       = 1  ! number of "k-points"-pools
   INTEGER :: me_pool     = 0  ! index of the processor within a pool 
   INTEGER :: root_pool   = 0  ! index of the root processor within a pool
   INTEGER :: my_pool_id  = 0  ! index of my pool
   INTEGER :: nproc_pool  = 1  ! number of processors within a pool
   INTEGER :: inter_pool_comm  = 0  ! inter pool communicator
   INTEGER :: intra_pool_comm  = 0  ! intra pool communicator
   INTEGER :: nproc_pool_file  = 1  ! saved on file

   !
   ! communicators
   ! ... ortho (or linear-algebra) groups
   !
   INTEGER :: np_ortho(2) = 1  ! size of the processor grid used in ortho
   INTEGER :: me_ortho(2) = 0  ! coordinates of the processors
   INTEGER :: me_ortho1   = 0  ! task id for the ortho group
   INTEGER :: nproc_ortho = 1  ! size of the ortho group:
   INTEGER :: leg_ortho   = 1  ! the distance in the father communicator
                              ! of two neighbour processors in ortho_comm
   INTEGER :: ortho_comm  = 0  ! communicator for the ortho group
   INTEGER :: ortho_row_comm  = 0  ! communicator for the ortho row group
   INTEGER :: ortho_col_comm  = 0  ! communicator for the ortho col group
   INTEGER :: ortho_comm_id= 0 ! id of the ortho_comm
   INTEGER :: nproc_ortho_file = 1  ! value saved on file
   !
   INTEGER :: me_blacs   =  0  ! BLACS processor index starting from 0
   INTEGER :: np_blacs   =  1  ! BLACS number of processor
   INTEGER :: world_cntx = -1  ! BLACS context of all processor 
   INTEGER :: ortho_cntx = -1  ! BLACS context for ortho_comm

END MODULE para_module

