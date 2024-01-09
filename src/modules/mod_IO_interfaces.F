!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
module IO_int
 !
 implicit none
 !
 ! Interface
 !
 interface 
   !
   ! OLD: def and IO in the same subroutine
   !
   subroutine io_elemental(ID,VAR,VAR_SZ,CHECK,WARN,OP,I0,DB_I0,I1,DB_I1,&
&                          R0,DB_R0,R1,DB_R1,D0,DB_D0,C0,DB_C0,CH0,DB_CH0,CH1,L0,DB_L0,UNIT,MENU,&
&                          DESCRIPTOR,DO_NOT_DUMP,TERMINATOR)
     !
     use pars,       ONLY:SP,DP,lchlen
     use descriptors,ONLY:IO_desc
     integer      :: ID
     character(*), optional :: VAR,OP(:)
     integer,      optional :: VAR_SZ,MENU
     integer,      optional :: I0,DB_I0,I1(:),DB_I1(:)
     real(SP),     optional :: R0,DB_R0,R1(:),DB_R1(:),UNIT
     real(DP),     optional :: D0,DB_D0
     complex(SP),  optional, target :: C0
     complex(SP),  optional :: DB_C0
     logical     , optional :: L0,DB_L0,CHECK,WARN,DO_NOT_DUMP
     character(*), optional :: CH0,DB_CH0,CH1(:),TERMINATOR
     type(IO_desc),optional :: DESCRIPTOR
     !
   end subroutine
   !
   subroutine io_bulk(ID,VAR,VAR_SZ,I0,I1,I2,I3,I4,I5,&
                      &             R0,R1,R2,R3,R4,R5,&
                      &             D0,D1,D2,D3,D4,D5,&
                      &             C0,C1,C2,C3,C4,C5,&
                      &             Z0,Z1,Z2,Z3,Z4,Z5,&
                      &             L0,L1,L2,L3,L4,   &
                      &                            IPOS)
     !
     use pars,    ONLY:SP,DP,LP
     integer      :: ID
     character(*),optional :: VAR
     integer,     optional :: VAR_SZ(:),IPOS(:)
     integer,     optional :: I0,I1(:),I2(:,:),I3(:,:,:),I4(:,:,:,:),I5(:,:,:,:,:)
     real(SP),    optional :: R0,R1(:),R2(:,:),R3(:,:,:),R4(:,:,:,:),R5(:,:,:,:,:)
     real(DP),    optional :: D0,D1(:),D2(:,:),D3(:,:,:),D4(:,:,:,:),D5(:,:,:,:,:)
     complex(SP), optional :: C0,C1(:),C2(:,:),C3(:,:,:),C4(:,:,:,:),C5(:,:,:,:,:)
     complex(DP), optional :: Z0,Z1(:),Z2(:,:),Z3(:,:,:),Z4(:,:,:,:),Z5(:,:,:,:,:)
     integer(LP), optional :: L0,L1(:),L2(:,:),L3(:,:,:),L4(:,:,:,:)
     !
   end subroutine
   !
   integer function io_connect(desc,subfolder,type,no_full_RESET,FORCE_READ_MODE,ID)
     character(*)           :: desc
     integer,      optional :: type,ID
     character(*), optional :: subfolder
     logical,      optional :: no_full_RESET
     logical,      optional :: FORCE_READ_MODE
   end function
   !
   subroutine io_disconnect(ID,FORCE)
     !
     integer           :: ID
     logical, optional :: FORCE
     !
   end subroutine io_disconnect
   !
   subroutine io_control(ACTION,MODE,COM,SEC,ID,COMM,DO_IT)
     use parallel_m,        ONLY:yMPI_comm
     integer,intent(in)                  :: ACTION
     integer,intent(inout)               :: ID
     integer,optional,intent(in)         :: MODE,COM,SEC(:)
     type(yMPI_comm),optional,intent(in) :: COMM
     logical,optional,intent(in)         :: DO_IT
     !
   end subroutine io_control
   !
   !  NEW: def and IO subroutines split in two
   !
   subroutine def_variable_elemental(ID,VAR,VAR_SZ,VAR_KIND,MENU,dim_names,par_io_kind)
     use pars,              ONLY:schlen
     integer     , intent(in) :: ID
     character(*), intent(in) :: VAR
     integer     , intent(in) :: VAR_SZ
     integer     , intent(in) :: VAR_KIND
     integer     , intent(in) :: MENU
     character(schlen), optional, intent(in) :: dim_names(:)
     character(*),      optional, intent(in) :: par_io_kind
   end subroutine def_variable_elemental
   !
   subroutine def_variable_bulk(ID,VAR,VAR_ID,VAR_SZ,VAR_KIND,dim_names,par_io_kind,silent,chunksize)
     use pars,              ONLY:schlen
     integer,      intent(in)  :: ID
     character(*), intent(in)  :: VAR
     integer,      intent(in)  :: VAR_ID
     integer,      intent(in)  :: VAR_SZ(:)
     integer,      intent(in)  :: VAR_KIND
     character(schlen), optional, intent(in) :: dim_names(:)
     character(*),      optional, intent(in) :: par_io_kind
     logical,           optional, intent(in) :: silent
     integer,           optional, intent(in) :: chunksize(:)
   end subroutine def_variable_bulk
   !
   subroutine io_variable_elemental(ID,VAR,CHECK,WARN,OP,     &
        &                       I0, DB_I0, I1, DB_I1, &
        &                       R0, DB_R0, R1, DB_R1, &
        &                       D0, DB_D0, C0, DB_C0, &
        &                       L0, DB_L0,            &
        &                       CH0,DB_CH0,           &
        &                       UNIT,DESCRIPTOR,DO_NOT_DUMP,TERMINATOR)
     !
     use pars,       ONLY:SP,DP,LP,lchlen
     use descriptors,ONLY:IO_desc
     integer               :: ID
     character(*),optional :: VAR
     character(*),optional :: OP(:)
     integer,     optional :: I0,DB_I0,I1(:),DB_I1(:)
     real(SP),    optional :: R0,DB_R0,R1(:),DB_R1(:),UNIT
     real(DP),    optional :: D0,DB_D0
     complex(SP), optional, target :: C0
     complex(SP), optional :: DB_C0
     logical,     optional :: L0,DB_L0,CHECK,WARN,DO_NOT_DUMP
     character(*),optional :: CH0,DB_CH0,TERMINATOR
     type(IO_desc),optional:: DESCRIPTOR
     !
   end subroutine io_variable_elemental
   !
   subroutine io_variable_bulk(ID, VAR_ID, I0, I1, I2, I3, I4, I5,    &
                                        &  R0, R1, R2, R3, R4, R5,    &
                                        &  D0, D1, D2, D3, D4, D5,    &
                                        &  C0, C1, C2, C3, C4, C5, C6,&
                                        &  Z0, Z1, Z2, Z3, Z4, Z5,    &
                                        &  L0, L1, L2, L3, L4,        &
                                        & CH0,CH1,CH2,CH3,CH4,        &
                                        & STR0,STR1,              IPOS)
     !
     use pars,   ONLY:SP,DP,LP
     integer, intent(in)   :: ID, VAR_ID
     integer,     optional, intent(in)    :: IPOS(:)
     integer,     optional, intent(inout) ::  I0, I1(:), I2(:,:), I3(:,:,:), I4(:,:,:,:), I5(:,:,:,:,:)
     real(SP),    optional, intent(inout) ::  R0, R1(:), R2(:,:), R3(:,:,:), R4(:,:,:,:), R5(:,:,:,:,:)
     real(DP),    optional, intent(inout) ::  D0, D1(:), D2(:,:), D3(:,:,:), D4(:,:,:,:), D5(:,:,:,:,:)
     complex(SP), optional, target, intent(inout) ::  C0, C1(:)
     complex(DP), optional, target, intent(inout) ::  Z0, Z1(:)
     complex(SP), optional, target, contiguous, intent(inout) ::  C2(:,:), C3(:,:,:), C4(:,:,:,:), C5(:,:,:,:,:), C6(:,:,:,:,:,:)
     complex(DP), optional, target, contiguous, intent(inout) ::  Z2(:,:), Z3(:,:,:), Z4(:,:,:,:), Z5(:,:,:,:,:)
     integer(LP), optional, intent(inout) ::  L0, L1(:), L2(:,:), L3(:,:,:), L4(:,:,:,:)
     character,   optional, intent(inout) :: CH0,CH1(:),CH2(:,:),CH3(:,:,:),CH4(:,:,:,:)
     character(*),optional, intent(inout) :: STR0,STR1(:)
     !
   end subroutine io_variable_bulk
   !
#if !defined _io_lib
   integer function io_header(ID,QPTS,R_LATT,WF,IMPOSE_SN,TEMP,KPTS,D_LATT,XC_KIND,CUTOFF,GAUGE,&
&                             IMPOSE_GAUGE,ACCEPT_SPIN,PARALLEL_STRUCTURE,FRAG,IMPOSE,NOWARN_SER)
     integer               :: ID
     logical,optional      :: QPTS,R_LATT,WF,IMPOSE_SN,TEMP,KPTS,D_LATT,CUTOFF,GAUGE,IMPOSE_GAUGE,ACCEPT_SPIN,FRAG,IMPOSE,NOWARN_SER
     character(*),optional :: XC_KIND,PARALLEL_STRUCTURE
   end function
   !
   logical function ver_is_gt_or_eq(ID,version,revision)
     integer          :: ID
     integer,optional :: version(3),revision
   end function
   !
   subroutine IO_and_Messaging_switch(what,CONDITION)
     character(*)      :: what
     logical, optional :: CONDITION
   end subroutine
   !
   subroutine io_fragment(ID,ID_frag,i_fragment,j_fragment,ierr)
     integer, intent(in)   :: ID
     integer, intent(out)  :: ID_frag
     integer, optional     :: i_fragment
     integer, optional     :: j_fragment
     integer, optional     :: ierr
   end subroutine
#endif
   !
   function get_name(desc,subfolder,type,CORE_IO,MORE_IO,COM_IO,ALT_JOB_STRING,ALT_JOB_DIR)
     use pars,       ONLY:lchlen
     character(lchlen)      ::get_name
     character(*)           ::desc
     integer                ::type
     logical                ::CORE_IO,MORE_IO,COM_IO
     character(*), optional ::subfolder
     character(*), optional ::ALT_JOB_STRING
     character(*), optional ::ALT_JOB_DIR
   end function get_name
   !
   function all_locations(desc,subfolder)
     use pars,       ONLY:lchlen,num_of_alternative_locations
     character(*)           ::desc
     character(*), optional ::subfolder
     character(lchlen), dimension(num_of_alternative_locations) :: all_locations
   end function all_locations
   !
 end interface
 !
#if defined _HDF5_IO
 interface c_interface
   function nf90_set_log_level(level) bind (C, name = "nc_set_log_level")
     use iso_c_binding
     implicit none
     integer(c_int) :: nf90_set_log_level
     integer(c_int), intent (in) :: level
   end function nf90_set_log_level
 end interface c_interface
#endif
 !
end module IO_int
