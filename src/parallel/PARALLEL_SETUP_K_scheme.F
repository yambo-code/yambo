!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine PARALLEL_SETUP_K_scheme(PAR_K_scheme_kind)
 !
 use R_lattice,    ONLY:nXkibz,nXkbz
 use parallel_m,   ONLY:COMM_copy,PAR_INDEX_copy,COMM_reset,PP_indexes_reset,PAR_K_scheme
 ! DIP
 use parallel_m,  ONLY:PAR_COM_DIPk_ibz_INDEX,PAR_COM_DIPk_ibz_A2A,PAR_IND_DIPk_ibz,PAR_IND_DIPk_bz,&
 &                     PAR_DIPk_ibz_index,PAR_DIPk_bz_index,PAR_DIPk_nibz,PAR_DIPk_nbz,             &
 &                     PAR_COM_BZINDXk_ibz_INDEX,PAR_IND_BZINDXk_ibz,PAR_IND_BZINDXk_bz,            &
 &                     PAR_BZINDXk_ibz_index,PAR_BZINDXk_bz_index,PAR_BZINDXk_nibz,PAR_BZINDXk_nbz, &
 &                     PAR_COM_BZINDXk_ibz_A2A,mpi_comm_world
 ! X
 use parallel_m,  ONLY:PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_IND_Xk_ibz,PAR_IND_Xk_bz,&
 &                     PAR_Xk_ibz_index,PAR_Xk_bz_index,PAR_Xk_nibz,PAR_Xk_nbz,             &
 &                     PAR_COM_X_WORLD
 !
 implicit none
 !
 character(*),  intent(in)    :: PAR_K_scheme_kind
 !
 integer :: i1
 !
 call COMM_reset(PAR_K_scheme%COM_ibz_INDEX)
 call COMM_reset(PAR_K_scheme%COM_ibz_A2A)
 call PP_indexes_reset(PAR_K_scheme%IND_ibz)
 call PP_indexes_reset(PAR_K_scheme%IND_bz)
 if (allocated(PAR_K_scheme%ibz_index)) deallocate(PAR_K_scheme%ibz_index)
 if (allocated(PAR_K_scheme%bz_index))  deallocate(PAR_K_scheme%bz_index)
 !
 select case(trim(PAR_K_scheme_kind))
 case ("BZINDX")
   call COMM_copy(PAR_COM_BZINDXk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_BZINDXk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_BZINDXk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_BZINDXk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_BZINDXk_nibz
   PAR_K_scheme%nbz =PAR_BZINDXk_nbz
   if (allocated(PAR_BZINDXk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_BZINDXk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_BZINDXk_ibz_index
   endif
   if (allocated(PAR_BZINDXk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_BZINDXk_bz_index)))
     PAR_K_scheme%bz_index=PAR_BZINDXk_bz_index
   endif
   PAR_K_scheme%comm_world=mpi_comm_world
 case ("DIP","RT","SC","QED","YPP")
   call COMM_copy(PAR_COM_DIPk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_DIPk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_DIPk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_DIPk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_DIPk_nibz
   PAR_K_scheme%nbz =PAR_DIPk_nbz
   if (allocated(PAR_DIPk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_DIPk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_DIPk_ibz_index
   endif
   if (allocated(PAR_DIPk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_DIPk_bz_index)))
     PAR_K_scheme%bz_index=PAR_DIPk_bz_index
   endif
   PAR_K_scheme%comm_world=mpi_comm_world
 case ("X","NL")
   call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_Xk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_Xk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_Xk_nibz
   PAR_K_scheme%nbz =PAR_Xk_nbz
   if (allocated(PAR_Xk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_Xk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_Xk_ibz_index
   endif
   if (allocated(PAR_Xk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_Xk_bz_index)))
     PAR_K_scheme%bz_index=PAR_Xk_bz_index
   endif
   PAR_K_scheme%comm_world=PAR_COM_X_WORLD%COMM
 case ("K")
   call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_K_scheme%COM_ibz_INDEX)
   call COMM_copy(PAR_COM_Xk_ibz_A2A,  PAR_K_scheme%COM_ibz_A2A)
   call PAR_INDEX_copy(PAR_IND_Xk_ibz,PAR_K_scheme%IND_ibz)
   call PAR_INDEX_copy(PAR_IND_Xk_bz, PAR_K_scheme%IND_bz )
   PAR_K_scheme%nibz=PAR_Xk_nibz
   PAR_K_scheme%nbz =PAR_Xk_nbz
   if (allocated(PAR_Xk_ibz_index)) then
     allocate(PAR_K_scheme%ibz_index(size(PAR_Xk_ibz_index)))
     PAR_K_scheme%ibz_index=PAR_Xk_ibz_index
   endif
   if (allocated(PAR_Xk_bz_index)) then
     allocate(PAR_K_scheme%bz_index(size(PAR_Xk_bz_index)))
     PAR_K_scheme%bz_index=PAR_Xk_bz_index
   endif
   PAR_K_scheme%comm_world=mpi_comm_world
 case ("Kdef")
   PAR_K_scheme%COM_ibz_INDEX%n_CPU=1
   PAR_K_scheme%COM_ibz_INDEX%CPU_id=0
   PAR_K_scheme%COM_ibz_A2A%CPU_id=0
   allocate(PAR_K_scheme%IND_ibz%element_1d(nXkibz))
   allocate(PAR_K_scheme%IND_bz%element_1d(nXkbz))
   PAR_K_scheme%IND_ibz%element_1d=.true.
   PAR_K_scheme%IND_bz%element_1d=.true.
   PAR_K_scheme%nibz=nXkibz
   PAR_K_scheme%nbz =nXkbz
   allocate(PAR_K_scheme%ibz_index(nXkibz))
   do i1=1,nXkibz
     PAR_K_scheme%ibz_index(i1)=i1
   enddo
   allocate(PAR_K_scheme%bz_index(nXkbz))
   do i1=1,nXkbz
     PAR_K_scheme%bz_index(i1)=i1
   enddo
   PAR_K_scheme%comm_world=mpi_comm_world
 end select
 !
end subroutine PARALLEL_SETUP_K_scheme
