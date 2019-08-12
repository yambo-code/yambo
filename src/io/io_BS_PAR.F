!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM
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
subroutine io_BS_PAR_init(iq,X,ID)
 !
 ! Intialize variables on BSE netcdf parallel file
 !
 use netcdf
 use parallel_m
 use com,           ONLY:error
 use pars,          ONLY:SP,schlen,lchlen
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_K_dim,BS_K_coupling
 use BS_solvers,    ONLY:variables_BS
 use IO_m,          ONLY:frag_BS_K,write_is_on,VERIFY,&
&                        io_connect,io_elemental,def_variable_bulk,&
&                        io_header,io_mode,io_file
 !
 implicit none
 !
 type(X_t) :: X 
 integer :: iq, ID
 !
 integer :: io_BS,    VAR_IDs(8), n_vars, i1
 integer :: flattened_block_size_dim,block_size_dim
 character(schlen) :: db_name, PARALLEL_STRUCTURE
 character(lchlen) :: local_description(100),ch(4)
 character(schlen) :: dim_names(3), dim_names_done(2)
 !
 !
 ! Connect to parallel netCDF file.
 !
 db_name='BS_PAR_Q'//trim(intc(iq))
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 !
 ! Check if the file already contains data and it its consistent
 PARALLEL_STRUCTURE="NONE"
 if (io_mode(ID)==VERIFY.or.write_is_on(ID)) PARALLEL_STRUCTURE="Response_T_space"
 io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",&
 &                    PARALLEL_STRUCTURE=trim(PARALLEL_STRUCTURE),CUTOFF=.true.,FRAG=frag_BS_K)
 !
 io_BS=variables_BS(ID,iq,local_description,.TRUE.,X=X)
 if (io_BS/=0) then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   call error('Invalid dimensions found in BSE restart file '//trim(io_file(ID))//&
 &            ' Remove the file or change the input file')
 endif
 ! 
 ! Define vars
 !
 ch(1)="BSE_RESONANT"
 ch(2)="BSE_COUPLING"
 ch(3)="BSE_ANTI-RESONANT"
 ch(4)="BSE_ANTI-COUPLING"
 VAR_IDs=(/1,2,3,4,5,6,7,8/)
 !
 if (.not.BS_K_coupling)           n_vars=1
 if (     BS_K_coupling.and.iq==1) n_vars=2
 if (     BS_K_coupling.and.iq/=1) n_vars=4
 !
#ifdef UPLO
 !
 ! This is the upper triangular mode
 !
 dim_names =      [character(schlen) :: 'complex', 'BS_K_flat_dim']
 dim_names_done = [character(schlen) :: 'BS_K_flat_dim']
 flattened_block_size_dim = BS_K_dim*(BS_K_dim+1)/2
 !
 ch=trim(ch)//"_FLAT"
 !
 do i1=1,n_vars
   call def_variable_bulk(ID,trim(ch(i1)),       VAR_IDs(2*i1-1),&
           &               [2,flattened_block_size],SP,dim_names(:2),     par_io_kind='independent')
   call def_variable_bulk(ID,trim(ch(i1))//"_DONE",VAR_IDs(2*i1),&
           &                 [flattened_block_size], 0,dim_names_done(:1),par_io_kind='independent')
 enddo
 !
#else
 !
 ! Here is the 2D mode
 !
 dim_names =      [character(schlen) :: 'complex', 'BS_K_dim', 'BS_K_dim']
 dim_names_done = [character(schlen) :: 'BS_K_dim', 'BS_K_dim']
 !
 do i1=1,n_vars
   call def_variable_bulk(ID,trim(ch(i1)),       VAR_IDs(2*i1-1),&
           &                [2,BS_K_dim,BS_K_dim],SP,dim_names(:3),     par_io_kind='independent')
   call def_variable_bulk(ID,trim(ch(i1))//"_DONE",VAR_IDs(2*i1),&
           &                  [BS_K_dim,BS_K_dim],0 ,dim_names_done(:2),par_io_kind='independent')
 enddo
 !
#endif
 !
end subroutine

subroutine io_BS_PAR_free(ID)
 !
 ! close BSE netcdf parallel file
 !
 use IO_m,          ONLY:io_disconnect
 !
 integer, intent(in)  :: ID
 !
 call io_disconnect(ID)
 !
end subroutine


subroutine io_BS_par_check(iq,ID)
 !
 use pars,          ONLY:schlen
 use parallel_m,    ONLY:MPI_COMM_WORLD
 use stderr,        ONLY:intc
 use BS,            ONLY:l_restart_bse
 use IO_m,          ONLY:io_connect,io_disconnect,io_file
 use com,           ONLY:msg,file_exists
 !
 implicit none
 !
 integer  :: iq,ID
 character(schlen) :: db_name
 !
 integer :: io_BS
 !
 ! Hack to find if the name exists
 ! DS warning. Here if file is found in alternative -J folder code will not work ...
 !
 db_name='BS_PAR_Q'//trim(intc(iq))
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 !
 if (file_exists(io_file(ID))) then
   call msg("s","BSE file found in "//trim(io_file(ID)))
   l_restart_bse = .true.
 else
   call msg("s","BSE file not found. Creating an empty one." )
   l_restart_bse = .false.
 endif
 !
 call io_disconnect(ID)
 !
end subroutine io_BS_par_check


subroutine io_BS_PAR_block(iq,i_block,ID)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use pars,          ONLY:schlen
 use BS,            ONLY:BS_blk
 use timing_m,      ONLY:timing
 !
 implicit none
 !
 integer  :: ID
 !
 ! Work Space
 !
 integer             ::i_block,x1,x2,n1,n2,iq,VAR_IDs(2)
 character(schlen)   ::ch
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 select case(BS_blk(i_block)%mode)
 case("R")
   ch="BSE_RESONANT"
   VAR_IDs=(/1,2/)
 case("C")
   ch="BSE_COUPLING"
   VAR_IDs=(/3,4/)
 case("A")
   ch="BSE_ANTI-RESONANT"
   VAR_IDs=(/5,6/)
 case("Q")
   ch="BSE_ANTI-COUPLING"
   VAR_IDs=(/7,8/)
 end select
 !
 ! Get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 n1=BS_blk(i_block)%size(1)
 n2=BS_blk(i_block)%size(2)
 !  
 call io_BS_PAR_arbitrary_block(ID,ch,VAR_IDs,x1,x2,n1,n2,BS_blk(i_block)%mat,BS_blk(i_block)%done)
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine

subroutine io_BS_PAR_arbitrary_block(ID,ch,VAR_IDs,x1,x2,n1,n2,mat,done)
 !
 ! Given coordinates and dimensions of the blocks write them from the file
 ! (This will be used to create the matrix distributed among the cpus)
 !
 use netcdf
 use pars,          ONLY:SP,schlen
 use BS,            ONLY:BS_K_dim
 use IO_m,          ONLY:io_variable_bulk,io_unit,netcdf_call,read_is_on,write_is_on
 !
 implicit none
 !
 character(schlen)     ::ch
 integer, intent(in)   ::ID,x1,x2,n1,n2,VAR_IDs(2)
 integer               ::idx,jdx,stat,g1,g2,i1,i2
 complex(SP),intent(inout)::mat(n1,n2) 
 character,intent(inout)  ::done(n1,n2)
 ! 
#ifdef UPLO
 !
 ! Write one element at a time in 1D upper triangular
 !
 do i2=1,n2
   do i1=1,n1
     !calculate general index
     g1 = (x1-1) + (i1-1)
     g2 = (x2-1) + (i2-1)
     !ignore elements not in upper triangular part
     if (g1>g2) cycle
     !calculate tridiagonal index
     jdx = g1*BS_K_dim+g2    ! position in full matrix
     idx = jdx-g1*(g1+1)/2+1 ! position in upper triangular matrix
     !write the element 
     !write(*,*) i1,i2,g1,g2,idx
     call io_variable_bulk(ID, VAR_IDs(1),  C1=  mat(i1:i1,i2), IPOS=[1,idx] )
     call io_variable_bulk(ID, VAR_IDs(2), CH1= done(i1:i1,i2), IPOS=[  idx] )
   enddo
 enddo
 !
#else
 !
 ! Write one block at a time in 2D
 !
 call io_variable_bulk(ID, VAR_IDs(1),  C2=mat , IPOS=(/1,x1,x2/) )
 call io_variable_bulk(ID, VAR_IDs(2), CH2=done, IPOS=(/  x1,x2/) )
 !
#endif
 !
 stat = nf90_sync(io_unit(ID)); call netcdf_call(stat,ID)
 !
end subroutine
