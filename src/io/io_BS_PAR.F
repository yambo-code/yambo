!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use com,           ONLY:msg,get_name,error,file_exists
 use pars,          ONLY:SP,schlen,lchlen,nf90_SP
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_K_dim, l_restart_bse, BS_K_coupling
 use IO_m,          ONLY:frag_BS_K,write_is_on,VERIFY,netcdf_call,io_action,OP_WR,&
&                        io_connect,io_disconnect,io_elemental,def_variable_bulk,&
&                        io_header,io_mode,io_unit,io_netcdf_var,io_file,io_control,RD_CL,OP_WR_CL
 use BS_solvers,    ONLY:variables_BS
 implicit none
!#define UPLO
#if defined _DOUBLE
#define NF_PUT_VARA nf_put_vara_double
#define NF_GET_VARA nf_get_vara_double
#else
#define NF_PUT_VARA nf_put_vara_real
#define NF_GET_VARA nf_get_vara_real
#endif
 include 'netcdf.inc'
 !
 type(X_t) :: X 
 integer :: io_BS, iq, mode_flag, stat, ID, dims(3)
 integer :: flattened_block_size_dim,block_size_dim,complex_dim
 character(schlen) :: db_name, file_to_use, PARALLEL_STRUCTURE
 character(lchlen) :: local_description(100)
 character(schlen) :: dim_names(3), dim_names_done(2)
 !
 ! Connect to parallel netCDF file.
 !
 db_name='BS_Q'//trim(intc(iq))//'.nc'
 ! Hack to find if the name exists
 call io_control(ACTION=RD_CL,ID=ID)
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 if (file_exists(io_file(ID))) then
    call msg("s","BSE file found in "//trim(io_file(ID)))
    l_restart_bse = .true.
 else
    call msg("s","BSE file not found. Creating an empty one." )
 endif
 call io_disconnect(ID)
 !
 call io_control(ACTION=OP_WR_CL,ID=ID)
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 !
 ! Check if the file already contains data and it its consistent
 PARALLEL_STRUCTURE="NONE"
 if (io_mode(ID)==VERIFY.or.write_is_on(ID)) PARALLEL_STRUCTURE="Response_T_space"
 io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",&
 &                    PARALLEL_STRUCTURE=trim(PARALLEL_STRUCTURE),CUTOFF=.true.,FRAG=frag_BS_K)
 !
 io_BS=variables_BS(ID,local_description,X=X)
 if (io_BS/=0) then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   call error('Invalid dimensions found in BSE restart file '//trim(io_file(ID))//&
 &            ' Remove the file or change the input file')
 endif
 ! 
 ! Define vars
 !
#ifdef UPLO
 !
 ! This is the upper triangular mode
 !
 dim_names =      [character(schlen) :: 'complex', 'BS_K_flat_dim']
 dim_names_done = [character(schlen) :: 'BS_K_flat_dim']
 flattened_block_size_dim = BS_K_dim*(BS_K_dim+1)/2
 call def_variable_bulk( ID, "BSE_RESONANT_FLAT", [2,flattened_block_size], SP, dim_names(:2), par_io_kind='independent')
 call def_variable_bulk( ID, "BSE_RESONANT_FLAT_DONE", [flattened_block_size], 0, dim_names_done(:1), par_io_kind='independent')
 !
 if (BS_K_coupling) then
   call def_variable_bulk( ID, "BSE_COUPLING_FLAT", [2,flattened_block_size], SP, dim_names(:2), par_io_kind='independent')
   call def_variable_bulk( ID, "BSE_COUPLING_FLAT_DONE", [flattened_block_size], 0, dim_names_done(:1), par_io_kind='independent')
 end if
 !
#else
 !
 ! Here is the 2D mode
 !
 dim_names =      [character(schlen) :: 'complex', 'BS_K_dim', 'BS_K_dim']
 dim_names_done = [character(schlen) :: 'BS_K_dim', 'BS_K_dim']
 call def_variable_bulk( ID, "BSE_RESONANT", [2,BS_K_dim,BS_K_dim], SP, dim_names(:3), par_io_kind='independent')
 call def_variable_bulk( ID, "BSE_RESONANT_DONE", [BS_K_dim,BS_K_dim], 0, dim_names_done(:2), par_io_kind='independent')
 !
 if (BS_K_coupling) then
   call def_variable_bulk( ID, "BSE_COUPLING", [2,BS_K_dim,BS_K_dim], SP, dim_names(:3), par_io_kind='independent')
   call def_variable_bulk( ID, "BSE_COUPLING_DONE", [BS_K_dim,BS_K_dim], 0, dim_names_done(:2), par_io_kind='independent')
 end if
 !
#endif
 !
end subroutine

subroutine io_BS_PAR_free(ID)
 !
 ! close BSE netcdf parallel file
 !
 use netcdf
 use IO_m,          ONLY:io_disconnect,io_unit
 include 'netcdf.inc'
 !
 integer  :: ID, stat
 !
 stat = nf_close(io_unit(ID))
 call io_disconnect(ID)
 !
end subroutine


subroutine io_BS_PAR_write_block(iq,i_block,ID)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use pars,          ONLY:SP,schlen,lchlen
 use X_m,           ONLY:X_t
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_blk
 use IO_m,          ONLY:io_bulk,io_fragment,IO_NOT_ALLOWED,manage_OP_IF_START_and_CL_IF_END
 use timing_m,      ONLY:timing
 implicit none
 !
 integer  :: ID
 !
 ! Work Space
 !
 integer             ::i_block,x1,x2,n1,n2,iq
 character(schlen)   ::ch
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 if (BS_blk(i_block)%mode=="R") ch="BSE_RESONANT"
 if (BS_blk(i_block)%mode=="A") ch="BSE_COUPLING"
 !
 ! Get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 n1=BS_blk(i_block)%size(1)
 n2=BS_blk(i_block)%size(2)
 !  
 call io_BS_PAR_write_arbitrary_block(ID,ch,x1,x2,n1,n2,BS_blk(i_block)%mat,BS_blk(i_block)%done)
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine

subroutine io_BS_PAR_write_arbitrary_block(ID,ch,x1,x2,n1,n2,mat,done)
 !
 ! Given coordinates and dimensions of the blocks write them from the file
 ! (This will be used to create the matrix distributed among the cpus)
 !
 use netcdf
 use pars,          ONLY:SP,schlen
 use BS,            ONLY:BS_K_dim
 use IO_m,          ONLY:io_unit, io_netcdf_var, netcdf_call
 implicit none
 include 'netcdf.inc'
 !
 character(schlen)     ::ch
 integer, intent(in)   ::ID,x1,x2,n1,n2
 integer               ::idx,jdx,stat,g1,g2,i1,i2,matid,doneid
 complex(SP),intent(in)::mat(n1,n2) 
 character,intent(in)  ::done(n1,n2)
 ! 
#ifdef UPLO
 !
 ! Write one element at a time in 1D upper triangular
 !
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_FLAT',matid); call netcdf_call(stat,ID)
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_FLAT_DONE',doneid); call netcdf_call(stat,ID)
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
     stat = NF_PUT_VARA(io_unit(ID), matid, [1,idx], [2,1], mat(i1,i2))
     stat = nf_put_vara_text(io_unit(ID), doneid, [idx], [1], done(i1,i2))
   enddo
 enddo
#else
 !
 ! Write one block at a time in 2D
 !
 stat = nf90_inq_varid(io_unit(ID),ch,matid); call netcdf_call(stat,ID)
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_DONE',doneid); call netcdf_call(stat,ID)
 !
 stat = NF_PUT_VARA(io_unit(ID), matid, [1,x1,x2], [2,n1,n2], mat); call netcdf_call(stat,ID)
 stat = nf_put_vara_text(io_unit(ID), doneid, [x1,x2], [n1,n2], done); call netcdf_call(stat,ID)
 !
#endif
 !
 stat = nf_sync(io_unit(ID)); call netcdf_call(stat,ID)
 !
end subroutine

subroutine io_BS_PAR_read_block(iq,i_block,ID)
 !
 ! Read one block of the BSE matrix to disk
 !
 use netcdf
 use pars,          ONLY:SP,schlen,lchlen
 use X_m,           ONLY:X_t
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_blk,BS_K_dim
 use IO_m,          ONLY:io_elemental,io_bulk,netcdf_call,&
&                        read_is_on,write_is_on,io_unit,io_netcdf_var,VERIFY, &
&                        io_fragment,IO_NOT_ALLOWED,manage_OP_IF_START_and_CL_IF_END
 use timing_m,      ONLY:timing
 implicit none
 include 'netcdf.inc'
 !
 integer  :: ID,stat
 !
 ! Work Space
 !
 integer             ::i_block,i1,i2,n1,n2,idx,jdx,g1,g2,x1,x2,iq
 character(schlen)   ::ch
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 if (BS_blk(i_block)%mode=="R") ch="BSE_RESONANT"
 if (BS_blk(i_block)%mode=="A") ch="BSE_COUPLING"
 !
 ! Get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 n1=BS_blk(i_block)%size(1)
 n2=BS_blk(i_block)%size(2)
 !  
 call io_BS_PAR_read_arbitrary_block(ID,ch,x1,x2,n1,n2,BS_blk(i_block)%mat,BS_blk(i_block)%done)
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine

subroutine io_BS_PAR_read_arbitrary_block(ID,ch,x1,x2,n1,n2,mat,done)
 !
 ! Given coordinates and dimensions of the blocks read them from the file
 ! (This will be used to create the matrix distributed among the cpus)
 !
 use netcdf
 use pars,          ONLY:SP,schlen
 use BS,            ONLY:BS_K_dim
 use IO_m,          ONLY:io_unit, io_netcdf_var, netcdf_call
 implicit none
 include 'netcdf.inc'
 !
 character(schlen)       ::ch
 integer, intent(in)     ::ID,x1,x2,n1,n2
 integer                 ::idx,jdx,stat,g1,g2,i1,i2,matid,doneid
 complex(SP),intent(inout) ::mat(n1,n2) 
 character,intent(inout)   ::done(n1,n2)
 !
 ! First read the done/not done logicals
 !
#ifdef UPLO
 !
 ! Read one block from upper triangular
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_FLAT',matid); call netcdf_call(stat,ID)
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_FLAT_DONE',doneid); call netcdf_call(stat,ID)
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
     stat = nf_get_vara_text(io_unit(ID), doneid, [idx], [1], done(i1,i2))
     stat = NF_GET_VARA(io_unit(ID), matid, [1,idx], [2,1], mat(i1,i2))
   enddo
 enddo
#else
 !
 ! Read one block at a time in 2D
 stat = nf90_inq_varid(io_unit(ID),trim(ch),matid); call netcdf_call(stat,ID)
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_DONE',doneid); call netcdf_call(stat,ID)
 !
 stat = nf_get_vara_text(io_unit(ID), doneid, [x1,x2], [n1,n2], done); call netcdf_call(stat,ID)
 stat = NF_GET_VARA(io_unit(ID), matid, [1,x1,x2], [2,n1,n2], mat); call netcdf_call(stat,ID)
 !
#endif
 !
end subroutine

