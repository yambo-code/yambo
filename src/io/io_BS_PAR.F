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
 ! Intialize BSE netcdf parallel file
 !
 use netcdf
 use parallel_m
 use com,           ONLY:msg,get_name,error
 use pars,          ONLY:SP,schlen,lchlen,nf90_SP
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_K_dim, l_restart_bse, BS_K_coupling
 use IO_m,          ONLY:frag_BS_K,write_is_on,VERIFY,netcdf_call,io_action,OP_RD,&
&                        io_connect,io_elemental,io_header,io_mode,io_unit,io_netcdf_var,io_file
 use BS_solvers,    ONLY:variables_BS
 implicit none
#define UPLO
#if defined DOUBLE
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
 character(schlen) :: db_name, PARALLEL_STRUCTURE
 character(lchlen) ::local_description(100)
 !
 ! Create parallel netCDF file.
 !
 db_name='BS_Q'//trim(intc(iq))//'.nc'
 io_file(ID)=get_name(db_name,' ',2,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
 mode_flag = ior(ior(nf_netcdf4, nf_mpiio),nf90_write)
 !
 ! Try to open the file 
 !
 stat = nf90_open(io_file(ID),mode_flag,comm=MPI_COMM_WORLD,info=MPI_INFO_NULL,ncid=io_unit(ID))
 !
 if (stat.eq.nf90_noerr) then
    !
    ! If we are restarting check if the variables are present otherwise create them
    call msg("s","BSE file found in "//trim(io_file(ID))//" Restarting calculation." )
    !
    io_action(ID) = OP_RD
    io_mode(ID)=VERIFY
    ! Read dimensions
    stat = nf90_inq_dimid(io_unit(ID), "complex", complex_dim); call netcdf_call(stat,ID) 
    stat = nf90_inq_dimid(io_unit(ID), "BS_K_dim", block_size_dim); call netcdf_call(stat,ID)
    stat = nf90_inq_dimid(io_unit(ID), "flat_BS_K_dim",flattened_block_size_dim); call netcdf_call(stat,ID)
    stat = nf_redef(io_unit(ID)); call netcdf_call(stat,ID)
    !
 else
    !
    ! If it does not work create it
    l_restart_bse = .false.
    call msg("s","BSE file not found. Creating an empty one." )
    stat = nf90_create(io_file(ID),mode_flag,comm=MPI_COMM_WORLD,info=MPI_INFO_NULL,ncid=io_unit(ID))
    call netcdf_call(stat,ID)
    !
    ! Initialize dimensions
    stat = nf_def_dim(io_unit(ID), "complex", 2, complex_dim); call netcdf_call(stat,ID) 
    stat = nf_def_dim(io_unit(ID), "BS_K_dim", BS_K_dim, block_size_dim); call netcdf_call(stat,ID)
    stat = nf_def_dim(io_unit(ID), "flat_BS_K_dim",BS_K_dim*(BS_K_dim+1)/2, flattened_block_size_dim)
    call netcdf_call(stat,ID) 
    !
 endif
 !
 ! Write empty vars
 !
#ifdef UPLO
 !
 ! This is the upper triangular mode
 !
 stat = nf90_inq_varid(io_unit(ID),'BSE_RESONANT_FLAT',io_netcdf_var(ID))
 if (stat.ne.nf90_noerr) then
   dims(:2) = [complex_dim,flattened_block_size_dim]
   stat = nf_def_var(io_unit(ID), "BSE_RESONANT_FLAT", nf90_SP, 2, dims, io_netcdf_var(ID))
   call netcdf_call(stat,ID)
   stat = nf_var_par_access(io_unit(ID), io_netcdf_var(ID), nf_independent); call netcdf_call(stat,ID)
 end if
 !
 stat = nf90_inq_varid(io_unit(ID),'BSE_COUPLING_FLAT',io_netcdf_var(ID))
 if (BS_K_coupling .and. (stat.ne.nf90_noerr)) then
   dims(:2) = [complex_dim,flattened_block_size_dim]
   stat = nf_def_var(io_unit(ID), "BSE_COUPLING_FLAT", nf90_SP, 2, dims, io_netcdf_var(ID))
   call netcdf_call(stat,ID)
   stat = nf_var_par_access(io_unit(ID), io_netcdf_var(ID), nf_independent); call netcdf_call(stat,ID)
 end if
 !
#else
 !
 ! Here is the 2D mode
 !
 stat = nf90_inq_varid(io_unit(ID),'BSE_RESONANT',io_netcdf_var(ID))
 if (stat.ne.nf90_noerr) then 
   dims(:) = [complex_dim,block_size_dim,block_size_dim]
   stat = nf_def_var(io_unit(ID), "BSE_RESONANT", nf90_SP, 3, dims, io_netcdf_var(ID))
   call netcdf_call(stat,ID)
   stat = nf_var_par_access(io_unit(ID), io_netcdf_var(ID), nf_independent);
   call netcdf_call(stat,ID)
 end if
 !
 stat = nf90_inq_varid(io_unit(ID),'BSE_COUPLING',io_netcdf_var(ID))
 if (BS_K_coupling .and. (stat.ne.nf90_noerr)) then 
   dims = [complex_dim,block_size_dim,block_size_dim]
   stat = nf_def_var(io_unit(ID), "BSE_COUPLING", nf90_SP, 3, dims, io_netcdf_var(ID))
   call netcdf_call(stat,ID)
   stat = nf_var_par_access(io_unit(ID), io_netcdf_var(ID), nf_independent); 
   call netcdf_call(stat,ID)
 end if
 !
#endif
 stat = nf_enddef(io_unit(ID)); call netcdf_call(stat,ID)
 !
 ! Write some metadata
 !
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
 call io_BS_PAR_write_arbitrary_block(ID,ch,x1,x2,n1,n2,BS_blk(i_block)%mat)
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine

subroutine io_BS_PAR_write_arbitrary_block(ID,ch,x1,x2,n1,n2,mat)
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
 character(schlen)   ::ch
 integer, intent(in) ::ID,x1,x2,n1,n2
 integer             ::idx,jdx,stat,g1,g2,i1,i2
 complex(SP)         ::mat(n1,n2) 
 ! 
#ifdef UPLO
 !
 ! Write one element at a time in 1D upper triangular
 !
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_FLAT',io_netcdf_var(ID))
 call netcdf_call(stat,ID)
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
     stat = NF_PUT_VARA(io_unit(ID), io_netcdf_var(ID), [1,idx], [2,1], mat(i1,i2))
   enddo
 enddo
#else
 !
 ! Write one block at a time in 2D
 !
 stat = nf90_inq_varid(io_unit(ID),ch,io_netcdf_var(ID))
 call netcdf_call(stat,ID)
 stat = NF_PUT_VARA(io_unit(ID), io_netcdf_var(ID), [1,x1,x2], [2,n1,n2], mat)
 call netcdf_call(stat,ID)
 stat = nf_sync(io_unit(ID)); call netcdf_call(stat,ID)
 !
#endif

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
 integer                 ::idx,jdx,stat,g1,g2,i1,i2
 complex(SP),intent(out) ::mat(n1,n2) 
 logical,intent(out)     ::done(n1,n2)
 logical                 ::transition_done 
 !
#ifdef UPLO
 !
 ! Read one block from upper triangular
 stat = nf90_inq_varid(io_unit(ID),trim(ch)//'_FLAT',io_netcdf_var(ID))
 call netcdf_call(stat,ID)
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
     stat = NF_GET_VARA(io_unit(ID), io_netcdf_var(ID), [1,idx], [2,1], mat(i1,i2))
   enddo
 enddo
#else
 !
 ! Write one block at a time in 2D
 stat = nf90_inq_varid(io_unit(ID),ch,io_netcdf_var(ID)); call netcdf_call(stat,ID)
 stat = NF_GET_VARA(io_unit(ID), io_netcdf_var(ID), [1,x1,x2], [2,n1,n2], mat)
 call netcdf_call(stat,ID)
 !
#endif
 !
 ! Fill in the done logicals
 do i2=1,n2
   do i1=1,n1
     !if the value of the mat is different from the default it means the transition was computed
     transition_done = (real(mat(i1,i2)) .ne. nf_fill_float)
     done(i1,i2) = transition_done
   enddo
 enddo
 !
end subroutine

