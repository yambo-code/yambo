!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM, DS
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
 use parallel_m,    ONLY:master_cpu,MPI_COMM_WORLD,PP_redux_wait
 use com,           ONLY:msg
 use pars,          ONLY:IP,SP,schlen,lchlen
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_K_dim,BS_K_coupling,l_BSE_restart,l_BSE_kernel_complete,&
&                        l_BSK_IO_triangular
 use BS_solvers,    ONLY:variables_BS,BSS_desc
 use IO_m,          ONLY:frag_BS_K,write_is_on,read_is_on,get_name,&
&                        io_connect,io_elemental,def_variable_bulk,variable_exist,&
&                        io_header,io_file,def_variable_elemental,io_variable_elemental
 use descriptors,   ONLY:IO_desc,IO_desc_duplicate
 !
 implicit none
 !
 type(X_t) :: X 
 integer :: iq, ID
 !
 logical, external :: file_exists
 !
 logical :: def_var, io_var
 integer :: io_BS,    VAR_IDs(8), n_vars, i1
 integer :: flattened_block_size
 type(IO_desc)     :: local_desc
 character(schlen) :: db_name
 character(lchlen) :: full_db_name,ch(4)
 character(schlen) :: dim_names(3),dim_names_done(2)
 !
 ! Connect to parallel netCDF file.
 !
 db_name='BS_PAR_Q'//trim(intc(iq))
 full_db_name=get_name(trim(db_name)," ",2,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
 !
 l_BSE_kernel_complete=.false.
 !
 if (write_is_on(ID)) then
   if (.not.file_exists(trim(full_db_name))) then
     call msg("s","BSE file not found. Creating an empty one.")
     l_BSE_restart=.false.
   else
     call msg("s","Partial BSE file found in "//trim(full_db_name)//". Using for restart.")
     l_BSE_restart = .true.
   endif
 endif
 !
 call PP_redux_wait()
 !
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 if(io_BS/=0) return
 !
 ! Check if the file already contains data and it is consistent
 ! DS: warning. This is potentially dangerous since all MPI tasts are all writing
 !              both the header and the variables
 !              I cannot put here if(master_cpu) because I need all MPI_tasks to
 !              go through the definition of the variables (see similar issue in io_X.F)
 !
 io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",CUTOFF=.true.,FRAG=frag_BS_K)
 !
 io_BS=variables_BS(ID,iq,local_desc,.TRUE.,X=X)
 !
 if (io_BS/=0) then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   call warning('Invalid dimensions found. BSE restart file '//trim(io_file(ID))//' not accepted')
   return
 endif
 !
 ! Check if BSE kernel is complete in reading mode, set the variable to false in writing mode
 !
 def_var=                write_is_on(ID) .or.variable_exist(ID,"BSE_KERNEL_COMPLETE")
 io_var =(master_cpu.and.write_is_on(id)).or.variable_exist(ID,"BSE_KERNEL_COMPLETE")
 if(def_var) call def_variable_elemental(ID,"BSE_KERNEL_COMPLETE",1,IP+100,0,par_io_kind="independent")
 if(io_var)  call io_variable_elemental(ID,VAR=" Kernel complete          :",&
              &                          DB_L0=l_BSE_kernel_complete,L0=l_BSE_kernel_complete)
 !
 if(read_is_on(ID)) then
   if(l_BSE_kernel_complete) then
     call msg("s","Complete BSE file found in "//trim(io_file(ID))//". Loading kernel.")
   else
     call msg("s","Partial BSE file found in "//trim(io_file(ID))//". Not accepted in reading mode.")
   endif
 endif
 !
 call PP_redux_wait()
 !
 ! Descriptions transfer
 !   
 call IO_desc_duplicate(local_desc,BSS_desc)
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
 if(l_BSK_IO_triangular) then
   !
   ! This is the upper triangular mode
   !
   dim_names(:2) =      [character(schlen) :: 'complex', 'BS_K_flat_dim']
   dim_names_done(:1) = [character(schlen) :: 'BS_K_flat_dim']
   flattened_block_size = BS_K_dim*(BS_K_dim+1)/2
   !
   do i1=1,n_vars
     ch(i1)=trim(ch(i1))//"_FLAT"
     call def_variable_bulk(ID,trim(ch(i1)),       VAR_IDs(2*i1-1),&
             &               [2,flattened_block_size],SP,dim_names(:2),     par_io_kind='independent')
     call def_variable_bulk(ID,trim(ch(i1))//"_DONE",VAR_IDs(2*i1),&
             &                 [flattened_block_size], 0,dim_names_done(:1),par_io_kind='independent')
   enddo
   !
 else
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
 endif
 !
end subroutine
!
!
subroutine io_BS_PAR_free(ID,l_kernel_complete)
 !
 ! close BSE netcdf parallel file
 !
 use pars,          ONLY:LP
 use parallel_m,    ONLY:PP_redux_wait,master_cpu
 use BS,            ONLY:l_BSE_kernel_complete,BS_K_has_been_calculated_loaded
 use IO_m,          ONLY:io_disconnect,write_is_on,def_variable_elemental,io_variable_elemental
 !
 implicit none
 !
 integer, intent(in)  :: ID
 logical, intent(in)  :: l_kernel_complete
 !
 call PP_redux_wait()
 !
 BS_K_has_been_calculated_loaded=l_kernel_complete
 l_BSE_kernel_complete          =l_kernel_complete
 !
 if(l_kernel_complete.and.write_is_on(ID)) then
   call def_variable_elemental(ID,"BSE_KERNEL_COMPLETE",1,LP+100,0,par_io_kind="independent")
   if(master_cpu) call io_variable_elemental(ID,VAR=" Kernel complete          :",L0=l_BSE_kernel_complete)
 endif
 !
 call io_disconnect(ID)
 !
end subroutine
!
!
subroutine io_BS_PAR_block(iq,i_block,ID)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use BS,            ONLY:BS_blk,l_BSK_IO_triangular
 use timing_m,      ONLY:timing
 use IO_m,          ONLY:io_variable_bulk,io_unit,netcdf_call,nf90_sync
 !
 implicit none
 !
 integer  :: ID
 !
 ! Work Space
 !
 integer             :: i_block,x1,x2,iq,VAR_IDs(2),stat
 integer             :: n2,i2,idx,imax
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 select case(BS_blk(i_block)%mode)
 case("R")
   VAR_IDs=(/1,2/)
 case("C")
   VAR_IDs=(/3,4/)
 case("A")
   VAR_IDs=(/5,6/)
 case("Q")
   VAR_IDs=(/7,8/)
 end select
 !
 ! Get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 !
 if(l_BSK_IO_triangular) then
   !
   ! Write column by column in 1D
   !
   do i2=1,BS_blk(i_block)%size(2)
     n2  = (x2+i2-1)
     idx = n2*(n2-1)/2+x1
     imax=min(BS_blk(i_block)%size(1),n2-x1+1)
     if(imax<1) cycle
     call io_variable_bulk(ID, VAR_IDs(1),  C1=BS_blk(i_block)%mat(:imax,i2),  IPOS=[1,idx] )
     call io_variable_bulk(ID, VAR_IDs(2), CH1=BS_blk(i_block)%done(:imax,i2), IPOS=[  idx] )
   enddo
   !
 else
   ! 
   ! Write one block at a time in 2D
   !
   call io_variable_bulk(ID, VAR_IDs(1),  C2=BS_blk(i_block)%mat , IPOS=(/1,x1,x2/) )
   call io_variable_bulk(ID, VAR_IDs(2), CH2=BS_blk(i_block)%done, IPOS=(/  x1,x2/) )
   !
 endif
 !
 stat = nf90_sync(io_unit(ID)); call netcdf_call(stat,ID)
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine
