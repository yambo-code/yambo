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
subroutine io_BS_header(iq,X,ID,mode)
 !
 ! Intialize variables on BSE netcdf parallel file
 !
 use parallel_m,    ONLY:MPI_COMM_WORLD,PP_redux_wait
 use com,           ONLY:msg
 use pars,          ONLY:SP,schlen,lchlen
 use stderr,        ONLY:intc,real2ch
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_K_cutoff,l_BSE_restart,l_BSE_kernel_complete
 use BS_solvers,    ONLY:variables_BS,BSS_desc
 use IO_m,          ONLY:frag_BS_K,write_is_on,read_is_on,get_name,&
&                        io_connect,io_elemental,io_header,io_BS_K
 use descriptors,   ONLY:IO_desc,IO_desc_duplicate
 !
 implicit none
 !
 type(X_t) :: X 
 integer   :: iq, ID
 character(*), intent(in) :: mode
 !
 logical, external :: file_exists
 integer, external :: variables_BS_more
 !
 integer           :: io_BS
 type(IO_desc)     :: local_desc
 character(schlen) :: db_name,tmp_ch
 character(lchlen) :: full_db_name
 !
 ! Connect to parallel netCDF file.
 !
 if(.not.trim(mode)=="connect") then
   !
   db_name='BS_PAR_Q'//trim(intc(iq))
   full_db_name=get_name(trim(db_name)," ",2,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
   !
   l_BSE_kernel_complete=.false.
   !
 endif
 !
 ! If l_BSE_restart true at this point, I'm already re-starting from a compressed kernel
 !
 if (write_is_on(ID).and.io_BS_K.and..not.l_BSE_restart) then
   if (.not.file_exists(trim(full_db_name))) then
     call msg("s","BSE file not found. Creating an empty one.")
     l_BSE_restart=.false.
   else if(trim(mode)=="full") then
     call msg("s","Partial BSE file found in "//trim(full_db_name)//". Using for restart.")
     l_BSE_restart = .true.
   endif
 endif
 !
 call PP_redux_wait()
 !
 db_name='BS_head_Q'//trim(intc(iq))
 !
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 if(io_BS/=0) return
 if(trim(mode)=="connect") return
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
   call warning('Invalid dimensions found. BSE restart file '//trim(full_db_name)//' not accepted')
   return
 endif
 !
 io_BS=variables_BS_more(ID,'start')
 !
 if(read_is_on(ID).and.io_BS_K) then
   if(l_BSE_kernel_complete) then
     call msg("s","Trying to load complete BSE file  ",trim(full_db_name))
   else
     call msg("s","Possible partial BSE file not accepted in reading mode ",trim(full_db_name))
   endif
 endif
 !
 if(read_is_on(ID).and.BS_K_cutoff>0._SP.and.l_BSE_kernel_complete) then
   db_name='BS_COMPRESS'//trim(adjustl(real2ch(BS_K_cutoff)))//'_Q'//trim(intc(iq))
   full_db_name=get_name(trim(db_name)," ",2,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
   if (     io_BS_K) tmp_ch="Backup option will be compressed BSE file"
   if (.not.io_BS_K) tmp_ch="Trying to load compressed BSE file"
   call msg("s",trim(tmp_ch),trim(full_db_name))
 endif
 !
 call PP_redux_wait()
 !
 ! Descriptions transfer
 !   
 call IO_desc_duplicate(local_desc,BSS_desc)
 !
end subroutine io_BS_header
!
!
integer function io_BS_PAR_init(iq,ID_head,ID,mode)
 !
 use stderr,        ONLY:intc,real2ch
 use parallel_m,    ONLY:MPI_COMM_WORLD
 use com,           ONLY:msg
 use pars,          ONLY:IP,IPL,SP,schlen,lchlen,max_io_vars_open
 use BS,            ONLY:BS_K_dim,BS_IO_dim,BS_K_coupling,l_BSE_restart,l_BSE_kernel_complete,&
&                        BSK_IO_mode,MAX_BSK_LIN_SIZE,BS_lin_size_compressed,BS_K_cutoff
 use IO_m,          ONLY:io_connect,def_variable_bulk,io_BS_K,io_file,write_is_on,read_is_on
 !
 implicit none
 !
 integer,      intent(in)    :: iq,ID,ID_head
 character(*), intent(inout) :: mode
 !
 ! Workspace
 !
 character(schlen) :: db_name,ch_tmp,dim_names(3),BSK_IO_local
 character(lchlen) :: ch(8,max_io_vars_open)
 logical           :: def_var, io_var
 integer(IPL)      :: BS_lin_size
 integer           :: VAR_IDs(max_io_vars_open), n_vars, i1, i_rep,i_var, BSK_n_dims,n_replica
 !
 io_BS_PAR_init=-1
 !
 if(.not.io_BS_K .and. (.not.BS_K_cutoff>0._SP)) return
 !
 if(.not.io_BS_K .and. trim(mode)=="full" ) mode="head"
 !
 if(trim(mode)=="full")       db_name='BS_PAR_Q'//trim(intc(iq))
 if(trim(mode)=="compressed") db_name='BS_COMPRESS'//trim(adjustl(real2ch(BS_K_cutoff)))//'_Q'//trim(intc(iq))
 !
 if(trim(mode)=="full" .or. trim(mode)=="compressed") then
   io_BS_PAR_init=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
   if(write_is_on(ID).and.io_BS_PAR_init/=0)  return ! here it should give error
   if( read_is_on(ID).and.io_BS_PAR_init/=0.and.trim(mode)=="compressed") return
   !if( read_is_on(ID).and.io_BS_PAR_init==0.and.trim(mode)=="compressed") no action
   if( read_is_on(ID).and.                      trim(mode)=="full") then
     if (.not.l_BSE_kernel_complete ) return
     if (     l_BSE_kernel_complete ) then
       if(io_BS_PAR_init/=0) then
         call msg("s","Complete BSE file not found. Switching to head mode")
         mode="head"
       endif
       call msg("s","Complete BSE file loading  ",trim(io_file(ID)))
     endif
   endif
 endif
 !
 io_BS_PAR_init=0
 !
 ! Define vars
 !
 ch(1,:)="BSE_RESONANT"
 ch(2,:)="BSE_COUPLING"
 ch(3,:)="BSE_ANTI-RESONANT"
 ch(4,:)="BSE_ANTI-COUPLING"
 !
 dim_names(1)  =  'complex'
 !
 if (.not.BS_K_coupling)           n_vars=1
 if (     BS_K_coupling.and.iq==1) n_vars=2
 if (     BS_K_coupling.and.iq/=1) n_vars=4
 !
 BSK_IO_local=trim(BSK_IO_mode)
 if(trim(mode)=="compressed") BSK_IO_local=trim(mode)
 !
 select case(trim(BSK_IO_local))
 case("1D_linear","compressed")
   !
   ! This is the upper triangular matrix rescared in (many) 1D arrays
   !
   BSK_n_dims=1
   !
   !                46341 is the sqare root of the maximum integer 2147483647
   MAX_BSK_LIN_size=45000*45000
   !
   if(trim(BSK_IO_local)=="1D_linear" ) then
     dim_names(2:3)=[ 'BS_K_linearized1', 'BS_K_linearized2' ]
     BS_LIN_size=int(BS_K_dim,kind=IPL)*int(BS_K_dim+1,kind=IPL)/int(2,kind=IPL)
     ch_tmp="_LINEARIZED"
   endif
   if(trim(BSK_IO_local)=="compressed") then
     dim_names(2:3)=[ 'BS_K_compressed1', 'BS_K_compressed2' ]
     BS_LIN_size=BS_lin_size_compressed
     ch_tmp="_COMPRESSED"
   endif
   BS_IO_dim=int(min(int(MAX_BSK_LIN_size,IPL),BS_lin_size),IP)
   !
   n_replica=1+int(BS_LIN_size/int(MAX_BSK_LIN_size,IPL),IP)
   do i_rep=1,n_replica
     do i_var=1,n_vars
       i1=i_var+n_vars*(i_rep-1)
       ch(i_var,i_rep)=trim(ch(i_var,i_rep))//trim(ch_tmp)//trim(intc(i_rep))
     enddo
   enddo
   BS_LIN_size=BS_LIN_size-int(MAX_BSK_LIN_size,IPL)*int(n_replica-1,IPL)
   !
   BS_IO_dim(2)=int(BS_LIN_size,IP)
   !
 case("2D_standard")
   !
   ! Here is the 2D mode
   !
   BSK_n_dims=2
   n_replica=1
   !
   dim_names(2:3) = ['BS_K_dim', 'BS_K_dim']
   !
   BS_IO_dim = BS_K_dim
   !
 end select
 !
 do i_rep=1,n_replica
   do i_var=1,n_vars
     i1=i_var+n_vars*(i_rep-1)
     if( (trim(BSK_IO_local)=="1D_linear".or.trim(BSK_IO_local)=="compressed") &
     &   .and.i_rep==n_replica.and.i_rep>1) then
       dim_names(2)=dim_names(3)
       BS_IO_dim(1)=BS_IO_dim(2)
     endif
     if(trim(mode)=="full" .or. trim(mode)=="compressed") &
     &  call def_variable_bulk(ID,trim(ch(i_var,i_rep)),         1+(i_var-1)*2+(i_rep-1)*8,&
     &               [2,BS_IO_dim(1:BSK_n_dims)],SP,dim_names(1:BSK_n_dims+1),par_io_kind='independent')
     if(trim(mode)=="head" .or. trim(mode)=="full") &
     &  call def_variable_bulk(ID_head,trim(ch(i_var,i_rep))//"_DONE",2+(i_var-1)*2+(i_rep-1)*8,&
     &                  BS_IO_dim(1:BSK_n_dims) , 0,dim_names(2:BSK_n_dims+1),par_io_kind='independent')
   enddo
 enddo
 !
end function io_BS_PAR_init
!
subroutine io_BS_PAR_free(ID_head,ID,mode,l_kernel_complete)
 !
 ! close BSE netcdf parallel file
 !
 use parallel_m,    ONLY:PP_redux_wait
 use BS,            ONLY:l_BSE_kernel_complete,BS_K_has_been_calculated_loaded
 use IO_m,          ONLY:io_disconnect,read_is_on,write_is_on
 !
 implicit none
 !
 integer,      intent(in)  :: ID,ID_head
 logical,      intent(in)  :: l_kernel_complete
 character(*), intent(in)  :: mode
 !
 integer              :: io_BS
 integer, external    :: variables_BS_more
 !
 call PP_redux_wait()
 !
 if(trim(mode)=="head".or.trim(mode)=="full") then
   if(write_is_on(ID)) then
     BS_K_has_been_calculated_loaded=l_kernel_complete
     l_BSE_kernel_complete          =l_kernel_complete
   endif
   io_BS=variables_BS_more(ID_head,'end')
   call io_disconnect(ID_head)
   if(read_is_on(ID)) then
     BS_K_has_been_calculated_loaded=l_kernel_complete
     l_BSE_kernel_complete          =l_kernel_complete
   endif
 endif
 !
 if(trim(mode)=="compressed".or.trim(mode)=="full") call io_disconnect(ID)
 !
end subroutine
!
!
subroutine io_BS_PAR_block(iq,i_block,ID_head,ID,mode)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use pars,          ONLY:IP,IPL,SP,schlen
 use BS,            ONLY:BS_blk,BSK_IO_mode,BS_K_cutoff,BS_IO_dim,MAX_BSK_LIN_size
 use timing_m,      ONLY:timing
 use IO_m,          ONLY:io_variable_bulk,io_unit,netcdf_call,nf90_sync,&
&                        write_is_on,io_BS_K
 !
 implicit none
 !
 integer,      intent(in) :: ID,ID_head
 integer,      intent(in) :: iq,i_block
 character(*), intent(in) :: mode
 !
 ! Work Space
 !
 character(schlen)   :: BSK_IO_local
 integer             :: x1,x2,VAR_IDs(2),stat
 integer             :: n2,i1,i2,idx1,idx2,imax,i_rep(2),ilast
 integer(IPL)        :: idx
 !
 complex(SP), allocatable :: TMP_IO_VAR(:)
 !
 if(.not.io_BS_K .and. (.not.BS_K_cutoff>0._SP)) return
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
 ! Get lower bound coordinates in 2D matrix
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 !
 BSK_IO_local=trim(BSK_IO_mode)
 if(trim(mode)=="compressed") BSK_IO_local=trim(mode)
 !
 select case(trim(BSK_IO_local))
 case("1D_linear")
   !
   do i2=1,BS_blk(i_block)%size(2)
     !
     ! tmp coordinate in 2D matrix, runs from x2 to x2 + SIZE -1
     n2  = (x2+i2-1)
     !
     ! coordinate in linearized matrix n2*(n2-1)/2 counting all the elements of the
     ! triangular matrix as follow
     ! 1  2  4  7 11
     ! x  3  5  8 12
     ! x  x  6  9 
     ! x  x  x 10 
     idx = int(n2,kind=IPL)*int(n2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
     !
     ! number of rows from the first in the block to the diagonal of the matrix
     ! I'm going to write elements from idx to idx+imax
     imax=min(BS_blk(i_block)%size(1),n2-x1+1)
     if(imax<1) cycle
     !
     ! if idx > MAX_BSK_LIN_size switch to the next variable
     i_rep(1)=1+int(idx/int(MAX_BSK_LIN_size,IPL),IP)
     ! if idx+imax  > MAX_BSK_LIN_size switch to the next variable
     i_rep(2)=1+int((idx+int(imax,IPL))/int(MAX_BSK_LIN_size,IPL),IP)
     ! I need both the previous two because a block can be written
     ! part on linear variable, part on the next
     !
     idx1= int(idx-int(MAX_BSK_LIN_size,IPL)*int(i_rep(1)-1,IPL),IP)
     !
     ! Write column by column in 1D
     !
     if(idx1+imax-1<MAX_BSK_LIN_size) then
       ! Block is all in one variable
       call io_variable_bulk(ID_head, VAR_IDs(2)+(i_rep(1)-1)*8, CH1=BS_blk(i_block)%done(:imax,i2), IPOS=[  idx1] )
     else
       ! Block is split
       ilast=MAX_BSK_LIN_size-idx1+1
       call io_variable_bulk(ID_head, VAR_IDs(2)+(i_rep(1)-1)*8, CH1=BS_blk(i_block)%done(:ilast,i2),       IPOS=[  idx1] )
       call io_variable_bulk(ID_head, VAR_IDs(2)+(i_rep(2)-1)*8, CH1=BS_blk(i_block)%done(ilast+1:imax,i2), IPOS=[     1] )
     endif
     !
     if(trim(mode)=="full") then
       if(idx1+imax-1<MAX_BSK_LIN_size) then
         ! Block is all in one variable
         call io_variable_bulk(ID,    VAR_IDs(1)+(i_rep(1)-1)*8,  C1=BS_blk(i_block)%mat(:imax,i2),  IPOS=[1,idx1] )
       else
         ! Block is split
         ilast=MAX_BSK_LIN_size-idx1+1
         call io_variable_bulk(ID,    VAR_IDs(1)+(i_rep(1)-1)*8,  C1=BS_blk(i_block)%mat(:ilast,i2),        IPOS=[1,idx1] )
         call io_variable_bulk(ID,    VAR_IDs(1)+(i_rep(2)-1)*8,  C1=BS_blk(i_block)%mat(ilast+1:imax,i2),  IPOS=[1,   1] )
       endif
     endif
     !
   enddo
   !
 case("compressed")
   !
   imax=count(BS_blk(i_block)%done(:,:)=="t")
   !
   allocate(TMP_IO_VAR(imax))
   !
   idx=0
   do i1=1,BS_blk(i_block)%size(1)
     do i2=1,BS_blk(i_block)%size(2)
       if( BS_blk(i_block)%done(i1,i2)/="t") cycle
       idx=idx+1
       TMP_IO_VAR(idx)=BS_blk(i_block)%mat(i1,i2)
     enddo
   enddo
   !
   idx = BS_blk(i_block)%idx_compr
   !
   ! if idx > MAX_BSK_LIN_size switch to the next variable
   i_rep(1)=1+int(idx/int(MAX_BSK_LIN_size,IPL),IP)
   ! if idx+imax  > MAX_BSK_LIN_size switch to the next variable
   i_rep(2)=1+int((idx+int(imax,IPL))/int(MAX_BSK_LIN_size,IPL),IP)
   ! I need both the previous two because a block can be written
   ! part on linear variable, part on the next
   !
   idx1= int(idx-int(MAX_BSK_LIN_size,IPL)*int(i_rep(1)-1,IPL),IP)
   !
   ! Write BS_blk with %mat set to zero
   !
   if(idx1+imax-1<MAX_BSK_LIN_size) then
     ! Block is all in one variable
     call io_variable_bulk(ID, VAR_IDs(1)+(i_rep(1)-1)*8,  C1=TMP_IO_VAR,  IPOS=[1,idx1] )
   else
     ! Block is split
     ilast=MAX_BSK_LIN_size-idx1+1
     call io_variable_bulk(ID, VAR_IDs(1)+(i_rep(1)-1)*8,  C1=TMP_IO_VAR(:ilast),        IPOS=[1,idx1] )
     call io_variable_bulk(ID, VAR_IDs(1)+(i_rep(2)-1)*8,  C1=TMP_IO_VAR(ilast+1:imax),  IPOS=[1,   1] )
   endif
   !
   deallocate(TMP_IO_VAR)
   !
 case("2D_standard")
   ! 
   ! Write one block at a time in 2D
   !
   call io_variable_bulk(ID, VAR_IDs(1),  C2=BS_blk(i_block)%mat , IPOS=(/1,x1,x2/) )
   call io_variable_bulk(ID_head, VAR_IDs(2), CH2=BS_blk(i_block)%done, IPOS=(/  x1,x2/) )
   !
 end select
 !
 if( trim(mode)=="head" .or. trim(mode)=="full" ) then
   if(write_is_on(ID_head)) then
     stat = nf90_sync(io_unit(ID_head))
     call netcdf_call(stat,ID_head)
   endif
 endif
 !
 if( trim(mode)=="compressed" .or. trim(mode)=="full" ) then
   if(write_is_on(ID)) then
     stat = nf90_sync(io_unit(ID))
     call netcdf_call(stat,ID)
   endif
 endif
 !
 call timing('io_BS',OPR='stop')
 !
end subroutine
