!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_apply(bands,E,K,what,VERBOSE,IO_COM,Time,keep_RT_carriers,TIMES_ONLY)
 !
 use drivers,        ONLY:l_elel_corr,l_rt_carriers_in_use
 use pars,           ONLY:schlen,SP,lchlen
 use stderr,         ONLY:STRING_split,real2ch,intc,STRING_same
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,nkibz
 use IO_m,           ONLY:OP_RD_CL,NONE,DUMP,io_control,DB_path
 use units,          ONLY:FS2AUT,AUT2FS,PS2AUT
 use real_time,      ONLY:RT_carriers,NE_i_time,RT_step,RT_obj_free
 use RT_control,     ONLY:NEQ_Residuals,NEQ_Kernel,CARR_RT_IO_t,RT_ctl_db,RT_ctl_interp_neigh
 use com,            ONLY:msg,jobstr,alt_jobstr,n_alt_jobstr,depth,alt_jobdir,jobdir
 !
 implicit none
 !
 integer,intent(in)    :: bands(2)
 type(levels), intent(inout) :: E
 type(bz_samp), intent(in)   :: K
 character(*),optional,intent(in) :: what ! X,K,G,R
 integer,     optional,intent(in) :: IO_COM
 logical ,    optional,intent(in) :: VERBOSE
 real(SP),    optional,intent(in) :: Time
 logical ,    optional,intent(in) :: keep_RT_carriers
 logical ,    optional,intent(in) :: TIMES_ONLY
 !
 ! Work space
 !
 integer               :: I_id,i1,i_t,io_err,ID,COM_,i2,nkibz_save,i_j
 real(SP)              :: local_Time,TIME_diff,REF_diff
 integer,parameter     :: n_pieces=10
 integer, external     :: io_RT_components
 character(schlen)     :: db_piece(n_pieces),T_unit,ch
 character(lchlen)     :: db_name
 character(3)          :: what_
 logical               :: l_E,l_f,l_W,l_elel_corr_save,is_def,l_verbose,l_keep_carriers,l_BOLTZ,load_times_only
 !
 logical, external     :: file_exists
 !
 local_Time=0._SP
 if (present(Time)) local_Time=Time
 ! 
 if (     present(IO_COM)) COM_=IO_COM
 if (.not.present(IO_COM)) COM_=NONE
 !
 l_verbose=.false.
 if (present(VERBOSE)) l_verbose=VERBOSE
 !
 l_keep_carriers=.false.
 if (present(keep_RT_carriers)) l_keep_carriers=keep_RT_carriers
 !
 load_times_only=.false.
 if (present(TIMES_ONLY)) load_times_only=TIMES_ONLY
 !
 if (present(what)) then
   !
   ! Yambo
   !=======
   !
   what_=' '//trim(what)//' '
   ! 
   ! Kind
   !====== 
   !
   if (what=="X")  I_id=1
   if (what=="K")  I_id=2
   if (what=="G")  I_id=3
   if (what=="R")  I_id=4
   !
   call parser(what//'fnRTdb',is_def)
   !
   if (.not.is_def) return
   !
   l_E    =.FALSE.
   l_f    =.FALSE.
   l_W    =.FALSE.
   l_BOLTZ=.FALSE.
   !
   ! First check the DB file & Time
   !================================
   !
   call STRING_split(RT_ctl_db(I_id),db_piece)
   !
   if(trim(db_piece(1))=="none") return
   !
   i1=index(RT_ctl_db(I_id),"<")
   i2=index(RT_ctl_db(I_id),"@")
   !
   if (i1==0 .or. i2==0) then
     call warning (' Input variable '//what//'fnRTdb is not properly defined')
     return
   endif
   !
   T_unit='fs'
   !
   do i1=1,n_pieces
     if (trim(db_piece(i1))=="<") then
       db_name=db_piece(i1+1)
       if (len_trim(db_name)==0) return
       do i2=i1+1,n_pieces
         if (STRING_same(trim(db_piece(i2)),"ubi" )) l_BOLTZ=.TRUE.
       enddo
       do i2=1,i1-1
         if (STRING_same(trim(db_piece(i2)),"e" )) l_E=.TRUE.
         if (STRING_same(trim(db_piece(i2)),"f" )) l_f=.TRUE.
         if (STRING_same(trim(db_piece(i2)),"w" )) l_W=.TRUE.
       enddo
     endif
     if (trim(db_piece(i1))=="@") then
       read (db_piece(i1+1),*) local_Time
       if (i1+2<=n_pieces) read (db_piece(i1+2),*) T_unit
     endif
   enddo
   !
   if (trim(T_unit)=='fs') then
     local_Time=local_Time*FS2AUT
   elseif (trim(T_unit)=='ps') then
     local_Time=local_Time*PS2AUT
   endif
   !
   if (l_f .and. I_id==2) NEQ_Kernel   =.true.
   if (l_f .and. I_id==4) NEQ_Residuals=.true.
   !
 else
   !
   ! YPP
   !=====
   !
   what_='YPP'
   !
   db_name=DB_path("SAVE"," ",'ndb.RT_carriers')
   if (COM_/=NONE) call msg('s','Searching the carriers database in '//trim(db_name)//'...')
   if (.not.file_exists(trim(db_name))) then
     db_name=DB_path(jobdir,jobstr,'ndb.RT_carriers')
     if (COM_/=NONE) call msg('l',trim(db_name)//'...')
   endif
   if (.not.file_exists(trim(db_name))) then
     job_loop: do i_j=1,n_alt_jobstr
       db_name=DB_path(alt_jobdir(i_j),alt_jobstr(i_j),'ndb.RT_carriers')
       if (COM_/=NONE) call msg('l',trim(db_name)//'...')
       if (file_exists(trim(db_name))) exit job_loop
     enddo job_loop
   endif 
   if (COM_/=NONE) then
     if (     file_exists(trim(db_name))) call msg('l','found')
     if (.not.file_exists(trim(db_name))) call msg('l','not found')
   endif
   !
 endif
 !
 ! I/O Time
 !==========
 !
 if (present(Time)) local_Time=Time
 !
 l_elel_corr_save      =l_elel_corr
 nkibz_save            =nkibz
 ! 
 ! CARRIERS I/O (I)
 !====================
 !
 call io_control(ACTION=OP_RD_CL,COM=COM_,MODE=DUMP,SEC=(/1/),ID=ID) 
 io_err=io_RT_components(trim(db_name),ID)
 !
 nkibz            =nkibz_save
 l_elel_corr      =l_elel_corr_save
 !
 if (io_err< 0 .and. trim(what_)/='YPP') then
   call warning (' Carriers database '//trim(db_name)//' not found')
   return
 endif
 if (io_err==0) l_rt_carriers_in_use=.TRUE.
 !
 ! CARRIERS I/O (II)
 !======================
 ! 
 ! NE_time=(NE_i_time-1)*RT_step but I/O only when NE_i_time= N * RT_IO_steps
 !
 NE_i_time= nint(local_Time/RT_step)+1
 if (NE_i_time<=0) then
   call warning(' Input time is negative. Carriers not loaded')
   NE_i_time=1
   return
 endif
 !
 ! TIMES...
 !----------
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2/),MODE=DUMP,ID=ID)
 io_err=io_RT_components(trim(db_name),ID)
 !
 if (load_times_only) return
 !
 REF_diff=CARR_RT_IO_t%INTERVAL_steps/2._SP
 do i_t=1,CARR_RT_IO_t%N
   TIME_diff= abs ( CARR_RT_IO_t%Time(i_t) - local_time )
   if (abs(TIME_diff)<REF_DIFF ) then 
     REF_diff=TIME_diff
     CARR_RT_IO_t%N=i_t
   endif
 enddo
 !
 ! Occupations...
 !----------------
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),MODE=DUMP,ID=ID)
 io_err=io_RT_components(trim(db_name),ID)
 !
 if (io_err< 0) then 
   call warning('Carriers database '//trim(db_name)//' section #'//trim(intc(NE_i_time))//' not found')
   l_rt_carriers_in_use=.FALSE.
   NE_i_time=1
   return
 endif
 !
 !
 if (present(what)) then
   ch='External RT corrections ('//what//')'
   if (depth>0 ) call section('=',trim(ch))
   if (depth==0) call section('+',trim(ch))
 endif
 !
 if (l_verbose) call msg('s','[RT] Loaded Carrier components @'//trim(real2ch(local_Time*AUT2FS))//' fs')
 !
 ! Interpolatle the RT_carriers object inside the En levels object (energy, occupations and lifetimes)
 ! Only yambo enters here. YPP fills the RT_carriers object and directly uses it instead
 !
 ! Yambo
 !=======
 if (present(what))        call RT_interpolation(bands,E,K,what_,"NN",local_Time,&
&                                                l_f,l_E,l_W,l_BOLTZ,RT_ctl_interp_neigh(I_id))
 !
 if (.not.l_keep_carriers) call RT_obj_free(RT_carriers)
 !
end subroutine
