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
subroutine RT_apply(bands,E,K,what,VERBOSE,IO_com,Time,keep_RT_carriers)
 !
 use drivers,        ONLY:l_elel_corr,l_rt_carriers_in_use
 use pars,           ONLY:schlen,SP
 use stderr,         ONLY:string_split,real2ch,intc
 use memory_m,       ONLY:mem_est
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp,nkibz
 use QP_m,           ONLY:QP_ctl_db
 use IO_m,           ONLY:OP_RD_CL,NONE,DUMP,io_control
 use units,          ONLY:FS2AUT,PS2AUT,HA2EV
 use real_time,      ONLY:RT_carriers,NE_i_time,RT_step,RT_obj_free
 use rt_ctl,         ONLY:RT_IO_JPO_steps
 use com,            ONLY:error,msg,jobstr,alt_jobstr,n_alt_jobstr,file_exists
 !
 implicit none
 !
 integer      :: bands(2)
 type(levels) :: E
 type(bz_samp):: K
 character(*),optional :: what ! X,K,G
 integer,     optional :: IO_com
 logical ,    optional :: VERBOSE
 real(SP),    optional :: Time
 logical ,    optional :: keep_RT_carriers
 !
 ! Work space
 !
 integer               :: I_id,i1,io_err,ID,i2,nb(2),ik,nkibz_save,i_j
 real(SP)              :: l_Time,df_Max,dE_Max,N_carriers
 integer,parameter     :: n_pieces=10
 integer, external     :: io_RT_components
 character(schlen)     :: db_piece(n_pieces),db_name,T_unit
 character(3)          :: what_
 logical               :: l_E,l_f,l_elel_corr_save,is_def
 real(SP), allocatable :: r_dummy(:,:,:)
 !
 l_Time=0._SP
 !
 if (present(what)) then
   !
   what_=' '//trim(what)//' '
   ! 
   ! Kind
   !====== 
   !
   if (what=="X")  I_id=1
   if (what=="K")  I_id=2
   if (what=="G")  I_id=3
   !
   call parser(what//'fnQPdb',is_def)
   !
   if (.not.is_def) return
   !
   l_E=.FALSE.
   l_f=.FALSE.
   !
   ! First check the DB file & Time
   !================================
   !
   call string_split(QP_ctl_db(I_id),db_piece)
   !
   T_unit='fs'
   !
   do i1=1,n_pieces
     if (trim(db_piece(i1))=="<") then
       db_name=db_piece(i1+1)
       if (len_trim(db_name)==0) return
       do i2=1,i1-1
         if (trim(db_piece(i2))=="E") l_E=.TRUE.
         if (trim(db_piece(i2))=="f") l_f=.TRUE.
       enddo
     endif
     if (trim(db_piece(i1))=="@") then
       read (db_piece(i1+1),*) l_Time
       if (i1+2<=n_pieces) read (db_piece(i1+2),*) T_unit
     endif
   enddo
   !
   if (trim(T_unit)=='fs') then
     l_Time=l_Time*FS2AUT
   elseif (trim(T_unit)=='ps') then
     l_Time=l_Time*PS2AUT
   endif
   !
 else
   !
   what_=' '
   !
   l_E=.TRUE.
   l_f=.TRUE.
   !
   db_name=trim(jobstr)//'/ndb.RT_carriers'
   if (.not.file_exists(trim(db_name))) then
     job_loop: do i_j=1,n_alt_jobstr
       db_name=trim(alt_jobstr(i_j))//'/ndb.RT_carriers'
       if (file_exists(trim(db_name))) exit job_loop
     enddo job_loop
   endif 
   !
 endif
 !
 ! I/O Time
 !==========
 !
 if (present(Time)) l_Time=Time
 !
 l_elel_corr_save=l_elel_corr
 nkibz_save      =nkibz
 ! 
 ! CARRIERS I/O (I)
 !====================
 ! 
 if (present(IO_COM)) then
   call io_control(ACTION=OP_RD_CL,COM=IO_COM,MODE=DUMP,SEC=(/1/),ID=ID) 
 else
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID) 
 endif
 io_err=io_RT_components(trim(db_name),ID)
 !
 if (io_err< 0) call error ('Carriers database '//trim(db_name)//' not found')
 if (io_err==0) l_rt_carriers_in_use=.TRUE.
 !
 ! CARRIERS I/O (II)
 !======================
 ! 
 ! NE_time=(NE_i_time-1)*RT_step but I/O only when NE_i_time= N * RT_IO_steps
 !
 NE_i_time= nint(l_Time/RT_step)+1
 NE_i_time= NE_i_time-mod(NE_i_time-1,RT_IO_JPO_steps)
 !
 if(NE_i_time>0) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/NE_i_time+1/),MODE=DUMP,ID=ID)
   io_err=io_RT_components(trim(db_name),ID)
   !
   if (io_err< 0) call error ('Carriers database '//trim(db_name)//' section'//trim(intc(NE_i_time))//'not found')
   !
 endif
 !
 ! Interpolation
 !===============
 !
 nb(1)=max(bands(1),RT_carriers%nb(1))
 nb(2)=min(bands(2),RT_carriers%nb(2))
 !
 if (l_f) then
   !
   allocate(r_dummy(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol))
   call Nearest_kpt_interpolation(RT_carriers%nk,K%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                 RT_carriers%kpt,K%pt,RT_carriers%k_weight,K%weights,RT_carriers%delta_f,r_dummy,&
&                                 what_,.TRUE.)
   !
   df_Max=maxval(abs(r_dummy))
   N_carriers=0._SP
   do ik=1,K%nibz
     N_carriers=N_carriers+sum(abs(r_dummy(:,ik,:)))*K%weights(ik)/2._SP
   enddo
   !
   E%nbm=max(RT_carriers%nb(2)  ,E%nbm)
   E%nbf=min(RT_carriers%nb(1)-1,E%nbf)
   !
   allocate(E%fo(E%nb,E%nk,n_sp_pol))
   call mem_est("E-fo",(/size(E%fo)/),(/SP/))
   !
   E%fo( nb(1):nb(2),:,:)=E%f(nb(1):nb(2),:,:)
   E%f ( nb(1):nb(2),:,:)=E%f(nb(1):nb(2),:,:)+r_dummy(nb(1):nb(2),:,:)
   !
   deallocate(r_dummy)
   !
 endif
 !
 if (l_E) then
   !
   allocate(r_dummy(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol))
   call Nearest_kpt_interpolation(RT_carriers%nk,K%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                 RT_carriers%kpt,K%pt,RT_carriers%k_weight,K%weights,RT_carriers%delta_E,r_dummy,&
&                                 what_,.TRUE.)
   !
   dE_Max=maxval(abs(r_dummy))
   !
   allocate(E%Eo(E%nb,E%nk,n_sp_pol))
   call mem_est("E-Eo",(/size(E%fo)/),(/SP/))
   !
   E%Eo( nb(1):nb(2),:,:)=E%E(nb(1):nb(2),:,:)
   !
   E%E( nb(1):nb(2),:,:)=E%E(nb(1):nb(2),:,:)+r_dummy(nb(1):nb(2),:,:)
   !
   deallocate(r_dummy)
   !
 endif
 !
 if (.not.present(keep_RT_carriers)) call RT_obj_free(RT_carriers)
 !
 nkibz      =nkibz_save
 l_elel_corr=l_elel_corr_save
 !
 if (present(VERBOSE)) then
   !
   if (VERBOSE) then
     !
     call msg('s','[RT] Loaded Carrier components @'//trim(real2ch(l_Time/FS2AUT))//' fs')
     !
     call msg('nr','Updated Carrier components')
     call msg('rn','==========================')
     call msg('r','[RT] Reference Time                                      [fs]: ',l_Time/FS2AUT)
     call msg('r',  '[RT] Bands range                                             : ', (/nb(1),nb(2)/)) 
     if (l_f) call msg('r',  '[RT] Max change in occupations / N of carriers per cell      : ',&
&                            (/df_Max,N_carriers/)) 
     if (l_E) call msg('r',  '[RT] Max change in energt levels                         [ev]: ',&
&                            dE_Max*HA2EV) 
     call msg('rn',  '[RT] N full bands, N metallic bands                          : ', (/E%nbf,E%nbm/)) 
     !
     call REPORT_OCCUPATIONS(E)
     !
   endif
   !
 endif
 !
end subroutine
