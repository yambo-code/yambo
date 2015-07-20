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
 use electrons,      ONLY:levels,n_sp_pol,BZ_DbGd_tot_nkpts,BZ_DbGd_kpts,BZ_DbGd_nbands
 use R_lattice,      ONLY:bz_samp,nkibz
 use QP_m,           ONLY:QP_ctl_db
 use IO_m,           ONLY:OP_RD_CL,NONE,DUMP,io_control
 use units,          ONLY:FS2AUT,PS2AUT,HA2EV
 use real_time,      ONLY:RT_carriers,NE_i_time,RT_step,RT_obj_free
 use rt_ctl,         ONLY:RT_IO_JPO_steps
 use com,            ONLY:warning,msg,jobstr,alt_jobstr,n_alt_jobstr,file_exists
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
 integer               :: I_id,i1,io_err,ID,i2,nb(2),ik,nkibz_save,i_j,BZ_DbGd_tot_nkpts_save
 real(SP)              :: l_Time,df_Max,dE_Max,N_carriers(2)
 integer,parameter     :: n_pieces=10
 integer, external     :: io_RT_components
 character(schlen)     :: db_piece(n_pieces),db_name,T_unit
 character(3)          :: what_
 logical               :: l_E,l_f,l_elel_corr_save,is_def,l_DbGd
 !
 l_Time=0._SP
 !
 l_DbGd=BZ_DbGd_tot_nkpts>0
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
 l_elel_corr_save      =l_elel_corr
 nkibz_save            =nkibz
 BZ_DbGd_tot_nkpts_save=BZ_DbGd_tot_nkpts
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
 BZ_DbGd_tot_nkpts=BZ_DbGd_tot_nkpts_save
 nkibz            =nkibz_save
 l_elel_corr      =l_elel_corr_save
 !
 if (io_err< 0) then
   call warning ('Carriers database '//trim(db_name)//' not found')
   return
 endif
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
   if (io_err< 0) then 
     call warning('Carriers database '//trim(db_name)//' section'//trim(intc(NE_i_time))//'not found')
     l_rt_carriers_in_use=.FALSE.
     return
   endif
   !
 endif
 !
 ! Interpolation
 !===============
 !
 nb(1)=max(bands(1),RT_carriers%nb(1))
 nb(2)=min(bands(2),RT_carriers%nb(2))
 !
 N_carriers=0._SP
 !
 if (l_f) then
   !
   allocate(E%df(E%nb,E%nk,n_sp_pol))
   call mem_est("E-df",(/size(E%df)/),(/SP,SP/))
   call Interpolate_and_transfer("df",RT_carriers%delta_f,K%pt,E%nk)
   !
   if (l_DbGd) then
     allocate(E%df_DbGd(E%nb,BZ_DbGd_tot_nkpts,n_sp_pol))
     call mem_est("E-df_DbGd",(/size(E%df_DbGd)/),(/SP,SP/))
     call Interpolate_and_transfer("df",RT_carriers%delta_f,BZ_DbGd_kpts,BZ_DbGd_tot_nkpts)
   endif
   !
 endif
 !
 if (l_E) then
   !
   allocate(E%Eo(E%nb,E%nk,n_sp_pol))
   call mem_est("E-Eo",(/size(E%Eo)/),(/SP/))
   call Interpolate_and_transfer("dE",RT_carriers%delta_E,K%pt,E%nk)
   !
   if (l_DbGd) call Interpolate_and_transfer("dE",RT_carriers%delta_E,BZ_DbGd_kpts,BZ_DbGd_tot_nkpts)
   !
 endif
 !
 !DEBUG>
 !do i1=1,E%nk
 !  do i2=1,E%nb
 !    write (100,*) E%E(i2,i1,1)*HA2EV,E%f(i2,i1,1),E%df(i2,i1,1)
 !  enddo
 !enddo
 !if (l_DbGd) then
 !  do i1=1,BZ_DbGd_tot_nkpts
 !    do i2=1,BZ_DbGd_nbands
 !      write (200,*) E%E_DbGd(i2,i1,1)*HA2EV,E%f_DbGd(i2,i1,1)
 !    enddo
 !  enddo
 !endif
 !DEBUG<
 !
 ! Metallic and Insulating bands update
 !
 if (.not.present(keep_RT_carriers)) call RT_obj_free(RT_carriers)
 !
 if (present(VERBOSE)) then
   !
   if (VERBOSE) then
     !
     call msg('s','[RT] Loaded Carrier components @'//trim(real2ch(l_Time/FS2AUT))//' fs')
     !
     call msg('nr',   'Updated Carrier components')
     call msg('rn',   '==========================')
     call msg('r',    '[RT] Reference Time           [fs]: ',l_Time/FS2AUT)
     call msg('r',    '[RT] Bands range                  : ', (/nb(1),nb(2)/)) 
     !
     if (l_f) then
       call msg('r',  '[RT] Maximum occupations change   : ',df_Max)
       call msg('r',  '[RT] N of carriers                : ',N_carriers(1))
       if (l_DbGd) call msg('r',  '[RT]                 (Double Grid): ',N_carriers(2))
     endif
     !
     if (l_E) then
       call msg('r',  '[RT] Max energy levels change [ev]: ',dE_Max*HA2EV)
     endif
     !
     call msg('rn',  '[RT] Full and metallic bands       : ', (/E%nbf,E%nbm/)) 
     !
     call REPORT_OCCUPATIONS(E)
     !
   endif
   !
 endif
 !
 contains
  !
  subroutine Interpolate_and_transfer(OBJ,V_in,K_in,NK)
    !
    character(*) :: OBJ
    integer      :: NK
    real(SP)     :: V_in(RT_carriers%nstates),K_in(NK,3)
    !
    ! ws
    !
    integer      :: i_carr,nb_DbGd
    real(SP)     :: K_weights(NK),V_out(RT_carriers%nb(1):RT_carriers%nb(2),NK,n_sp_pol)
    !
    if (NK/=E%nk) K_weights=1._SP/real(BZ_DbGd_tot_nkpts,SP)
    if (NK==E%nk) K_weights(:)=K%weights(:)
    !
    call Nearest_kpt_interpolation(RT_carriers%nk,NK,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                  RT_carriers%kpt,K_in,RT_carriers%k_weight,K_weights,V_in,V_out,what_,.TRUE.)
    !
    if (OBJ=="df") then
      !
      if(NK==E%nk) then
        E%df( nb(1):nb(2),:,:)=V_out(nb(1):nb(2),:,:)
        df_Max=maxval(abs(V_out))
        i_carr=1
      else if (NK/=E%nk) then
        nb_DbGd=min(nb(2),BZ_DbGd_nbands)
        E%df_DbGd( nb(1):nb_DbGd,:,:)=V_out(nb(1):nb_DbGd,:,:)
        i_carr=2
      endif
      !
      do ik=1,NK
        N_carriers(i_carr)=N_carriers(i_carr)+sum(abs(V_out(:,ik,:)))*K_weights(ik)/2._SP
      enddo
      !
    else if (OBJ=="dE") then
      !
      if(NK==E%nk) then
        E%Eo=E%E
        E%E( nb(1):nb(2),:,:)=E%E(nb(1):nb(2),:,:)+V_out(nb(1):nb(2),:,:)
        dE_Max=maxval(abs(V_out))
      else if (NK/=E%nk) then
        E%E_DbGd( nb(1):nb_DbGd,:,:)=E%E_DbGd(nb(1):nb_DbGd,:,:)+V_out(nb(1):nb_DbGd,:,:)
      endif
      !
    endif
    !
  end subroutine Interpolate_and_transfer
  !
end subroutine
