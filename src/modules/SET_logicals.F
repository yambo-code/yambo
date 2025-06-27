!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_logicals()
 !
 use pars,       ONLY:schlen,n_max_DB_IO_off_fields
 use drivers,    ONLY:l_eval_collisions
 use parallel_m, ONLY:IO_write_default,master_cpu,myid,master_cpu,myid,n_log_CPUs,ncpu
 use com,        ONLY:write_to_of,write_to_report,fat_log
 use IO_m,       ONLY:IO_write,IO_read,DBs_IO_control_string,DBs_FRAG_control_string, &
&                     io_DIP,io_RESPONSE,io_HF,io_COLLs,io_GF,io_OBSERVABLES,io_CARRIERs,io_SC,&
&                     io_BS_K,io_SCREEN,io_MULTIPOLE,io_V_bands, &
&                     frag_DIP,frag_RESPONSE,frag_MULTIPOLE,frag_HF,frag_SC,frag_DG,   &
&                     frag_BS_K,frag_SCREEN,frag_QINDX,frag_ELPH,frag_RT
 use stderr,     ONLY:write_to_log,write_fragments_IO_log,STRING_split,write_to_log_default
 use LIVE_t,     ONLY:live_timing_is_on
 !
 implicit none
 !
 ! Work Space
 !
 integer            ::i_ch
 character(schlen)  ::ch_piece(n_max_DB_IO_off_fields)
 !
 ! Parallel Master & CPU log
 !===========================
 !
 master_cpu                =myid==0
 !
 if (n_log_CPUs==0) write_to_log_default = .TRUE.
 if (n_log_CPUs> 0) write_to_log_default = (mod(myid,(ncpu-1)/n_log_CPUs+1)==0)
 !
 ! Write permissions 
 !===================
 !
 write_fragments_IO_log    =write_to_log_default.and.fat_log
 IO_write_default(myid+1)  =master_cpu
 IO_write                  =master_cpu
 write_to_of               =master_cpu
 write_to_report           =master_cpu
 IO_read                   =.TRUE.
 !
 ! Both set to write_to_log_default after the log file is initialized
 !
 write_to_log              =.FALSE. 
 live_timing_is_on         =.FALSE.
 !
 ! DBs IO
 !========
 io_DIP         =.TRUE.
 io_RESPONSE    =.TRUE.
 io_MULTIPOLE   =.TRUE.
 io_SCREEN      =.FALSE.
 io_HF          =.TRUE.
 io_COLLs       =.TRUE.
 io_GF          =.TRUE.
 io_V_bands     =.FALSE.
 io_OBSERVABLES =.TRUE.
 io_CARRIERs    =.TRUE.
 io_SC          =.TRUE.
 io_BS_K        =.TRUE.
 !
#if defined _NL
 io_CARRIERs    =.FALSE.
 io_GF          =.FALSE.
 io_V_bands     =.TRUE.
 io_OBSERVABLES =.TRUE.
#endif
 !
 ch_piece=' '
 call STRING_split(DBs_IO_control_string,ch_piece)
 !
 do i_ch=1,n_max_DB_IO_off_fields
   if (str_check(4,(/"DIP","dip","ALL","all"/),ch_piece(i_ch)) ) io_DIP     =.FALSE.
   if (str_check(4,(/"X  ","x  ","ALL","all"/),ch_piece(i_ch)) ) io_RESPONSE=.FALSE.
   if (str_check(4,(/"W  ","w  ","ALL","all"/),ch_piece(i_ch)) ) io_SCREEN  =.FALSE.
   if (str_check(4,(/"HF ","hf ","ALL","all"/),ch_piece(i_ch)) ) io_HF      =.FALSE.
   if (str_check(5,(/"COLLS","COLLs","colls","ALL  ","all  "/),&
&                                               ch_piece(i_ch)) ) io_COLLs      =.FALSE.
   if (str_check(4,(/"GF ","gf ","ALL","all"/),ch_piece(i_ch)) ) io_GF          =.FALSE.
   if (str_check(4,(/"Vb ","vb ","ALL","all"/),ch_piece(i_ch)) ) io_V_bands     =.FALSE.
   if (str_check(4,(/"J  ","j  ","ALL","all"/),ch_piece(i_ch)) ) io_OBSERVABLES =.FALSE. ! Back Compatibility
   if (str_check(4,(/"OBS","obs","ALL","all"/),ch_piece(i_ch)) ) io_OBSERVABLES =.FALSE. 
   if (str_check(5,(/"CARRIERs","carriers","CARRIERS","ALL     ","all     "/),&
&                                               ch_piece(i_ch)) ) io_CARRIERs=.FALSE.
   if (str_check(4,(/"SC ","sc ","ALL","all"/),ch_piece(i_ch)) ) io_SC      =.FALSE.
   if (str_check(4,(/"k  ","K  ","ALL","all"/),ch_piece(i_ch)) ) io_BS_K    =.FALSE.
   if (str_check(4,(/"BS ","bs ","ALL","all"/),ch_piece(i_ch)) ) io_BS_K    =.FALSE.
 enddo
 !
 ! Override input in some cases
 !
 io_RESPONSE=io_RESPONSE.or.l_eval_collisions
 !
 ! DBs fragmentation
 !===================
 frag_DIP      =.TRUE.
 frag_RESPONSE =.TRUE.
 frag_MULTIPOLE=.TRUE.
 frag_SCREEN   =.TRUE.
 frag_HF       =.FALSE.
 frag_SC       =.TRUE.
 frag_RT       =.TRUE.
 frag_ELPH     =.TRUE.
 frag_QINDX    =.FALSE.
 frag_BS_K     =.FALSE.
 frag_DG       =.TRUE.
 !
 ch_piece=' '
 call STRING_split(DBs_FRAG_control_string,ch_piece)
 !
 do i_ch=1,n_max_DB_IO_off_fields
   !
   if (str_check(4,(/"+DIP","+dip","+ALL","+all"/),ch_piece(i_ch)) ) frag_DIP     =.TRUE.
   if (str_check(4,(/"+X  ","+x  ","+ALL","+all"/),ch_piece(i_ch)) ) frag_RESPONSE=.TRUE.
   if (str_check(4,(/"+DG ","+dg ","+ALL","+all"/),ch_piece(i_ch)) ) frag_DG      =.TRUE.
   if (str_check(4,(/"+W  ","+w  ","+ALL","+all"/),ch_piece(i_ch)) ) frag_SCREEN  =.TRUE.
   if (str_check(4,(/"+HF ","+hf ","+ALL","+all"/),ch_piece(i_ch)) ) frag_HF      =.TRUE.
   if (str_check(4,(/"+k  ","+K  ","+ALL","+all"/),ch_piece(i_ch)) ) frag_BS_K    =.TRUE.
   if (str_check(4,(/"+BS ","+bs ","+ALL","+all"/),ch_piece(i_ch)) ) frag_BS_K    =.TRUE.
   if (str_check(4,(/"+QINDX","+qindx","+ALL  ","+all  "/),ch_piece(i_ch)) ) frag_QINDX   =.TRUE.
   !
   if (str_check(4,(/"-DIP","-dip","-ALL","-all"/),ch_piece(i_ch)) ) frag_DIP     =.FALSE.
   if (str_check(4,(/"-X  ","-x  ","-ALL","-all"/),ch_piece(i_ch)) ) frag_RESPONSE=.FALSE.
   if (str_check(4,(/"-DG ","-dg ","-ALL","-all"/),ch_piece(i_ch)) ) frag_DG      =.FALSE.
   if (str_check(4,(/"-W  ","-w  ","-ALL","-all"/),ch_piece(i_ch)) ) frag_SCREEN  =.FALSE.
   if (str_check(4,(/"-HF ","-hf ","-ALL","-all"/),ch_piece(i_ch)) ) frag_HF      =.FALSE.
   if (str_check(4,(/"-k  ","-K  ","-ALL","-all"/),ch_piece(i_ch)) ) frag_BS_K    =.FALSE.
   if (str_check(4,(/"-BS ","-bs ","-ALL","-all"/),ch_piece(i_ch)) ) frag_BS_K    =.FALSE.
   if (str_check(4,(/"-QINDX","-qindx","-ALL  ","-all  "/),ch_piece(i_ch)) ) frag_QINDX   =.FALSE.
   !
 enddo 
 !
contains
 !
 logical function str_check(n,strings,val)
    integer      :: n
    character(*) :: strings(n)
    character(*) :: val
    !
    integer :: i
    str_check=.false.
    !
    do i = 1, n
      if ( trim(strings(i))==trim(val) ) then
        str_check=.true.
        return
      endif
    enddo
 end function    
 !
end subroutine
