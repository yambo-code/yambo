!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
integer function ydb_i(lnstr,instr,lstrings,desc,vers,runlevels,iif)
 !
 use pars,                ONLY:schlen,lchlen
 use IO_m,                ONLY:mk_dir,cp_file,io_control,OP_APP_WR_CL,IO_write,IO_read
 use com,                 ONLY:file_exists
 use LOGO,                ONLY:pickup_a_random
 use stderr,              ONLY:string_split,write_to_log,log_as_a_file,intc,cstr
 use LIVE_t,              ONLY:LIVE_message
 use parser_lib,          ONLY:iparse_init,iparse_end
 use parser_m,            ONLY:parser
 use parallel_m,          ONLY:ncpu
 !
 implicit none
 integer,            intent(inout) :: lnstr
 character(lnstr),   intent(inout) :: instr
 integer,            intent(inout) :: iif,lstrings
 character(lstrings),intent(inout) :: desc,vers,runlevels
 !
 ! Work Space
 !
 integer,parameter::n_max_pieces=100
 character(schlen)::rstr_piece(n_max_pieces),known_runlevels(200)
 character(lchlen)::PATH,file_name,host_name,file_PATH,runl_PATH,desc_PATH,vers_PATH,db_PATH
 integer          ::hn_len,n_pieces,i1,i_random,i_err,ID
 integer          ::dz(8),year,day,month,hour,minutes,i_ref
 logical          ::l_found
 character(5)     ::cz
 character(8)     ::cd
 character(10)    ::ctz
 integer,external :: io_YDB
 !
 ! Presets
 !
 ydb_i =0
 iif   =0
 ncpu  =1
 write_to_log =.TRUE.
 log_as_a_file=.FALSE.
 IO_write     =.TRUE.
 IO_read      =.TRUE.
 known_runlevels(:)=' '
 known_runlevels(1)='setup'
 known_runlevels(1)='optics'
 known_runlevels(2)='chi'
 known_runlevels(3)='chik'
 known_runlevels(4)='bse'
 known_runlevels(5)='bsk'
 known_runlevels(6)='bss'
 known_runlevels(7)='tddft'
 known_runlevels(8)='em1s'
 known_runlevels(9)='em1d'
 known_runlevels(10)='ppa'
 known_runlevels(11)='HF_and_locXC'
 known_runlevels(12)='gw0'
 known_runlevels(13)='life'
 known_runlevels(14)='rim_cut'
 known_runlevels(15)='cohsex'
 known_runlevels(16)='acfdt'
 known_runlevels(17)='ElPhHam'
 known_runlevels(18)='scpot'
 known_runlevels(19)='collisions'
 known_runlevels(20)='negf'
 known_runlevels(21)='magnetic'
 known_runlevels(22)='sursp'
 known_runlevels(23)='reels'
 known_runlevels(24)='ras'
 known_runlevels(25)='el_ph'
 known_runlevels(26)='el_el'
 known_runlevels(27)='bzgrids'
 known_runlevels(28)='K_grid'
 known_runlevels(29)='Q_grid'
 known_runlevels(30)='dos'
 known_runlevels(31)='Shifted_Grid'
 known_runlevels(32)='High_Symm'
 known_runlevels(33)='excitons'
 known_runlevels(34)='electrons'
 known_runlevels(35)='freehole'
 known_runlevels(36)='amplitude'
 known_runlevels(37)='plot'
 known_runlevels(38)='density'
 known_runlevels(39)='wannier'
 known_runlevels(40)='bnds'
 known_runlevels(41)='wavefunction'
 known_runlevels(42)='eliashberg'
 known_runlevels(43)='phonons'
 known_runlevels(44)='gkkp'
 known_runlevels(45)='angular_m'
 known_runlevels(46)='position'
 known_runlevels(47)='surf'
 known_runlevels(48)='ras'
 known_runlevels(49)='reels'
 known_runlevels(50)='loc'
 known_runlevels(51)='trans'
 known_runlevels(52)='spin'
 known_runlevels(53)='magnetization'
 known_runlevels(54)='bzrim'
 known_runlevels(55)='QPDBs'
 known_runlevels(56)='QPDB_edit'
 known_runlevels(57)='QPDB_merge'
 known_runlevels(58)='bxsf'
 known_runlevels(59)='fixsyms'
 known_runlevels(60)='occupations'
 known_runlevels(61)='rtX'
 known_runlevels(62)='Gret'
 known_runlevels(63)='MeanPot'
 !
 ! INPUT string splitter
 !
 call string_split(instr(:lnstr),rstr_piece)
 n_pieces=0
 do i1=1,n_max_pieces
   if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
 enddo
 if (n_pieces==0) return
 !
 ! Date and Time
 !
 call date_and_time(cd,ctz,cz,dz)
 call igethname(host_name,hn_len)
 year=dz(1)
 month=dz(2)
 day=dz(3)
 hour=dz(5)
 minutes=dz(6)
 call LIVE_message('YDB running @'//host_name(:hn_len)//' '//trim(intc(day))//&
&                  '-'//trim(intc(month))//'-'//trim(intc(year))//' '//&
&                  trim(intc(hour))//':'//trim(intc(minutes)),"n","n","%s")
 !
 ! PATH
 !
 if (file_exists('~/.ydb/PATH')) then
   open(unit=1,file='~/.ydb/PATH',form='formatted')
   read (1,'(a)') PATH
   db_PATH=trim(PATH)//'/ndb.YDB'
   close(1)
 else 
   call LIVE_message('[ERROR] DB not found ',"n","n","%s")
   stop
 endif
 call LIVE_message('DB is in '//trim(PATH),"n","n","%s")
 !
 ! REF
 !
 i_ref=0
 if (check_for_word("ref")>0) then
   read(rstr_piece(check_for_word("ref")+1),'(i)') i_ref
   call LIVE_message('Reference ID is '//trim(intc(i_ref)),"n","n","%s")
 endif
 !
 ! ADD
 !
 if (check_for_word("add")>0) then
   !
   ! DB
   !
   call io_control(ACTION=OP_APP_WR_CL,ID=ID)
   !
   if (check_for_word("in")>0.or.check_for_word("out")>0) then
     read(rstr_piece(check_for_word("in")+1),'(a)') file_name
     if (.not.file_exists(trim(file_name))) then
       call LIVE_message('File '//trim(file_name)//' not found',"n","n","%s")
       return
     endif
   endif
   call LIVE_message('Adding ... '//trim(file_name),"n"," ","%s")
   !
   i_random=pickup_a_random(1000000.)
   !
   call mk_dir(trim(PATH)//'/'//trim(intc(year)))
   call mk_dir(trim(PATH)//'/'//trim(intc(year))//'/'//trim(intc(month)))
   call mk_dir(trim(PATH)//'/'//trim(intc(year))//'/'//trim(intc(month))//'/'//trim(intc(day)))
   PATH=trim(PATH)//'/'//trim(intc(year))//'/'//trim(intc(month))//&
&              '/'//trim(intc(day))//'/'//trim(intc(i_random))
   !
   if (check_for_word("in")>0) then
     call mk_dir(trim(PATH))
     call build_up_paths(i_random)
     open(unit=1,file=trim(desc_PATH),form='formatted')
     open(unit=2,file=trim(vers_PATH),form='formatted')
     open(unit=3,file=trim(runl_PATH),form='formatted')
     i_err=iparse_init(cstr(trim(file_name)),cstr('-'))
     do i1=1,200
       if (len_trim(known_runlevels(1))==0) cycle
       call parser(trim(known_runlevels(i1)),l_found)
       if (l_found) write (3,'(a)') ' '//trim(known_runlevels(i1))
     enddo
     iif=0
     write (1,'(/a/)') ' Description relative INPUT file '//trim(file_name)
     call iparse_end()
     call cp_file(trim(file_name),trim(file_PATH),i_err)
     close(1)
     close(2)
     close(3)
   endif
   !
!   if (check_for_word("out")>0) then
!     call mk_dir(trim(PATH))
!     call build_up_paths(i_random)
!     open(unit=1,file=trim(desc_PATH),form='formatted')
!     i_err=iparse_init(cstr(trim(file_name)),cstr('-'))
!     do i1=1,200
!       if (len_trim(known_runlevels(1))==0) cycle
!       call parser(trim(known_runlevels(i1)),l_found)
!       if (l_found) write (3,'(a)') ' '//trim(known_runlevels(i1))
!     enddo
!     iif=0
!     write (1,'(/a/)') ' Description relative INPUT file '//trim(file_name)
!     call iparse_end()
!     call cp_file(trim(file_name),trim(file_PATH),i_err)
!     !i_err=io_YDB(trim(db_PATH),ID) 
!     close(1)
!   endif
   !
 endif
 !
 call LIVE_message(' ',"n","n","%s")
 !
 contains
   !
   integer function check_for_word(word)
     character(*) :: word
     integer      :: ic
     check_for_word=-1
     do ic=1,n_max_pieces
       if (trim(rstr_piece(ic))==word) check_for_word=ic
     enddo
   end function
   !
   subroutine build_up_paths(id)
     integer :: id
     desc_PATH=trim(PATH)//'/description'
     runl_PATH=trim(PATH)//'/runlevels'
     vers_PATH=trim(PATH)//'/version'
     file_PATH=trim(PATH)//'/'//trim(file_name)
     desc=desc_PATH
     vers=vers_PATH
     runlevels=runl_PATH
   end subroutine
   !
end function
