
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
integer function spp_i(instr,lnstr,inf,iinf,ind,iind,od,iod,&
&                      js,ijs,np,pid)
 !
 use drivers,             ONLY:infile_editing
 use par_proc_m,          ONLY:ncpu,myid,p_finalize
 use electrons,           ONLY:levels,E_reset,n_bands,default_nel
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,nkibz,ng_vec,&
&                              nXkibz,nqibz,k_pt
 use D_lattice,           ONLY:default_Tel,alat,nsym
 use IO_m,                ONLY:io_control,OP_RD_CL,NONE,DUMP
 use com,                 ONLY:msg,write_to_report
 use it_m,                ONLY:it_reset,infile
 use SPP
 implicit none
 !
 type(levels)                 :: en,Xen,Ken
 type(bz_samp)                :: k,Xk,q
 integer,          intent(in) :: lnstr,iind,iod,ijs,np,pid
 integer,       intent(inout) :: iinf
 character(lnstr), intent(in) :: instr
 character(iinf),  intent(in) :: inf
 character(iind),  intent(in) :: ind
 character(iod),   intent(in) :: od
 character(ijs),   intent(in) :: js
 ! 
 ! I/O 
 !
 integer           :: ID,i_err
 integer, external :: ioDB1,ioQINDX,iogrot,elph_pw_dbs
 !
 ! Presets
 !
 iinf  =0
 spp_i =0
 ncpu  =np
 myid  =pid
 infile=inf
 call std_presets(instr,ind,od,js)
 call bz_samp_reset(k)
 call bz_samp_reset(q)
 call bz_samp_reset(Xk)
 call E_reset(en)
 call E_reset(Xen)
 call E_reset(Ken)
 call it_reset(1)
 !
 ! Switch off report file support
 !
 write_to_report=.FALSE.
 !
 ! DB1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 iinf=ioDB1(en,k,ID)
 if (iinf/=0) then
   call spp_finalize()
   return
 endif
 !  
 ! G operations table and shells
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=ID)
 i_err=iogrot(ID) 
 if (i_err/=0) then
   call spp_finalize()
   return
 endif
 !
 ! INPUT FILE
 !
 call spp_init(instr,lnstr,.false.)
 if (infile_editing) then
   iinf=1
   call spp_finalize()
   return
 endif
 !
 call section('*','S(elf) P(ost)/(re) P(rocessor)')
 !
 call section('*','Core DB')
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',default_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 call msg('s',':: K points              :',nkibz)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmmetries           :',nsym)
 call msg('s',':: RL vectors            :',ng_vec)
 !
 ! BZ sampling index 
 !
 call section('*','K-point grid')
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2,3/),MODE=DUMP,ID=ID)
 i_err=ioQINDX(k,q,ID)
 ! 
 ! Allocate and fill the k_pt array for the DBs header
 !
 allocate(k_pt(k%nibz,3))
 k_pt=k%pt
 !
 if (i_err/=0) then
   call spp_finalize()
   return
 endif
 !
 call msg('s',':: Q-points   (IBZ):',nqibz)
 call msg('s',':: X K-points (IBZ):',nXkibz)
 !
 ! Main SETUP
 !
 call setup(en,Xen,Ken,k,Xk)
 !
 ! Specific SECTIONS
 !
 if (lkanalyze) call k_analize(en,k,Xk,q)
 !
 if (lbzrim) call BZ_RIM(Xk,Xen)
 !
 if (l_exc_props) call exc_properties(Xk,Xen)
 !
#if defined _SPP_ELPH
 !
 if (lelph) then
   i_err=elph_pw_dbs(k,en,q)
   call elph_eliashberg_dos(k,en,q)
 endif
 !
#endif
 !
#if defined _SPP_RAS
 !
 if (lras) call ras_spp
 if (lloc) call loc_spp
 if (ltrans) call trans_spp(en,k)
 !
#endif
 !
 ! END
 !
 call spp_finalize()
 return
 !
contains
  !
  subroutine spp_finalize()
    if (.not.infile_editing) then
      call section('X','Game Over')
      call spp_init(instr,lnstr,.TRUE.)
      call msg("nl","")
      call p_finalize()
    else
      call spp_init(instr,lnstr,.TRUE.)
    endif
  end subroutine
  !
end function
