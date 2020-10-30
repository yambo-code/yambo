!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, FP
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
integer function ELPH_databases(k,E,q)
 !
 use pars,                ONLY:SP,schlen
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use interfaces,          ONLY:ELPH_alloc
 use ELPH,                ONLY:ph_freqs_sq,ph_modes,elph_nb,elph_use_q_grid,elph_nQ,&
&                              ph_kpt_bz,elph_nk_bz,GKKP,GKKP_bare,ph_qpt
 use com,                 ONLY:msg
 use R_lattice,           ONLY:bz_samp,qindx_B
 use D_lattice,           ONLY:nsym,n_atoms
 use parser_m,            ONLY:parser
 use YPPm,                ONLY:k_grid_printer
 use YPP_ELPH,            ONLY:ph_freqs_file,ph_modes_file,use_ext_ph_freqs,use_ext_ph_modes,&
&                              NQ_max,ikbz_is_table,DB_ph_qpt,DB_ph_freq_sq,grad_at_gamma
 use IO_m,                ONLY:io_control,OP_APP_CL,OP_RD_CL,VERIFY,IO_and_Messaging_switch
 use LIVE_t,              ONLY:live_timing 
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work Space...
 !
 integer            ::io_err(2),Q_IO_map(NQ_max),elph_nDBs,iq
 !
 ! I/O...
 integer            ::idb,io_elph_err,ID
 !
 logical            ::l_gkkp_expand,l_gkkp_bare
 character(schlen)  ::db_name,what
 integer            ::elph_nk,iq_bz,iq_s,i1
 !
 ! Externals...
 integer, external  ::io_ELPH,ELPH_databases_IO
 logical, external  ::file_exists
 !
 ! Logicals & Init
 !=================
 ELPH_databases=0
 call parser('GkkpReadBare',l_gkkp_bare)
 call parser('GkkpExpand',l_gkkp_expand)
 YAMBO_ALLOC(DB_ph_qpt,(NQ_max,3))
 !
 !NOTE: The bare databases are copies of the screened ones in all variables
 !      except elph_gkkp. This depends on how elphondb.f90 in QE/PHonon was
 !      modified.
 !      Therefore such redundant variables are overwritten when l_gkkp_bare is
 !      on, including io_err at the function call of ELPH_databases_IO.
 !
 what="(dressed)"
 if (l_gkkp_bare) what="(dressed+bare)"
 call section("*","== Electron-Phonon Interface: PW"//trim(what)//"->Yambo Databases ==")
 !
 ! K expand
 !==========
 call k_ibz2bz(k,'i',.false.)
 if (l_gkkp_expand.and..not.allocated(qindx_B)) then
   call msg('s',"Missing k/q scattering database")
   call error("Please run a setup with the BSEscatt flag activated")
 endif
 !
 call msg('s','PW(ELPH) databases ...')
 !=====================================
 !...CHECK
 elph_nDBs=0
 Q_IO_map =0
 do while (.TRUE.)
   elph_nDBs=elph_nDBs+1
   io_err(1)=ELPH_databases_IO("dV",GKKP,k,E,q,-elph_nDBs)
   io_err(2)=ELPH_databases_IO("dV_bare",GKKP_bare,k,E,q,-elph_nDBs)
   if (.not.io_err(2)==0) l_gkkp_bare=.FALSE.
   if (any((/io_err==-1/))) exit
   if (any((/io_err==-2/))) then
     call msg("l","incorrect K-point correspondance")
     return
   endif
   if (io_err(1)>0) Q_IO_map(elph_nDBs)=io_err(1)
 enddo
 elph_nDBs=elph_nDBs-1
 if (elph_nDBs==0) then
   ELPH_databases=-2
   call msg("l","not found")
   return
 endif
 !
 db_name='gkkp'
 if (l_gkkp_bare) db_name='gkkp_plus_bare'
 elph_nQ=elph_nDBs
 !
 if (elph_use_q_grid) then
   !
   call msg("l",'found '//trim(intc(elph_nQ))//' Q-grid compatible')
   !
   if (l_gkkp_expand) then
     !
     call k_ibz2bz(q,'i',.false.)
     !
     elph_nQ=q%nbz
     db_name='gkkp_expanded'
     if (l_gkkp_bare) db_name='gkkp_expanded_plus_bare'
     !
     call msg("s",'Database expanded to the whole '//trim(intc(elph_nQ))//' BZ Q-grid')
     !
   endif
   !
 else
   !
   call msg("l",'found '//trim(intc(elph_nQ))//' NOT Q-grid compatible')
   !
   ! In this case there is no sense in expanding the gkkp
   !
   l_gkkp_expand=.FALSE.
   !
 endif
 !
 ! Define elph_nk_bz before the ioELPH call to avoid a io_elph_err<0 here and 
 ! >0 after elph_nk_bz is defined
 !
 elph_nk_bz=k%nbz
 !
 ! Check if DB is already done
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_elph_err=io_ELPH(ID,trim(db_name))
 !
 if (io_elph_err==0) return
 !
 ! External freqs & modes ?
 !==========================
 if (file_exists(trim(ph_freqs_file))) then
   call msg('s',"Phonon frequencies (re)read from: "//trim(ph_freqs_file))
   use_ext_ph_freqs=.true.
 endif
 if (file_exists(trim(ph_modes_file))) then
   call msg('s',"Phonon modes (re)read from: "//trim(ph_modes_file))
   use_ext_ph_modes=.true.
 endif
 !
 ! Allocation...
 !===============
 call ELPH_alloc('PHONONS')
 call ELPH_alloc('DW',GKKP=GKKP,Nq=1)
 call ELPH_alloc('dV',GKKP=GKKP,Nq=1)
 YAMBO_ALLOC(grad_at_gamma,(k%nbz,elph_nb,elph_nb,n_atoms,3))
 YAMBO_ALLOC(DB_ph_freq_sq,(NQ_max,ph_modes))
 if (l_gkkp_bare) call ELPH_alloc('dV_bare',GKKP=GKKP_bare,Nq=1)
 !
 ! KBZ + SYM table
 !=================
 YAMBO_ALLOC(ikbz_is_table,(k%nbz,nsym))
 call k_syms_tab(ikbz_is_table)
 !
 ! Keep a copy of k-points to be used by _RT to rotate the unperturbed gkkp m.e.
 !
 ph_kpt_bz=k%ptbz
 !
 ! Force DB fragmentation
 !
 call IO_and_Messaging_switch("+DB_is_fragmented")
 !
 ! With the external ph_freqs_file I can overwrite the 
 ! phonon frequencies read from the s.dbph* files. In this way I can, for example, 
 ! use ph freqs obtained via interpolation (q2r, matdyn ...).
 ! 
 if (use_ext_ph_modes) then
   open(unit=98,file=trim(ph_modes_file))
   read(98,*)
   read(98,*)
 endif
 !
 call live_timing('ELPH databases (WRITE)',elph_nDBs)
 !
 iq_bz=0
 !
 do idb=1,elph_nDBs
   !
   iq=idb
   if (elph_use_q_grid) iq=Q_IO_map(idb)
   !
   if (l_gkkp_expand) then
     !
     ! Note that ph_qpt is correclty pointing to the q%ptbz. But ph_freqs_sq, instead, is not
     ! and must be re-pointed
     !
     do i1=1,q%nstar(iq)
       !
       iq_bz=iq_bz+1
       ph_qpt(iq_bz,:)=q%ptbz(iq_bz,:)
       !
       iq_s=q%star(iq,i1)
       !
       if (.not.l_gkkp_bare) then
         io_err(1)=ELPH_databases_IO(k,E,q,idb)
       else
         io_err(1)=ELPH_databases_IO(k,E,q,idb) ! Obtain elph_gkkp
         io_err(1)=ELPH_databases_IO(k,E,q,idb) ! Obtain elph_gkkp_bare
       endif
       !
       ph_freqs_sq(iq_bz,:)=DB_ph_freq_sq(idb,:)
       !
       if (iq_bz==1) then
         call io_control(ACTION=OP_APP_CL,SEC=(/1/),ID=ID)
         io_elph_err=io_ELPH(ID,trim(db_name))
       endif
       !
       call ELPH_gkkp_expand(iq_bz,iq_s,k,.FALSE.) ! Expand elph_gkkp
       if (l_gkkp_bare) call ELPH_gkkp_expand(iq_bz,iq_s,k,.TRUE.) ! Expand elph_gkkp_bare
       !
       call io_control(ACTION=OP_APP_CL,SEC=(/iq_bz+1/),ID=ID)
       io_elph_err=io_ELPH(ID,trim(db_name))
       !
     enddo
     !
   else
     !
     ph_qpt(iq,:)=DB_ph_qpt(idb,:)
     ph_freqs_sq(iq,:)=DB_ph_freq_sq(idb,:)
     !
     io_err(1)=ELPH_databases_IO("dV",GKKP,k,E,q,idb)
     if (l_gkkp_bare) io_err(2)=ELPH_databases_IO("dV_bare",GKKP_bare,k,E,q,idb)
     !
     if (io_err(1)==-1) then
       call live_timing(steps=1)
       cycle
     endif
     !
     if (io_err(1)==-4) call warning("K-grid is not fully q-connected @ Q"//trim(intc(iq)))
     !
     if (idb==1) then
       call io_control(ACTION=OP_APP_CL,SEC=(/1/),ID=ID)
       io_elph_err=io_ELPH(ID,trim(db_name))
     endif
     !
     call io_control(ACTION=OP_APP_CL,SEC=(/iq+1/),ID=ID)
     io_elph_err=io_ELPH(ID,trim(db_name))
     !
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 if (use_ext_ph_freqs) close(99)
 if (use_ext_ph_modes) close(98)
 !
 call msg("s",' Modes           ',ph_modes)
 call msg("s",' Bands range     ',elph_nb)
 !
 ! Print the list of PW q-points (to be used with matdyn, for example)
 !
 call msg("s",'Databases Q-points PW-formatted')
 call k_grid_printer(elph_nDBs,ph_qpt,'iku',"PW",coo_out="alat")
 !
 call ELPH_alloc('FREE',GKKP)
 call ELPH_alloc('FREE',GKKP_bare)
 YAMBO_FREE(DB_ph_qpt)
 YAMBO_FREE(DB_ph_freq_sq)
 YAMBO_FREE(grad_at_gamma)
 !
 call IO_and_Messaging_switch("-DB_is_fragmented")
 !
 contains
   !
   subroutine k_syms_tab(ikbz_is_table)
   !-----------------------------------
    use pars,           ONLY:SP
    use vec_operate,    ONLY:c2a
    use D_lattice,      ONLY:nsym,sop_tab
    use R_lattice,      ONLY:rl_sop,nkbz
    implicit none
    integer :: ikbz_is_table(nkbz,nsym)
    integer :: i1,is,ir,ikibz,i2
    real(SP):: r_v(3)
    ikbz_is_table=0
    !
    ! First I find the action of all the syms on the IBZ kpts
    !
    ! R_is k_ibz = k_{s_table(k_ibz,is)}
    !
    ! where s_table(k_ibz,is) is a BZ index
    !
    call k_build_up_BZ_tables(k)
    call k_ibz2bz(k,'i',.false.) ! in k_sym2sym there is the k_ibz2bz(k,'d',...) call
    !
    ! To extend s_table to the BZ I simply have that
    !
    ! R_is k_bz = R_is S k_ibz = s_table(k_ibz,sop_tab(R_is,S))
    !
    do i1=1,k%nbz
      ikibz=k%sstar(i1,1)
      ir   =k%sstar(i1,2)
      do is=1,nsym
        ikbz_is_table(i1,is)=k%k_table(ikibz,sop_tab(is,ir))
      enddo
    enddo
    !
   end subroutine
   !
end function
