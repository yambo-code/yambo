!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine ELPH_databases(k,E,q)
 !
 use pars,                ONLY:schlen,SP
 use stderr,              ONLY:intc
 use electrons,           ONLY:levels
 use interfaces,          ONLY:ELPH_alloc
 use ELPH,                ONLY:elph_use_q_grid,ph_modes,elph_nb,elph_nQ,GKKP,GKKP_bare,PH_W_debye,&
&                              elph_nk_bz,PH_freqs_sq,PH_qpt,PH_pol_vector,GKKP_me
 use com,                 ONLY:msg
 use D_lattice,           ONLY:nsym,sop_tab
 use R_lattice,           ONLY:bz_samp,qindx_B,nqibz
 use parser_m,            ONLY:parser
 use vec_operate,         ONLY:v_is_zero
 use zeros,               ONLY:k_iku_zero
 use YPP_ELPH,            ONLY:ELPH_DB_alloc,ELPH_DB_free,DB_kind,DB_nq,DB_ph_K_sym_table,DB_nb,&
&                              ph_freqs_file,ph_modes_file,use_ext_ph_freqs,use_ext_ph_modes,DB_ph_modes,&
&                              DB_PH_pol_vec,DB_Q_map,DB_ph_freq_sq,DB_PH_qpt
 use LIVE_t,              ONLY:live_timing 
 use IO_m,                ONLY:io_control,OP_APP_CL
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work Space...
 !
 logical            ::l_gkkp_expand,l_gkkp_bare
 integer            ::io_err(3),iq,ik,is,idb,i_star,iq_bz,ID
 character(schlen)  ::what
 integer, external  ::ELPH_databases_IO_elemental,io_ELPH
 logical, external  ::file_exists
 real(SP)           ::v(3)
 type(GKKP_me)      ::GKKP_save,GKKP_bare_save
 !
 call section("*","== Electron-Phonon Databases ==")
 !
 ! Logicals & Init
 !=================
 call parser('GkkpReadBare',l_gkkp_bare)
 call parser('GkkpExpand',l_gkkp_expand)
 !
 if (l_gkkp_expand.and..not.allocated(qindx_B)) then
   call msg('s',"Missing k/q scattering database")
   call error("Please run a setup with the BSEscatt flag activated")
 endif
 !
 ! Existence and dimensions 
 !==========================
 call msg('s','Inspecting databases ...')
 io_err(1)=ELPH_databases_IO_elemental("dV EXIST",1)
 if (io_err(1)/=0) then
   call msg("l","not found")
   return
 endif
 what="dressed"
 if (l_gkkp_bare) then
   io_err(2)=ELPH_databases_IO_elemental("dV_bare EXIST",1)
   if (io_err(2)==0) what="dressed+bare"
 endif
 call msg('l',trim(DB_kind)//" ("//trim(what)//')...')
 !
 ! Number of Q-points
 !===================
 DB_nq=1
 do while (io_err(1)==0) 
   io_err(1)=ELPH_databases_IO_elemental("dV EXIST",DB_nq+1)
   if (l_gkkp_bare) then 
     io_err(2)=ELPH_databases_IO_elemental("dV_bare EXIST",DB_nq+1)
     if (io_err(1)==0.and.io_err(2)/=0) call error("dV_bare is damaged/non existing")
   endif
   if (io_err(1)==0) DB_nq=DB_nq+1
 enddo
 call msg('l','found '//trim(intc(DB_nq))//' Q-points')
 !
 ! Actual reading
 !================
 call ELPH_DB_alloc( )
 !
 ! ... K table
 !-------------
 YAMBO_ALLOC(DB_ph_K_sym_table,(k%nbz,nsym))
 !
 ! First I find the action of all the syms on the IBZ kpts
 !
 ! R_is k_ibz = k_{s_table(k_ibz,is)}
 !
 ! where s_table(k_ibz,is) is a BZ index
 !
 call k_build_up_BZ_tables(k)
 !
 ! ... k/q expansion
 call k_ibz2bz(k,'i',.false.) ! in k_sym2sym there is the k_ibz2bz(k,'d',...) call
 call k_ibz2bz(q,'i',.false.) 
 !
 ! To extend s_table to the BZ I simply have that
 !
 ! R_is k_bz = R_is S k_ibz = s_table(k_ibz,sop_tab(R_is,S))
 !
 do ik=1,k%nbz
   do is=1,nsym
     DB_ph_K_sym_table(ik,is)=k%k_table(k%sstar(ik,1),sop_tab(is,k%sstar(ik,2)))
   enddo
 enddo
 !
 ! External freqs & modes ?
 !-------------------------
 use_ext_ph_freqs=.false.
 if (file_exists(trim(ph_freqs_file))) then
   call msg('s',"Phonon frequencies read from: "//trim(ph_freqs_file))
   use_ext_ph_freqs=.true.
 endif
 use_ext_ph_modes=.false.
 if (file_exists(trim(ph_modes_file))) then
   call msg('s',"Phonon modes read from: "//trim(ph_modes_file))
   use_ext_ph_modes=.true.
 endif
 !
 ! ... Frequencies & Pol Vectors
 !-------------------------------
 call live_timing('ELPH databases: phonon frequencies and eigenvectors',DB_nq)
 do iq=1,DB_nq
   io_err(1)=ELPH_databases_IO_elemental("dV FREQS POL_VEC",iq)
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 ! ... Grids check 
 !-----------------
 call live_timing('ELPH databases: K+Q-grid check',DB_nq)
 do iq=1,DB_nq
   io_err(1)=ELPH_databases_IO_elemental("dV KPT",iq)
   call ELPH_databases_IO_grids_check(k)
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 ! ... Q-grid is uniform?
 !------------------------
 do idb=1,DB_nq
   do iq=1,nqibz
     v=DB_PH_qpt(iabs(IDB),:)
     if (v_is_zero(v+q%pt(iq,:),zero_=k_iku_zero)) DB_Q_map(iabs(IDB))=iq
   enddo
 enddo
 elph_use_q_grid=all((/DB_Q_map/=0/))
 !
 call msg('s',':: Code generator        :',DB_kind)
 call msg('s',':: DB Kind               :',what)
 call msg('s',':: Expanded              :',l_gkkp_expand)
 call msg('s',':: Q-points              :',DB_nq)
 call msg('s',':: Bands                 :',DB_nb)
 call msg('s',':: Branches              :',DB_ph_modes)
 call msg('s',':: Uniform sampling      :',elph_use_q_grid)
 call msg('s',':: Debye Energy          :',PH_W_debye*1000.,"[meV]")
 !
 ! ... Transfer and write ...
 !----------------------------
 ph_modes   =DB_ph_modes
 elph_nb    =DB_nb
 elph_nQ    =DB_nq
 if (l_gkkp_expand) elph_nQ=q%nbz
 elph_nk_bz =k%nbz
 call ELPH_alloc("PHONONS")
 call ELPH_alloc("dV DW",GKKP=GKKP,Nq_mem=1)
 if (l_gkkp_bare) call ELPH_alloc("dV_bare",GKKP=GKKP_bare,Nq_mem=1)
 !
 !... Frequencies & pol vectors
 !
 do idb=1,DB_nq
   iq=DB_Q_map(idb)
   if (l_gkkp_expand) then
     do i_star=1,q%nstar(iq)
       iq_bz=i_star
       if (iq>1) iq_bz=sum(q%nstar(:iq-1))+i_star
       PH_freqs_sq(iq_bz,:)      =abs(DB_ph_freq_sq(idb,:))
       PH_qpt(iq_bz,:)           =q%ptbz(iq_bz,:)
       PH_pol_vector(:,:,:,iq_bz)=DB_PH_pol_vec(idb,:,:,:)
     enddo
   else
     PH_freqs_sq(iq,:)      =abs(DB_ph_freq_sq(idb,:))
     PH_qpt(iq,:)           =DB_PH_qpt(idb,:)
     PH_pol_vector(:,:,:,iq)=DB_PH_pol_vec(idb,:,:,:)
   endif
 enddo
 !
 !... GKKP
 call live_timing('ELPH databases: transfer and I/O',DB_nq)
 do idb=1,DB_nq
   !
   io_err(1)=ELPH_databases_IO_elemental("dV KPT GKKP",idb)
   call ELPH_databases_IO_grids_check(k)
   call ELPH_databases_IO_transfer_and_write(idb,k,E,q,GKKP)
   call ELPH_databases_IO_gkkp_expand("K",DB_Q_map(idb),1,k,GKKP)
   !
   if (l_gkkp_bare) then 
     io_err(2)=ELPH_databases_IO_elemental("dV_bare GKKP",idb)
     call ELPH_databases_IO_transfer_and_write(idb,k,E,q,GKKP_bare)
     call ELPH_databases_IO_gkkp_expand("K",DB_Q_map(idb),1,k,GKKP_bare)
   endif
   !
   call live_timing(steps=1)
   !
   if (l_gkkp_expand) then
     if (.not.allocated(GKKP_save%dVc)) call ELPH_alloc("dV",GKKP=GKKP_save,Nq_mem=1)
     if (.not.allocated(GKKP_bare_save%dVc).and.l_gkkp_bare) call ELPH_alloc("dV",GKKP=GKKP_bare_save,Nq_mem=1)
     GKKP_save%dVc=GKKP%dVc
     if (l_gkkp_bare) GKKP_bare_save%dVc=GKKP_bare%dVc
     iq=DB_Q_map(idb)
     do i_star=1,q%nstar(iq)
       iq_bz=i_star
       if (iq>1) iq_bz=sum(q%nstar(:iq-1))+i_star
       GKKP%dVc=GKKP_save%dVc
       call ELPH_databases_IO_gkkp_expand("Q",iq_bz,q%star(iq,i_star),k,GKKP)
       if (l_gkkp_bare) then
         GKKP%dVc=GKKP_bare_save%dVc
         call ELPH_databases_IO_gkkp_expand("Q",iq_bz,q%star(iq,i_star),k,GKKP_bare)
       endif
       if (iq_bz==1) call io_control(ACTION=OP_APP_CL,SEC=(/1,2/),ID=ID)
       if (iq_bz> 1) call io_control(ACTION=OP_APP_CL,SEC=(/iq_bz+1/),ID=ID)
       if (     l_gkkp_bare) io_err(3)=io_ELPH(ID,'gkkp_expanded_plus_bare')
       if (.not.l_gkkp_bare) io_err(3)=io_ELPH(ID,'gkkp_expanded')
     enddo
   else
     if (idb==1) call io_control(ACTION=OP_APP_CL,SEC=(/1,2/),ID=ID)
     if (idb> 1) call io_control(ACTION=OP_APP_CL,SEC=(/idb+1/),ID=ID)
     if (     l_gkkp_bare) io_err(3)=io_ELPH(ID,'gkkp_plus_bare')
     if (.not.l_gkkp_bare) io_err(3)=io_ELPH(ID,'gkkp')
   endif
   !
 enddo
 call live_timing()
 !
 ! Clean
 !=======
 call ELPH_DB_free( )
 call ELPH_alloc("FREE",GKKP=GKKP)
 call ELPH_alloc("FREE",GKKP=GKKP_save)
 if (l_gkkp_bare) then
   call ELPH_alloc("FREE",GKKP=GKKP_bare)
   call ELPH_alloc("FREE",GKKP=GKKP_bare_save)
 endif
 !
end subroutine
