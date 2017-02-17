!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
integer function yambo_driver(instr,lnstr,inf,iinf,ind,&
&                             iind,od,iod,com_dir,icd,js,ijs,np,pid)
 !
 !Y88b    /   e           e    e      888~~'    ,88~-_   
 ! Y88b  /   d8b         d8b  d8b     888   |  d888   '  
 !  Y88b/   /Y88b       d888bdY88b    888 _/  88888    | 
 !   Y8Y   /  Y88b     / Y88Y Y888b   888  '  88888    | 
 !    Y   /____Y88b   /   YY   Y888b  888   |  Y888   /  
 !   /   /      Y88b /          Y888b 888__/    `88_-~   
 !
 use drivers,       ONLY:l_ppa,l_HF_and_locXC,l_sc_run,l_collisions_IO,l_real_time,&
&                        l_optics,l_chi,l_em1s,l_ppa,l_cohsex,l_bss,l_bse,l_em1d,&
&                        l_gw0,l_el_corr,l_life,l_acfdt,l_elph_Hamiltonian
 use X_m,           ONLY:X_t
 use par_proc_m,    ONLY:myid,ncpu,p_finalize
 use it_m,          ONLY:it_reset,infile
 use QP_m,          ONLY:QP_ng_Sx
 use wave_func,     ONLY:wf_ng
 use electrons,     ONLY:levels,E_reset
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use frequency,     ONLY:w_samp
 !
 implicit none
 type(levels) ::en,Xen,Ken
 type(bz_samp)::q,k,Xk
 ! 
 ! 4 response function & energies types: x s d p
 !
 type(X_t)    ::X(4) 
 type(w_samp) ::Xw(4)
 !
 integer,          intent(in)    :: lnstr,iind,iod,ijs,np,pid,icd
 integer,          intent(inout) :: iinf
 character(lnstr), intent(in)    :: instr
 character(iinf),  intent(in)    :: inf
 character(iind),  intent(in)    :: ind
 character(iod),   intent(in)    :: od
 character(ijs),   intent(in)    :: js
 character(icd),   intent(in)    :: com_dir
 !
 ! Local Variables
 !
 integer           ::i_err
 integer, external ::X_em1,init
 logical           ::l_pp_em1,l_dyn_em1
 !
 ! Presets
 !
 ncpu  =np
 myid  =pid
 infile=inf
 !
 call std_presets(instr,ind,od,js,com_dir)
 call it_reset(1)
 !
 ! not resetted in init call
 !
 call bz_samp_reset(Xk)  
 call E_reset(Xen) 
 call E_reset(Ken) 
 !
 ! INPUT FILE PARSING
 !====================
 !
 yambo_driver=0
 !
 ! iinf=0 => No error message from driver.c 
 !
 iinf=0 
 iinf=init(en,q,k,X,Xw,instr,lnstr,.false.,.false.)
 !
 ! the iinf==2 (DB listing mode) stops the job so no file editing
 ! or error msg from the driver.c is possible
 !
 if (iinf==2) then
   call p_finalize
   return
 endif
 if (iinf/=0) return
 !
 ! RUN SUMMARY 
 !=============
 !
 call IO_Setup(en,k)
 !
 ! SETUPS ...
 !============
 !
 ! Basical 
 !
 call setup(en,Xen,Ken,k,Xk)
 !
 ! Update the default variables
 !
 i_err=init(en,q,k,X,Xw,instr,lnstr,.true.,.false.)
 !
 ! BZ sampling
 !
 call bz_samp_indexes(k,Xk,q) 
 !
 ! COLOUMB DRIVER
 !================
 !
 call col_driver(maxval((/wf_ng,QP_ng_Sx/)),q)
 !
 ! reset the section numbering
 !
 call section('*','') 
 !
 ! RESPONSE FUNCTIONS  QP CORRECTIONS (FROM INPUT FILE)
 !======================================================
 !
 call QP_external_corrections(Xen,en,Xk,k,X)
 !
 ! HARTREE-FOCK AND VXC
 !======================
 !
 if (l_HF_and_locXC.and..not.any((/l_sc_run,l_collisions_IO,l_real_time/))) call XCo_driver(en,k,Xk,q)
 !
 ! RAS & REELS
 !=============
 !
 !
 ! OPTICS
 !========
 !
 if (l_optics.and.l_chi) call O_driver(Xen,Xk,q,Xw(3),X(3))
 ! 
 ! INVERSE DIELECTRIC FUNCTIONS
 !===============================
 !
 ! Static
 !--------
 !
 if (l_em1s)       i_err=X_em1(Xen,Xk,q,X(2),Xw(2),.false.)
 !
 ! Plasmon Pole 
 !--------------
 !
 l_pp_em1=l_ppa.and. ( l_em1d.or.(l_gw0.and.l_el_corr).or.l_bse )
 !
 if (l_pp_em1)     i_err=X_em1(Xen,Xk,q,X(4),Xw(4),.false.)
 !
 !
 ! EXTENDED COLLISIONS (HF & COHSEX)
 !===================================
 !
 !
 ! SELF-ENERGIES
 !===============
 !
 if (l_gw0.or.l_life) then
   !
   if (l_cohsex) then
     !
     ! COHSEX 
     !========
     !
     call QP_driver(X(2),Xen,Xk,en,k,q,Xw(2))
     !
   else if (l_ppa) then
     !
     ! PPA 
     !=====
     !
     call QP_driver(X(4),Xen,Xk,en,k,q,Xw(4))
     !
   else 
     !
     !
   endif
   !
 endif
 !
 ! BETHE-SALPETER
 !================
 !
 if ((l_optics.and.l_bse).or.l_bss) then
   if (l_ppa)      call K_driver(Xen,Ken,Xk,q,X(4),Xw(4))
   if (.not.l_ppa) call K_driver(Xen,Ken,Xk,q,X(2),Xw(2))
 endif
 !
 !
 if (l_sc_run) then
   !
   ! SC
   !====
   !
   !
   ! REAL-TIME
   !==========
   !
   !
 endif
 !
 ! Electron-Phonon Hamiltonian
 !=============================
 !
 ! GAME OVER :(
 !==============
 !
 call section('X','Game Over & Game summary')
 !
 ! Update init defs
 !
 i_err=init(en,q,k,X,Xw,instr,lnstr,.false.,.true.)
 !
 ! Finalize
 !
 call it_reset(-1)
 call p_finalize
 !
end function
