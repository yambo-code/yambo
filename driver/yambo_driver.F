!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 use drivers
 use X_m,           ONLY:X_t
 use com,           ONLY:msg
 use par_proc_m,    ONLY:myid,ncpu,p_finalize
 use it_m,          ONLY:it_reset,infile
 use QP_m,          ONLY:QP_ng_Sx
 use QP_CTL_m,      ONLY:QP_apply
 use wave_func,     ONLY:wf_ng
 use electrons,     ONLY:levels,E_reset,n_bands
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use frequency,     ONLY:w_samp
#if defined _RAS 
 use ras_module,    ONLY:lras, lreels
#endif
#if defined _REELS 
 use reels_module,  ONLY:lreels
#endif
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
 integer           ::ierr
 integer, external ::X_em1,init
 logical           ::apply_QP
 !
 ! Presets
 !
 ncpu  =np
 myid  =pid
 infile=inf
 !
 call std_presets(instr,ind,od,js,com_dir)
#ifdef _RAS
 call ras_presets()
#endif
#ifdef _REELS
 call reels_presets()
#endif
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
 call job_setup(en,k)
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
 ierr=init(en,q,k,X,Xw,instr,lnstr,.true.,.false.)
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
 ! GPL_EXCLUDE_START
 !
 ! - Xo -
 !
 if (l_acfdt)   call QP_apply(X(1)%ib,Xen,Xk,1,msg_fmt='s')
 !
 ! GPL_EXCLUDE_END
 !
 ! - Dynamical X -
 !
 apply_QP=(l_optics.and.l_chi).or.(l_em1d.and..not.l_ppa).or.&
#if defined _REELS
        lreels.or.&
#endif
#if defined _RAS
        lras.or.lreels.or.&
#endif
        (l_gw0.and..not.l_ppa.and.l_el_corr).or.l_life
 if (apply_QP)  call QP_apply(X(3)%ib,Xen,Xk,1,msg_fmt='s')
 !
 ! - Static X -
 !
 apply_QP=l_em1s.or.(l_optics.and.l_bse).or.l_bss.or.(l_em1d.and.l_chosex)
 if (apply_QP)  call QP_apply(X(2)%ib,Xen,Xk,1,msg_fmt='s')
 !
 ! - Plasmon Pole X -
 !
 apply_QP=(l_em1d.and.l_ppa).or.(l_gw0.and.l_ppa.and.l_el_corr)
 if (apply_QP)  call QP_apply(X(4)%ib,Xen,Xk,1,msg_fmt='s')
 !
 ! - Green's Function (not the one used in the BSE -
 !
 if (l_gw0)     call QP_apply((/1,n_bands/),en,k,3,msg_fmt='s')
 !
 ! HARTREE-FOCK AND VXC
 !======================
 !
 if (l_xxvxc.and..not.l_sc_run) call QP_XX_Vxc(en,k,Xk,q)
 !
 ! RAS & REELS
 !=============
 !
#if defined _RAS
 if (lras.or.lreels) call rasdriver(Xen,Xk,q,Xw(3),X(3))
#endif
#if defined _REELS
 if (lreels)         call reelsdriver(Xen,Xk,q,Xw(3),X(3))
#endif
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
 !
 if (l_em1s)                            ierr=X_em1(Xen,Xk,q,X(2),Xw(2),.false.)
 !
 if (l_em1d.or.(l_gw0.and.l_el_corr)) then
   ! 
   ! Plasmon Pole 
   !
   if (l_ppa)                           ierr=X_em1(Xen,Xk,q,X(4),Xw(4),.false.)
   ! 
   ! GPL_EXCLUDE_START 
   ! 
   ! Dynamical 
   !
   if (.not.l_ppa.and..not.l_chosex)    ierr=X_em1(Xen,Xk,q,X(3),Xw(3),.false.)
   !
   ! GPL_EXCLUDE_END
   !
 endif
 !
 if (l_gw0) then
   !
   ! GPL_EXCLUDE_START
   !
   if (l_chosex) then
     !
     ! CHOSEX 
     !========
     !
     call QP_driver(X(2),Xen,Xk,en,k,q,Xw(2))
     !
   else if (l_ppa) then
     !
     ! GPL_EXCLUDE_END
     !
     ! PPA 
     !=====
     !
     call QP_driver(X(4),Xen,Xk,en,k,q,Xw(4))
     !
     ! GPL_EXCLUDE_START
     !
   else 
     !
     !
     ! Real Axis 
     !============
     !
     call QP_driver(X(3),Xen,Xk,en,k,q,Xw(3))
   endif
   !
   ! GPL_EXCLUDE_END
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
 ! GPL_EXCLUDE_START
 !
 ! ACFDT
 !=======
 !
 if (l_acfdt) call acfdt_tot_energy(Xen,Xk,q,X(1))
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SC
 ! SC
 !====
 !
 if (l_sc_run) call SC_driver(X(:2),Xw(:2),Xk,en,k,q)
#endif
 !
 ! GAME OVER :(
 !==============
 !
 call section('X','Game Over & Game summary')
 !
 ! Update init defs
 !
 ierr=init(en,q,k,X,Xw,instr,lnstr,.false.,.true.)
 !
 ! Finalize
 !
 call it_reset(-1)
 call p_finalize
 !
end function
