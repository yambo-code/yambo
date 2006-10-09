!
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
!   ______   ______   _       _______
!  /  ____) |  ____) | |     |  _____)
! (  (___   | (___   | |     | (_____
!  \___  \  |  ___)  | |     |  _____)
!  ____)  ) | (____  | |___  | |
! (______/  |______) |_____) |_| 
!
 integer function self_driver(instr,lnstr,inf,iinf,ind,&
&                             iind,od,iod,js,ijs,np,pid)
!=======================================================
 use drivers
 use X_m,           ONLY:X_t
 use com,           ONLY:msg
 use par_proc_m,    ONLY:myid,ncpu,p_finalize
 use it_m,          ONLY:it_reset,infile
 use QP_CTL,        ONLY:QP_apply
 use wave_func,     ONLY:wf_ng
 use electrons,     ONLY:levels,E_reset
 use R_lattice,     ONLY:bz_samp,bz_samp_reset
 use frequency,     ONLY:w_samp
#if defined PJ_RAS 
 use ras_module,    ONLY:lras, lreels
#endif
#if defined PJ_REELS 
 use reels_module,  ONLY:lreels
#endif
 implicit none
 type(levels) ::en,Xen
 type(bz_samp)::q,k,Xk
 type(X_t)    ::X(4) ! x s d p
 type(w_samp) ::Xw(4)
 integer,          intent(in)    :: lnstr,iind,iod,ijs,np,pid
 integer,          intent(inout) :: iinf
 character(lnstr), intent(in)    :: instr
 character(iinf),  intent(in)    :: inf
 character(iind),  intent(in)    :: ind
 character(iod),   intent(in)    :: od
 character(ijs),   intent(in)    :: js
!ws
 integer           ::ierr
 integer, external ::X_em1,init
 logical           ::switch
!----------------------------------------------------------------
! PRESETS
!:::::::::
 ncpu  =np
 myid  =pid
 infile=inf
 call std_presets(instr,ind,od,js)
#ifdef PJ_RAS
 call ras_presets()
#endif
#ifdef PJ_REELS
 call reels_presets()
#endif
 call it_reset(1)
 call bz_samp_reset(Xk)  
 call E_reset(Xen) 
!----------------------------------------------------------------
! Input file parsing
!::::::::::::::::::::
 self_driver=0
 iinf=0 !  No error message from driver.c 
 iinf=init(en,q,k,X,Xw,instr,lnstr,.false.,.false.)
!
! note the goto 1 stops the job so no file editing
! of error msg from the driver.c is possible
 if (iinf==2) goto 1
 if (iinf/=0) return
!----------------------------------------------------------------
! Run Summary 
!::::::::::::::::
!
 call run_summary(en,k)
!----------------------------------------------------------------
! Internal setup
!::::::::::::::::
 call setup(en,Xen,k,Xk)
 ierr=init(en,q,k,X,Xw,instr,lnstr,.true.,.false.)
 call qindx(k,Xk,q) 
 call col_driver()
!-----------------------------------------------------------------------
! QP corrections (from input file)
!::::::::::::::::::::::::::::::::::
 call section('*','') ! Reset the section numbering
 switch=(l_optics.and.l_lr).or.(l_em1d.and..not.l_ppa).or.&
#if defined PJ_REELS
        lreels.or.&
#endif
#if defined PJ_RAS
        lras.or.&
#endif
        (l_gowo.and..not.l_ppa.and.l_el_corr).or.l_life
 if (switch) call QP_apply(X(3)%ib,Xen,Xk,1,msg_fmt='s')
 switch=l_em1s.or.(l_optics.and.l_bse).or.l_bss
 if (switch) call QP_apply(X(2)%ib,Xen,Xk,1,msg_fmt='s')
 switch=(l_em1d.and.l_ppa).or.(l_gowo.and.l_ppa.and.l_el_corr)
 if (switch) call QP_apply(X(4)%ib,Xen,Xk,1,msg_fmt='s')
 if (l_acfdt) call QP_apply(X(1)%ib,Xen,Xk,1,msg_fmt='s')
!----------------------------------------------------------------------------
 if (l_xxvxc) call QP_xxvxc(en,k,Xk,q)

#if defined PJ_RAS
 if (lras.or.lreels) call rasdriver(Xen,Xk,q,Xw(3),X(3))
#endif
#if defined PJ_REELS
 if (lreels)         call reelsdriver(Xen,Xk,q,Xw(3),X(3))
#endif
 if (l_optics.and.l_lr) call O_driver(Xen,Xk,q,Xw(3),X(3))

 if (l_em1s)                            ierr=X_em1(Xen,Xk,q,X(2),Xw(2),.false.)
 if (l_em1d.or.(l_gowo.and.l_el_corr)) then
   if (l_ppa)                           ierr=X_em1(Xen,Xk,q,X(4),Xw(4),.false.)
   if (.not.l_ppa)                      ierr=X_em1(Xen,Xk,q,X(3),Xw(3),.false.)
 endif
 if ((l_gowo.and..not.l_ppa).or.l_life) call QP_driver(X(3),Xen,Xk,en,k,q,Xw(3))
 if (l_gowo.and.l_ppa)                  call QP_driver(X(4),Xen,Xk,en,k,q,Xw(4))
 if ((l_optics.and.l_bse).or.l_bss) call K_driver(Xen,Xk,q,X(2),Xw(2))
 if (l_acfdt) call acfdt_tot_energy(Xen,Xk,q,X(1))
!----------------------------------------------------------------------------
 call section('X','Game Over & Game summary')
 ierr=init(en,q,k,X,Xw,instr,lnstr,.false.,.true.)
 call it_reset(-1)
1 call p_finalize
 end function
