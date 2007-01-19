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
 subroutine elph_gkkp_E_dep(k,en,q)
 !---------------------------------
 use pars,                ONLY:SP,schlen,pi,HA2GHZ,HA2THZ,HARTREE
 use com,                 ONLY:msg,of_open_close
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,nqibz,qindx_X
 use SPP,                 ONLY:elph_steps,elph_Ef,elph_gamma_broad,&
&                              elph_Emax,elph_broad
 use ELPH,                ONLY:ph_modes,elph_nb,elph_gkkp,ph_freqs_sq
 use IO_m,                ONLY:io_control,OP_RD,VERIFY,RD_CL_IF_END,DUMP
 use timing,              ONLY:live_timing
 use par_proc_m,          ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m,       ONLY:par_indexes
 implicit none
 type(levels) ::en
 type(bz_samp)::k,q
 !
 ! Work Space GPL_END
 !
 integer           ::i1,iw,im,iq,iibz1,iibz2,ib1,ib2
 real(SP)          ::gkkp_Ekkp_dep(elph_steps),Ekkp(elph_steps),e_step,&
&                    Ekkp_limits(2),E_diff
 real(SP), external::dFfermi
 character(schlen) ::o_file_name,ch
 type(pp_indexes)  ::px
 !
 !I/O
 !
 integer           ::io_err,ID
 integer, external ::ioELPH
 !
 call section("*","== Electron-Phonon Interface: Energy dependence ==")
 !---------------------------------------------------------------------
 !
 ! zeroing
 !
 gkkp_Ekkp_dep=0.
 Ekkp=0.
 !
 ! E->E' energy limit
 !
 ! all possible b -> b' (<b) 
 !
 Ekkp_limits=(/-10./HARTREE, maxval(en%E)-minval(en%E) /)
 e_step=( Ekkp_limits(2)-Ekkp_limits(1) )/(elph_steps-1)
 !
 do iw=1,elph_steps
   Ekkp(iw)=Ekkp_limits(1)+(iw-1)*e_step
 enddo
 !
 ! ELPH DB is sane
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=ioELPH(ID)
 call msg('s',':: Checking databases ...')
 if (io_err<0) then
   call msg('l','incorrect')
   return
 endif
 call msg('l','sane')
 !
 ! Eliashberg Function
 !
 call par_indexes(px,(/q%nibz/))
 call live_timing('Gkkp En Dep',px%stps(myid+1))
 !
 ! calculate q%weights
 !
 do iq=1,nqibz 
   !
   ! I/O
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID)  
   !
   if (.not.px%i1p(iq)) cycle
   !
   ! G_kkp as a function of the electronic transition
   !
   do i1=1,k%nbz
     iibz1=k%sstar(i1,1)
     iibz2=k%sstar(qindx_X(iq,i1,1),1)
     do ib1=1,elph_nb
       do ib2=ib1,1,-1
         ! 
         E_diff=en%E(ib1,iibz1,1)-en%E(ib2,iibz2,1)
         !
         do im=1,ph_modes
           do iw=1,elph_steps
             !
             gkkp_Ekkp_dep(iw)=gkkp_Ekkp_dep(iw) + &
&             conjg(elph_gkkp(i1,im,ib1,ib2))*elph_gkkp(i1,im,ib1,ib2)* &
&             dFfermi(Ekkp(iw)-E_diff,elph_broad)
             !
           enddo
         enddo
       enddo
     enddo
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 call pp_redux_wait(gkkp_Ekkp_dep)
 !
 ! Output file
 !
 o_file_name='gkkp'
 call of_open_close(o_file_name,'ot')
 call msg('o gkkp','#',' G_kkp m.e. as a function of the electronic transition',INDENT=0)
 call msg('o gkkp','#','',INDENT=0)
 call msg('o gkkp',         '#  Bands              :',elph_nb,INDENT=0)
 call msg('o gkkp','#','',INDENT=0)
 call msg('o gkkp','#',(/'E [eV]','Gkkp(E) [ThZ]'/),USE_TABS=.true.)
 call msg('o gkkp','#','',INDENT=0)
 do iw=1,elph_steps
   gkkp_Ekkp_dep(iw)=gkkp_Ekkp_dep(iw)*HA2THZ/real(k%nbz)**2.
   call msg('o gkkp','',(/Ekkp(iw)*HARTREE,gkkp_Ekkp_dep(iw)/),USE_TABS=.true.)
 enddo
 call of_open_close(o_file_name)
 !
 ! GPL_START
 !
 end subroutine
