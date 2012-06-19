!
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team 
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
subroutine QP_Green_DOS(qp)
 !
 ! This routine calcualtes the dos constructed from the Green functions
 !
 use pars,      ONLY:SP,lchlen,schlen,pi
 use com,       ONLY:msg,of_open_close
 use units,     ONLY:HA2EV
 use QP_m,      ONLY:QP_t,QP_Sc_steps,QP_table,QP_n_states
 use electrons, ONLY:spin,n_sp_pol,spin_occ
 !
 implicit none
 !
 type(QP_t)            :: qp
 !
 ! Work Space
 !
 character(lchlen)     :: DOS_Green_name
 character(schlen)     :: titles(3*n_sp_pol+1)
 integer               :: n_steps,iw_ref,iw,iw_G,i_spin,n_titles,iqp,i_sp
 real(SP),allocatable  :: Green_DOS(:,:,:)
 real(SP),allocatable  :: E_grid(:)
 real(SP)              :: E_range(2),off_set,deltaE,values(3*n_sp_pol+1)
 real(SP)              :: GreenF1,GreenF2,shifted_GreenF,M(n_sp_pol),Q(n_sp_pol)
 !
 DOS_Green_name='DOS_Greens'
 call of_open_close(DOS_Green_name,'ot')
 call msg('o DOS','# GW [DOS from Greens functions]')
 call msg('o DOS','#')
 !
 ! Compute the energy range
 E_range(1)=minval(real(qp%GreenF_W(:,1)))
 E_range(2)=maxval(real(qp%GreenF_W(:,QP_Sc_steps)))
 deltaE=real(qp%GreenF_W(1,2)-qp%GreenF_W(1,1))
 n_steps=int((E_range(2)-E_range(1))/deltaE)+1
 E_range(2)=E_range(1)+deltaE*(n_steps-1)
 !write(*,*) '#',E_range(:),n_steps,deltaE
 call msg('o DOS','# Energy range: ',E_range*HA2EV,indent=0)
 call msg('o DOS','# nsteps:       ',n_steps,indent=0)
 !
 allocate(E_grid(n_steps),Green_DOS(n_steps,n_sp_pol,3))
 Green_DOS=0.
 forall(iw=1:n_steps) E_grid(iw)=E_range(1)+deltaE*(iw-1)
 !
 do iqp=1,QP_n_states
   i_sp=spin(QP_table(iqp,:))
   !
   do iw=1,n_steps
     if( E_grid(iw)<real(qp%GreenF_W(iqp,1)) ) cycle
     iw_ref=iw-1
     off_set=E_grid(iw)-qp%GreenF_W(iqp,1)
     exit
   enddo
   !
   !write(*,*) '#',off_set
   do iw_G=1,QP_Sc_steps-1
     GreenF1=aimag(qp%GreenF(iqp,iw_G))/pi*spin_occ
     GreenF2=aimag(qp%GreenF(iqp,iw_G+1))/pi*spin_occ
     shifted_GreenF=abs(GreenF1)+(abs(GreenF2)-abs(GreenF1))*off_set/deltaE
     !
     Green_DOS(iw_ref+iw_G,i_sp,1)=Green_DOS(iw_ref+iw_G,i_sp,1)+shifted_GreenF
     if( (GreenF1+(GreenF2-GreenF1)*off_set/deltaE)<0 ) cycle
     Green_DOS(iw_ref+iw_G,i_sp,2)=Green_DOS(iw_ref+iw_G,i_sp,2)+shifted_GreenF
   enddo
 enddo
 !
 do iw=2,n_steps
   Green_DOS(iw,:,3)=Green_DOS(iw-1,:,3)+(Green_DOS(iw,:,2)+Green_DOS(iw-1,:,2))*deltaE/2._SP
 enddo
 !
 !do iw=2,n_steps
 !  M=(Green_DOS(iw-1,:,2)-Green_DOS(iw,:,2))/DeltaE
 !  Q= Green_DOS(iw-1,:,2)-M*E_grid(iw-1)
 !  Green_DOS(iw,:,3)=Green_DOS(iw-1,:,3)+1./2.*M*(E_grid(iw)**2.-E_grid(iw-1)**2.)+Q*(E_grid(iw)-E_grid(iw-1))
 !enddo
 !
 titles(1)='E[eV]'
 n_titles=3*n_sp_pol+1
 if(n_sp_pol==1) then
   titles(2)='all'
   titles(3)='occ'
   titles(4)='nel'
 else
   titles(2)='all_sp1'
   titles(3)='all_sp2'
   titles(4)='occ_sp1'
   titles(5)='occ_sp2'
   titles(6)='nel_sp1'  
   titles(7)='nel_sp2'  
 endif
 !
 call msg('o DOS','#')
 call msg('o DOS','#',titles,INDENT=0,USE_TABS=.TRUE.)
 !
 values=0
 do iw=1,n_steps
   values(1)=E_grid(iw)*HA2EV
   values(2           :1+  n_sp_pol)=Green_DOS(iw,:,1)/HA2EV
   values(2+  n_sp_pol:1+2*n_sp_pol)=Green_DOS(iw,:,2)/HA2EV
   values(2+2*n_sp_pol:1+3*n_sp_pol)=Green_DOS(iw,:,3)
   call msg('o DOS','',values,INDENT=-2,USE_TABS=.TRUE.) 
 enddo
 !
 deallocate(E_grid,green_DOS)
 !
end subroutine

