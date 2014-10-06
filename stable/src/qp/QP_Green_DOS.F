!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
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
subroutine QP_Green_DOS(qp,k)
 !
 ! This routine calcualtes the dos constructed from the Green functions
 !
 use pars,        ONLY:SP,lchlen,schlen,pi
 use com,         ONLY:msg,of_open_close
 use units,       ONLY:HA2EV
 use QP_m,        ONLY:QP_t,QP_Sc_steps,QP_table,QP_n_states,use_GreenF_Zoom
 use electrons,   ONLY:spin,n_sp_pol,spin_occ
 use stderr,      ONLY:intc
 use vec_operate, ONLY:sort
 use D_lattice,   ONLY:DL_vol
 use R_lattice,   ONLY:bz_samp
 !
 implicit none
 !
 type(QP_t)            :: qp
 type(bz_samp)         :: k
 !
 ! Work Space
 !
 integer               :: n_titles=4
 integer               :: n_zone,iw_start,iw_end
 integer               :: i1,iqp,i_sp,ik,i_zone,iw,iw_G
 integer               :: QP_zone_table(QP_n_states),QP_sort_table(QP_n_states)
 integer, allocatable  :: iw_table(:),n_steps(:)
 character(lchlen)     :: DOS_Green_name,filename
 character(schlen)     :: titles(4),out_units(4)
 real(SP)              :: values(4),tmp_vect(QP_n_states)
 real(SP)              :: GreenF1,GreenF2,shifted_GreenF
 real(SP),allocatable  :: Green_DOS(:,:,:,:),E_grid(:,:),off_set(:)
 real(SP),allocatable  :: E_range(:,:),deltaE(:),deltaE_tmp
 !
 do iqp=1,QP_n_states
   iw=maxloc( abs(aimag(qp%GreenF(iqp,:))),1 )
   tmp_vect(iqp)=qp%GreenF_W(iqp,iw)
 enddo
 call sort(tmp_vect,indx=QP_sort_table)
 !
 n_zone=QP_n_states
 i_zone=0
 do i1=1,QP_n_states
   iqp=QP_sort_table(i1)
   i_zone=i_zone+1
   QP_zone_table(iqp)=i_zone
   if(i1==1) cycle
   if(real(qp%GreenF_W(iqp,1))<=real(qp%GreenF_W(QP_sort_table(i1-1),QP_Sc_steps)) ) then
     QP_zone_table(iqp)=i_zone-1
     i_zone=i_zone-1
     n_zone=n_zone-1
   endif
 enddo
 !
 if(.not.use_GreenF_Zoom) then
   n_zone=1
   QP_zone_table=1
 endif
 !
 allocate(deltaE(n_zone),E_range(2,n_zone),n_steps(n_zone))
 !
 E_range(1,:)=maxval(real(qp%GreenF_W(:,QP_Sc_steps)))
 E_range(2,:)=minval(real(qp%GreenF_W(:,1)))
 deltaE=E_range(1,1)-E_range(2,1)
 !
 do i1=1,QP_n_states
   iqp=QP_sort_table(i1)
   i_zone=QP_zone_table(iqp)
   E_range(1,i_zone)=min(E_range(1,i_zone),real(qp%GreenF_W(iqp,1)))
   E_range(2,i_zone)=max(E_range(2,i_zone),real(qp%GreenF_W(iqp,QP_Sc_steps)))
   deltaE_tmp=real(qp%GreenF_W(iqp,QP_Sc_steps))-real(qp%GreenF_W(iqp,1))
   do iw=1,QP_Sc_steps-1
     deltaE_tmp=min(deltaE_tmp,real(qp%GreenF_W(iqp,iw+1))-real(qp%GreenF_W(iqp,iw)))
   enddo
   deltaE(i_zone)=min(deltaE(i_zone),deltaE_tmp)
 enddo
 !
 !
 n_steps=0
 do i_zone=1,n_zone
   n_steps(i_zone)=int((E_range(2,i_zone)-E_range(1,i_zone))/deltaE(i_zone))+1
 enddo
 !
 allocate(E_grid(maxval(n_steps),n_zone),Green_DOS(maxval(n_steps),n_zone,n_sp_pol,3))
 allocate(off_set(maxval(n_steps)),iw_table(maxval(n_steps)))
 Green_DOS=0.
 !
 do i_zone=1,n_zone
   do iw=1,n_steps(i_zone)
     E_grid(iw,i_zone)=E_range(1,i_zone)+deltaE(i_zone)*(iw-1)
     if(iw==n_steps(i_zone)) E_range(2,i_zone)=E_grid(iw,i_zone)
   enddo
 enddo
 !
 do i1=1,QP_n_states
   iqp=QP_sort_table(i1)
   i_sp=spin(QP_table(iqp,:))
   ik=QP_table(iqp,3)
   i_zone=QP_zone_table(iqp)
   !
   iw_start=1
   do iw=1,n_steps(i_zone)
     if( E_grid(iw,i_zone) < real(qp%GreenF_W(iqp,1)) ) cycle
     iw_start=iw
     exit
   enddo
   !
   iw_end=0
   do iw=iw_start,n_steps(i_zone)
     if( E_grid(iw,i_zone) < real(qp%GreenF_W(iqp,QP_Sc_steps)) ) cycle
     iw_end=iw-1
     exit
   enddo
   if(iw_end==0) iw_end=n_steps(i_zone)
   !
   iw_G=0
   do iw=iw_start,iw_end
     if( E_grid(iw,i_zone) >= real(qp%GreenF_W(iqp,min(iw_G+1,QP_Sc_steps))) ) iw_G=iw_G+1
     iw_table(iw)=max(iw_G,1)
     off_set(iw)=E_grid(iw,i_zone)-qp%GreenF_W(iqp,iw_G)
   enddo
   !
   do iw=iw_start,iw_end
     !
     GreenF1=aimag(qp%GreenF(iqp,iw_table(iw)))
     GreenF2=aimag(qp%GreenF(iqp,iw_table(iw)+1))
     shifted_GreenF=abs(GreenF1)+(abs(GreenF2)-abs(GreenF1))*off_set(iw)/deltaE(i_zone)
     shifted_GreenF=shifted_GreenF/pi*spin_occ*k%weights(ik)
     !
     Green_DOS(iw,i_zone,i_sp,1)=Green_DOS(iw,i_zone,i_sp,1)+max(shifted_GreenF,0.)
     if( (GreenF1+(GreenF2-GreenF1)*off_set(iw)/deltaE(i_zone))<0 ) cycle
     Green_DOS(iw,i_zone,i_sp,2)=Green_DOS(iw,i_zone,i_sp,2)+shifted_GreenF
   enddo
 enddo
 !
 do i_zone=1,n_zone
   do iw=2,n_steps(i_zone)
     Green_DOS(iw,i_zone,:,3)=Green_DOS(iw-1,i_zone,:,3)+ &
&       (Green_DOS(iw,i_zone,:,2)+Green_DOS(iw-1,i_zone,:,2))*deltaE(i_zone)/2._SP
   enddo
 enddo
 !
 !Green_DOS(:,:,:,1:2)=Green_DOS(:,:,:,1:2)/DL_vol
 !
 titles(1)='E'
 titles(2)='DOS '
 titles(3)='DOS(occ)'
 titles(4)='nel/DL_vol'
 out_units(1)='[eV]'
 out_units(2)='[(DL_vol*eV)^-1]'
 out_units(3)=' '
 out_units(4)='nel/DL_vol'
 !
 if(n_zone>1) n_titles=3
 !
 do i_zone=1,n_zone
   !
   DOS_Green_name='DOS_Greens'
   !
   if(n_zone>1) DOS_Green_name=trim(DOS_Green_name)//'_zone'//trim(intc(i_zone))
   !
   do i_sp=1,n_sp_pol
     !
     filename=trim(DOS_Green_name)
     !
     if(n_sp_pol>1) filename=trim(DOS_Green_name)//'_sp'//trim(intc(i_sp))
     !       
     call of_open_close(filename,'ot')
     !
     call msg('o DOS','# GW [DOS from Greens functions]')
     call msg('o DOS','#')
     call msg('o DOS','# Energy range: ',E_range(:,i_zone)*HA2EV,indent=0)
     call msg('o DOS','# nsteps:       ',n_steps(i_zone),indent=0)
     !
     call msg('o DOS','#')
     call msg('o DOS','#',titles(:n_titles),INDENT=0,USE_TABS=.TRUE.)
     call msg('o DOS','#',out_units(:n_titles),INDENT=0,USE_TABS=.TRUE.)
     !
     values=0
     do iw=1,n_steps(i_zone)
       values(1)=E_grid(iw,i_zone)*HA2EV
       values(2)=Green_DOS(iw,i_zone,i_sp,1)/HA2EV
       values(3)=Green_DOS(iw,i_zone,i_sp,2)/HA2EV
       values(4)=Green_DOS(iw,i_zone,i_sp,3)
       !
       call msg('o DOS','',values(:n_titles),INDENT=-2,USE_TABS=.TRUE.) 
       !
     enddo
     !
     call of_open_close(filename)
     !
   enddo
   !
 enddo
 !
 deallocate(n_steps,deltaE,E_range)
 deallocate(E_grid,green_DOS)
 deallocate(off_set,iw_table)
 !
end subroutine
