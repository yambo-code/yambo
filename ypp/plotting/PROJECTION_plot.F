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
subroutine PROJECTION_plot( what, N, bands, W, TIME, OBS_K, OBS_B)
 !
 use pars,           ONLY:SP,schlen
 use vec_operate,    ONLY:c2a,v_norm
 use units,          ONLY:HA2EV,AUT2FS
 use com,            ONLY:msg,of_open_close
 use LIVE_t,         ONLY:live_timing
 use YPP_interfaces, ONLY:PROJECTION_k_on_the_path_or_plane
#if defined _YPP_RT
 use YPP_real_time,  ONLY:integrated_Nel,integrated_Nhole
#endif
 use YPPm,           ONLY:PROJECT_depth,l_PROJECT_line,CIRCUIT_k_label,PROJECT_threshold,&
&                         l_PROJECT_plane,PROJECT_plane_X,PROJECT_plane_Y,PROJECT_steps
 use interpolate,    ONLY:GRID_k
 use electrons,      ONLY:n_spin
 use stderr,         ONLY:intc,real2ch,STRING_remove
 !
#include<memory.h>
 !
 character(*)       :: what
 integer            :: N,bands(2)
 real(SP)  ,optional:: W(N),TIME(N)
 real(SP)  ,optional:: OBS_K(GRID_k%nbz/PROJECT_depth,N,n_spin)
 real(SP)  ,optional:: OBS_B(bands(1):bands(2),GRID_k%nbz/PROJECT_depth,n_spin)
 !
 ! Work Space
 !
 character(schlen)  :: of_name,titles(max(3,bands(2)+2)),k_label,extension
 integer            :: ik,i_N,is,ik_plane,ib,ik_path
 real(SP)           :: O(3),k(3),kdist(GRID_k%nbz),max_OBS,projection(2),W_or_T
 logical            :: label_printed,l_DOS,l_BANDS,l_OCC,l_GAMMA
 !
 ! Allocation
 !============
 !
 l_DOS    = what=="DOS"
 l_BANDS  = what=="BANDS"
 l_OCC    = index(what,"occupations")>0
 l_GAMMA  = index(what,"lifetimes")>0
 !
 if (l_PROJECT_line) then
   !================
   !
   ! |k| distance along the path
   !-----------------------------
   kdist=0._SP
   O=GRID_k%pt(1,:)
   do ik=2,GRID_k%nbz
     ik_path=PROJECTION_k_on_the_path_or_plane(ik)
     if (ik_path==0) cycle
     k=GRID_k%pt(ik,:)-O
     call c2a(v_in=k,mode='ki2c')
     kdist(ik_path)=kdist(ik_path)+v_norm(k)
     if (len_trim( CIRCUIT_k_label(ik_path))>0) then
       if (ik_path>1) kdist(ik_path+1:)=kdist(ik_path)
       O=GRID_k%pt(ik,:)
     endif
   enddo
   !  
   ! Open + add titles
   !-------------------
   if (l_OCC.or.l_GAMMA) then
     titles(1)="|k| (a.u.)"
     titles(2)="Time [fs]"
     if (l_GAMMA) titles(3)='Width [meV]'
     if (l_OCC) titles(3)='Occ'
     if ( index(what,"_electrons")>0 ) titles(3)=trim(titles(3))//"(e)"
     if ( index(what,"_holes")>0     ) titles(3)=trim(titles(3))//"(h)"
     of_name=trim(what)//"_projected_LINE"
     call of_open_close(trim(of_name),'ot')
     call msg('o '//trim(what),'#')
     call msg('o '//trim(what),"#",titles(:3),INDENT=0,USE_TABS=.true.)
     call msg('o '//trim(what),'#')
   else
     of_name="dos"
   endif
   !
   call live_timing('Printing the projected '//what,GRID_k%nbz/PROJECT_depth)
   !
   max_OBS=maxval(OBS_K)
   !
   do ik=1,GRID_k%nbz
     !
     ik_path=PROJECTION_k_on_the_path_or_plane(ik)
     if (ik_path==0) cycle
     !
     label_printed=.FALSE.
     !
     do i_N=1,N
       !
       if (present(W)) then
         W_or_T=W(i_N)*HA2EV
         if (.not.any((/OBS_K(ik_path,i_N,:)> max_OBS*PROJECT_threshold/100./))) cycle
       else
         W_or_T=TIME(i_N)*AUT2FS
       endif
       !
       k_label=" "
       if (len_trim(CIRCUIT_k_label(ik_path))>0.and..not.label_printed) then
         k_label=" ["//trim(CIRCUIT_k_label(ik_path))//"]"
         label_printed=.TRUE.
       endif
       !
       if (n_spin==1) then
         call msg('o '//trim(of_name),'',(/kdist(ik_path),W_or_T,OBS_K(ik_path,i_N,1)/),&
&                 INDENT=-2,USE_TABS=.true.,after_msg=k_label)
       else if (n_spin==2) then
         call msg('o '//trim(of_name),'',(/kdist(ik_path),W_or_T,OBS_K(ik_path,i_N,:),  &
&                            OBS_K(ik_path,i_N,1)+OBS_K(ik_path,i_N,2)/),&
&                            INDENT=-2,USE_TABS=.true.,after_msg=k_label)
       endif
       !
     enddo
     !
     call live_timing(steps=1)
     !
     call msg('o '//trim(of_name)," ")
     !
   enddo
   !
   if (l_OCC.or.l_GAMMA) call of_open_close(trim(of_name))
   !
   call live_timing()
   !
 endif
 !
 if (l_PROJECT_plane) then
   !================
   !
   call live_timing('Printing the projected '//what,GRID_k%nbz/PROJECT_depth*N)
   !
   do i_N=1,N
     !
     if (l_BANDS) of_name="bands_projected" 
     if (l_OCC.or.l_GAMMA) of_name=what//"_projected" 
     !
     of_name=trim(of_name)//'_PLANE'
     if (l_OCC.or.l_GAMMA) then
       extension=STRING_remove(real2ch( TIME(i_N)*AUT2FS )," ","")
       of_name=trim(of_name)//"_T_"//trim(extension)
     endif
     !
     call of_open_close(trim(of_name),'ot')
     if (l_OCC.or.l_GAMMA) then
       call msg('o '//trim(of_name),"#"," ",INDENT=0)
       call msg('o '//trim(of_name),"#",' TIME= '//trim(real2ch( TIME(i_N)*AUT2FS ))//' fs',INDENT=0)
#if defined _YPP_RT
       if (l_OCC) then
         call msg('o '//trim(of_name),"#",' Integrated E>E_Fermi Carriers (built-in)= '//&
&                 trim(real2ch(integrated_Nel(i_N,1))),INDENT=0)
         call msg('o '//trim(of_name),"#",'                           (interpolated)= '//&
&                 trim(real2ch(integrated_Nel(i_N,2))),INDENT=0)
         call msg('o '//trim(of_name),"#",' Integrated E<E_Fermi Carriers (built-in)= '//&
&                 trim(real2ch(integrated_Nhole(i_N,1))),INDENT=0)
         call msg('o '//trim(of_name),"#",'                           (interpolated)= '//&
&                 trim(real2ch(integrated_Nhole(i_N,2))),INDENT=0)
       endif
#endif
     endif
     call msg('o '//trim(of_name),'#')
     call msg('o '//trim(of_name),'# Plane X versor [cc]:',PROJECT_plane_X,INDENT=0)
     call msg('o '//trim(of_name),'# Plane Y versor [cc]:',PROJECT_plane_Y,INDENT=0)
     call msg('o '//trim(of_name),'#')
     titles(1:2)=(/"K_proj_X","K_proj_Y"/)
     if (l_BANDS) then
       do ib=bands(1),bands(2)
         titles(3+ib-bands(1))="b"//trim(intc(ib))//" [eV]"
       enddo
       call msg('o '//trim(of_name),'#',titles(:3+bands(2)-bands(1)),INDENT=0,USE_TABS=.true.)    
     else if (l_GAMMA) then
       titles(3)="Int. Gamma"
       call msg('o '//trim(of_name),'#',titles(:3),INDENT=0,USE_TABS=.true.)    
     else if (l_OCC) then
       titles(3)="Int. Occ"
       if (index(what,"_electrons")>0) titles(3)=trim(titles(3))//" (e)"
       if (index(what,"_holes")>0    ) titles(3)=trim(titles(3))//" (h)"
       call msg('o '//trim(of_name),'#',titles(:3),INDENT=0,USE_TABS=.true.)    
     endif
     call msg('o '//trim(of_name),'#')
     !
     do ik=1,GRID_k%nbz
       !
       ik_plane=PROJECTION_k_on_the_path_or_plane(ik)
       if (ik_plane==0) cycle
       !
       k=GRID_k%pt(ik,:)
       call c2a(v_in=k,mode='ki2c')
       projection(1)=dot_product(k,PROJECT_plane_X)
       projection(2)=dot_product(k,PROJECT_plane_Y)
       !
       ik_path=0
       k_label=" "
       if (ik_plane==1                                ) ik_path=1
       if (ik_plane==PROJECT_steps*(PROJECT_steps+1)+1) ik_path=PROJECT_steps+1
       if (ik_plane==(PROJECT_steps+1)**2             ) ik_path=2*PROJECT_steps+1
       if (ik_path/=0) k_label=" ["//trim(CIRCUIT_k_label(ik_path))//"]"
       !
       if (l_BANDS) then
         call msg('o '//trim(of_name),'',(/projection,OBS_B(bands(1):bands(2),ik_plane,1)*HA2EV/),INDENT=-2,USE_TABS=.true.,&
  &               after_msg=k_label)
       else if (l_OCC.or.l_GAMMA) then
         call msg('o '//trim(of_name),'',(/projection,OBS_K(ik_plane,i_N,1)/),INDENT=-2,USE_TABS=.true.,&
  &               after_msg=k_label)
       endif
       !
       call live_timing(steps=1)
       !
       if (mod(ik_plane,PROJECT_steps+1)==0) call msg('o '//trim(of_name)," ")
       !
     enddo
     !
     call live_timing()
     !
     call of_open_close(trim(of_name))
     !
   enddo 
   !
 endif
 !
end subroutine
