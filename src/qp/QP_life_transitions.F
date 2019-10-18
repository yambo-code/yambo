!
!        Copyright (C) 2000-2019 the YAMBO team
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
integer function QP_life_transitions(iqibz_in,E,k,q,X_life_W)
 !
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_t,QP_n_G_bands,QP_n_states,QP_table,&
&                        QP_cg_percent,h2ht,e2et
 use com,           ONLY:error
 use frequency,     ONLY:w_samp,bare_grid_N,coarse_grid_N,coarse_grid_Pt,ordered_grid_index
 use electrons,     ONLY:levels,spin
 use R_lattice,     ONLY:qindx_S,bz_samp
 !
#include<memory.h>
 type(levels) ::E
 type(bz_samp)::q,k
 type(w_samp) ::X_life_W
 integer      ::iqibz_in
 !
 ! Work Space
 !
 integer :: iqbz,iqibz,ob,ib,ik,ok,ob_max,ob_min,i1,&
&           n_of_transitions(q%nibz),i_or,i2,isp,osp
 real(SP)             :: F
 real(SP),allocatable :: X_freqs(:,:)
 real(SP),external    :: FREQUENCIES_damping
 !
 !                :
 !               /:\ iqibz
 !                :
 !  (ik,ib ) --<--:
 !                 \_
 !                 |\
 !                   (ok,ob)     
 !
 ! 2 conditions:
 !
 ! [1] E(ib,ik,isp)-E(ob,ok,osp)>0 & spin_occ-f(ok,ob,osp)+bose(E(ik,ib,isp)-E(ok,ob,osp))>0
 ! [2] E(ib,ik,isp)-E(ob,ok,osp)<0 & f(ok,ob,isp)+bose(E(ok,ob)-E(ik,ib,isp))>0
 !
 QP_life_transitions=0
 !
 do i1=1,2
   !
   n_of_transitions=0
   ob_min=QP_n_G_bands(2)
   ob_max=-1
   !
   do iqbz=1,q%nbz
     iqibz=q%sstar(iqbz,1)
     do i2=1,QP_n_states
       ib=QP_table(i2,1)
       ik=QP_table(i2,3)
       isp=spin(QP_table(i2,:))
       ok=k%sstar(qindx_S(ik,iqbz,1),1)
       osp=isp
       do ob=QP_n_G_bands(1),QP_n_G_bands(2)
         i_or=IOR(e2et((/ib,ik,isp/),(/ob,ok,osp/),E,F),h2ht((/ib,ik,isp/),(/ob,ok,osp/),E,F))
         if (i_or==0) cycle
         n_of_transitions(iqibz)=n_of_transitions(iqibz)+1
         if (i1==2) X_freqs(iqibz,n_of_transitions(iqibz))=abs(E%E(ib,ik,isp)-E%E(ob,ok,osp))
         ob_min=min(ob_min,ob)
         ob_max=max(ob_max,ob)
       enddo
     enddo
   enddo
   if (i1==2) QP_n_G_bands=(/ob_min,ob_max/)
   if (i1==2) exit
   !
   if (maxval(n_of_transitions)==0) then
     call error('All virtual transitions are null. Change QP states')
   else if (any(n_of_transitions==0)) then
     call error('One or more Q-point virtual transitions are null. Change QP states')
   endif
   !
   if (i1==1) then
     YAMBO_ALLOC(X_freqs,(q%nibz,maxval(n_of_transitions)))
     X_freqs=0.
   endif
   !
 enddo
 QP_n_G_bands=(/ob_min,ob_max/)
 !
 QP_life_transitions=maxval(n_of_transitions)
 !
 if (iqibz_in<0) then
   YAMBO_FREE(X_freqs)
   return
 endif
 !
 ! Here I reduce the frequencies
 !
 call FREQUENCIES_coarse_grid('Life',X_freqs(iqibz_in,:n_of_transitions(iqibz_in)),&
&                 n_of_transitions(iqibz_in),QP_cg_percent,(/0.0_SP/),.FALSE.)
 X_life_W%n_freqs=coarse_grid_N
 !
 X_life_W%er=(/minval(coarse_grid_Pt),maxval(coarse_grid_Pt)/)
 !
 YAMBO_ALLOC(X_life_W%p,(coarse_grid_N))
 do i1=1,coarse_grid_N
   ! 
   X_life_W%p(i1)=coarse_grid_Pt(i1)+FREQUENCIES_damping(X_life_W,coarse_grid_Pt(i1))*cmplx(0.,1.,SP)
   !
 enddo
 !
 ! Clean
 !
 YAMBO_FREE(bare_grid_N)
 YAMBO_FREE(coarse_grid_Pt)
 YAMBO_FREE(ordered_grid_index)
 YAMBO_FREE(X_freqs)
 !
end function
