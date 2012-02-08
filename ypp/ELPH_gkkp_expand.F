!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine ELPH_gkkp_expand(iq_bz,iq_s,k)
 !
 use pars,                ONLY:SP
 use R_lattice,           ONLY:bz_samp,ik_is_table,qindx_S
 use D_lattice,           ONLY:nsym,i_time_rev,sop_inv,sop_tab
 use ELPH,                ONLY:QP_PH_n_G_bands,ph_modes,elph_nb,elph_gkkp
 implicit none
 type(bz_samp) ::k
 integer       ::iq_bz,iq_s
 !
 ! Work Space 
 !
 integer       ::ik_bz,ib,ob,ok,ik_bz_gkkp,ob_gkkp,ib_gkkp,os,ik,&
&                okbz,il
 complex(SP)   ::elph_gkkp_expanded(k%nbz,ph_modes,elph_nb,elph_nb)
 !
 ! GKKP m.e. rotation
 !
 do ik_bz=1,k%nbz
   !
   ik=k%sstar(ik_bz,1)
   !
   do ib=1,QP_PH_n_G_bands
     do ob=1,QP_PH_n_G_bands
       !
       ik_bz_gkkp=ik_bz
       ib_gkkp   =ib
       ob_gkkp   =ob
       !
       okbz=qindx_S(ik,iq_bz,1)
       ok=k%sstar(okbz,1)
       os=k%sstar(okbz,2)
       ! 
       ! When using a uniform Q grid I cycle on the q symmetries 
       ! as well. To rotate the gkkp m.e. I use:
       !
       if (iq_s>nsym/(i_time_rev+1)) then
         ! 
         ! gkkp_{I_need}= <k+IRq n'|dV_{SCF}/du^{IRq nu}|k n>=
         !                [<(R^-1 S p)+q n|dV_{SCF}/du^{q nu}|(R^-1 S p) n'>]^*= 
         !                [gkkp(ik_bz,nu,n,n')]^*
         ! 
         ! with k + IRq = Sp 
         !
         ib_gkkp   =ob
         ob_gkkp   =ib
         !                                 R^-1                 S
         !                                 -------------------- --
         ik_bz_gkkp=ik_is_table(ok,sop_tab(sop_inv(iq_s-nsym/2),os))
         !
         elph_gkkp_expanded(ik_bz,:,ib,ob)=conjg(elph_gkkp(ik_bz_gkkp,:,ob_gkkp,ib_gkkp))
         !
       else
         ! 
         ! gkkp_{I_need}= <k+Rq n'|dV_{SCF}/du^{Rq nu}|k n>=
         !                <(R^-1 k)+q n'|dV_{SCF}/du^{q nu}|(R^-1 k) n>= 
         !                gkkp(ik_bz,nu,n',n)
         !
         ik_bz_gkkp=ik_is_table(ik,sop_inv(iq_s))
         !
         elph_gkkp_expanded(ik_bz,:,ib,ob)=elph_gkkp(ik_bz_gkkp,:,ib,ob)
         !
       endif
       !
     enddo
   enddo
 enddo
 !
 elph_gkkp=elph_gkkp_expanded
 !
end subroutine
