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
subroutine RT_Lifetimes_merge_driver(what)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use RT_lifetimes,   ONLY:RT_EP_life,RT_EE_life,RT_EH_life,REF_lifetime
 use real_time,      ONLY:RTibz
 use D_lattice,      ONLY:Bose_Temp
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_Xk_ibz_INDEX,PAR_IND_Xk_ibz
 use R_lattice,      ONLY:nkibz
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 character(*)            :: what
 !
 ! Work Space
 !
 integer :: ik,k_range(2)
 !
 call timing('RT life merge ('//what//')',OPR='start')
 !
 ! INTRA-k/INTER-k parallel merging
 !==================================
 !
 if (l_elel_scatt) then
   if (what=="INTRA_k") then
     call RT_Lifetimes_merge_intra_k("elel",RT_EE_life%SPONT_abs)
     call RT_Lifetimes_merge_intra_k("elel",RT_EE_life%SPONT_emit)
   else
     call RT_Lifetimes_merge_inter_k("elel",RT_EE_life%SPONT_abs)
     call RT_Lifetimes_merge_inter_k("elel",RT_EE_life%SPONT_emit)
   endif
 endif
 !
 if (l_elph_scatt) then
   if (what=="INTRA_k") then
     call RT_Lifetimes_merge_intra_k("elph",RT_EP_life%SPONT_abs)
     call RT_Lifetimes_merge_intra_k("elph",RT_EP_life%SPONT_emit)
     if (Bose_Temp>0._SP) then
       call RT_Lifetimes_merge_intra_k("bose",RT_EP_life%STIM_emit)
       call RT_Lifetimes_merge_intra_k("bose",RT_EP_life%STIM_abs)
     endif
   else
     call RT_Lifetimes_merge_inter_k("elph",RT_EP_life%SPONT_abs)
     call RT_Lifetimes_merge_inter_k("elph",RT_EP_life%SPONT_emit)
     if (Bose_Temp>0._SP) then
       call RT_Lifetimes_merge_inter_k("bose",RT_EP_life%STIM_emit)
       call RT_Lifetimes_merge_inter_k("bose",RT_EP_life%STIM_abs)
     endif
   endif
 endif
 !
 if (l_elphoton_scatt) then
   if (what=="INTRA_k") then
     call RT_Lifetimes_merge_intra_k("elgm",RT_EH_life%SPONT_abs)
     call RT_Lifetimes_merge_intra_k("elgm",RT_EH_life%SPONT_emit)
   else
     call RT_Lifetimes_merge_inter_k("elgm",RT_EH_life%SPONT_abs)
     call RT_Lifetimes_merge_inter_k("elgm",RT_EH_life%SPONT_emit)
   endif
 endif
 !
 ! REF lifetimes PARALLEL merging
 !==================================
 !
 if (what=="REF") then
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     k_range=RTibz%k_range(ik,:)
     !
     if (l_elel_scatt) then
       REF_lifetime(:,k_range(1):k_range(2),1)=RT_EE_life%SPONT_abs(:,k_range(1):k_range(2))
       REF_lifetime(:,k_range(1):k_range(2),2)=RT_EE_life%SPONT_emit(:,k_range(1):k_range(2))
     endif
     !
     if (l_elph_scatt) then
       REF_lifetime(:,k_range(1):k_range(2),3)=RT_EP_life%SPONT_abs(:,k_range(1):k_range(2))
       REF_lifetime(:,k_range(1):k_range(2),4)=RT_EP_life%SPONT_emit(:,k_range(1):k_range(2))
       if(Bose_Temp>0._SP) then
         REF_lifetime(:,k_range(1):k_range(2),5)=RT_EP_life%STIM_emit(:,k_range(1):k_range(2))
         REF_lifetime(:,k_range(1):k_range(2),6)=RT_EP_life%STIM_abs(:,k_range(1):k_range(2))
       endif
     endif
     !
     if (l_elphoton_scatt) then
       REF_lifetime(:,k_range(1):k_range(2),7)=RT_EH_life%SPONT_abs(:,k_range(1):k_range(2))
       REF_lifetime(:,k_range(1):k_range(2),8)=RT_EH_life%SPONT_emit(:,k_range(1):k_range(2))
     endif
     !
   enddo
   !
   call PP_redux_wait(REF_lifetime,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   !
 endif
 !
 call timing('RT life merge ('//what//')',OPR='stop')
 !
end subroutine RT_Lifetimes_merge_driver
