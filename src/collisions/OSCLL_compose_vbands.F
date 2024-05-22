!
!        Copyright (C) 2020-2023 the Yambo team
!
! Authors (see AUTHORS file for details): CA
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
subroutine OSCLL_compose_vbands(V_input,H_nl_sc,k,q,E,i_k,i_sp_pol) 
 !
 ! This subroutine calculate the long-range part of the Screened exchange
 !
 use electrons,      ONLY:n_sp_pol,levels,spin_occ
 use pars,           ONLY:SP,cI,cZERO,cONE
 use FFT_m,          ONLY:fft_size
 use nl_optics,      ONLY:NL_bands,NL_nbands
 use collision_ext,  ONLY:ng_oscll,OSCLL,W
 use QP_m,           ONLY:QP_nk
 use R_lattice,      ONLY:bz_samp,qindx_S,nXkibz
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 use parallel_m,     ONLY:PAR_Xk_ibz_index
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 ! 
 implicit none
 !
 complex(SP), intent(inout) :: H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
 integer,     intent(in)    :: i_k,i_sp_pol
 type(bz_samp), intent(in)  :: k,q
 type(levels),  intent(in)  :: E
 complex(SP),  intent(in)  :: V_input(NL_bands(2),maxval(E%nbf),QP_nk,n_sp_pol)
 !
 ! Work Space
 !
 complex(SP) :: COLL(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk)
#if defined _DOUBLE
 complex(SP), external :: ZDOTU
#else
 complex(SP), external :: CDOTU
#endif
 complex(SP) :: COLL_val
 integer :: ib,ibp,i_kmq,i_n,i_m,iv,iqbz,ig1,ig2,ik_mem
 !
 ik_mem=PAR_Xk_ibz_index(i_k)
 !
#if defined _TIMING
 call timing('OSCLL compose NL',OPR='start')
#endif
 !
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n,NL_bands(2)
    !
    call OSCLL_compose_collision(k,q,i_n,i_m,i_k,COLL)
    !
    do iv=1,E%nbf(i_sp_pol)
     COLL_val=cZERO
     !
     do iqbz=1,q%nbz 
       i_kmq   = k%sstar(qindx_S(i_k,iqbz,1),1)
       do ib=NL_bands(1),NL_bands(2)
         do ibp=NL_bands(1),NL_bands(2)
           do ig1=1,ng_oscll
             do ig2=1,ng_oscll
                COLL_val=COLL_val+conjg(V_input(ibp,iv,i_kmq,i_sp_pol))*V_input(ib,iv,i_kmq,i_sp_pol)* & 
                        OSCLL(i_n,ib ,ik_mem,iqbz,ig2)*conjg(OSCLL(i_m,ibp,ik_mem,iqbz,ig1)) &
&                             *W(iqbz,ig2,ig1)
              
             enddo
           enddo
           
!           COLL_val=COLL_val+COLL(ib,ibp,i_kmq)* &
!&                   conjg(V_input(ibp,iv,i_kmq,i_sp_pol))*V_input(ib,iv,i_kmq,i_sp_pol)
         enddo
      enddo
     enddo
     !
     H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)+COLL_val*spin_occ
    enddo
    !
    do ib=NL_bands(1),E%nbf(i_sp_pol)
      H_nl_sc(i_n,i_m)=H_nl_sc(i_n,i_m)-sum(COLL(ib,ib,:))*spin_occ
    enddo
    !
   enddo
 enddo
 !
 ! Symmetrization
 ! 
 do i_n=NL_bands(1),NL_bands(2)
   do i_m=i_n+1,NL_bands(2)
     H_nl_sc(i_m,i_n)=conjg(H_nl_sc(i_n,i_m))
   enddo
   H_nl_sc(i_n,i_n)=real(H_nl_sc(i_n,i_n))
 enddo
 !
#if defined _TIMING
 call timing('OSCLL compose NL',OPR='stop')
#endif
 !
end subroutine OSCLL_compose_vbands
