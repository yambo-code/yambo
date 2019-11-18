!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
subroutine PP_compute_becp(becp, npwk, wf_nb, wf_c, wf_b_indx)
 !
 ! Compute becp = < beta_j | wfc_n > matrix elements
 ! assuming init_us_2 has already been called for the given kpt.
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:n_spinor
 use pseudo,        ONLY:pp_is_uspp,qe_pseudo_alloc
 use wave_func,     ONLY:wf_ncx
 use qe_pseudo_m,   ONLY:nkb,vkb,bec_type,is_allocated_bec_type,&
&                        allocate_bec_type,calbec
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 ! vars
 !
 type(bec_type), intent(inout) :: becp
 integer,        intent(in)    :: npwk,wf_nb
 integer,        intent(in)    :: wf_b_indx(wf_nb)
 complex(SP),    intent(in)    :: wf_c(wf_ncx,n_spinor,wf_nb)

 !
 ! Workspace
 !
 integer :: nb_max,ib,i_spinor,igs,ig
 complex(SP), allocatable :: wf_g(:,:)
 

 !
 ! checks
 !
 if (.not.pp_is_uspp) return
 if (.not.qe_pseudo_alloc) call error(' [PP] qe_pseudo not alloc in PP_compute_becp')
 call timing("PP_compute_becp","start")
 
 !
 ! allocations
 !----------------------
 !
 nb_max=maxval(wf_b_indx(:))
 !
 YAMBO_ALLOC(wf_g,(wf_ncx*n_spinor,1))
 !
 if (.not.is_allocated_bec_type(becp)) then
   !
   call allocate_bec_type(nkb, nb_max, becp)
   !
 else
   if (becp%nbnd < nb_max) call error(" [PP] becp not properly dimensioned")
 endif

 !
 ! calbec
 !--------------------------------
 !
 if (allocated(becp%k))  becp%k=0.0
 if (allocated(becp%nc)) becp%nc=0.0
 !
 do ib = 1, wf_nb
   !
   do i_spinor=1,n_spinor
     !
     igs=(i_spinor-1)*wf_ncx
     do ig = 1, wf_ncx
       wf_g(igs+ig,1)=wf_c(ig,i_spinor,ib)
     enddo
     !
   enddo
   !
   call calbec(npwk,vkb,wf_g,becp,wf_b_indx(ib),1)
   becp%init(wf_b_indx(ib))=.true.
   !
 enddo
 !
 YAMBO_FREE(wf_g)
 !
 call timing("PP_compute_becp","stop")
 return
 !
end subroutine PP_compute_becp
