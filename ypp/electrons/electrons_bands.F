! 
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS, AM
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
subroutine electrons_bands(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HA2EV
 use YPP,            ONLY:BANDS_steps,coo_in,coo_out,k_transform,INTERP_grid,&
&                         BANDS_bands,BANDS_QE_file,BANDS_Eo
 use electrons,      ONLY:levels,n_sp_pol,n_spinor
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:i_time_rev
 use com,            ONLY:msg,of_open_close,error,file_exists
 use vec_operate,    ONLY:v_norm,iku_v_norm
 use stderr,         ONLY:intc
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:reset_interpolation,electrons_bands_interpolate
 use QP_CTL_m,       ONLY:QP_apply
 !
#include<memory.h>
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(in)    :: Xen
 !
 ! Work Space
 !
 type(bz_samp)         :: USER_K,CIRCUIT_K,INTERP_K
 real(SP), pointer     :: bands_interpolated(:,:,:)
 real(SP), allocatable :: spin_interpolated(:,:,:),magn_interpolated(:,:,:)
 !
 integer               :: i1,is,ik,ikbz,ib,nbands,ID_interp
 real(SP)              :: tmp_k(3)
 real(SP), allocatable :: values(:),spin_values(:,:),magn_values(:,:),circuit_versor(:,:)
 character(8)          :: spin_ch
 character(schlen)     :: file_name_bands(2,n_sp_pol),file_name_grid(n_sp_pol),file_name_spin_magn(5)
 character(schlen), allocatable :: headings(:)
 logical               :: BANDS_internal,BANDS_interpolate,DOS_interpolate
 !
 call section('*','Interpolation tool')
 !=====================================
 !
 call parser('BANDS_built_in',BANDS_internal)
 if (.not.file_exists(trim(BANDS_QE_file))) BANDS_QE_file="none"
 BANDS_interpolate = BANDS_steps>0.or.trim(BANDS_QE_file)/="none"
 if (trim(BANDS_QE_file)/="none") BANDS_internal=.FALSE.
 DOS_interpolate   = all(INTERP_grid>0)
 !
 if ( i_time_rev==1 ) call error(' Bands interpolation does not work with T-rev symmetry')
 !
 ! Apply QP corrections
 !====================== 
 call QP_apply(BANDS_bands,Xen,Xk,'G',msg_fmt='s')
 !
 ! IBZ => BZ: need to be done here as QP_apply cleans the %ptbz allocation
 !
 call k_ibz2bz(Xk,'iku',.TRUE.)
 !
 if (BANDS_interpolate) then
   !
   ! Perform the energies interpolation
   !====================================
   call section('p','Interpolation@work: Bands')
   call electrons_bands_interpolate(Xen,Xk,ID_interp,USER_k,INTERP_k,CIRCUIT_k,BANDS_bands,bands_interpolated)
   !
   ! Perform interpolation of spinorial factors and magnetization
   !==============================================================
   if(n_spinor>1 .and. BANDS_steps> 0) then
     YAMBO_ALLOC(spin_interpolated,(BANDS_bands(1):BANDS_bands(2),CIRCUIT_k%nibz,n_spinor))
     YAMBO_ALLOC(magn_interpolated,(BANDS_bands(1):BANDS_bands(2),CIRCUIT_k%nibz,3))
     call section('p','Interpolation@work: Spin and Magnetization')
     call electrons_spin_and_magn_interpolate(Xk,CIRCUIT_k,BANDS_bands,spin_interpolated,magn_interpolated)
   endif
   !
 endif
 !
 call reset_interpolation(ID_interp)
 !
 ! HEADINGS...
 !
 ! - bands on circuit    if (BANDS_steps>0)  
 ! - dos on regular grid if (interp_grid>0)  
 !
 !==========================================
 !
 nbands=BANDS_bands(2)-BANDS_bands(1)+1
 !
 YAMBO_ALLOC(values,(nbands+4))
 allocate(headings(nbands+4))
 values=0._SP
 if(n_spinor==2) then
   YAMBO_ALLOC(spin_values,(nbands+4,2))
   YAMBO_ALLOC(magn_values,(nbands+4,3))
   spin_values=0._SP
   magn_values=0._SP
 endif
 !
 headings(1)=" |k| "
 do ib=BANDS_bands(1),BANDS_bands(2)
   headings(1+ib-BANDS_bands(1)+1)=' b'//intc(ib)
 enddo
 headings(nbands+2:nbands+4)=(/" kx  "," ky  "," kz  "/)
 !
 file_name_bands= " "
 file_name_grid = " "
 spin_ch        = " "
 !
 do is=1,n_sp_pol
   if (n_sp_pol>1.and.is==1) spin_ch="_spin-up"
   if (n_sp_pol>1.and.is==2) spin_ch="_spin-dn"
   !
   if (BANDS_internal    ) file_name_bands(2,is)="bands_built_in"//trim(spin_ch)
   if (BANDS_interpolate) file_name_bands(1,is)="bands_interpolated"//trim(spin_ch)
   if (DOS_interpolate)   file_name_grid(is)   ="grid"//trim(spin_ch)
   !
   call of_open_close(trim(file_name_grid(is)),'ot')
   call of_open_close(trim(file_name_bands(1,is)),'ot')
   call of_open_close(trim(file_name_bands(2,is)),'ot')
   !
 enddo
 !
 do is=1,n_sp_pol
   call msg('o '//trim(file_name_bands(1,is)),"#",headings,INDENT=0,USE_TABS=.true.)
   call msg('o '//trim(file_name_bands(1,is)),"#")
   call msg('o '//trim(file_name_bands(2,is)),"#",headings,INDENT=0,USE_TABS=.true.)
   call msg('o '//trim(file_name_bands(2,is)),"#")
 enddo
 !
 if(BANDS_interpolate.and.n_spinor==2) then
   file_name_spin_magn(1)="spin_factors_1"
   file_name_spin_magn(2)="spin_factors_2"
   file_name_spin_magn(3)="magnetization_x"
   file_name_spin_magn(4)="magnetization_y"
   file_name_spin_magn(5)="magnetization_z"
   do i1=1,5
     call of_open_close(trim(file_name_spin_magn(i1)),'ot')
     call msg('o '//trim(file_name_spin_magn(i1)),"#",headings,INDENT=0,USE_TABS=.true.)
     call msg('o '//trim(file_name_spin_magn(i1)),"#")
   enddo
 endif
 !
 call msg('o path',"#"," ",INDENT=0)
 !
 coo_out=coo_in
 !
 values(1)=0._SP
 !
 ! Bands on USER_k using the energy of the nearest in the internal list 
 !======================================================================
 !
 if (BANDS_internal) then
   !
   YAMBO_ALLOC(circuit_versor,(USER_K%nibz-1,3))
   !
   do ik=1,USER_K%nibz-1
     tmp_k=USER_K%pt(ik+1,:)-USER_K%pt(ik,:)
     circuit_versor(ik,:)=tmp_k/v_norm(tmp_k)
   enddo
   !
   do ik=1,USER_K%nibz-1
     !
     if (ik>1) values(1)=values(1)+iku_v_norm(USER_K%pt(ik,:)-USER_K%pt(ik-1,:))
     !
     do ikbz=1,Xk%nbz
       !
       tmp_k=Xk%ptbz(ikbz,:)-USER_K%pt(ik,:)
       !
       if ((abs(v_norm(tmp_k)-dot_product(tmp_k,circuit_versor(ik,:)))<1.E-5).and.&
&           iku_v_norm(tmp_k)<=iku_v_norm(USER_K%pt(ik+1,:)-USER_K%pt(ik,:))         ) then
         !
         do is=1,n_sp_pol
           values(1+1:nbands+1)=Xen%E(BANDS_bands(1):BANDS_bands(2),Xk%sstar(ikbz,1),is)*HA2EV
           values(nbands+2:nbands+4)=Xk%ptbz(ikbz,:)
           call k_transform(values(nbands+2:nbands+4),'iku')
           call msg('o '//trim(file_name_bands(2,is)),' ',(/values(1)+iku_v_norm(tmp_k),values(2:)/),INDENT=0,USE_TABS=.true.)
         enddo
         !
       endif
       !
     enddo
   enddo
   !
   YAMBO_FREE(circuit_versor)
   !
 endif
 !
 ! Bands on CIRCUIT using the energy of the nearest in the internal list 
 !=======================================================================
 !
 if (BANDS_interpolate) then
   !
   values(1)=0._SP
   !
   do ik=1,CIRCUIT_K%nibz
     !
     if (ik>1) values(1)=values(1)+iku_v_norm(CIRCUIT_K%pt(ik,:)-tmp_k)
     if (ik<CIRCUIT_K%nibz) tmp_k=CIRCUIT_K%pt(ik,:)
     !
     call k_transform(CIRCUIT_K%pt(ik,:),"iku")
     !
     if(n_spinor==2) then
       spin_values(1,:)=values(1)
       magn_values(1,:)=values(1)
     endif
     do is=1,n_sp_pol
       values(1+1:nbands+1)=bands_interpolated(BANDS_bands(1):BANDS_bands(2),is,ik)*HA2EV
       if (allocated(Xen%Eo).and.trim(BANDS_QE_file)/="none") then
         values(1+1:nbands+1)=values(1+1:nbands+1)+BANDS_Eo(ik,BANDS_bands(1):BANDS_bands(2))*HA2EV
       endif
       values(nbands+2:nbands+4)=CIRCUIT_K%pt(ik,:)
       call msg('o '//trim(file_name_bands(1,is)),' ',values,INDENT=0,USE_TABS=.true.)
       if(n_spinor==2) then
         do i1=1,2
           spin_values(1+1:nbands+1,i1)=spin_interpolated(BANDS_bands(1):BANDS_bands(2),ik,i1)
           spin_values(nbands+2:nbands+4,i1)=CIRCUIT_K%pt(ik,:)
           call msg('o '//trim(file_name_spin_magn(i1)),' ',(/spin_values(:,i1)/),INDENT=0,USE_TABS=.true.)
         enddo
         do i1=1,3
           magn_values(1+1:nbands+1,i1)=magn_interpolated(BANDS_bands(1):BANDS_bands(2),ik,i1)
           magn_values(nbands+2:nbands+4,i1)=CIRCUIT_K%pt(ik,:)
           call msg('o '//trim(file_name_spin_magn(i1+2)),' ',(/magn_values(:,i1)/),INDENT=0,USE_TABS=.true.)
         enddo
       endif
     enddo
     !
   enddo
   !
 endif
 !
 ! DOS
 !=====
 !
 if(DOS_interpolate) then
   !
   headings(1)  =  " ik  "
   headings(2:4)=(/" kx  "," ky  "," kz  "/)
   do ib=BANDS_bands(1),BANDS_bands(2)
     headings(4+ib-BANDS_bands(1)+1)=' b'//intc(ib)
   enddo
   !
   do is=1,n_sp_pol
     call msg('o '//trim(file_name_grid(is)),"#",headings,INDENT=0,USE_TABS=.true.)
   enddo
   !
   do ikbz=1,INTERP_K%nbz
     call k_transform(INTERP_K%ptbz(ikbz,:),"iku")
     values(2:4)=INTERP_K%ptbz(ikbz,:)
     !
     ik=INTERP_K%sstar(ikbz,1)
     !
     values(1)=real(ik,SP)
     !
     do is=1,n_sp_pol
       values(1+4:nbands+4)=bands_interpolated(BANDS_bands(1):BANDS_bands(2),is,ik)*HA2EV
       call msg('o '//trim(file_name_grid(is)),' ',values,INDENT=0,USE_TABS=.true.)
     enddo
     !
   enddo
   !
 endif
 !
 YAMBO_FREE(values)
 deallocate(headings)
 YAMBO_FREE(spin_values)
 YAMBO_FREE(magn_values)
 YAMBO_FREE_P(bands_interpolated)
 YAMBO_FREE(spin_interpolated)
 YAMBO_FREE(magn_interpolated)
 !
 do is=1,n_sp_pol
   call of_open_close(trim(file_name_bands(1,is)))
   call of_open_close(trim(file_name_bands(2,is)))
   call of_open_close(trim(file_name_grid(is)))
 enddo
 !
 if(BANDS_steps> 0.and.n_spinor==2) then
   do i1=1,5
     call of_open_close(trim(file_name_spin_magn(i1)))
   enddo
 endif
 !
end subroutine
