!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine WF_map_perturbative_SOC(kpt,En)
 !
 use YPP,                 ONLY:perturbative_SOC_path,SOC_split,SOC_bands_to_map,&
&                              SOC_bands_buffer
 use pars,                ONLY:lchlen,rZERO,cZERO,cI,SP
 use stderr,              ONLY:real2ch,intc
 use R_lattice,           ONLY:bz_samp
 use D_lattice,           ONLY:spin_sop,a,alat,x_cc
 use FFT_m,               ONLY:fft_size,fft_rot_r
 use com,                 ONLY:core_io_path,msg,warning,error
 use vec_operate,         ONLY:degeneration_finder,sort,iku_v_norm,k2bz,c2a
 use zeros,               ONLY:k_iku_zero
 use wave_func,           ONLY:WAVEs
 use interfaces,          ONLY:WF_load,WF_free
 use IO_m,                ONLY:io_control,LOG,OP_WR_CL
 use electrons,           ONLY:levels,n_sp_pol,n_spinor,n_spin,E_SOC_table,E_SOC_nbands
 use LIVE_t,              ONLY:live_timing
 use interfaces,          ONLY:io_DB1_selective_scan
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: kpt
 type(levels),  intent(inout) :: En
 !
 ! Work space
 !
 type(WAVEs)     :: WF,WF_SOC
 type(bz_samp)   :: kpt_SOC
 type(levels)    :: En_SOC
 !
 character(lchlen)    :: core_io_path_save
 integer              :: n_deg_grp,mapped_in_group(2,En%nb*n_sp_pol),kpts_map(2,kpt%nibz)
 integer              :: i_k,i_b,i_b_to_map(2),i_b_low_lim(2),i_b_upp_lim(2),i_grp,i_b_SOC,i_wf,i_wf_SOC,&
&                        i_sp_pol,n_miss,n_tot,i_sorted,i_deg,i_fft,i_fft_rot,i_k_SOC,i_k_SOC_bz,i_symm,i_star,&
&                        i_spinor,SOC_bands_to_point(2)
 integer              :: n_spin_save,n_spinor_save,n_sp_pol_save,fft_size_save,NB_to_map,NK_to_map,NB_up
 logical              :: group_already_mapped(En%nb*n_sp_pol),DB_is_ok,map_up,map_dw,kpoint_to_map(kpt%nibz)
 real(SP)             :: sq2,proj_up,proj_dw,mapping_tresh,min_sq(kpt%nibz),max_overlap,&
&                        kpt_diff(3),a_save(3,3),alat_save(3)
 !
 integer,     allocatable :: first_el(:),n_of_el(:),E_sort_map(:)
 real(SP),    allocatable :: E_sorted(:),E_vec(:)
 complex(SP), allocatable :: WF_SOC_rot(:,:)
 !
 integer                      :: io_err,ID
 integer, external            :: io_E_SOC_map
 !
 SOC_bands_to_point(1)=2*SOC_bands_to_map(1)-1
 SOC_bands_to_point(2)=2*SOC_bands_to_map(2)
 !
 call WF_load(WF,0,1,SOC_bands_to_map,(/1,kpt%nibz/),space='R',title='-SOC mapping')
 !
 core_io_path_save=core_io_path
 n_spin_save  =n_spin
 n_spinor_save=n_spinor
 n_sp_pol_save=n_sp_pol
 fft_size_save=fft_size
 !
 a_save=a
 alat_save=alat
 !
 io_err=io_DB1_selective_scan('all',trim(perturbative_SOC_path),En_SOC,kpt_SOC)
 if (io_err/=0) call error(' SOC database NOT FOUND @ '//trim(perturbative_SOC_path))
 !
 call fft_setup(0,1,.true.)
 !
 DB_is_ok=(kpt_SOC%nibz==kpt%nibz).and.(fft_size_save==fft_size).and. &
&         (all(a==a_save)).and.(all(alat==alat_save))
 !
 if (.not.DB_is_ok) call error(' SOC DBs and without SOC DB do not match')
 !
 call k_build_up_BZ_tables(kpt_SOC)
 call k_ibz2bz(kpt_SOC,'i',.false.)
 call k_expand(kpt_SOC)
 !
 call live_timing('K mapping',kpt%nibz)
 !
 do i_k=1,kpt%nibz
   !
   ik_SOC_loop: do i_k_SOC=1,kpt_SOC%nibz
     do i_star=1,kpt_SOC%nstar(i_k_SOC)
       i_symm     = kpt_SOC%star(i_k_SOC,i_star)
       i_k_SOC_bz = kpt_SOC%k_table(i_k_SOC,i_symm)
       kpt_diff=kpt_SOC%ptbz(i_k_SOC_bz,:)-kpt%pt(i_k,:)
       call k2bz(kpt_diff)
       if (iku_v_norm(kpt_diff,.false.)<iku_v_norm(k_iku_zero,.false.)) exit ik_SOC_loop
     enddo
   enddo ik_SOC_loop
   !
   kpts_map(:,i_k)=(/i_k_SOC,i_symm/)
   !
   if(i_k_SOC>kpt_SOC%nibz) call error(' Kpt #'//trim(intc(i_k))//' in the IBZ is not mapped')
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call build_spin_sop()
 !
 core_io_path = trim(perturbative_SOC_path)
 call WF_load(WF_SOC,0,1,SOC_bands_to_point,(/1,kpt%nibz/),space='R',title='-SOC mapping')
 !
 YAMBO_ALLOC(WF_SOC_rot,(fft_size,n_spinor))
 !
 core_io_path=core_io_path_save 
 n_spin  =n_spin_save
 n_spinor=n_spinor_save
 n_sp_pol=n_sp_pol_save
 !
 E_SOC_nbands=En_SOC%nb
 YAMBO_ALLOC(En%E_SOC,(E_SOC_nbands,kpt%nibz))
 YAMBO_ALLOC(E_SOC_table,(2,En%nb,kpt%nibz))
 En%E_SOC=0._SP
 E_SOC_table=0
 !
 mapping_tresh=0.95_SP
 !
 kpoint_to_map=.true.
 !
 min_sq=1._SP
 !
 NK_to_map=count(kpoint_to_map)
 NB_to_map=SOC_bands_to_map(2)-SOC_bands_to_map(1)+1
 YAMBO_ALLOC(E_vec,(NB_to_map*n_sp_pol))
 !
 YAMBO_ALLOC(x_cc,(fft_size,3,1))
 !
 call load_cc(.false.,(/a(1,1)/2._SP,a(2,2)/2._SP,a(3,3)/2._SP/))
 !
1 continue
 !
 call live_timing('WFs mapping @ tresh='//trim(real2ch(mapping_tresh)),NK_to_map*NB_to_map*2)
 !
 do i_k=1,kpt%nibz
   !
   i_k_SOC=kpts_map(1,i_k)
   i_symm =kpts_map(2,i_k)
   !
   kpt_diff=kpt%pt(i_k,:)-kpt_SOC%ptbz(kpt_SOC%k_table(i_k_SOC,i_symm),:)
   !
   call c2a(v_in=kpt_diff,mode='ki2c')
   !
   if(.not.kpoint_to_map(i_k)) cycle
   !
   min_sq(i_k)=1._SP
   !
   E_SOC_table(:,:,i_k)=0
   !
   i_sorted=0
   NB_up=0
   E_vec=0._SP
   do i_sp_pol=1,n_sp_pol
     do i_b=SOC_bands_to_map(1),SOC_bands_to_map(2)
       i_sorted=i_sorted+1
       if(i_sp_pol==1) NB_up=NB_up+1
       E_vec(i_sorted)=En%E(i_b,i_k,i_sp_pol)
     enddo
   enddo
   !
   NB_to_map=i_sorted
   YAMBO_ALLOC(E_sorted,(NB_to_map))
   YAMBO_ALLOC(E_sort_map,(NB_to_map))
   YAMBO_ALLOC(first_el,(NB_to_map))
   YAMBO_ALLOC(n_of_el,(NB_to_map))
   !
   E_sorted=E_vec(:NB_to_map)
   !
   call sort(E_sorted,indx=E_sort_map)
   !
   call degeneration_finder(E_sorted,NB_to_map,first_el,n_of_el,n_deg_grp,&
&                           SOC_split,Include_single_values=.true.)
   !
   group_already_mapped=.false.
   !
   mapped_in_group=0
   !
   do i_b_SOC=SOC_bands_to_point(1),SOC_bands_to_point(2)
     !
     i_wf_SOC=WF_SOC%index(i_b_SOC,i_k,1)
     !
     WF_SOC_rot=cZERO
     if(i_symm>1) then
       do i_fft=1,fft_size
         i_fft_rot=fft_rot_r(i_fft,i_symm)
         WF_SOC_rot(i_fft,:)=matmul( spin_sop(:,:,i_symm), WF_SOC%c(i_fft_rot,:,i_wf_SOC) )
       enddo
     else
       WF_SOC_rot=WF_SOC%c(:,:,i_wf_SOC)
     endif
     !
     do i_fft=1,fft_size
       WF_SOC_rot(i_fft,:)=WF_SOC_rot(i_fft,:)*exp(-cI*dot_product(kpt_diff,x_cc(i_fft,:,1)))
     enddo
     !
     proj_up=abs(dot_product( WF_SOC_rot(:, 1 ),WF_SOC_rot(:, 1) ))
     proj_dw=abs(dot_product( WF_SOC_rot(:, 2 ),WF_SOC_rot(:, 2) ))
     !
     max_overlap=0._SP
     !
     do i_grp=1,n_deg_grp
       !
       if (group_already_mapped(i_grp)) cycle 
       !
       sq2=rZERO
       !
       i_b_low_lim= SOC_bands_to_map(2)
       i_b_upp_lim=-1
       !
       do i_sorted=first_el(i_grp),first_el(i_grp)+n_of_el(i_grp)-1
         !
         i_deg=E_sort_map(i_sorted)
         !
         if(i_deg<=NB_up) then
           i_sp_pol=1
           i_b=i_deg+SOC_bands_to_map(1)-1
         else
           i_sp_pol=2
           i_b=i_deg-NB_up+SOC_bands_to_map(1)-1
         endif
         !
         i_b_low_lim(i_sp_pol)=min(i_b_low_lim(i_sp_pol),i_b)
         i_b_upp_lim(i_sp_pol)=max(i_b_upp_lim(i_sp_pol),i_b)
         !
         i_b_low_lim(i_sp_pol)=max(i_b_low_lim(i_sp_pol),SOC_bands_to_map(1))
         i_b_upp_lim(i_sp_pol)=min(i_b_upp_lim(i_sp_pol),SOC_bands_to_map(2))
         !
         if(n_sp_pol==1) then
           i_b_low_lim(2)=i_b_low_lim(1)
           i_b_upp_lim(2)=i_b_upp_lim(1)
         endif
         !
         i_wf=WF%index(i_b,i_k,i_sp_pol)
         !
         do i_spinor=1,2
           !
           if (n_sp_pol==2 .and. i_spinor/=i_sp_pol) cycle
           !
           sq2=sq2+abs(dot_product(WF_SOC_rot(:,i_spinor),WF%c(:,1,i_wf )))**2
           !
         enddo
         !
       enddo ! i_b
       !
       max_overlap=max(max_overlap,sq2)
       !
       if ( sqrt(sq2)>mapping_tresh) then
         !
         map_up=((abs(proj_up-proj_dw)> 1.E-3_SP).and.(proj_up>proj_dw)).or. &
         &      ((abs(proj_up-proj_dw)<=1.E-3_SP).and.mapped_in_group(1,i_grp)<=mapped_in_group(2,i_grp))
         map_dw=((abs(proj_up-proj_dw)> 1.E-3_SP).and.(proj_up<proj_dw)).or. &
         &      ((abs(proj_up-proj_dw)<=1.E-3_SP).and.mapped_in_group(1,i_grp)> mapped_in_group(2,i_grp))
         !
         i_b_to_map(:)=i_b_low_lim(:)+mapped_in_group(:,i_grp)
         !
         if( map_up .and. i_b_to_map(1)>i_b_upp_lim(1) ) then
           map_up=.false.
           map_dw=.true.
         endif
         !
         if( map_dw .and. i_b_to_map(2)>i_b_upp_lim(2) ) then
           map_up=.true.
           map_dw=.false.
         endif
         !
         if ( all(i_b_to_map(:)<SOC_bands_to_map(1)) ) cycle
         if ( all(i_b_to_map(:)>i_b_upp_lim(:))      ) cycle
         !
         min_sq=min(min_sq,sqrt(sq2))
         !
         if(map_up) then
           E_SOC_table(1,i_b_to_map(1),i_k)=i_b_SOC
           mapped_in_group(1,i_grp)=mapped_in_group(1,i_grp)+1
         else if(map_dw) then
           E_SOC_table(2,i_b_to_map(2),i_k)=i_b_SOC
           mapped_in_group(2,i_grp)=mapped_in_group(2,i_grp)+1
         endif
         En%E_SOC(i_b_SOC,i_k)=En_SOC%E(i_b_SOC,i_k_SOC,1)
         if( all(mapped_in_group(:,i_grp)==n_of_el(i_grp)) ) group_already_mapped(i_grp)=.true.
         exit
       endif
       !
     enddo   ! i_grp
     !
     call live_timing(steps=1)
     !
   enddo     ! i_b_SOC
   !
   YAMBO_FREE(E_sorted)
   YAMBO_FREE(E_sort_map)
   YAMBO_FREE(first_el)
   YAMBO_FREE(n_of_el)
   !
 enddo       ! i_k
 !
 call live_timing()
 !
 if(any(E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2)-SOC_bands_buffer,:)==0)) then
   !
   n_tot =size (E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2)-SOC_bands_buffer,:)   )
   n_miss=count(E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2)-SOC_bands_buffer,:)==0)
   call warning(' With treshold '//trim(real2ch(mapping_tresh))//' '//trim(intc(n_miss))//'/'&
&                //trim(intc(n_tot))//' WFs non mapped. Reducing it.' )
   !
   do i_k=1,kpt%nibz
     kpoint_to_map(i_k)=any(E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2)-SOC_bands_buffer,i_k)==0)
     !do i_b=SOC_bands_to_map(1),SOC_bands_to_map(2)
     !  if(all(E_SOC_table(:,i_b,i_k)/=0)) cycle
     !  call warning(' State ik= '//trim(intc(i_k))//', ib= '//trim(intc(i_b))//' not mapped.' )
     !enddo
   enddo
   !
   if(abs(mapping_tresh-0.05_SP)<1.E-5) call error(' Minimum treshold reached.')
   mapping_tresh=mapping_tresh-0.1_SP
   goto 1
   !
 endif
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 io_err=io_E_SOC_map(En,kpt,ID) 
 !
 call msg('s',' Mapping finalized with treshold '//trim(real2ch(mapping_tresh))//' ')
 call msg('s',' Minimum overlap used is         '//trim(real2ch(minval(min_sq)))//' ')
 !
 call WF_free(WF)
 call WF_free(WF_soc)
 !
 YAMBO_FREE(WF_SOC_rot)
 YAMBO_FREE(E_vec)
 YAMBO_FREE(E_SOC_table)
 YAMBO_FREE(x_cc)
 !
end subroutine WF_map_perturbative_SOC
