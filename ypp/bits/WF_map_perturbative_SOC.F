!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use YPP,                 ONLY:perturbative_SOC_path,SOC_split,SOC_bands_to_map
 use pars,                ONLY:lchlen,rZERO,SP,IP
 use stderr,              ONLY:real2ch,intc
 use units,               ONLY:HA2EV
 use R_lattice,           ONLY:bz_samp
 use com,                 ONLY:core_io_path,msg,warning,error
 use vec_operate,         ONLY:degeneration_finder
 use wave_func,           ONLY:WAVEs
 use interfaces,          ONLY:WF_load
 use memory_m,            ONLY:mem_est
 use IO_m,                ONLY:io_control,LOG,OP_WR_CL
 use electrons,           ONLY:levels,n_sp_pol,n_spinor,n_spin,E_SOC_table,E_SOC_nbands
 !
 implicit none
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
 integer              :: first_el(En%nb),n_of_el(En%nb),n_deg_grp,mapped_in_group(2,En%nb)
 integer              :: i_k,i_b,i_b_to_map(2),i_b_limit,i_grp,i_b_SOC,i_wf,i_wf_SOC(2),i_sp_pol,n_miss
 integer              :: n_spin_save,n_spinor_save,n_sp_pol_save
 logical              :: group_already_mapped(En%nb),DB_is_ok,map_up,map_dw,kpoint_to_map(kpt%nibz)
 real(SP)             :: sq2,proj_up,proj_dw,mapping_tresh,min_sq(kpt%nibz)
 !
 integer                      :: io_err,ID
 integer, external            :: io_DB1_selective_scan,io_E_SOC_map
 !
 call WF_load(WF,0,1,(/ 1, En%nb /),(/1,kpt%nibz/),space='G',title='-SOC mapping')
 !
 core_io_path_save=core_io_path
 n_spin_save=n_spin
 n_spinor_save=n_spinor
 n_sp_pol_save=n_sp_pol
 !
 io_err=io_DB1_selective_scan(En_SOC,kpt_SOC,trim(perturbative_SOC_path),'all')
 if (io_err/=0) call msg("s",'SOC database NOT FOUND @'//trim(perturbative_SOC_path))
 !
 !DB_is_ok=(En_SOC%nb==2*En%nb).and.(kpt_SOC%nibz==kpt%nibz)
 DB_is_ok=(kpt_SOC%nibz==kpt%nibz)
 !
 if (.not.DB_is_ok) call error(' SOC DBs and without SOC DB do not match')
 !
 core_io_path = trim(perturbative_SOC_path)
 call WF_load(WF_SOC,0,1,(/ 1, En_SOC%nb /),(/1,kpt%nibz/),space='G',title='-SOC mapping')
 !
 core_io_path=core_io_path_save 
 n_spin=n_spin_save
 n_spinor=n_spinor_save
 n_sp_pol=n_sp_pol_save
 !
 E_SOC_nbands=En_SOC%nb
 allocate(En%E_SOC(E_SOC_nbands,kpt%nibz))
 allocate(E_SOC_table(2,En%nb,kpt%nibz))
 call mem_est("E_SOC E_SOC_table",(/size(En%E_SOC),size(E_SOC_table)/),(/SP,IP/))
 En%E_SOC=0._SP
 E_SOC_table=0
 !
 mapping_tresh=0.95_SP
 !
 kpoint_to_map=.true.
 !
 min_sq=1._SP
 !
1 continue
 !
 do i_k=1,kpt%nibz
   !
   if(.not.kpoint_to_map(i_k)) cycle
   !
   min_sq(i_k)=1._SP
   !
   E_SOC_table(:,:,i_k)=0
   !
   do i_sp_pol=1,n_sp_pol
     !
     call degeneration_finder(En%E(:,i_k,i_sp_pol),En%nb,first_el,n_of_el,n_deg_grp,&
&                             SOC_split,Include_single_values=.true.)
     !
     group_already_mapped=.false.
     !
     mapped_in_group=0
     !
     do i_b_SOC=1,En_SOC%nb
       !
       i_wf_SOC=WF_SOC%index(i_b_SOC,i_k,:)
       !
       proj_up=abs(dot_product( WF_SOC%c(:, i_wf_SOC(1) ),WF_SOC%c(:, i_wf_SOC(1)) ))
       proj_dw=abs(dot_product( WF_SOC%c(:, i_wf_SOC(2) ),WF_SOC%c(:, i_wf_SOC(2)) ))
       !
       do i_grp=1,n_deg_grp
         !
         if (group_already_mapped(i_grp)) cycle 
         !
         sq2=rZERO
         !
         do i_b=first_el(i_grp),first_el(i_grp)+n_of_el(i_grp)-1
           !
           i_wf=WF%index(i_b,i_k,i_sp_pol)
           !
           !if(n_sp_pol==2) then
           !   Here I assume that up and down states are non degenerate
           !   I have to think about that
           !  sq2=sq2+abs(dot_product(WF_SOC%c(:, i_wf_SOC(i_sp_pol) ),WF%c(:, i_wf )))**2
           !  
           !else
             ! Here I assume that up and down states are degenerate
             sq2=sq2+abs(dot_product(WF_SOC%c(:, i_wf_SOC(1) ),WF%c(:, i_wf )))**2   &
                    +abs(dot_product(WF_SOC%c(:, i_wf_SOC(2) ),WF%c(:, i_wf )))**2
             !
           !endif
           !
         enddo ! i_b
         !
         if ( sqrt(sq2)>mapping_tresh) then
           !
           map_up=((abs(proj_up-proj_dw)> 1.E-3_SP).and.(proj_up>proj_dw)).or. &
           &      ((abs(proj_up-proj_dw)<=1.E-3_SP).and.mapped_in_group(1,i_grp)<=mapped_in_group(2,i_grp))
           map_dw=((abs(proj_up-proj_dw)> 1.E-3_SP).and.(proj_up<proj_dw)).or. &
           &      ((abs(proj_up-proj_dw)<=1.E-3_SP).and.mapped_in_group(1,i_grp)> mapped_in_group(2,i_grp))
           !
           i_b_to_map(:)=first_el(i_grp)+mapped_in_group(:,i_grp)
           !
           if(i_grp< n_deg_grp) i_b_limit=min(SOC_bands_to_map(2),first_el(i_grp+1)-1)
           if(i_grp==n_deg_grp) i_b_limit=    SOC_bands_to_map(2)
           !
           if( map_up .and. i_b_to_map(1)>i_b_limit ) then
             map_up=.false.
             map_dw=.true.
           endif
           !
           if( map_dw .and. i_b_to_map(2)>i_b_limit ) then
             map_up=.true.
             map_dw=.false.
           endif
           !
           if(all(i_b_to_map(:)<SOC_bands_to_map(1))) cycle
           if(all(i_b_to_map(:)>i_b_limit)          ) cycle
           !
           min_sq(i_k)=min(min_sq(i_k),sqrt(sq2))
           !
           if(map_up) then
             E_SOC_table(1,i_b_to_map(1),i_k)=i_b_SOC
             mapped_in_group(1,i_grp)=mapped_in_group(1,i_grp)+1
           else if(map_dw) then
             E_SOC_table(2,i_b_to_map(2),i_k)=i_b_SOC
             mapped_in_group(2,i_grp)=mapped_in_group(2,i_grp)+1
           endif
           En%E_SOC(i_b_SOC,i_k)=En_SOC%E(i_b_SOC,i_k,1)
           if( all(mapped_in_group(:,i_grp)==n_of_el(i_grp)) ) group_already_mapped(i_grp)=.true.
           exit
         endif
         !
       enddo   ! i_grp
     enddo     ! i_b_SOC
   enddo       ! i_sp_pol
 enddo         ! i_k
 !
 if(any(E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2),:)==0)) then
   !
   n_miss=count(E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2),:)==0)
   call warning(' With treshold '//trim(real2ch(mapping_tresh))//' '//trim(intc(n_miss))//' WFs non mapped. Reducing it.' )
   !
   do i_k=1,kpt%nibz
     kpoint_to_map(i_k)=any(E_SOC_table(:,SOC_bands_to_map(1):SOC_bands_to_map(2),i_k)==0)
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
 call msg('s',' Mapping finalized with treshold '//trim(real2ch(mapping_tresh))//' ')
 call msg('s',' Minimum overlap used is         '//trim(real2ch(minval(min_sq)))//' ')
 !
 call WF_free(WF)
 call WF_free(WF_soc)
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=LOG,ID=ID)
 io_err=io_E_SOC_map(En,kpt,ID) 
 !
end subroutine WF_map_perturbative_SOC
