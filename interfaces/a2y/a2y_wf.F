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
subroutine a2y_wf(wf_disk,i_sp_pol,ikibz,ib_grp,KSS_file_name)
 !
 ! Schematic I/O in outkss.F90:
 !
 ! open(untkss,file=filekss,form='unformatted')
 ! call hdr_io(fform,Hdr,rdwr,untkss)
 ! 1 write(untkss) title(1:80)
 ! 2 write(untkss) title(1:80)
 ! 3 write(untkss) nsym2,nbandksseff,npwkss,ishm,mpsang
 ! 4 write(untkss) (((symrel2(ii,jj,kk),ii=1,3),jj=1,3),kk=1,nsym2)
 ! 5 write(untkss) ((tnons2(ii,kk),ii=1,3),kk=1,nsym2)
 ! 6 write(untkss) ((gbig(ii,ig),ii=1,3),ig=1,npwkss)
 ! 7 write(untkss) (shlim(in),in=1,ishm)
 ! 8 write(untkss) ((vkbsign(is,il),il=1,mpsang),is=1,ntypat)
 ! do isppol=1,nsppol
 !  do ikpt=1,nkpt
 !    do is=1,ntypat
 !     do il=1,mpsang   (or mpsang*m_proj*n_spinor for HGH)
 !      write(untkss) (vkb (ig,is,il),ig=1,npwkss) !trsl(ig) is not used anymore in m_io_kss
 !      write(untkss) (vkbd(ig,is,il),ig=1,npwkss) !trsl(ig) is not used anymore in m_io_kss
 !     end do
 !    end do
 !    write(untkss) (en(ib),ib=1,nbandksseff)
 !    do ib=1,nbandksseff
 !     write(untkss) (wfg(:,ig,ib),ig=1,npwkss*nspinor)
 !    end do
 !  end do ! k-point
 ! end do ! spin
 ! close(unit=ntkss)
 !
 use pars,                  ONLY:SP,DP,cZERO
 use electrons,             ONLY:n_bands,n_spin,n_spinor,n_sp_pol
 use D_lattice,             ONLY:n_atomic_species
 use pseudo,                ONLY:pp_n_l_times_proj_max,PP_alloc_abinit,pp_kbs,  &
&                                pp_kb,pp_kbd,l_many_proj,pp_table
 use R_lattice,             ONLY:nkibz,ng_vec
 use wave_func,             ONLY:wf_ncx,wf_nc_k,wf_nb_io,wf_nb_io_groups,wf_igk
 use defs_datatypes,        ONLY:hdr_type, wffile_type
 use mod_com2y,             ONLY:artificial_spin_pol,ng_vec_abinit
 !
 implicit none
 !
 integer        :: ikibz,i_sp_pol,ib_grp
 character(*)   :: KSS_file_name
 complex(SP)    :: wf_disk(wf_ncx,n_spinor,wf_nb_io)
 ! 
 ! Workspace ...
 ! 
 integer            :: i1,i2,nb_to_read
 !
 ! ... abinit
 !
 type(hdr_type)     :: ahdr
 type(wffile_type)  :: wff
 real(DP)           :: pp_kb_kdb_disk(ng_vec_abinit),&
&                      pp_kbs_disk(n_atomic_species,pp_n_l_times_proj_max)
 complex(DP)        :: wf_disk_DP(ng_vec_abinit,n_spinor)
 integer            :: fform,i_spin,i_spinor
 !
 !
 if ((i_sp_pol==1.or.artificial_spin_pol).and.ikibz==1.and.ib_grp==1) then
   open(unit=11,file=KSS_file_name,form='unformatted')
   wff%unwff=11
   wff%accesswff=0
   fform=1
   call hdr_io_wfftype(fform,ahdr,5,wff)
   do i1=1,3
     read(11)
   enddo
   !
   if(l_many_proj) then
     read(11)
     read(11) ((pp_table(1,i2,i1),i1=1,pp_n_l_times_proj_max),i2=1,n_atomic_species)
     read(11) ((pp_table(2,i2,i1),i1=1,pp_n_l_times_proj_max),i2=1,n_atomic_species)
     read(11) ((pp_table(3,i2,i1),i1=1,pp_n_l_times_proj_max),i2=1,n_atomic_species)
     !
     pp_table(1,:,:)=pp_table(1,:,:)+1  ! l+1 , not l
     !
   endif
   !
   do i1=1,4
     read(11)
   enddo
   !
   call PP_alloc_abinit()
   read(11) ((pp_kbs_disk(i1,i2),i2=1,pp_n_l_times_proj_max),i1=1,n_atomic_species)
   pp_kbs=pp_kbs_disk
   !
 endif
 !
 wf_disk=cZERO
 pp_kb=0._SP
 pp_kbd=0._SP
 !
 if (ib_grp==1) then
   do i1=1,n_atomic_species
     do i2=1,pp_n_l_times_proj_max
       read(11) pp_kb_kdb_disk
       pp_kb(:ng_vec_abinit,i1,i2,i_sp_pol)=pp_kb_kdb_disk
       read(11) pp_kb_kdb_disk
       pp_kbd(:ng_vec_abinit,i1,i2,i_sp_pol)=pp_kb_kdb_disk
     enddo
   enddo   
 endif
 if (ib_grp==1) read(11) ! line with the energies
 !
 ! Read all bands or only the remaining in the last block
 !
 nb_to_read=wf_nb_io
 if (ib_grp*wf_nb_io>n_bands) nb_to_read=n_bands-wf_nb_io*(ib_grp-1) 
 !
 do i1=1,nb_to_read
   !
   read(11) wf_disk_DP(:,:)
   !
   wf_disk(:,:,i1)= cmplx(wf_disk_DP(wf_igk(:wf_nc_k(ikibz),ikibz),:),kind=SP)
   !
 enddo
 ! 
 if ((i_sp_pol==2.or.artificial_spin_pol).and.ikibz==nkibz.and.ib_grp==wf_nb_io_groups) close(11)
 !
end subroutine
