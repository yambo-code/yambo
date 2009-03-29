!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine a2y_wf(wf_disk,ikibz,ib_grp,KSS_file_name)
 !
 use pars,                  ONLY:SP,DP
 use electrons,             ONLY:n_bands,n_spin
 use D_lattice,             ONLY:n_atom_species
 use pseudo,                ONLY:pp_n_l_comp_max,PP_alloc,pp_kbs,pp_kb,pp_kbd
 use R_lattice,             ONLY:nkibz
 use wave_func,             ONLY:wf_ncx,wf_nb_io,wf_nb_io_groups
 use defs_datatypes,        ONLY:hdr_type, wffile_type
 implicit none
 !
 integer        :: ikibz,ib_grp
 character(*)   :: KSS_file_name
 real(SP)       :: wf_disk(2,wf_nb_io,wf_ncx,n_spin)
 ! 
 ! Workspace ...
 ! 
 integer            :: i1,i2,nb_to_read,number_of_records_each_k
 !
 ! ... abinit
 !
 type(hdr_type)     :: ahdr
 type(wffile_type)  :: wff
 real(DP)           :: pp_kb_kdb_disk(wf_ncx),&
&                      pp_kbs_disk(n_atom_species,pp_n_l_comp_max)
 complex(DP)        :: wf_disk_DP(wf_ncx)
 integer            :: fform,i_spin
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
 !     do il=1,mpsang
 !      write(untkss) (vkb (trsl(ig),is,il),ig=1,npwkss)
 !      write(untkss) (vkbd(trsl(ig),is,il),ig=1,npwkss)
 !     end do
 !    end do
 !    write(untkss) (en(ib),ib=1,nbandksseff)
 !    do ib=1,nbandksseff
 !     write(untkss) (wfg(:,ig,ib),ig=1,npwkss*nspinor)
 !    end do
 ! end do ! k-point
 !end do ! spin
 !close(unit=ntkss)
 !
 ! Number of records for each k-point
 !
 number_of_records_each_k=n_atom_species*pp_n_l_comp_max*2+1+nb_to_read
 !
 if (ikibz==1.and.ib_grp==1) then
   open(unit=11,file=KSS_file_name,form='unformatted')
   wff%unwff=11
   wff%accesswff=0
   fform=1
   call hdr_io_wfftype(fform,ahdr,5,wff)
   do i1=1,7
     read(11)
   enddo
   call PP_alloc()
   read(11) ((pp_kbs_disk(i1,i2),i2=1,pp_n_l_comp_max),i1=1,n_atom_species)
   pp_kbs=pp_kbs_disk
 endif
 !
 do i_spin=1,n_spin
   !
   if (ib_grp==1) then
     do i1=1,n_atom_species
       do i2=1,pp_n_l_comp_max
         read(11) pp_kb_kdb_disk
         pp_kb(:,i1,i2,1)=pp_kb_kdb_disk
         read(11) pp_kb_kdb_disk
         pp_kbd(:,i1,i2,1)=pp_kb_kdb_disk
       enddo
     enddo
     read(11) 
   endif
   !
   ! Read all bands or only the remaining in the last block
   !
   nb_to_read=wf_nb_io
   if (ib_grp*wf_nb_io>n_bands) nb_to_read=n_bands-wf_nb_io*(ib_grp-1) 
   !
   wf_disk(:,:,:,:)=0.
   !
   do i1=1,nb_to_read
     read(11) wf_disk_DP
     wf_disk(1,i1,:,1)=real(wf_disk_DP(:))
     wf_disk(2,i1,:,1)=aimag(wf_disk_DP(:))
   enddo
   !
   if (n_spin==2) then
     ! 
     ! BIG PROBLEM WITH ABINIT: abinit uses a different ordering
     ! of the k-points/spins. This makes the I/O very complicate.
     !
     if (i_spin==1) then
       !
       ! After the UP spin I must read enough records in order to reach the
       ! DN block.
       !
       do i1=1,number_of_records_each_k*(nkibz-1)
         read (11)
       enddo
     else 
       !
       ! After the DN spin I must go back to the end of the spin UP block.
       !
       do i1=1,number_of_records_each_k*nkibz
         backspace (11)
       enddo
     endif
     !
   endif
   !
 enddo
 !
 if (ikibz==nkibz.and.ib_grp==wf_nb_io_groups) close(11)
 !
end subroutine
