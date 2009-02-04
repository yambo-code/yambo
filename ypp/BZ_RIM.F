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
subroutine BZ_RIM(Xk,Xen)
 !
 use pars,                ONLY:SP,DP
 use units,               ONLY:HARTREE
 use R_lattice,           ONLY:bz_samp,nkibz
 use D_lattice,           ONLY:alat,nsym,default_Tel,a
 use com,                 ONLY:msg
 use vec_operate,         ONLY:c2a,ak2bz,v_norm
 use YPP,                 ONLY:BZ_RIM_nkpts,coo_out,BZ_RIM_path,gamma_radius
 use electrons,           ONLY:levels,default_nel,n_bands,&
&                              BZ_RIM_nbands,BZ_RIM_nkpt,BZ_RIM_table,&
&                              BZ_RIM_tot_nkpts,n_spin,BZ_RIM_max_filling
 use timing,              ONLY:live_timing_is_on,live_timing
 use IO_m,                ONLY:io_control,NONE,OP_WR_CL
 !
 implicit none
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 real(SP)     ::dummy,rand_kpt(BZ_RIM_nkpts+1,3),a_save(3,3),alat_save(3),&
&               gap(2)
 integer      ::i1,i2,ikbz
 integer, external :: k_the_nearest
 !
 ! Random Generator
 !
 character(12)      :: ch(3)
 integer            :: iseed(8),k_counter
 real(DP), external :: dlaran
 real(SP)           :: k_module,k_(3)
 logical      :: add_X_grid,impose_gamma_radius
 !
 ! BZ DB1 I/O
 !
 type(levels)      :: BZ_E
 type(bz_samp)     :: BZ_k
 integer           :: ID,i_err
 integer, external :: ioDB1_reload,ioE_RIM
 !
 ! Saves 2 check
 !
 a_save=a
 alat_save=alat
 !
 ! DB1 reloading
 !
 i_err=ioDB1_reload(BZ_E,BZ_k, trim(BZ_RIM_path) )
 !
 ! DB1 checks
 !
 if (i_err==0.and.any(a/=a_save))  then
   call msg("s",':: RIM DB1 DL structure does not fit')
   i_err=-1
 endif
 if (i_err==0.and.any(alat/=alat_save))  then
   call msg("s",':: RIM DB1 alat does not fit.')
   i_err=-1
 endif
 !
 impose_gamma_radius=gamma_radius>0.
 !
 if (i_err/=0.or.trim(BZ_RIM_path)==".") then
   !
   call date_and_time(ch(1),ch(2),ch(3),iseed)
   iseed=iabs(iseed)
   ! iseed(4) must be odd
   iseed(4)=2*(iseed(4)/2)+1
   !
   call section('*',"== BZ RIM grid generator ==")
   !
   live_timing_is_on=.false.
   !
   call msg("s"," ========== RIM K-grid ==========")
   !
   k_counter=1
   !
   do while ( k_counter < BZ_RIM_nkpts -1 )
     do i2=1,3
       rand_kpt(k_counter,i2)=(2.*dlaran(iseed(4:))-1.)
     enddo
     dummy=ak2bz(rand_kpt(k_counter,:))
     call c2a(v_in=rand_kpt(k_counter,:),mode="ka2i")
     !
     ! select only point with |k| < gamma_radius [a.u.]
     !
     if (impose_gamma_radius) then
       call c2a(v_in=rand_kpt(k_counter,:),v_out=k_,mode="ki2c")
       k_module=v_norm(k_)
       if (k_module > gamma_radius) cycle
     endif
     !
     if (trim(coo_out)=="cc") call c2a(v_in=rand_kpt(k_counter,:),mode="ka2c")
     !
     call msg("s"," ",rand_kpt(k_counter,:))
     !
     k_counter=k_counter+1
     !
   enddo
   live_timing_is_on=.true.
   return
 endif 
 !
 call section('*','BZ RIM DB')
 !
 gap(1)=minval(BZ_E%E(Xen%nbf+1,:,1))-maxval(BZ_E%E(Xen%nbf,:,1)) 
 gap(2)=10000.
 do i1=1,nkibz
   if (BZ_E%E(Xen%nbf+1,i1,1)-BZ_E%E(Xen%nbf,i1,1)<gap(2)) then
     gap(2)=BZ_E%E(Xen%nbf+1,i1,1)-BZ_E%E(Xen%nbf,i1,1)
   endif
 enddo
 !
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',default_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 call msg('s',':: K points              :',nkibz)
 call msg('s',':: Bands                 :',n_bands)
 call msg('s',':: Symmmetries           :',nsym)
 if (Xen%nbf==Xen%nbm) then
   call msg('s',':: Gap indirect      [ev]:',gap(1)*HARTREE)
   call msg('s','::     direct        [ev]:',gap(2)*HARTREE)
 endif
 !
 call section('+','BZ RIM K-points expansion ...')
 !
 ! Xk (IBZ->BZ)
 !
 call k_ibz2bz(Xk,'i',.true.)
 !
 ! BZ_k (IBZ->BZ)
 !
 call k_ibz2bz(BZ_k,'i',.true.)
 !
 call msg('l','done')
 !
 add_X_grid=.true.
 !
 BZ_RIM_nbands=BZ_E%nb
 BZ_RIM_tot_nkpts=BZ_k%nibz
 if (add_X_grid) BZ_RIM_tot_nkpts=BZ_RIM_tot_nkpts+Xk%nbz
 !
 allocate(BZ_RIM_nkpt(Xk%nbz))
 BZ_RIM_nkpt=0
 !
 call live_timing('BZ RIM Tables',BZ_k%nbz*2,SERIAL=.true.)
 do while(.not.allocated(BZ_RIM_table)) 
   if (maxval(BZ_RIM_nkpt)>0) then
     if (add_X_grid)      allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)+1))
     if (.not.add_X_grid) allocate(BZ_RIM_table(Xk%nbz,maxval(BZ_RIM_nkpt)))
     BZ_RIM_table=0
   endif
   BZ_RIM_nkpt=0
   do i1=1,BZ_k%nbz
     !
     ikbz=k_the_nearest(BZ_k%ptbz(i1,:),Xk%ptbz(:,:),Xk%nbz,.FALSE.,i_err)
     BZ_RIM_nkpt(ikbz)=BZ_RIM_nkpt(ikbz)+1
     !
     if (allocated(BZ_RIM_table)) BZ_RIM_table(ikbz,BZ_RIM_nkpt(ikbz))=BZ_k%sstar(i1,1)
     !      
     call live_timing(steps=1)
     !
   enddo
 enddo
 call live_timing()
 !
 BZ_RIM_max_filling=maxval(BZ_RIM_nkpt)
 if (add_X_grid) BZ_RIM_max_filling=BZ_RIM_max_filling+1
 call msg('s',':: BZ blocks filling     :',(/minval(BZ_RIM_nkpt),BZ_RIM_max_filling/))
 !
 !  At the end I add to the Xen%E_RIM array the Xen%E energies
 !
 allocate(Xen%E_RIM(BZ_RIM_nbands,BZ_RIM_tot_nkpts,n_spin))
 Xen%E_RIM(:,:BZ_k%nibz,:)=BZ_E%E(:,:,:)
 !
 if (add_X_grid) then
   forall(i1=1:Xk%nbz) BZ_RIM_nkpt(i1)=BZ_RIM_nkpt(i1)+1
   forall(i1=1:Xk%nbz) BZ_RIM_table(i1,BZ_RIM_nkpt(i1))=BZ_k%nibz+i1
   do i1=1,Xk%nbz
     Xen%E_RIM(:,BZ_k%nibz+i1,:)=Xen%E(:BZ_RIM_nbands,Xk%sstar(i1,1),:)+Xen%Efermi(1)
   enddo
 endif
 !
 ! Final I/O
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=NONE,ID=ID)
 i_err=ioE_RIM(Xen,ID)
 !
 ! CLEAN
 !
 deallocate(BZ_RIM_nkpt,BZ_RIM_table,Xen%E_RIM)
 call k_ibz2bz(Xk,'d',.false.) 
 call k_ibz2bz(BZ_k,'d',.false.) 
 !
end subroutine
