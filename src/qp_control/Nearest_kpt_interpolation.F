!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine Nearest_kpt_interpolation(Nki,Nko,Nb,Nstates,N_int,n_neigh,table,k_pti,k_pto,wki,wko,&
&                                    Vali,Valo,what,l_rescale)
 !
 ! Interpolation with nearest neighbor method but averaging up to n_neigh neighbors
 ! The input values are known on the k_pti kgrid and there can be N_int quantities to
 ! be interpolated. The quantities to be interpolated are on a vector which is mapped to the 
 ! kpts, bands and spin indexes by the table.
 ! what is used just for the message
 !
 use pars,           ONLY:SP
 use com,            ONLY:msg
 use vec_operate,    ONLY:sort
 use electrons,      ONLY:n_sp_pol
 use R_lattice,      ONLY:k_the_nearest
#if defined _RT
 use R_lattice,      ONLY:nkibz
#endif
 !
 implicit none
 !
 integer,            intent(in) :: Nki,Nko,Nb(2),Nstates,table(Nstates,4)
 integer,            intent(in) :: N_int,n_neigh
 real(SP),           intent(in) :: k_pti(Nki,3),k_pto(Nko,3),wki(Nki),wko(Nko)
 real(SP),           intent(in) :: Vali(Nstates,N_int)
 real(SP),           intent(out):: Valo(Nb(1):Nb(2),Nko,n_sp_pol,N_int)
 character(3),       intent(in) :: what
 logical,            intent(in) :: l_rescale
 !
 ! Work Space
 !
 logical  :: write_message,done
 real(SP) :: idist(Nki),odist(Nki),k_dist(n_neigh),toti(N_int),toto(N_int)
 integer  :: order(Nki),ik_near(n_neigh),iko,iki,i_err,nk_exact,&
&            nk_interpolated,i_state,i_val
 !
 write_message=.true.
 !
#if defined _RT
 write_message=Nko==nkibz
#endif
 !
#if defined _YPP_RT
 write_message=(what=="RT0")
#endif
 !
 nk_exact=0
 nk_interpolated=0
 !
 do iko=1,Nko
   !
   ik_near=0
   k_dist=1.E5
   idist=1.E5
   !
   ik_near(1)=k_the_nearest(k_pto(iko,:),k_pti,Nki,k_dist=idist,ROTATE_k_ref=.TRUE.)
   !
   if (Nki>1) then
     call sort(idist,odist,order)
     ik_near(:min(Nki,n_neigh))=order(:min(Nki,n_neigh))
     k_dist(:min(Nki,n_neigh))=odist(:min(Nki,n_neigh))
   else
     ik_near(1)=1
     k_dist(1)=idist(1)
   endif
   !
   ! Transfer OR interpolate
   !
   ! First I check if there is an exact correposndance
   ! between k_pto and k_pti. This exists when i_err==0
   !
   ik_near(1)=k_the_nearest(k_pto(iko,:),k_pti,Nki,ROTATE_k_ref=.TRUE.,i_err=i_err)
   !
   if (i_err==0.and.n_neigh==1) then
     call val_transfer()
     if (done) nk_exact=nk_exact+1
   else
     call val_transfer()
     if (done) nk_interpolated=nk_interpolated+1
   endif
   !
 enddo
 !
 if(l_rescale) then
   toti=0._SP
   do i_state=1,Nstates
     iki=table(i_state,3)
     toti(:)=toti(:)+abs(Vali(i_state,:))*wki(iki)
   enddo
   toto=0._SP
   do i_val=1,N_int
     do iko=1,Nko
       toto(i_val)=toto(i_val)+sum(abs(Valo(:,iko,:,i_val)))*wko(iko)
     enddo
     if(abs(toto(i_val))>0._SP) Valo(:,:,:,i_val)=Valo(:,:,:,i_val)*toti(i_val)/toto(i_val)
   enddo
 endif
 !
 if(write_message) then
   call msg('nrs','['//trim(what)//'] Kpts covered exactly  [o/o]:',real(nk_exact)/real(Nko)*100._SP)
   if (nk_interpolated>0) then
     call msg('sr','[Interpolate] Nighbours         :',n_neigh)
     call msg('sr','[Interpolate] Kpts covered [o/o]:',real(nk_interpolated)/real(Nko)*100._SP)
   endif
 endif
 !
 contains
   !
   subroutine val_transfer()
     !
     use electrons,   ONLY:spin
     !
     ! Work Space
     !
     integer     :: i_neigh,i_band,i_spin
     !
     done=.false.
     !
     do i_spin=1,n_sp_pol
       do i_val=1,N_int
         Valo(:,iko,i_spin,i_val)=0._SP
       enddo
     enddo
     !
     do i_neigh=1,n_neigh
       do i_state=1,Nstates
         !
         if( table(i_state,3)/=ik_near(i_neigh)) cycle
         !
         i_band=table(i_state,1)
         i_spin=spin(table(i_state,:))
         !
         if (i_band<Nb(1).or.i_band>Nb(2)) cycle
         !
         done=.true.
         !
         do i_val=1,N_int
           Valo(i_band,iko,i_spin,i_val)=Valo(i_band,iko,i_spin,i_val)+Vali(i_state,i_val)/real(n_neigh)
         enddo
         !
       enddo
     enddo
     !
   end subroutine
   !
end subroutine Nearest_kpt_interpolation
