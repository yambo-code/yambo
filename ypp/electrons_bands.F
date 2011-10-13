! 
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team
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
subroutine electrons_bands(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HA2EV
 use YPP,            ONLY:BANDS_steps,coo_in,BANDS_range
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use com,            ONLY:msg,of_open_close,error
 use vec_operate,    ONLY:c2a,v_norm
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 implicit none
 !
 type(bz_samp), intent(in) ::Xk
 type(levels),  intent(in) ::Xen
 !
 ! Work Space
 !
 type(bz_samp)         :: USER_K,CIRCUIT_K
 real(SP)              :: real_ctl,v(3),max_dist
 real(SP), allocatable :: distances(:),values(:),bands(:,:)
 integer               :: i1,i2,ic
 integer,  allocatable :: int_distances(:)
 character(schlen)     :: bands_f_name
 character(schlen), allocatable :: headings(:)
 !
 if(BANDS_steps<=0)  call error("Set BANDS_steps > 0")
 !
 !Input file parsing
 !
 real_ctl=0._SP
 call bz_samp_reset(USER_K)
 call bz_samp_reset(CIRCUIT_K)
 !
 USER_K%nibz=1
 !
 kgrid_main_loop: do while(real_ctl/=999._SP)
   if (associated(USER_K%pt)) deallocate(USER_K%pt)
     allocate(USER_K%pt(USER_K%nibz,3))
     USER_K%pt(USER_K%nibz,:)=(/0._SP,0._SP,999._SP/)
     call parser('BKpts',USER_K%pt)
     real_ctl=USER_K%pt(USER_K%nibz,3)
     if (real_ctl/=999._SP)  USER_K%nibz=USER_K%nibz+1
 enddo kgrid_main_loop
 !
 USER_K%nibz=USER_K%nibz-1
 !
 do i1=1,USER_K%nibz
   !      
   v=USER_K%pt(i1,:)
   if (trim(coo_in)=="cc")  call c2a(v_in=USER_K%pt(i1,:),v_out=v,mode="kc2a")
   if (trim(coo_in)=="iku") call c2a(v_in=USER_K%pt(i1,:),v_out=v,mode="ki2a")
   USER_K%pt(i1,:)=v
   !
 enddo
 !
 USER_K%units="a"
 !
 if(USER_K%nibz==0) return
 !
 if(any(BANDS_range<=0)) call error("Wrong bands range")
 !
 allocate(distances(USER_K%nibz-1),int_distances(USER_K%nibz-1))
 !
 call msg("s",'Number of K-points in the circuit :',USER_K%nibz)
 !
 do i1=1,USER_K%nibz-1
   v=USER_K%pt(i1,:)-USER_K%pt(i1+1,:)
   distances(i1)=v_norm(v)
 enddo
 !
 max_dist=maxval(distances)
 !
 do i1=1,USER_K%nibz-1
   int_distances(i1)=maxval((/nint(BANDS_steps*distances(i1)/max_dist),1/))
 enddo
 !
 CIRCUIT_K%nibz=sum(int_distances)
 allocate(CIRCUIT_K%pt(CIRCUIT_K%nibz,3))
 !
 ic=1
 do i1=1,USER_K%nibz-1
   v=(USER_K%pt(i1+1,:)-USER_K%pt(i1,:))/int_distances(i1)
   do i2=1,int_distances(i1)
     CIRCUIT_K%pt(ic,:)=USER_K%pt(i1,:)+(i2-1)*v
     ic=ic+1
   enddo
 enddo
 !
 deallocate(distances,int_distances)
 !
 do i1=1,Xk%nibz
   v=Xk%pt(i1,:)
   call c2a(v_in=Xk%pt(i1,:),v_out=v,mode="ki2a")
   Xk%pt(i1,:)=v
 enddo
 !
 call bz_interp_setup(Xk)
 !
 call bz_interp_coeff(Xk,Xen,BANDS_range(1),BANDS_range(2))
 !
 do i1=1,Xk%nibz
   v=Xk%pt(i1,:)
   call c2a(v_in=Xk%pt(i1,:),v_out=v,mode="ka2i")
   Xk%pt(i1,:)=v
 enddo
 !
 allocate(values(3+BANDS_range(2)-BANDS_range(1)+1),headings(3+BANDS_range(2)-BANDS_range(1)+1))
 allocate(bands(BANDS_range(1):BANDS_range(2),CIRCUIT_k%nibz))
 !
 call bz_interpolation(CIRCUIT_k,bands,BANDS_range(1),BANDS_range(2))
 !
 bands_f_name="bands"
 call of_open_close(bands_f_name,'ot')
 !
 headings(1:3)=(/" kx  "," ky  "," kz  "/)
 do i1=BANDS_range(1),BANDS_range(2)
   headings(3+i1-BANDS_range(1)+1)=' b'//intc(i1)
 enddo
 !
 call msg('o bands',"#",headings,INDENT=0,USE_TABS=.true.)
 call msg('o path',"#"," ",INDENT=0)
 !
 do i1=1,CIRCUIT_K%nibz
   !
   values(1:3)=CIRCUIT_K%pt(i1,:)
   values(3+1:3+BANDS_range(2)-BANDS_range(1)+1)= & 
&    bands(BANDS_range(1):BANDS_range(2),i1)*HA2EV
   call msg('o bands',"",values,INDENT=0,USE_TABS=.true.)
   !
 enddo
 call of_open_close(bands_f_name)
 !
end subroutine
