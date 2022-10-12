!
 integer function io_DIPOLES_cut(Dip, ID)
!======================================
 use pars
 use units,            ONLY : HA2EV
 use DIPOLES,          ONLY:DIPOLE_t,DIP_desc
 use parallel_m,       ONLY:PAR_DIPk_nibz,PAR_K_scheme
 use electrons,        ONLY : levels, n_spinor, n_spin, n_sp_pol
 use R_lattice,        ONLY : nXkibz
 use matrix_operate,   ONLY : mat_c2r,mat_r2c
 use IO_int,           ONLY : io_connect, io_disconnect, io_elemental,&
&                             io_bulk, io_header,io_fragment
 use IO_int,           ONLY : io_variable_elemental,def_variable_elemental
 use IO_m,             ONLY : io_sec, io_status, read_is_on, write_is_on,io_extension,frag_DIP
 use descriptors,      ONLY:IO_desc_reset
 use optcut,           ONLY : z0f, dcf, DIP_iR_cut, DIP_alloc_cut
 use timing_m,            ONLY:timing
#include<memory.h>
 !implicit none
 type(DIPOLE_t)            :: Dip
 type(levels)              :: Xen
 integer                   :: ID
!ws
 integer                   :: i1,ixyz,sec_size, i_spin, ID_frag
 character(schlen)         :: VAR_name, lch, tmp_VAR, tmp_xyz
 real(SP), allocatable     :: DIP_iR_cut_disk(:,:,:)

 call timing('io_Dipoles_cut',OPR='start')

 PAR_K_scheme%nibz=PAR_DIPk_nibz
 io_DIPOLES_cut = io_connect(desc='dipoles_cut',type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_DIP)
 if (io_DIPOLES_cut/=0) goto 1
 
 sec_size=12
 if (any((/io_sec(ID,:)==1/))) then

   io_DIPOLES_cut=io_header(ID,R_LATT=.true.,IMPOSE_SN=.true.,FRAG=frag_DIP)
   if (io_DIPOLES_cut/=0) goto 1
    
    call IO_desc_reset(DIP_desc)
    call def_variable_elemental(ID,"PARS",sec_size,SP,0)
    call io_variable_elemental(ID,DB_I1=Dip%db_ib,VAR="DIP band range",I1=Dip%ib,CHECK=.true.,OP=(/">=","<="/),&
   &                           DESCRIPTOR=DIP_desc)
    call io_variable_elemental(ID,DB_I1=Dip%db_ib_lim,VAR="DIP band range limits",I1=Dip%ib_lim,CHECK=.true.,OP=(/"<=",">="/),&
   &                           DESCRIPTOR=DIP_desc)
    call io_variable_elemental(ID,UNIT=HA2EV,&
   &     VAR="DIP e/h energy range ",TERMINATOR="[eV]",R1=Dip%ehe,CHECK=.true.,OP=(/">=","<="/),DESCRIPTOR=DIP_desc)
    call io_variable_elemental(ID,VAR="RL vectors in the sum",I0=Dip%ng,WARN=.true.,OP=(/"<="/),&
   &                           DESCRIPTOR=DIP_desc)
    call io_variable_elemental(ID,VAR="[r,Vnl] included",L0=Dip%Vnl_included,CHECK=.true.,OP=(/"=="/),&
   &                           DESCRIPTOR=DIP_desc)
    call io_variable_elemental(ID,VAR="Bands ordered",L0=Dip%bands_ordered,CHECK=.true.,OP=(/">="/),&
   &                           DESCRIPTOR=DIP_desc)
    call io_variable_elemental(ID,VAR="Direct v evaluation",L0=Dip%force_v_g_space,CHECK=.true.,OP=(/"=="/),&
   &                           DESCRIPTOR=DIP_desc)
   call io_elemental(ID,&
&       VAR=" Cutoff zero                                      :",R0=z0f,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=DIP_desc)
   call io_elemental(ID,&
&       VAR=" Cutoff width                                     :",R0=dcf,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=DIP_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0)
   io_DIPOLES_cut=io_status(ID)
   if (io_DIPOLES_cut/=0) goto 1
 endif
!
! On disk the size is DIP_iR_or_P(3,db_nb(2),db_nbm,nXkibz)
!
  sec_size=3*Dip%db_ib(2)*maxval(Dip%db_ib_lim)
 
    YAMBO_ALLOC(DIP_iR_cut_disk,(Dip%db_ib(2),maxval(Dip%db_ib_lim),2))
 
    if (read_is_on(ID)) then
      call DIP_alloc_cut('DIP_iR_cut',(/3,Dip%ib_lim(2),Dip%ib(2),Dip%ib(1),Dip%ib_lim(1),PAR_DIPk_nibz/))
    endif
   
    do i1=1,nXkibz
      do ixyz=1,3
        do i_spin = 1, n_sp_pol
          if (write_is_on(ID)) call mat_c2r(DIP_iR_cut(ixyz,:,:,i1,i_spin),DIP_iR_cut_disk)
 
          write (VAR_name,'(3(a,i4.4))') 'DIP_iR_cut_k_',i1,'_xyz_',ixyz,'_spin_',i_spin
 
          call io_bulk(ID,VAR=trim(VAR_name),VAR_SZ=shape(DIP_iR_cut_disk))
          call io_bulk(ID,R3=DIP_iR_cut_disk)

          if (read_is_on(ID)) call mat_r2c(DIP_iR_cut_disk,DIP_iR_cut(ixyz,:,:,i1,i_spin))
 
        enddo
      enddo
    enddo
 
   YAMBO_FREE(DIP_iR_cut_disk)

1 call io_disconnect(ID=ID)

  call timing('io_Dipoles_cut',OPR='stop')
 
 end function
