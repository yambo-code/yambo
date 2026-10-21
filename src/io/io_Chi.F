!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
integer function io_Chi(X,Xw,ID)
 !
 ! ndb.Chi: chi0 (Kohn-Sham), P (quasiparticle) and fxc=chi0^-1-P^-1 in the plane-wave
 ! basis. Header in ndb.Chi, per-q data in ndb.Chi_fragment_<iq> when the response
 ! databases are fragmented. See mod_Chi for the conventions.
 !
 ! Sections:  1        -> header, descriptors, G-vectors and |q+G|
 !            2*iq     -> per-q descriptors
 !            2*iq+1   -> per-q matrices
 !
 use pars,          ONLY:SP,schlen
 use units,         ONLY:HA2EV,HA2KEL
 use R_lattice,     ONLY:g_vec,bare_qpg,nqibz
 use X_m,           ONLY:X_t
 use frequency,     ONLY:w_samp
 use Chi_m,         ONLY:Chi_desc,Chi_ng,Chi_n_freqs,Chi_alloc,Chi_freqs,Chi_0,Chi_P,Chi_fxc,&
&                        Chi_QP_applied,Chi_QP_action,Chi_convention
 use Chi_m,         ONLY:Chi_G_mode,Chi_G_db,Chi_G_norm_tol,Chi_G_check_tol,Chi_rcond_floor,&
&                        Chi_G_mu,Chi_G_kbt,Chi_G0_error,Chi_rcond
 use Chi_m,         ONLY:Chi_G_axis,Chi_optical_q0,Chi_optical_q0_known
 use Chi_m,         ONLY:Chi_occupations,Chi_occ_bands,Chi_occ_nk,Chi_occ_nsp,Chi_occ_nbf,Chi_occ_nbm,&
&                        Chi_occ_Tel,Chi_occ_alloc,Chi_occ_free
 use IO_int,        ONLY:io_connect,io_disconnect,io_elemental,io_header,io_fragment,&
&                        def_variable_bulk,io_variable_bulk,def_variable_elemental,io_variable_elemental
 use IO_m,          ONLY:io_sec,io_com,io_status,write_is_on,read_is_on,NONE,frag_RESPONSE,&
&                        IO_NO_DATABASE,io_mode,DUMP,variable_exist
 use descriptors,   ONLY:IO_desc_reset
 use stderr,        ONLY:intc
 use timing_m,      ONLY:timing
 !
#include<y_memory.h>
 !
 type(X_t)   :: X
 type(w_samp):: Xw
 integer     :: ID
 !
 ! Work Space
 !
 integer                 :: i1,iq,io_com_save,io_err,ID_frag,ng_qpg,tmp_ib(2)
 character(schlen)       :: ch,axis
 real(SP),   allocatable :: RL_vecs_disk(:,:)
 complex(SP),allocatable :: qpg_disk(:,:)
 !
 call timing('io_Chi',OPR='start')
 !
 ID_frag=0
 io_err =0
 !
 iq=0
 do i1=1,size(io_sec(ID,:))
   if (io_sec(ID,i1)==0) cycle
   iq=io_sec(ID,i1)/2
 enddo
 !
 io_Chi=io_connect(desc="Chi",type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_RESPONSE)
 if (io_Chi/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_Chi=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.false.,CUTOFF=.true.,FRAG=frag_RESPONSE)
   if (io_Chi/=0) goto 1
   !
   call IO_desc_reset(Chi_desc)
   tmp_ib=X%ib
   !
   ! CHI_PARS_1: ng(1) + n_freqs(1) + bands(2) + QP applied(1) = 5 slots
   !
   call io_elemental(ID,VAR="CHI_PARS_1",VAR_SZ=5,MENU=0)
   call io_elemental(ID,I0=Chi_ng,&
&       VAR="Matrix size                     ",CHECK=.true.,OP=(/"<="/),DESCRIPTOR=Chi_desc)
   call io_elemental(ID,I0=Chi_n_freqs,&
&       VAR="Number of frequencies           ",CHECK=.true.,OP=(/"=="/),DESCRIPTOR=Chi_desc)
   call io_elemental(ID,I1=tmp_ib,&
&       VAR="Bands range                     ",CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=Chi_desc)
   call io_elemental(ID,L0=Chi_QP_applied,&
&       VAR="QP corrections applied to P     ",WARN=.true.,OP=(/"=="/),DESCRIPTOR=Chi_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="CHI_QP_ACTION",CH0='',VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=Chi_QP_action,&
&       VAR="QP action (XfnQPdb)             ",DESCRIPTOR=Chi_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="CHI_CONVENTION",CH0='',VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=Chi_convention,&
&       VAR="Convention                      ",DESCRIPTOR=Chi_desc)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   if (read_is_on(ID)) then
     Chi_G_mode='LEVELS'
     Chi_G_db=''
   endif
   if (write_is_on(ID).or.variable_exist(ID,'CHI_G_MODE')) then
     call io_elemental(ID,VAR="CHI_G_MODE",CH0='',VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=Chi_G_mode)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   axis=Chi_G_axis
   if (trim(Chi_G_mode)=='LEVELS') axis='NATIVE'
   if (write_is_on(ID).or.variable_exist(ID,'CHI_G_AXIS')) then
     call io_elemental(ID,VAR="CHI_G_AXIS",CH0='',VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=axis)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     if (read_is_on(ID).and.trim(Chi_G_mode)/='LEVELS') Chi_G_axis=axis
   endif
   if (write_is_on(ID).or.variable_exist(ID,'CHI_G_DB')) then
     call io_elemental(ID,VAR="CHI_G_DB",CH0='',VAR_SZ=1,MENU=0)
     call io_elemental(ID,CH0=Chi_G_db)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   if (write_is_on(ID).or.variable_exist(ID,'CHI_G_PARAMETERS')) then
     call io_elemental(ID,VAR="CHI_G_PARAMETERS",VAR_SZ=5,MENU=0)
     call io_elemental(ID,R0=Chi_G_norm_tol)
     call io_elemental(ID,R0=Chi_G_check_tol)
     call io_elemental(ID,R0=Chi_rcond_floor)
     call io_elemental(ID,R0=Chi_G_mu)
     call io_elemental(ID,R0=Chi_G_kbt)
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   ! The numerical |q0| fixes the normalization of optical head/wings.
   ! Older finite-q databases remain readable without optical metadata.
   if (read_is_on(ID)) then
     Chi_optical_q0=0._SP
     Chi_optical_q0_known=.FALSE.
   endif
   if (write_is_on(ID).or.variable_exist(ID,'CHI_OPTICAL_Q0')) then
     call io_elemental(ID,VAR='CHI_OPTICAL_Q0',VAR_SZ=3,MENU=0)
     call io_elemental(ID,R1=Chi_optical_q0,&
&         VAR='Optical q0 direction and norm    ',DESCRIPTOR=Chi_desc)
     call io_elemental(ID,VAR='',VAR_SZ=0,MENU=0)
     Chi_optical_q0_known=.TRUE.
   endif
   ! Occupations of the BSE levels behind a BSEChiOut export (array after |q+G|).
   ! Databases without them remain readable and are not checked.
   if (read_is_on(ID)) call Chi_occ_free(.TRUE.)
   if ((write_is_on(ID).and.allocated(Chi_occupations)).or.&
&      (read_is_on(ID).and.variable_exist(ID,'CHI_OCC_PARS'))) then
     call io_elemental(ID,VAR='CHI_OCC_PARS',VAR_SZ=7,MENU=0)
     call io_elemental(ID,I1=Chi_occ_bands,VAR='Occupations: bands               ',DESCRIPTOR=Chi_desc)
     call io_elemental(ID,I0=Chi_occ_nk)
     call io_elemental(ID,I0=Chi_occ_nsp)
     call io_elemental(ID,I0=Chi_occ_nbf,VAR='Occupations: full bands         ',DESCRIPTOR=Chi_desc)
     call io_elemental(ID,I0=Chi_occ_nbm,VAR='Occupations: last occupied band ',DESCRIPTOR=Chi_desc)
     call io_elemental(ID,R0=Chi_occ_Tel,UNIT=HA2KEL,&
&         VAR='Occupations: temperature',TERMINATOR='K',DESCRIPTOR=Chi_desc)
     call io_elemental(ID,VAR='',VAR_SZ=0,MENU=0)
   endif
   !
   io_Chi=io_status(ID)
   if (io_Chi/=0) goto 1
   !
   ! G-vectors, in the ordering used for the matrices
   !
   YAMBO_ALLOC(RL_vecs_disk,(Chi_ng,3))
   if (write_is_on(ID)) RL_vecs_disk(:,:)=g_vec(:Chi_ng,:)
   call def_variable_bulk(ID,'CHI_RL_vecs',2,(/Chi_ng,3/),SP)
   call  io_variable_bulk(ID,2,R2=RL_vecs_disk)
   if (read_is_on(ID)) then
     if (Chi_ng<1.or.Chi_ng>size(g_vec,1)) call error('[Chi] invalid stored G-basis size')
     if (maxval(abs(RL_vecs_disk-g_vec(:Chi_ng,:)))>1.E-5_SP) &
&     call error('[Chi] stored G vectors differ from the current reciprocal basis')
   endif
   YAMBO_FREE(RL_vecs_disk)
   !
   ! |q+G| as used in the Coulomb potential (cutoff included when active)
   !
   YAMBO_ALLOC(qpg_disk,(nqibz,Chi_ng))
   qpg_disk=(0._SP,0._SP)
   if (write_is_on(ID)) then
     ng_qpg=min(Chi_ng,size(bare_qpg,2))
     qpg_disk(:,:ng_qpg)=bare_qpg(:nqibz,:ng_qpg)
   endif
   call def_variable_bulk(ID,'CHI_QPG',3,(/2,nqibz,Chi_ng/),SP)
   call  io_variable_bulk(ID,3,C2=qpg_disk)
   if (read_is_on(ID)) then
     if (Chi_ng>size(bare_qpg,2)) call error('[Chi] stored |q+G| basis exceeds the current Coulomb basis')
     if (maxval(abs(qpg_disk-bare_qpg(:nqibz,:Chi_ng))/&
&        max(abs(qpg_disk),1.E-10_SP))>5.E-4_SP) &
&        call error('[Chi] stored |q+G| differs from the current Coulomb normalization/cutoff')
   endif
   YAMBO_FREE(qpg_disk)
   !
   if (Chi_occ_bands(1)>0) then
     if (read_is_on(ID)) call Chi_occ_alloc()
     call def_variable_bulk(ID,'CHI_OCCUPATIONS',4,(/Chi_occ_bands(2)-Chi_occ_bands(1)+1,Chi_occ_nk,Chi_occ_nsp/),SP)
     call  io_variable_bulk(ID,4,R3=Chi_occupations)
   endif
   !
   if (.not.any((/io_sec(ID,:)==2/))) then
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     io_Chi=io_status(ID)
     if (io_Chi/=0) goto 1
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
   !
 endif
 !
 if (iq==0) goto 1
 !
 call io_fragment(ID,ID_frag,i_fragment=iq,ierr=io_err)
 io_Chi=io_err
 if (io_err/=0) goto 1
 !
 if (any((/io_sec(ID,:)==2*iq/))) then
   !
   ch="CHI_PARS_sec_iq"//trim(intc(iq))
   !
   io_com_save=io_com(ID_frag)
   if (iq>2) io_com(ID_frag)=NONE
   !
   ! 6 slots: iq, n_freqs, energy range(2), damping range(2)
   !
   call def_variable_elemental(ID_frag,trim(ch),6,SP,1)
   call io_variable_elemental(ID_frag,VAR="Current Q-pt index      ",I0=iq,CHECK=.true.,OP=(/"=="/))
   call io_variable_elemental(ID_frag,VAR="Number of frequencies   ",I0=Chi_n_freqs,CHECK=.true.,OP=(/"=="/))
   call io_variable_elemental(ID_frag,UNIT=HA2EV,VAR="X energy range ",TERMINATOR="eV",R1=Xw%er,WARN=.true.,OP=(/"==","=="/))
   call io_variable_elemental(ID_frag,UNIT=HA2EV,VAR="X damping range",TERMINATOR="eV",R1=Xw%dr,WARN=.true.,OP=(/"==","=="/))
   call def_variable_elemental(ID_frag,"",0,0,1)
   io_Chi=io_status(ID_frag)
   !
   io_com(ID_frag)=io_com_save
   !
   if (io_Chi/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(ID,:)==2*iq+1/))) then
   !
   if (read_is_on(ID)) call Chi_alloc(Chi_ng,Chi_n_freqs)
   !
   ch="CHI_FREQ_Q_"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(ch),1,(/2,Chi_n_freqs/),SP)
   call  io_variable_bulk(ID_frag,1,C1=Chi_freqs)
   !
   ch="CHI0_Q_"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(ch),2,(/2,Chi_ng,Chi_ng,Chi_n_freqs/),SP)
   call  io_variable_bulk(ID_frag,2,C3=Chi_0)
   !
   ch="P_Q_"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(ch),3,(/2,Chi_ng,Chi_ng,Chi_n_freqs/),SP)
   call  io_variable_bulk(ID_frag,3,C3=Chi_P)
   !
   ch="FXC_Q_"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(ch),4,(/2,Chi_ng,Chi_ng,Chi_n_freqs/),SP)
   call  io_variable_bulk(ID_frag,4,C3=Chi_fxc)
   ch="CHI_RCOND_Q_"//trim(intc(iq))
   if (read_is_on(ID)) Chi_rcond=0._SP
   if (write_is_on(ID_frag).or.variable_exist(ID_frag,trim(ch))) then
     call def_variable_bulk(ID_frag,trim(ch),5,(/2,Chi_n_freqs/),SP)
     call io_variable_bulk(ID_frag,5,R2=Chi_rcond)
   endif
   ch="CHI_G0_ERROR_Q_"//trim(intc(iq))
   if (read_is_on(ID)) Chi_G0_error=-1._SP
   if (write_is_on(ID_frag).or.variable_exist(ID_frag,trim(ch))) then
     call def_variable_bulk(ID_frag,trim(ch),6,(/1/),SP)
     call io_variable_bulk(ID_frag,6,R0=Chi_G0_error)
   endif
   !
 endif
 !
 if (io_mode(ID)/=DUMP) then
   io_Chi=IO_NO_DATABASE
   if (io_err==0) io_Chi=iq
 endif
 !
1 call io_fragment_disconnect(ID,ID_frag)
 call io_disconnect(ID)
 !
 call timing('io_Chi',OPR='stop')
 !
end function
