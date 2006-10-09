c
      program main
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Permission to use and copy this software and its documentation is hereby          c
c  granted without fee. If you use the program please cite the  article              c
c  "M. Bockstedte, A. Kley, and M. Scheffler, Comp. Phys. Comm. to be  published".   c
c  Comments and suggestions are greatly welcome. In particular we are                c
c  very interested in knowing for which applications and projects the program has    c
c  been used. If you modify the code to optimize it on other platforms or by         c
c  implementing new algorithms please send a note to:                                c 
c                                                                                    c
c          fhimd@fhi-berlin.mpg.de                                                   c
c                                                                                    c
c This enables us to make these improvements available to everyone.                  c
c For more information see the page                                                  c
c                                                                                    c
c         http://www.rz-berlin.mpg.de/th/fhi96md.html                                c
c                                                                                    c 
c on the world wide web.                                                             c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c										     c
c fhi96spin:									     c
c This revised version allows to treat spin-polarized systems using a perturbation   c
c approach. There is only one set of wave-functions, and the energy of		     c
c the spin-up and spin-down states is derived from first order perturbation 	     c
c theory.									     c
c										     c
c Version 1, August 17, 1997, changed by E. Pehlke, TU Muenchen.		     c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



      implicit none
      include 'parameter.h'
      include 'common1.h'
      include 'common2.h'

c local variables here --->

      integer iprint, nstepe, ibrav, ngww, ik, i, ii, j,i1,i2,i3,ind
      integer is, ia, istepe, n_it, nfi, nfi_old, i_low_delt, ig
      integer istep1, istepmax, nomor1
      integer idyn,i_edyn, i_xc, n_it_gga
      integer nOrder,npos,nthm,nfi_rescale, max_no_force, istep_no_gga

      integer      i_lm
      integer      lm_end
      real*8       ampre
      real*8       amprp
      real*8       ekin_ion
      real*8       delta_ion
      real*8       T_ion
      real*8       Q
      real*8       time
      real*8       timequeue
      real*8       vpot
      real*8       vpot0
      real*8       vpot_free
      real*8       vdg(ngwx)
      real*8       dsum
      real*8       facc
      real*8       par
      real*8       ttt
      real*8       ts
      real*8       ezero
      real*8       force_eps(2)
      real*8       delt1
      real*8       delt2
      real*8       delt
      real*8       delt_norm
      logical      t_postc
      logical      t_grad
      logical      t_chg_dlt
      logical      t_sw_fnct
      logical      tfion
      logical      t_last_loop
      logical      tprint
      logical      trane
      logical      tranp
      logical      tsdp
      logical      ttfor

      logical      tdipol

      logical      tfor
      logical      tdyn
      common/t_for/ tfor,
     &             tdyn
      logical      tford(nax,nsx)
      common/ford/ tford

      real*8       mean_force
      real*8       mean_rel_change

      real*8       vloc0
      real*8       vloc(nr1x+1,nr2x,nr3x)
      real*8       vl(nr1x+1,nr2x,nr3x)

      real*8       emass
      parameter   (emass=33.333)

      real*4       time1
      real*4       timeinit
      real*4       timestart
      real*4       cputime
      external     cputime

      real*4       v_rand(ngwx*2)
      complex*16   c2(ngwx)
      complex*16   speed
      complex*16   c_eig
      real*8       rhoe(nr1x+1,nr2x,nr3x)
      real*8       a1(3)
      real*8       a2(3)
      real*8       a3(3)

      real*8       ekt
      real*8       efermi
      real*8       seq
      real*8       sneq
      logical      tmetal
      common/ferm/ ekt,
     &             efermi,
     &             seq,
     &             sneq,
     &             tmetal

      real*8       fion(3,nax,nsx)
      common/forc/ fion
      real*8       fion_nl(3,nax,nsx)
      logical      tfion_nl
      common/forc_nl/ fion_nl,
     &             tfion_nl

      real*8       ecuti
      real*8       ecut
      integer      pgind
      common/u71/  ecuti,
     &             ecut,
     &             pgind

      character*10 atom(nsx)
      common/u72/  atom

c  ion_fac control

      real*8       tmp_fac(nsx)


      complex*8    c_fft_store(nr1x+1,nr2x,nr3x,n_fft_store)
      common/store/ c_fft_store

c

c   schultze type of algorithm
      
      real*8       dt
      real*8       dts
      real*8       dtbye
      real*8       e1
      real*8       ecos1(ngwx)
      real*8       omega2
      complex*8    c0_old(((ngwx-1)*nschltz)+1,
     &             ((nspin+1)*nx-1)*nschltz+1,nkptx)
      complex*16   c0_temp
      complex*16   beta((ngwx-1)*nschltz+1)
      real*8       alpha(ngwx)
      real*8       gamma
      real*8       gamma1
      real*8       gamma2
      real*8       gamma_norm

c      


c the following variables are for time-step and convergency control

      logical      t_el_conv
      logical      t_force_conv
      logical      t_ekinc_conv
      logical      t_eig_conv

      real*8       epsel
      real*8       epsfor
      real*8       epsekinc
      common/eps_conv/ epsel,
     &             epsfor,
     &             epsekinc

      real*8       eps_chg_dlt
      real*8       eps_sw_fnct

      real*8       etot_h

      real*8       etot_m
      real*8       mm_etot
      real*8       mmm_etot

      real*8       move_etot
      real*8       m_move_etot

      real*8       ekinc
      real*8       ekinc_m
      real*8       mm_ekinc

      real*8       force_change_m
      real*8       mm_force_change
      real*8       mmm_force_change


c mixing factor for charge density mixing in rhoofr.f

      real*8       rho_mix_fac
      real*8       stand_rho_mix_fac
      parameter    (stand_rho_mix_fac=0.0)
      common/mix_rho/ rho_mix_fac
c
c  matrices for MATHEMATICA output  (POTential, RHO)
      real*8       v_pot(nr3x,6)
      real*8       v_left
      real*8       v_right
      real*8       e_left
      real*8       e_right
      integer      imin
      common/c_pot/ v_pot,
     &             v_left,
     &             v_right,
     &             e_left,
     &             e_right,
     &             imin
c

      real*8       wkptu(nkptx)
      integer      ngwold(nkptx)
      integer      igkold(ngwx,nkptx)
      integer      nbeg
      integer      nrho
      integer      nkpu
      logical      tband
      common/band/ wkptu,
     &             nrho,
     &             nkpu,
     &             ngwold,
     &             igkold,
     &             tband,
     &             nbeg
c
      real*8       au
      parameter    (au=27.2116)
c
      integer      nomore_init
      integer      init_basis

      integer      nozeta
      real*8       eigav
      real*8       qp
      real*8	   zeta(nr1x+1,nr2x,nr3x)
      real*8       zeta_dummy(nr1x+1,nr2x,nr3x)

      real*8       ti_rhoofr
      real*8       ti_dforce
      real*8       ti_graham
      real*8       ti_vofrho
      real*8       nlrh_t
      integer*4    iseed
      real*4       time_zero      

ctmp
      real*4       tcaxpy,tcdotc,tcnorm

      data         tcaxpy/0./
      data         tcnorm/0./
      data         tcdotc/0./
ctmp
      data         ti_rhoofr/0./
      data         ti_dforce/0./
      data         ti_graham/0./
      data         ti_vofrho/0./
      data         nlrh_t/0./
      data         iseed/1234/
      data         time_zero/0.0/




c=======================================================================
c     start of program 
c=======================================================================


c set starting time of program
      timestart=cputime(time_zero)


c=======================================================================     
c     open main input and output files
c=======================================================================

      open( 6, file='fort.6')              
      open(87, file='fort.87')              
      close(87)


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     get and set initial values
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cpf   magnetization sums in positive/negative (z-)direction initially set to zero
      rsumup=0.0D0
      rsumdn=0.0D0

cpf   eigenvalues per k-point of Electron- Kohn-Sham- Orbitals initially set to zero
      do ii=1,(nspin_pt+1)*nx
        do ik=1,nkptx
          eig(ii,ik)=0.0D0
        enddo
      enddo

cpf   ?
      do ii=1,nr1x*nr2x*nr3x
        dvxc_vec(ii)=0.0
      enddo

c-----------------------------------------------------------------------
cpf   read in run toggles from standard input fort.5
c-----------------------------------------------------------------------
      call header(nbeg, nomore, iprint, delt1, delt2, gamma1, 
     &     gamma2,delta_ion, idyn, nOrder, max_no_force, force_eps, 
     &     nstepe,i_edyn, i_xc, t_postc,  trane, ampre, tranp, amprp, 
     &     tfor,tdyn, tsdp, tdipol, timequeue, nomore_init, init_basis,
     &     tspin, nozeta,istep_no_gga, eps_chg_dlt,eps_sw_fnct)


cpf   at least one iteration without updating initial spin polarization
      if (nozeta.lt.1) nozeta=1

cpf   hand over read electronic time step size and corresponding damping value
cpf   (delt1 and gamma1 are intended to stay original values)
      delt=delt1
      gamma=gamma1

cpf   hand over factor of mixing charge densities in rhoofr
cpf   (stand_rho_mix_fac intended to stay original value)
      rho_mix_fac = stand_rho_mix_fac

cpf   tell rho_mix_fac-value
      if (rho_mix_fac .eq. 0.0) then
        write(6,*) 'normally no mixing of old charge is done'
      else
        write(6,'(''>>>main sets rho_mix_fac to '',f6.3)') rho_mix_fac
      endif

cpf   write out all present standardout messages
      call flush(6)


c-----------------------------------------------------------------------
c     t_grad indicates whether actually to include core charge gradients
c     into the exchange correlation functional
c-----------------------------------------------------------------------
      t_grad=.false.

cpf   redundant toggles, one of them sufficient
      if (.not. t_postc .and. (i_xc .gt. 0)) then
        t_grad=.true.
      endif

cpf   initialize maximum number of LDA-steps in case of following GGA-calc.
      n_it_gga=0

c=======================================================================
c
c     initialization of wave functions  (reading in system, positioning atoms etc...)
c
c=======================================================================


      time1=cputime(time_zero)

      call init(ibrav, a1, a2, a3, tranp, amprp, tdyn,
     &     tdipol, npos, nthm, T_ion, nfi_rescale, Q,
     &     nomore_init, init_basis, i_xc, t_grad, zeta)

      time1=cputime(time1)

      write(6,'(''time elapsed for init:'',f12.4)') time1
      call flush(6)

cpf   determine total number of electron basis vectors? 
      ngww = 0
      do ik=1,nkpt
         ngww = ngww + ngw(ik)
      enddo
      write(6,'(''total number of wave function components:'',i6)')ngww


c=======================================================================
c
c     randomization of wave functions, proportional to wave
c
c=======================================================================

cpf   enlarge/diminish components of electronic wave function at random
      if (trane) then
        do ik=1,nkpt
          do i=1,n
cpf         get random value v_rand (maximum value=?)
            call ranv(iseed,ngw(ik)*2,v_rand)
            do j=1,ngw(ik)
cpf           enlarge/diminish both real and imaginary part to the same extent
              c0(j,i,ik) = cmplx(
     &             real( c0(j,i,ik))* (1. + ampre *(v_rand(2*j-1)-0.5)),
     &             aimag(c0(j,i,ik))* (1. + ampre *(v_rand(2*j-1)-0.5)))
            enddo
          enddo
cpf       orthogonalize (per k-point) resulting wave functions
ctmp          call graham(ik,n,ngw(ik),.true.)
          call graham(ik,n,ngw(ik),.true.,tcaxpy,tcnorm,tcdotc)
        enddo
      endif
cpf   end of randomization

cpf   (reminder: atomic positions were randomized init)

c
cpf   no forces to act on atoms if intended to be fixed
      if (.not.(tfor.or.tdyn)) then
        do ii=1,3
          do is=1,nsp
            do ia=1,na(is)
              fion(ii,ia,is)=0.0
            enddo
          enddo
        enddo
      endif
c
cpf   starting time of all main calculations
      timeinit=cputime(timestart)

cpf   next iteration not yet last one
      t_last_loop=.false.
c


c-----------------------------------------------------------------------
c (old)
c     loop control (?!)
c       tfion and controls if forces should be computed.
c       ttfor also controls whether
c       ions should be moved. Both are normally set in fionsc.
c     For runs with ionic moves i.e. tfor=true istepe is set to 1 for the
c       iteration after the ionic move occured. So routines which depend
c       on changed coordinates as strucf, phfac, forcs and ewald should
c       be called when istepe=1 (<- why not later as well ?!)
c-----------------------------------------------------------------------

      istepe = 0
      n_it = 0
      ttfor = .false.
      if (tfor.or.tdyn) tfion = .true.

c     ATTENTION: old nfi should be read in from old wave (<-?! )
c     (but being set in following lines?!)

cpf   use of nfi? (Number of Force Iterations?)
      if (nbeg.ne.-1) then
        nfi = 1
      else
        nfi = 0
      endif

cpf   number of electron self consistency calculations increased by one
cpf   after initial self consistency calculation on reduced basis set (?)

cpf   save nfi
      nfi_old = nfi


cpf -- start settings --- (moved outside self-consistency loop)

      i_low_delt = 0
cpf   normal time step 
      delt_norm = delt

      mean_force = 0.0
      mm_force_change = 0.0
      force_change_m = 0.0
      mean_rel_change = 0.0
      mm_etot  = 0.0
      etot_m   = 0.0
      ekinc_m  = 0.0
cpf   forces only to be calculated if any cores to move
      t_force_conv = .not. tfor

      t_eig_conv   = .not. tmetal
cpf   electrons, kinetic energy not yet converged
      t_el_conv    = .false.
      t_ekinc_conv = .false.
cpf   whether to change delt
      t_chg_dlt    = .false.
cpf   whether to change exchange correlation functional
      t_sw_fnct    = .false.
cpf ---




cpf   start of electronic self-consistency loops
      write(6,*)' '
      write(6,*)'======================================================'
      write(6,*)'           ITERATIONS IN MAIN  STARTED'
      write(6,*)'======================================================'
      write(6,*)' '



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     begin of electronic self consistency loop 
c     (including relaxation or molecular dynamics)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ctmp
      tcaxpy=0.0
      tcnorm=0.0
      tcdotc=0.0
ctmp

c
 1000 continue
c

cpf   loop counting (total number of electron iterations, number of ion moves rsp.)
      n_it=n_it+1
      istepe=istepe+1

cpf   in case cores move :
      if (tfor.or.tdyn) then

cpf     convergence of ionic forces achieved and ions have been moved
        if (ttfor) then
          istepe=1
          if (tfor) nfi=nfi+1
          ttfor = .false.

        else if (nfi .ge. nomore+nfi_old .and. istepe .eq. 3) then
          t_last_loop = .true. 

        else if (istepe.ge.nstepe) then
cpf       stop further calculations          
          t_last_loop = .true.
          write(6,'(''ATTENTION: forces did not converge after'',i3,
     &         '' timesteps so I stop'')') nstepe
        endif

cpf   fixed cores:
      else
cpf     end calculations if (core?) iterations exceed given maximum number
        if (istepe .ge. nomore-1) t_last_loop = .true.

      endif


      

c-----------------------------------------------------------------------
c     the timestep  delt is reduced  by a factor of 0.6
c     for the first two steps in case of self-constructed initial wave functions
c     and ... ( i_edyn.ne.2 = ? ) (= except for particular propagation algorithm?!)
c  old comm.:   if etot or ekinc increases at a normal timestep (?)
c               i.e. not directly after ionic movement (?)
c-----------------------------------------------------------------------
      
      if (nbeg .eq.-1 .and. n_it .eq. 1 .and. i_edyn .ne. 2) 
     &     delt=0.6*delt
      if (nbeg .eq.-1 .and. n_it .eq. 3 .and. i_edyn .ne. 2) 
     &     delt=delt/0.6

cpf   extent of mixing old and new wave functions ('rho_mix_fac') charge densities
      if (((nbeg .eq.-1 .and. n_it .le. 7) .or. 
     &     (t_grad .and. (n_it-n_it_gga) .lt. 7)).and.i_edyn.ne.2) then
        rho_mix_fac = 0.95 - (n_it-n_it_gga-1) * 0.15
        write(6,'(a,f6.3)') 
     &     ">>>main mixes charge densities with rho_mix_fac=",
     &     rho_mix_fac
      endif
c     
      
cmb --- 

cpf   fix decreasing mixing of charge densities in case of dynamics
cpf   (rewritten 9.9.98, original at end of this file)
      if ( tdyn .and. (n_it.gt.8) .and. (rho_mix_fac.gt. 0.0) ) then

cpf     decreasing mixing during electron iterations of first core moves
        if ( istepe.lt.5 ) then
          rho_mix_fac = 0.95 - (istepe-1) * 0.1
          write(6,'(a,f6.3)') 
     &       ">>>main mixes charge densities with rho_mix_fac=",
     &       rho_mix_fac

cpf     end mixing after fourth core move 
        else if ( istepe.eq.5 ) then
          rho_mix_fac = 0.0
          write(6,*)">>>main stops mixing charge densities"

        endif

      endif

cmb ---

cpf   still necessary to set some 'previous' total energy?
      if (n_it .eq. 2) move_etot = etot

c     check if timestep should be changed
cpf adjusted to ?

      if (n_it .gt. 2) then

        if (i_low_delt .gt. 0) then

          if (i_low_delt .eq. 1) delt = delt_norm
          i_low_delt = i_low_delt - 1

        else if( ( etot.ge.etot_m  .and. .not. tband  ) .or.
     &           (ekinc.gt.ekinc_m .and. istepe .ne. 2) .or.
     &            force_change_m * mm_force_change * mmm_force_change 
     &            * mean_rel_change .ge. 0.2**4)
     &         then

          if (i_edyn.ne.2) then

            delt = 0.6 * delt
            i_low_delt = 3
            write(6,'(''>>>main reduces delt. from'', f6.2, '' to'',f6.2,
     &           '' for'', i3,'' steps'')') delt_norm, delt, i_low_delt

            if (force_change_m *mm_force_change * mmm_force_change
     &           * mean_rel_change .ge. 0.37**4) then
              rho_mix_fac = 0.3
              write(6,'(''>>>main mixes charge densities with 
     &             rho_mix_fac='', f6.3)') rho_mix_fac
            endif

          endif
        else

          if (n_it.gt.7 .and. rho_mix_fac.ne.stand_rho_mix_fac) then
            rho_mix_fac = stand_rho_mix_fac
            write(6,'(''>>>main sets rho_mix_fac to '',f6.3)') 
     &                                                   rho_mix_fac
          endif
        endif
      endif


c ---------------------------------------------------------------------

c     check convergency of system parts
c     (etot and ekinc) (from 5th iteration on (pf))
c     ---------------------------------------------------------------

      if (n_it .gt. 4) then

*ThL    Next statement modified because, even if condition is true once
*       in the run, it may become false again.  In this case, t_el_conv
*       is now reset to false.

         t_el_conv = 
     &        abs(mmm_etot-mm_etot).lt.epsel .and.
     &        abs(mm_etot -etot_m) .lt.epsel .and.
     &        abs(etot_m  -etot_h) .lt.epsel .and.
     &        (.not.tfor .or. ((m_move_etot - move_etot) .gt. 0. .and.
     &                         (m_move_etot - move_etot) .lt. 7*epsel) )

         t_chg_dlt = 
     &        abs(mmm_etot-mm_etot).lt.eps_chg_dlt .and.
     &        abs(mm_etot- etot_m) .lt.eps_chg_dlt .and.
     &        abs(etot_m - etot_h) .lt.eps_chg_dlt .and.
     &        (.not.tfor .or.
     &                ( (m_move_etot - move_etot) .gt. 0. .and.
     &                  (m_move_etot - move_etot) .lt. 7*eps_chg_dlt )
     &        ) .and.
     &        t_eig_conv .and.
     &        .not. (delt_norm .eq. delt2)

         t_sw_fnct = 
     &        abs(mmm_etot-mm_etot).lt.eps_sw_fnct .and.
     &        abs(mm_etot- etot_m) .lt.eps_sw_fnct .and.
     &        abs(etot_m - etot_h) .lt.eps_sw_fnct .and.
     &        (.not.tfor .or.
     &                ( (m_move_etot - move_etot) .gt. 0. .and.
     &                  (m_move_etot - move_etot) .lt. 7*eps_sw_fnct )
     &        ) .and.
     &        t_eig_conv .and.
     &        .not. (delt_norm .eq. delt2)

*ThL  Explanation: t_el_conv is set to .true. if 
*          1. etot has decreased during the last 3 steps ----> REMOVED !
*     AND  2. the variation of etot at each of these steps was < epsel
*     AND     (only if geometry relaxation is done)
*          3. etot decreased after the last ionic move
*     AND  4. this variation was < 7*epsel.

*ThL    Next statement modified because, even if condition is true once
*     in the run, it may become false again.  In this case, t_ekinc_conv
*     is now reset to false.

         t_ekinc_conv = mm_ekinc .lt. epsekinc .and.
     &                   ekinc_m .lt. epsekinc .and.
     &                     ekinc .lt. epsekinc
         
      endif
c     end of (n_it>4)


c     check total convergency
c     ----------------------------

      if ( t_el_conv    .and.
     &     t_force_conv .and.
     &     t_ekinc_conv .and.
     &     t_eig_conv   .and.
     &     .not. tdyn ) then
        t_last_loop = .true.
      endif
cpf   added 21.4.98; commented out for reason of non-convergency, 23.4.98 (in previous if)
c     &     .not. (tspin .and. nozeta.lt.istepe) .and.
c ----------------------------------------------------------------------


cpf   in order to switch from LDA to GGA in view of following commands
cpf   (either when reaching a certain convergency or
cpf    if exceeding a given maximum number of LDA steps)
      if ( (i_xc .ne. 0)  .and.
     &     .not. t_grad   .and. 
     &     (  (0.lt.istep_no_gga .and. istep_no_gga.lt.istepe)
     &        .or. t_sw_fnct )  ) 
     &     t_last_loop=.true.


cmb   if t_last_loop, and GC where requested continue with the i_xc-Functional  
cmb   instead of LDA.   
      
      if ( (i_xc .ne. 0) .and.
     &     .not. t_grad  .and.
     &     t_last_loop)  then

        if ( (istep_no_gga .gt. 0 .and. istep_no_gga .lt. istepe)
     &       .or. t_sw_fnct) then

          t_grad=.true.
          write(6,'(a,i4)')">>> Results below are obtained with E_xc:"
     &         ,i_xc

cpf       switch functional (LDA->GGA) and reset convergency toggles
          if (t_postc .and. (i_xc.lt.0)) then
            t_el_conv   =.false.
            t_ekinc_conv=.false.
            t_chg_dlt   =.false.
            t_last_loop =.false.
            i_xc=-i_xc
            if (i_edyn.ne.2) then
              write(6,*)
     &             '>>> Main stores density for mixing in c_fft_store'
              i_low_delt=8
              delt=0.2*delt
              write(6,*)
     &             '>>> Main reduces delt to ',delt,'for 8 steps.'
              do  i3=1,nr3x
                do  i2=1,nr2x
                  do  i1=1,nr1x
                    c_fft_store(i1,i2,i3,1)=rhoe(i1,i2,i3)
                  enddo
                enddo
              enddo
            endif
            n_it_gga=n_it
          endif
        else
          write(6,'(a,i4)') 
     &         ">>> Convergency was not reached, allow for more"
          write(6,'(a,i4)') 
     &         ">>> iterations, an appropriate EKT may be helpful"
          write(6,'(a,i4)') ">>> as well. No post GC was done !!!"
        endif
      endif
c     end of (i_xc .ne. 0)

cmb
cpf   to be set without if ?
      if (n_it .gt. 1) then

cpf     last three total energies past electronic iteration
        mmm_etot = mm_etot
        mm_etot  = etot_m
        etot_m   = etot_h

cpf     last two total energies past core moves
        if (istepe.eq.1) then
          m_move_etot = move_etot
          move_etot = etot
        endif

cpf     last two 'kinetic energies'
        mm_ekinc = ekinc_m
        ekinc_m  = ekinc

cpf     last three (cumulated?) force values
        mmm_force_change = mm_force_change
        mm_force_change = force_change_m
        force_change_m = mean_rel_change

      endif

c     timestep?
      dt = delt
c     division by emass to normalize dt
      dtbye = dt/emass
c      

cpf   write out extended informations every 'iprint' iterations
      tprint = mod(n_it-1,iprint).eq.0 .or. t_last_loop
      if (tprint) write(6,*) '=== LOOP n_it=',n_it

cpf   new forces if tfion?
      if (.not.(tfor.or.tdyn)) tfion = tprint
      if (tfion) call zero_v(nax*nsx*3,fion,1)

      call flush(6)


c     call phfac with t_coord_auto=.false. to calculate all non local
c     forces in nlrhkb

      if (t_last_loop) then
         do is=1,nsp
            do ia=1,na(is)
               do ii=1,3
                  t_coord_auto(ii,ia,is)=.false.
               enddo
            enddo
         enddo
         call phfac(a1,a2,a3,t_last_loop)
      endif
     
      time1=cputime(time_zero)

cpf   non-local potential contributions

      call nlrhkb(tfion,tprint,t_last_loop)

cpf   time control for nlrhkb
      time1=cputime(time1)
      nlrh_t = nlrh_t + time1
      if (n_it.eq.1) then
         write(6,'(''time elapsed for nlrh t = '',f12.4)') nlrh_t
         call flush(6)
      endif

cpf -- charge density
      time1=cputime(time_zero)
cpf
      if (tspin) then
         nozeta=nozeta-1
         if (nozeta.ge.0) then
            write(6,
     &      '(''>>>no update of spin-polarization in this iteration'')')
            call rhoofr(tprint,rhoe,zeta_dummy)
         else
            call rhoofr(tprint,rhoe,zeta)
         endif
      else
         call rhoofr(tprint,rhoe,zeta)
         call zero_v(nnrx,zeta,1)
      endif

cpf   time control for rhoofr
      time1=cputime(time1)
      ti_rhoofr = ti_rhoofr + time1
      if (n_it.eq.1) then
         write(6,'(''time elapsed for rhoofr t = '',f12.4)') ti_rhoofr
         call flush(6)
      endif

cpf -- energies + local potential
      time1=cputime(time_zero)
cpf
      call vofrho(istepe, tprint,t_last_loop,tfion, etot_h, tdipol,
     &     vpot,vpot_free,vpot0, rhoe,zeta, a1,a2,a3, nbeg, i_xc,t_grad,
     &     vloc)

cpf   time control for vofrho
      time1=cputime(time1)
      ti_vofrho = ti_vofrho + time1
      if (n_it.eq.1) then
        write(6,'(''time elapsed for vofrho t = '',f12.4)') ti_vofrho
        call flush(6)
      endif

c-----------------------------------------------------------------------
c
c     move ions
c
c-----------------------------------------------------------------------

cpf   simple relaxation
      if (tfor) then
        call fionsc(force_eps(1),max_no_force,mean_force,
     &       mean_rel_change,istepe,nfi_old,tsdp,ttfor,tfion,
     &       t_last_loop,t_force_conv)
      endif

cpf   complete dynamics
      if (tdyn) then 
        call fiondyn(npos,nthm,nOrder,time,idyn,istepe,nsp,na,
     &       delta_ion,fion,fion_nl,epsfor,force_eps,max_no_force,
     &       ekin_ion,vpot,vpot_free,vpot0,T_ion,nfi_rescale,Q,
     &       ion_fac,tau0,vau0,mean_force,mean_rel_change,nfi,
     &       ttfor,tford,tfion,tfion_nl,t_last_loop,tprint,
     &       (t_el_conv .and. t_eig_conv),atom, i_edyn)
      endif


c-----------------------------------------------------------------------
c
c     ionic moves 
c
c-----------------------------------------------------------------------
 
      if (ttfor) then

        istepe=1

        delt=delt1
        delt_norm=delt1
        if (i_edyn.eq.2) then
          gamma=gamma1
          gamma_norm=gamma1
        endif

        write(6,'(''>>>Changing delt to'',f12.6)')delt
        if (i_edyn.eq.2) then
          write(6,'(''>>>Changing gamma to'',f12.6)')gamma
        endif


c       new positions => new phase and structure factors

        call phfac(a1,a2,a3,t_last_loop)
        call strucf
        if (tfion) call zero_v(nax*nsx*3,fion,1)
        call flush(6)

        time1=cputime(time_zero)
        call nlrhkb(tfion,tprint,t_last_loop)
        time1=cputime(time1)
        nlrh_t = nlrh_t + time1

        time1=cputime(time_zero)
        call vofrho(istepe,.true.,t_last_loop,tfion,etot_h,tdipol,
     &       vpot,vpot_free,vpot0,rhoe,zeta,a1,a2,a3,nbeg,i_xc,t_grad,
     &       vloc)
        time1=cputime(time1)
        ti_vofrho = ti_vofrho + time1
        t_el_conv=.false.
        t_ekinc_conv=.false.
      endif
c


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     update electronic wavefunctions
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c-----------------------------------------------------------------------
c     calculate diagonal matrix elements of hamiltonian (<g+k|H|g+k>)
c     for Williams/Soler and Schultze algorithm
c-----------------------------------------------------------------------

cpf ?
      if (i_edyn.gt.0) then
        vloc0=dsum(nnrx,vloc,1)/nnr
      endif
      ekinc=0.0


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      
c     start of loop over k-points (nkpt)      
c      
c ======================================================================

      do 4199 ik=1,nkpt

        if (i_edyn.gt.0) then
cp        ini_rvec
          do ig=1,ngw(ik)
            vdg(ig)=0.0
          enddo
          do is=1,nsp
            lm_end=l_max(is)**2+1-2*l_loc(is)
            do i_lm=1,lm_end
              facc=wnl(is,i_lm)*na(is)
              do ig=1,ngw(ik)
                vdg(ig)=vdg(ig)+pkg_a(i_lm,ig,is,ik)**2*facc
              enddo
            enddo
          enddo
        endif


c ======================================================================
c
c       start of loop over bands (n)        
c        
c ----------------------------------------------------------------------

        do 4226 i=1,(nspin+1)*nx

c----------------------------------------------------------------------
c         calculate   c2(g)=(-1)*<g+k|H|Psi_{n,k}> 
c         (Hamilton on wave function, evaluated in plane wave basis)
c----------------------------------------------------------------------
          time1=cputime(time_zero)

cpf       prepare local potential 
          if (nspin.eq.1) then
            if (i.le.nx) then
              ind=1
              do i3=1,nr3x
                 do i2=1,nr2x
                   do i1=1,nr1x
                    vl(i1,i2,i3)=vloc(i1,i2,i3)+0.5*dvxc_vec(ind)
                    ind=ind+1
                  enddo
                enddo
              enddo
            else
               ind=1
               do i3=1,nr3x
                  do i2=1,nr2x
                     do i1=1,nr1x
                        vl(i1,i2,i3)=vloc(i1,i2,i3)-0.5*dvxc_vec(ind)
                        ind=ind+1
                     enddo
                  enddo
               enddo
            endif
          else
            call dcopy(nnrx,vloc,1,vl,1)
          endif
            
cpf       compute c2 
          call dforce(i,ik,c2(1),vl)
          ti_dforce = ti_dforce + cputime(time1)


c-----------------------------------------------------------------------
c         calculate eigenvalues  (for given state i at k-point ik)
c         eig=           <Psi_{n,k}|          H|Psi_{n,k}>
c            =   sum_{g} <Psi_{n,k}|g+k>*<g+k|H|Psi_{n,k}>
c            = - sum_{g} <Psi_{n,k}|g+k>*    c2(g)
c         and add correction term accounting for spin-polarization (?!)
c-----------------------------------------------------------------------

cpf       raw eigen value
          c_eig=(0.0, 0.0)
          do ig=1,ngw(ik)
            c_eig = c_eig - conjg(c0(ig,i,ik))*c2(ig)
          enddo
          eigav = dble(c_eig)
          
cpf       corrected with energy shift due for spin polarization computed in dforce
          if (tspin .and. nspin.eq.0) then
            eig(i,ik)    = eigav + 0.5 * deig(i,ik)
            eig(i+nx,ik) = eigav - 0.5 * deig(i,ik)
          else
            eig(i,ik) = eigav
          endif
          

c ----------------------------------------------------------------------
c
c         propagate wave functions according to either :
c
c ----------------------------------------------------------------------

c ----------------------------------------------------------------------
c         0. Verlet algorithm
c ----------------------------------------------------------------------

          if (i_edyn.eq.0) then
c            
            do j=1,ngw(ik)
              speed=c0(j,i,ik)
              c0(j,i,ik)=speed+dtbye*(c2(j)+eigav*speed)
c             c0(j,i,ik)=speed+dtbye*c2(j)              
              speed=c0(j,i,ik)-speed
              ekinc=ekinc + abs(speed)**2
            enddo


c ----------------------------------------------------------------------
c         1. Williams/Soler algorithm
c ----------------------------------------------------------------------

          elseif ( i_edyn.eq.1          .or.
     &           (i_edyn.eq.2 .and. n_it.lt.2)) then
            
c            
            if(i_edyn.eq.2.and.gamma.gt.1.0) then
              dts=dt/gamma 
            else
              dts=dt
            endif

            do ig=1,ngw(ik)
              ttt=0.5*xkg(ig,ik)*tpiba2+vloc0+vdg(ig)
              alpha(ig)=(eigav-ttt)/emass
            enddo
            do ig=1,ngw(ik)
              c0_temp=c0(ig,i,ik)
              if(i_edyn.eq.2) c0_old(ig,i,ik)=c0(ig,i,ik)
              par=exp(alpha(ig)*dts)-1.0
c             if(ig.eq.1) write(6,*)'alpha,par=',alpha(ig),par,dts
              c2(ig)=par*(c2(ig)+eigav*c0(ig,i,ik))/alpha(ig)/emass
              c0(ig,i,ik)=c0_temp+c2(ig)
              speed=c0(ig,i,ik)-c0_temp
              ekinc=ekinc + abs(speed)**2
            enddo

c    or 
c ----------------------------------------------------------------------
c         2. Schultze algorithm  
c ----------------------------------------------------------------------

          else  if (i_edyn.eq.2) then 
c            
            do ig=1,ngw(ik) 
              ttt=0.5*xkg(ig,ik)*tpiba2+vloc0+vdg(ig)
              alpha(ig)=(eigav-ttt)/emass
              beta(ig)=(-c2(ig)-ttt*c0(ig,i,ik))/emass          
            enddo

            e1=exp(-gamma*dt)
            do ig=1,ngw(ik)
              omega2=-alpha(ig)-gamma*gamma/4.0           
              if(omega2.ge.0.0) then          
                ecos1(ig)=2.0*cos(sqrt(omega2)*dt)
     &                       *exp(-gamma/2.0*dt)
              else          
                if(gamma*dt.gt.650.0) then
                  if(abs(alpha(ig)**2.0/gamma**3.0).gt.1.0d-3)then
                    write(6,*)' Time step too large'
                    write(6,*)' numerical accuracy is exceeded'
                    stop
                  else 
                    ecos1(ig)=exp(alpha(ig)/gamma*dt)
                  endif
                else
                  ecos1(ig)=2.0*cosh(sqrt(-omega2)*dt)
     &                 *exp(-gamma/2.0*dt)      
                endif          
              endif
            enddo
               
            do ig=1,ngw(ik) 
              c0_temp= c0(ig,i,ik)
              c0(ig,i,ik)=(c0_temp - beta(ig)/alpha(ig)) * ecos1(ig)
     &             + (beta(ig)/alpha(ig)-c0_old(ig,i,ik)) * e1
     &             + beta(ig)/alpha(ig)                        
              c0_old(ig,i,ik)=c0_temp        
              speed=c0(ig,i,ik)-c0_temp
              ekinc=ekinc + abs(speed)**2
            enddo
          endif
          

c ----------------------------------------------------------------------
c          
 4226   continue
c        
c       end of loops over bands (states at single k-point)
c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
c       imposing orthogonality of states at each k-point
c-----------------------------------------------------------------------

ctmp        call graham(ik,n,ngw(ik),.false.)
        call graham(ik,n,ngw(ik),.false.,tcaxpy,tcnorm,tcdotc)
        
        ti_graham = ti_graham + cputime(time1)



c ----------------------------------------------------------------------
c
c       output at end of k-loops
c


cpf     output time consumption of an entire electron iteration during first one
        if (n_it.eq.1.and.ik.eq.nkpt)
     &       write(6,'(''time elapsed for n x nkpt x graham/ortho = ''
     &                                              ,f12.4)') ti_graham
ctmp
        if (n_it.eq.1.and.ik.eq.nkpt)
     &       write(6,'(''time elapsed for n x nkpt x graham/cdotc = ''
     &                                              ,f12.4)') tcdotc
        if (n_it.eq.1.and.ik.eq.nkpt)
     &       write(6,'(''time elapsed for n x nkpt x graham/caxpy = ''
     &                                              ,f12.4)') tcaxpy
        if (n_it.eq.1.and.ik.eq.nkpt)
     &       write(6,'(''time elapsed for n x nkpt x graham/cnorm = ''
     &                                              ,f12.4)') tcnorm
ctmp
cpf     telling eigenvalues for 'non-metals' when desired
        if (tprint.and..not.tmetal) then
          write(6,'(''eigenvalues at k-point: '',3f6.3)') 
     &                                                (xk(i,ik),i=1,3)
          if (tspin) then
            write(6,'(''spin-up:'')')
            write(6,'(f7.3,9f8.3)') (eig(i,ik)*au,i=1,n)
            write(6,'(f7.3,9f8.3)') (focc(i,ik),i=1,n)
            write(6,'(''spin-down:'')')
            write(6,'(f7.3,9f8.3)') (eig(i+nx,ik)*au,i=1,n)
            write(6,'(f7.3,9f8.3)') (focc(i+nx,ik),i=1,n)
          else
            write(6,'(f7.3,9f8.3)') (eig(i,ik)*au,i=1,n)
            write(6,'(f7.3,9f8.3)') (focc(i,ik),i=1,n)
          endif

        endif
        if (.not.tmetal) then
           do ii=1,nx*(nspin_pt+1)
              write(87,'(3i4,2F12.6)') n_it, ii, ik, 
     &           eig(ii,ik)*27.2116, focc(ii,ik)
           enddo
        endif

c ----------------------------------------------------------------------
c
 4199 continue
c
c     end of loop over k-points

c     ( end of propagating wave functions )

c ======================================================================




c ----------------------------------------------------------------------
c     evaluate change of wavefunction(s) in ekinc 
c ----------------------------------------------------------------------

c     (ekinc is not the kinetic energy but 1000 times the sqrt of the mean
c     square force on a wave function component)  (<- ?!)
      if(i_edyn.eq.2.and.gamma.gt.1.0) then
        ekinc = gamma*sqrt(ekinc / n / ngww / dtbye**2.0) *1000.0
      else
        ekinc =       sqrt(ekinc / n / ngww / dtbye**2.0) *1000.0
      endif


c ======================================================================
c     fill electrons into states corresponding to damped eigenvalues
c     (in case of metals and according to given 'temperature' ekt
c ======================================================================

      if (tmetal) then

        if ( .not. (tspin .and. (nozeta .eq. istepe+1)) ) then

cpf       mix old with new eigenvalues ('damped pseudodynamics')
          call fermi(istepe,t_eig_conv,tprint,t_last_loop)

        else

cpf       restart 'pseudodynamics' from scratch
          write(6,*) '>>>main: restarting pseudo-eigenvalues'
          call fermi(1,t_eig_conv,tprint,t_last_loop)

        endif

      endif


c=======================================================================
c     output some results
c=======================================================================

cpf   current energies, entropies, etc.
cpf   (distinguishing metal,molecular dynamics yes/no)

cpf   labels (during iterations with extended output)
      if (tprint) then
         if (tmetal) then
            if (.not.tdyn) then
               write(6,*)'>>>n_it nfi   Ekinc    Etot      Eharr',
     &              '    Ezero    mForce    mChange     Seq     Sneq',
     &              '    Efermi   Dvolt     Pol'
            else
               write(6,*)'>>>n_it nfi    Ekinc     EtotE      EharrE',
     &              '     EkinI      EtotS      EzeroS     EfreeS',
     &              '     mForce     mChange     Seq     Sneq',
     &              '    Efermi    Dvolt     Pol'
            endif
         else
            if (.not.tdyn) then
               write(6,
     &              '(//">>>",2X,3Hnfi,4X,5HEkinc,8X,4HEtot,5X,3HPol)')
            else
               write(6,
     &              '(//">>>",2X,3Hnfi,4X,5HEkinc,7X,5HEtotE,7X,5H Ekin,
     &              7X,5HEtotS,7X,7H mForce,5X,8H mChange ,5X,3HPol)')
            endif
         endif
      endif

cpf   compute cell's entire spin polarization
      if (tspin) then
         qp=rsumup-rsumdn
      else
         qp=0.0
      endif
cpf   normal output of iteration number, energies, forces, entropies etc. and spin polarization every iteration
      if (tmetal) then
        ts=ekt*sneq/(2.0*13.6058)
        ezero=etot-ts/2.0
        if (.not.tdyn) then
          write(6,'(">>>",i4,i3,f10.5,f11.5,f11.5,f11.5,f10.5,f9.3,
     &         f9.4,f9.4,f9.4,f9.4,f9.4)')n_it,nfi,ekinc,etot,etot2,
     &         ezero,mean_force,mean_rel_change,seq,sneq,efermi,dvolt,qp
        else
          write(6,'(">>>",i6,i5,3f12.5,f8.5,3f12.5,3f10.5,5f9.4)')
     &         n_it,nfi,ekinc,etot,etot2,ekin_ion,etot+ekin_ion,
     &         ezero+ekin_ion,vpot_free+ekin_ion,mean_force,
     &         mean_rel_change,seq,sneq,efermi,dvolt,qp
        endif
      else
         if (.not.tdyn) then
            write(6,'(3H>>>,i5,1x,2f11.5,1f9.4)') nfi,ekinc,etot,qp
         else
            write(6,'(3H>>>,i8,i5,f10.5,3f11.5,3f10.5,1f9.4)')
     &           n_it,nfi,ekinc,etot,ekin_ion,etot+ekin_ion,mean_force,
     &           mean_rel_change,qp
         endif
      endif


c ----------------------------------------------------------------------
c     output for xmgr      
c ----------------------------------------------------------------------


      call o_potx
      call o_rhox
      call o_runx(n_it,etot,etot2)


c ----------------------------------------------------------------------
c     output for balsac      
c ----------------------------------------------------------------------
cpf   (to visualize atomic positions)

      call balsacout(a1,a2,a3,ibrav,2,nsp,na,tau0,zv,alat) 

c      call project2

c ----------------------------------------------------------------------
c     write restartfile fort.71
c ----------------------------------------------------------------------

cpf   (every loop instead?)
      if (t_last_loop .or. tprint)
     &
     &  call write_wavef_n_cell(                                           &
     &     ecut, xk, wkpt, igk, n1, n2, n3, nr1, nr2, nr3,                 & ! basis
     &     nel, ngw, c0, eig, focc, zeta, efermi,                          & ! electrons
     &     pgind, ibrav, alat, omega,                                      & ! lattice
     &     nsp, na, atom, tau0,                                            & ! atoms 
     &     ng,g,gg)


c     END OF OUTPUT
c ----------------------------------------------------------------------


c=======================================================================

cpf   end program when last loop finished

c ----------------------------------------------------------------------

      if (t_last_loop) goto 1001

c=======================================================================



c ----------------------------------------------------------------------
c     communication with files
c ----------------------------------------------------------------------


c     check whether program should be stopped in a given number of iterations

      open(unit=50,file='stopprogram')
      rewind 50
      read(50,*,err=126,end=126) istep1
      rewind 50
      write(50,*) ' '
      write(6,*) '>>> OK, I stop after timestep nr.', istep1
      if (istep1.le.n_it-1) t_last_loop = .true.
c     label read err
 126  continue
      close(50)


c     time control

      if (n_it.eq.1) then
         time1=cputime(timestart)-timeinit
         write(6,'(''time elapsed per electronic time step t = ''
     &        ,f12.4)') time1
         istepmax = 0.95 * (timequeue-timeinit)/time1
         if ((istepmax .eq. 1) .and. .not. tdyn) goto 1001
         if (.not. tdyn) 
     &        write(6,
     &        '(''time in queue:'',i8,'' max. number of steps:'',i8)')
     &        INT(timequeue),istepmax
      endif

      if ((n_it.eq.5) .and. .not. tdyn) then
         time1=(cputime(time_zero)-timeinit)/5
         istepmax = 0.95 * (timequeue-timeinit)/time1
         write(6,'(''new max. number of steps:'',i8)')istepmax
      endif

c      if ((n_it.ge.istepmax-1) .and. .not. tdyn ) then
c         t_last_loop = .true.
c       write(6,*)'>>>I have not enough time in the queue: not stopped'
c      endif


c     CHECK WHETHER nomore IS STILL VALID:

      open(unit=51,file='stopfile')
      rewind 51
      read(51,*,err=121,end=121) nomor1
      nomore=nomor1
      rewind 51
      write(51,*) ' '
      write(6,*) '>>> OK, I change NOMORE to',nomore
c     label read err
 121  continue
      close(51)

c     timestep control:

      if(t_chg_dlt) then
         delt_norm=delt2
         gamma_norm=gamma2
         delt=delt2
         gamma=gamma2
         t_chg_dlt=.false.
         write(6,'(''>>>Changing delt & gamma to'',2f12.6)')delt,gamma
      else
         open(unit=53,file='delt')
         if (i_edyn.eq.1) then
            read(53,*,err=124,end=124) delt
            write(6,'(''>>>delt changed:'',f12.6)') delt
         else
            read(53,*,err=124,end=124) delt,gamma
            write(6,'(''>>>delt & gamma changed'',2f12.6)')
     &           delt,gamma
         endif
         delt_norm = delt
         gamma_norm=gamma
         rewind 53
         write(53,*) ' '
c        label read err
 124     continue
         close(53)
      endif

c     ion_fac control

      if (tfor) then
         open(unit=53,file='ion_fac')
         read(53,*,err=128,end=128) (tmp_fac(i),i=1,nsx)
         do i=1,nsx
            ion_fac(i)=tmp_fac(i)
         enddo
         write(6,'(''>>>ion_fac changed:'',20f7.2)') 
     &                             (ion_fac(i),i=1,nsx)
         rewind 53
         write(53,*) ' '
 128     continue
         close(53)
      endif


c ----------------------------------------------------------------------
c
c     end of electronic self consistency loop 
c     ('while'-loop, including relaxation and molecular dynamics respectively)
c

      goto 1000


c=======================================================================
c
c     program termination
c
c=======================================================================

 1001 write(6,*) '============= END OF THE MAIN-LOOP ================'



c ----------------------------------------------------------------------
cpf   time informations
      write(6,'(''av. time elapsed for nlrh t = '',f12.4)') nlrh_t/n_it
      write(6,'(''av. time elapsed for rhoofr t = '',f12.4)') ti_rhoofr/
     &     n_it
      write(6,'(''av. time elapsed for vofrho t = '',f12.4)') ti_vofrho/
     &     n_it
      write(6,'(''av. time elapsed for n x nkpt x dforce = '',f12.4)')
     &     ti_dforce/n_it
      write(6,'(''av. time elapsed for  nkpt x graham/ortho = '',
     &     f12.4)')ti_graham/n_it
      write(6,'(''av. time elapsed for  nkpt x graham/cdotc = '',
     &     f12.4)')tcdotc/n_it
      write(6,'(''av. time elapsed for  nkpt x graham/caxpy = '',
     &     f12.4)')tcaxpy/n_it
      write(6,'(''av. time elapsed for  nkpt x graham/cnorm = '',
     &     f12.4)')tcnorm/n_it
      
      time1=cputime(timestart)-timeinit
      write(6,'(''av. time elapsed for rest (in main) t = '',f12.4)')
     &     (time1
     &     - nlrh_t - ti_rhoofr - ti_vofrho - ti_dforce - ti_graham)
     &     / n_it
      write(6,'(''av. time elapsed per elec. time step t = '',f12.4)')
     &     time1/n_it
c ----------------------------------------------------------------------

cpf   Done
      stop 'main: Ad majorem gloriam dei'

      end



c=======================================================================















