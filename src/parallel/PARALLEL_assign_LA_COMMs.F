!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine PARALLEL_assign_LA_COMMs(ENVIRONMENT,idriver,Ncpu_in)
 !
 use parallel_m,    ONLY:PAR_COM_WORLD,PAR_COM_NULL,&
&                        PAR_COM_SLK,PAR_COM_SLK_INDEX_global,PAR_COM_Q_A2A,PAR_COM_Q_INDEX,&
&                        linear_algebra_is_parallel
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_COM_A2A,SLK_COM_INDEX,SLK_ORTHO,SLK_POOL
#endif
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 !
 implicit none
 !
 character(*)            :: ENVIRONMENT
 integer                 :: idriver
 integer                 :: Ncpu_in
 !
 ! Work Space
 !
 integer :: Ncpu_lin_algebra, slk_error
 !
 !  LK -> Lapack,     serial linear algebra
 ! SLK -> Scalapack, parallel linear algebra
 !
 Ncpu_lin_algebra=1
#if defined _SCALAPACK 
 if (Ncpu_in==0) Ncpu_lin_algebra=1         ! safe default
 if (Ncpu_in>0 ) Ncpu_lin_algebra=Ncpu_in
#endif
 !
 ! smallest SLK grid is 2x2
 if (Ncpu_lin_algebra<4) Ncpu_lin_algebra=1
 !
 ! Serial (LK) case (altough this does not mean serial job)
 if (Ncpu_lin_algebra==1) then  
   !
   call SERIAL_fallback()
   call PARALLEL_PAR_to_PAR_comms_init()
   !
   return
   !
 endif
 !
#if defined _SCALAPACK
 !
 ! environment-dependent definitions
 !
 if (ENVIRONMENT=="Response_G_space".or.ENVIRONMENT=="Response_G_space_and_IO") then
   !
   call SLK_communicators( PAR_COM_Q_A2A, PAR_COM_Q_INDEX, Ncpu_in ) 
   !
 else if (ENVIRONMENT=="ScaLapacK") then
   !
   call SLK_communicators( SLK_COM_A2A(1), SLK_COM_INDEX(1), Ncpu_in )
   !
 else if (ENVIRONMENT=="Response_T_space") then
   !
   call SLK_communicators( PAR_COM_WORLD, PAR_COM_NULL, Ncpu_in )
   !
 endif
 !
 ! redef of input Ncpu and SLK init
 !
 linear_algebra_is_parallel=.TRUE.
 !
 Ncpu_lin_algebra=min(Ncpu_lin_algebra,PAR_COM_SLK%n_CPU)
 ! 
 if (Ncpu_lin_algebra<=3) linear_algebra_is_parallel=.FALSE.
 if (.not.linear_algebra_is_parallel) then
   call SERIAL_fallback( )
   call PARALLEL_PAR_to_PAR_comms_init()
   return
 endif
 !
 ! init SLK
 !
 call SLK_setup( idriver, Ncpu_lin_algebra, SLK_ORTHO, SLK_POOL, PAR_COM_SLK, PAR_COM_SLK_INDEX_global, slk_error)
 !
 if (slk_error/=0) then
   linear_algebra_is_parallel=.FALSE.
   call warning("SLK setup failed. Cannot build a proper BLACS grid. Switching to standard LK")
   call SLK_communicators( PAR_COM_NULL, PAR_COM_WORLD, 1)
 endif
 !
 ! Messaging
 !
 if (linear_algebra_is_parallel) then
   call msg('s','[LA@'//ENVIRONMENT//'] PARALLEL linear algebra uses a '//trim(intc(SLK_ORTHO%grid(1)))//&
&               "x"//trim(intc(SLK_ORTHO%grid(2)))//" SLK grid ("//&
&               trim(intc(product(SLK_ORTHO%grid)))//" cpu)")
 else
#endif
   call msg('s','[LA] SERIAL linear algebra')
#if defined _SCALAPACK
 endif
#endif
 !
 ! PAR to PAR comms init
 !
 call PARALLEL_PAR_to_PAR_comms_init()
 !
 contains
   !
   subroutine SERIAL_fallback
     !
     if (ENVIRONMENT=="Response_G_space_and_IO".or.ENVIRONMENT=="Response_G_space") then
       call SLK_communicators( PAR_COM_NULL, PAR_COM_Q_A2A, 1)
     else if (ENVIRONMENT=="Response_T_space") then
       call SLK_communicators( PAR_COM_NULL, PAR_COM_WORLD, 1)
     endif
     !
     linear_algebra_is_parallel=.FALSE.
     !
     call msg('s','[LA] SERIAL linear algebra')
     !
   end subroutine
   !
end subroutine
