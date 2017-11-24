program llimag

   use SystemModule
   use ParticleModule
   implicit none
   integer(4) :: systemSize = 33
   integer(4) :: particleNumber = 156
   integer(4) :: nstep = 100000
   integer(4) :: iimage

   integer(4) :: istep, ipStep, ipMove
   real(8) :: dT
   logical :: lsuccess
   logical :: limage = .false.
   logical :: lstat = .true.
   integer(4) :: timeInterval = 100


   real(8) :: rprob = 1.0
   real(8) :: r(1)
   integer :: n = 12, i
   integer :: seed(12)
   integer :: iseed = 1
   character(len=128) :: arg, tmparg
   !parse command line arguments
   if (command_argument_count() == 0) then
      write(*,*) 'Using default arguments'
   end if

   do i = 1, command_argument_count()
      call get_command_argument(i, arg)

      select case (arg)
      case ('-i')
         limage = .true.
      case ('-x')
         lstat = .false.
      case ('-z')
         call get_command_argument(i+1, tmparg)
         read(tmparg,'(i8)') iseed
      case ('-n')
         call get_command_argument(i+1, tmparg)
         read(tmparg,'(i8)') particleNumber
      case ('-s')
         call get_command_argument(i+1, tmparg)
         read(tmparg,'(i8)') systemSize
      case ('-t')
         call get_command_argument(i+1, tmparg)
         read(tmparg,'(i8)') timeInterval
      case ('-l')
         call get_command_argument(i+1, tmparg)
         read(tmparg,'(i8)') nstep
      case ('-r')
         call get_command_argument(i+1, tmparg)
         read(tmparg,'(f7.5)') rprob
      end select
   end do

   print *, "system size (-s)", systemSize
   print *, "maximum number of steps (-l)", nstep
   print *, "particle Number (-n)", particleNumber
   print *, "probability of reaction (-r)", rprob
   print *, "seed? (-z)", iseed
   print *, "making stat? (switch on with -x)", limage
   print *, "making images? (switch on with -i)", limage
   print *, "time interval for the images (-t)", timeInterval
   print *, "number of particles left   ", "number of steps done   ", "number of images"

   call InitCells(systemSize)
   call InitParticles(particleNumber)

   CALL RANDOM_SEED(size = n)
   seed = iseed
   CALL RANDOM_SEED(put = seed)

   dT = 0.0d0
   iimage = 0
   steploop: do istep = 1, nstep
      if (modulo(istep, 100) == 0) then
         print *, np, istep, iimage
      end if
      do ipStep = 1, particleNumber
         if(limage) then
            if((dT > timeInterval) .and. (iimage < 6000)) then
               dT = 0.0d0
               call VTFParticles(iimage)
               iimage = iimage + 1
            end if
         end if
         ipMove = iRandomParticle()
         call MoveParticle(ipMove, lsuccess)
         if(lsuccess) then
            if(lAtCluster(ipMove)) then
               if( rprob < 1.0) then
                  call random_number(r)
               else
                  r = 0.0
               end if
               if ( r(1) < rprob) then
                  call MakeCluster(ipMove)
                  if(lstat) then
                     call GetStat()
                  end if
                  if(np < (particleNumber * 0.1)) then
                     exit steploop
                  endif
               endif
            end if
         end if
         dT = dT + 1.0d0/np
      end do
   end do steploop

end program
