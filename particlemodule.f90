module ParticleModule

use SystemModule, only: cell_type
implicit none
! private
public particle_type, particle, np, iRandomParticle, lAtCluster, MakeCluster, cluster, VTFParticles

type particle_type
   type(cell_type), pointer :: pos
end type

integer(4) :: np
integer(4) :: nc
type(particle_type), allocatable :: particle(:)
type(particle_type), allocatable :: cluster(:)

contains

subroutine InitParticles(numberParticles)
   use SystemModule, only: cells, ncell
   implicit none
   integer(4), intent(in) :: numberParticles
   real(8)    :: r(3)
   integer(4) :: i(3)
   integer(4), parameter :: ntry = 100000
   integer(4) :: itry, ip
   integer(4) :: center

   np = numberParticles
   allocate(particle(np))
   allocate(cluster(np+1))

   center = floor(0.5*ncell)
   cluster(1)%pos => cells(center, center, center)
   cluster(1)%pos%lCluster = .true.
   cluster(1)%pos%lOccupied = .true.
   nc = 1

   iploop: do ip = 1, np
      do itry = 1, ntry
         call random_number(r)
         i = floor( r*ncell )
         if ( cells(i(1), i(2), i(3))%lOccupied .eqv. .false.) then
            particle(ip)%pos => cells(i(1), i(2), i(3))
            particle(ip)%pos%lOccupied = .true.
            cycle iploop
         end if
      end do
   end do iploop
end subroutine InitParticles

function iRandomParticle() result(ip)
   implicit none
   real(8)    :: r(1)
   integer(4) :: i(1)
   integer(4) :: ip

   call random_number(r)
   i = floor(r *np) + 1
   ip = i(1)
end function

subroutine MoveParticle(ip, lsuccess)
   use SystemModule, only: nneighcell
   implicit none
   integer(4), intent(in) :: ip
   logical, intent(out) :: lsuccess
   real(8)    :: r(1)
   integer(4) :: i(1)
   integer(4) :: idir
   type(cell_type), pointer  :: destcell      ! pointer to the cell

   call random_number(r)
   i = floor(r*nneighcell) + 1
   idir = i(1)
   if ( particle(ip)%pos%neighcell(idir)%p%lOccupied ) then
      lsuccess = .false.
   else
      lsuccess = .true.
      particle(ip)%pos%lOccupied = .false.
      destcell => particle(ip)%pos%neighcell(idir)%p
      particle(ip)%pos => destcell
      particle(ip)%pos%lOccupied = .true.
   end if
end subroutine MoveParticle

function lAtCluster(ip) result(lcluster)
   use SystemModule, only: nneighcell
   implicit none
   logical :: lcluster
   integer(4), intent(in) :: ip
   integer(4) :: idir
   type(cell_type), pointer  :: ipcell      ! pointer to the cell

   ipcell => particle(ip)%pos
   lcluster = .false.
   do idir = 1, nneighcell
      if (ipcell%neighcell(idir)%p%lCluster) then
         lcluster = .true.
         exit
      end if
   end do
end function lAtCluster

subroutine MakeCluster(ip)
   integer(4), intent(in) :: ip
   type(particle_type) :: tmpparticle

   tmpparticle = particle(np)
   particle(np) = particle(ip)
   particle(ip) = tmpparticle
   nc = nc + 1
   cluster(nc)%pos => particle(np)%pos
   cluster(nc)%pos%lCluster = .true.
   np = np - 1
end subroutine

subroutine VTFParticles(iimage)
   use SystemModule, only: ncell
   implicit none
   integer(4), intent(in) :: iimage
   integer(4) :: ix, iy, iz
   character(14)          :: filename
   integer(4), parameter :: funit = 32

   integer(4) :: ic, ip

   write(filename,'(a, i0.4, a)') 'frame_', iimage, '.vtf'
   open(funit, file = filename, form = 'formatted')
   write(funit,'(a5,i5,a8,E13.5,a6,a11,a6,a)') &
      ('atom ',ic - 1, &
      ' radius ',0.5,  &
      ' type ','agg',  &
      ' name ' , '0',  &
      ic = 1, nc)
   write(funit,'(a5,i5,a8,E13.5,a6,a11,a6,a)') &
      ('atom ',ip - 1 + nc, &
      ' radius ',0.5,       &
      ' type ','free',      &
      ' name ' , '1',       &
      ip = 1, np)
   write(funit,'(/)')
   write(funit,'(a8,3E13.5)') 'unitcell ', real(ncell), real(ncell), real(ncell)
   write(funit,'(/)')
   write(funit, '(a)') '# Start of image '
   write(funit, '(a)') 'timestep ordered '
   write(funit,'(3E13.5)') (real(cluster(ic)%pos%coord(1:3)), ic = 1, nc)
   write(funit,'(3E13.5)') (real(particle(ip)%pos%coord(1:3)), ip = 1, np)
   write(funit, '(a)') '# End Image      '
   close(funit)
end subroutine VTFParticles

subroutine GetStat()
   real(8) :: rg, rmean(3)
   integer(4) :: ic, imean(1:3)
   logical, save :: first = .true.
   integer(4), parameter :: sunit = 33

   imean(1:3) = 0
   do ic = 1, nc
      imean(1:3) = imean(1:3) + cluster(ic)%pos%coord(1:3)
   end do
   rmean(1:3) = real(imean(1:3))/real(nc)
   do ic = 1, nc
      rg = rg + sum((real(cluster(ic)%pos%coord(1:3)) - rmean(1:3))**2)
   end do
   rg = rg/real(nc)
   if(first) then
      open(sunit, file = "rg_nc.data", form = 'formatted')
      first = .false.
   end if
   write(sunit,'(2E13.5)') real(nc), rg
end subroutine GetStat

end module ParticleModule

