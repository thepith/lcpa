module SystemModule

implicit none
private
public cells, cell_type, InitCells, ncell, nneighcell

integer(4), parameter    :: nneighcell = 6     ! number of neighbouring cells (dim = 3)

type cell_pointer_array
   type(cell_type), pointer              :: p => null()        ! pointer to a cell, usefull to create an array of pointers
end type cell_pointer_array

type cell_type
   integer(4)               :: id                 ! for easy recognition
   logical :: lOccupied ! is the cell occupied?
   logical :: lCluster ! is the cell part of the cluster?
   type(cell_pointer_array) :: neighcell(nneighcell)       ! pointer to the neighbouring cells
   integer(4) :: coord(3)
end type cell_type

type(cell_type), target, allocatable     :: cells(:,:,:)        ! cells
integer(4)                               :: ncell = 0       ! number of cells in x y z in each octant

contains

subroutine InitCells(edgeLen)
   implicit none
   integer(4), intent(in) :: edgeLen
   integer(4) :: ix, iy, iz
   integer(4) :: ineigh, direction, magnitude, neigh(3)
   type(cell_type), pointer              :: icell      ! pointer to the cell
   integer(4) :: center

   ncell = edgeLen
   allocate(cells(0:(ncell-1),0:(ncell-1),0:(ncell-1)))
   do ix = 0, ncell - 1
      do iy = 0, ncell - 1
         do iz =  0, ncell - 1
            icell => cells(ix,iy,iz)
            icell%lOccupied = .false.
            icell%lCluster = .false.
            icell%coord(1:3) = (/ ix, iy, iz /)
            ineigh = 1
            do direction = 1, 3
               do magnitude = -1, 1, 2
                  neigh = (/ix, iy, iz/)
                  neigh(direction) = neigh(direction) + magnitude
                  if ((neigh(direction) >= ncell) .or. (neigh(direction) < 0)) then
                     neigh(direction) = modulo(neigh(direction),ncell)
                  end if
                  icell%neighcell(ineigh)%p => cells(neigh(1), neigh(2), neigh(3))
                  ineigh = ineigh + 1
               end do
            end do
         end do
      end do
   end do

end subroutine InitCells

end module SystemModule





