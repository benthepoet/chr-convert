program main
  implicit none

  integer :: i, x, y, offset, hi, lo
  integer, dimension(8) :: pixels

  pixels = (/0, 1, 2, 0, 0, 2, 3, 0/)

  hi = 0
  lo = 0
  
  do i = 1, 8
     x = pixels(i)
     offset = 8 - i
     
     ! Write Lo-Byte
     y = iand(1, x)
     y = lshift(y, offset)
     lo = ior(lo, y)

     ! Write Hi-Byte
     y = rshift(x, 1)
     y = lshift(y, offset)
     hi = ior(hi, y)

  end do

  print *, lo, hi

end program main
