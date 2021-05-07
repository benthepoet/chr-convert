program main
  implicit none

  integer :: i, j, x, y, offset, hi, lo
  integer, dimension(8, 8) :: pixels
  
  pixels = transpose(reshape((/0, 0, 0, 0, 0, 0, 0, 0, &
       0, 1, 1, 1, 1, 1, 1, 0, &
       0, 1, 0, 0, 0, 0, 1, 0, &
       0, 1, 0, 0, 0, 0, 1, 0, &
       0, 1, 0, 0, 0, 0, 1, 0, &
       0, 1, 0, 0, 0, 0, 1, 0, &
       0, 1, 1, 1, 1, 0, 1, 0, &
       0, 0, 0, 0, 0, 0, 0, 0/), shape(pixels)))

  do j = 1, 8
     hi = 0
     lo = 0
     
     do i = 1, 8
        x = pixels(j, i)
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

     print '(z4)', lo, hi
  end do

end program main
