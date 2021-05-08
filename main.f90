program main
  implicit none

  integer(kind=1) :: b, x, y, hi, lo
  integer :: seek
  integer :: ios
  integer :: i, j, offset
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
        y = iand(int(1, 1), x)
        y = lshift(y, offset)
        lo = ior(lo, y)

        ! Write Hi-Byte
        y = rshift(x, 1)
        y = lshift(y, offset)
        hi = ior(hi, y)

     end do

     print '(z4)', lo, hi
  end do

  print *, "-----"

  ios = 0
  
  open(1, file='chr.bmp', access='stream')
  call fseek(1, 10, 0)
  read(1, iostat=ios) seek
  call fseek(1, seek, 0)

  outer: do while (ios == 0)
     hi = 0
     lo = 0
     
     row: do i = 1, 8
        read(1, iostat=ios) x
        
        if (ios .ne. 0) then
           exit outer
        end if

        x = ishft(x, -6)
        offset = 8 - i

        ! Write Lo-Byte
        y = iand(int(1, 1), x)
        y = lshift(y, offset)
        lo = ior(lo, y)

        ! Write Hi-Byte
        y = rshift(x, 1)
        y = lshift(y, offset)
        hi = ior(hi, y)
     end do row

     print '(z4,z4)', lo, hi
  end do outer
  
  close(1)
  
end program main
