program main
  implicit none

  integer(kind=1) :: b, x, y, hi, lo
  integer :: seek, file_size
  integer :: ios
  integer :: i, j, offset
  integer(kind=1), dimension(8) :: lo_bytes, hi_bytes
  
  ios = 0
  
  open(1, file='chr.bmp', access='stream')

  call fseek(1, 2, 0)
  read(1, iostat=ios) file_size
  print '(z4)', file_size

  call fseek(1, 10, 0)
  read(1, iostat=ios) seek

  ! Move to pixel data
  call fseek(1, seek, 0)

  j = 8
  
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

     lo_bytes(j) = lo
     hi_bytes(j) = hi
     j = j - 1
     
  end do outer

  close(1)
  
  do i = 1, 8
     print '(z2, z2)', lo_bytes(i), hi_bytes(i)
  end do

  open(2, file='chr.bin', access='stream')

  ! Write lo-bytes
  do i = 1, 8
     write(2) lo_bytes(i)
  end do

  ! Write hi-bytes
  do i = 1, 8
     write(2) hi_bytes(i)
  end do
  
  close(2)   
  
end program main
