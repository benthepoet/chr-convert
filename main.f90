program main
  implicit none

  integer(kind=1) :: b, x, y, hi, lo
  integer :: seek, file_size, width, height
  integer :: ios
  integer :: i, j, k, l, offset
  integer(kind=1), dimension(256, 8) :: lo_bytes, hi_bytes
  character(32) :: file_input, file_output

  call get_command_argument(1, file_input)
  call get_command_argument(2, file_output)
  
  ios = 0
  
  open(1, file=trim(file_input), access='stream')

  call fseek(1, 2, 0)
  read(1, iostat=ios) file_size
  print '(z4)', file_size

  call fseek(1, 10, 0)
  read(1, iostat=ios) seek

  call fseek(1, 18, 0)
  read(1, iostat=ios) width
  read(1, iostat=ios) height

  ! Move to pixel data
  call fseek(1, seek, 0)

  do j = 1, 256
     do i = 1, 8
        lo_bytes(j, i) = 0
        hi_bytes(j, i) = 0
     end do
  end do
  
  l = 15
  
  outer: do while (l > -1)
     hi = 0
     lo = 0

     nrow: do j = 8, 1, -1
        print *, "nrow", (width / 8 * l) + 1, (width / 8 * l) + (width / 8)

        tile: do k = (width / 8 * l) + 1, (width / 8 * l) + (width / 8)
           print *, "tile", k, j
           bytes: do i = 1, 8
              read(1, iostat=ios) x

              if (x == 0) then
                 x = -1
              else if (x == -1) then
                 x = 0
              end if
              
              if (ios .ne. 0) then
                 exit outer
              end if

              x = ishft(x, -6)
              offset = 8 - i

              ! Write Lo-Byte
              y = iand(int(1, 1), x)
              y = lshift(y, offset)
              lo_bytes(k, j) = ior(lo_bytes(k, j), y)

              ! Write Hi-Byte
              y = rshift(x, 1)
              y = lshift(y, offset)
              hi_bytes(k, j) = ior(hi_bytes(k, j), y)
           end do bytes
           print *, "bytes", lo_bytes(k, j), hi_bytes(k, j)
        end do tile
     end do nrow

     print *, l
     l = l - 1
          
  end do outer

  close(1)

  open(2, file=trim(file_output), access='stream')

  tiles: do j = 1, 256
     ! Write lo-bytes
     do i = 1, 8
        write(2) lo_bytes(j, i)
     end do

     ! Write hi-bytes
     do i = 1, 8
        write(2) hi_bytes(j, i)
     end do
  end do tiles
  
  close(2)   
  
end program main
