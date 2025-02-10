!defining a functin 
REAL(8) FUNCTION phi(x) RESULT(r)
 real(8), intent(in) :: x
 real(8) :: pi
 pi = 3.14159265358
 if (x >= 40.0 .and. x < 70.0) then
     r = sin(pi * ((x - 40.0) / 30.0))**2
 else
     r = 0.0
 end if
end function
! a subroutine for the FTFS scheme
subroutine FTFS_sub(N,c,phi_now,phi_a)
 integer :: N
 real(8), intent(in) :: c
 real(8), intent(in), dimension(N) :: phi_now
 real(8), intent(out), dimension(N) :: phi_a
 do i = 1,N-1
    phi_a(i) = (1.0 + c) * phi_now(i)  - c * phi_now(i+1)
 end do
 phi_a(N) = phi_now(1)
end subroutine
! a subroutine for the FTBS scheme 
subroutine FTBS_sub(N,c,phi_now,phi_a)
 integer :: N
 real(8), intent(in) :: c
 real(8), intent(in), dimension(N) :: phi_now
 real(8), intent(out), dimension(N) :: phi_a
 do i = 2,N
    phi_a(i) = (1.0 - c) * phi_now(i)  + c * phi_now(i-1)
 end do
 phi_a(1) = phi_now(N)
end subroutine


program ass2
 implicit none
 real(8) :: x0, x1, dx, dt, u, c, x, t, t1, tp
 real(8), dimension(:), allocatable :: phi_new , x_phi, phi_a
 real(8), external :: phi
 integer :: i, N, ios
 !intial conditions 
 u = 0.087
 dx = 0.1
 dt = 1.1
 c = u * dt/dx
 x0 = 0.0
 x1 = 100.0
 x = 0.0
 t = 0.0
 t1 = 1000.0
 tp = 200.0
 N = nint((x1 - x0) / dx) + 1
 allocate(phi_new(N), x_phi(N),phi_a(N))
 !calculating phi for t = 0
 do i = 1,N
   x_phi(i) = x
   phi_new(i) = phi(x)
   x = x + dx
 end do
 
 open(10, iostat=ios, file="advection.txt", status="new", action='write')
 IF (ios==0) THEN
  do while (t < t1)
     if (u > 0) then
        call FTBS_sub(N,c,phi_new,phi_a)
     end if
     if (u < 0) then
        call FTFS_sub(N,c,phi_new,phi_a)
     end if
     phi_new = phi_a
     t = t + dt
    !writting into a file every tp step
        if (mod(t, tp) <= dt) then
            do i = 1,N
                write(10,*) x_phi(i), phi_new(i)
            end do
        end if
  end do
 else 
    print*,'error, can not write'
 End if
 close(10)
end program ass2
 