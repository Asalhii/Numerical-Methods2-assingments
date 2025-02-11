!defining a functin 
REAL(8) FUNCTION phi(x) RESULT(r)
 real(8), intent(in) :: x
 if (x < 200.0) r = 0.1
 if (x >= 200.0 .and. x < 250.0) r = 2.0
 if (x >= 250.0 .and. x <= 300.0) r = 1.0
 if (x > 300.0) r = 0.1  
end function
! a subroutine for the FTFS scheme
subroutine FTFS_sub(N,c,phi_now,phi_a)
 integer,intent(in) :: N
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
 integer,intent(in) :: N
 real(8), intent(in) :: c
 real(8), intent(in), dimension(N) :: phi_now
 real(8), intent(out), dimension(N) :: phi_a
 do i = 2,N
    phi_a(i) = (1.0 - c) * phi_now(i)  + c * phi_now(i-1)
 end do
 phi_a(1) = phi_now(N)
end subroutine

subroutine ctcs_sub(N,c,phi_old,phi_now,phi_a)
 integer,intent(in) :: N
 real(8), intent(in) :: c
 real(8), intent(in), dimension(N) :: phi_now,phi_old
 real(8), intent(out), dimension(N) :: phi_a
 do i = 2,N-1
    phi_a(i) = phi_old(i) - c * (phi_now(i+1) - phi_now(i-1))
 end do
  phi_a(1) = phi_old(N) - c * (phi_now(3) - phi_now(1))
  phi_a(N) = phi_old(1) - c * (phi_now(N) - phi_now(N-2))
end subroutine

subroutine filter(alpha,beta,N,phi_old,phi_now,phi_new,phi_old_o,phi_now_o)
 integer,intent(in) :: N
 real(8), intent(in) :: alpha, beta
 real(8), intent(in), dimension(N) :: phi_now,phi_old,phi_new
 real(8), intent(out), dimension(N) :: phi_old_o,phi_now_o
 real(8) , dimension(N) :: d
 do i = 1,N
   d(i) = alpha * (phi_old(i) + phi_new(i) - 2.0 * phi_now(i))
  end do
  do i = 1,N
   phi_old_o(i) = phi_now(i) + beta * d(i)
   phi_now_o(i) = phi_new(i) + (1.0 - beta) * d(i)
 end do
end subroutine
 

program ass03
 implicit none
 real(8) :: x0, x1, dx, dt, u, c, x, t, t1, tp, alpha,beta
 real(8), dimension(:), allocatable :: phi_new , x_phi, phi_a, phi_old, phi_now,d,phi_old_o,phi_now_o,phi_b
 real(8), external :: phi
 integer :: i, N, ios,j
 u = -0.31
 dx = 0.1  
 dt = 0.1 
 x0 = 0.0  
 x1 = 500.0  
 t = 0.0  
 t1 = 1000.0  
 tp = 200.0
 alpha = 0.05
 beta = 0.53
 c = u * dt / dx
 N = nint((x1 - x0) / dx) + 1
 allocate(phi_new(N), x_phi(N),phi_a(N),phi_now(N),phi_old(N),d(N),phi_old_o(N),phi_now_o(N),phi_b(N))

 do i = 1,N
   x_phi(i) = x
   phi_old(i) = phi(x)
   x = x + dx
 end do
 if (u > 0) then
      call FTBS_sub(N,c,phi_old,phi_a)
 end if
 if (u < 0) then
      call FTFS_sub(N,c,phi_old,phi_a)
 end if
 phi_now = phi_a
 t = t + dt
 open(10, iostat=ios, file="advection02.txt", status="replace", action='write')
 IF (ios==0) THEN
 do i = 1,N
   write(10,*) x_phi(i), phi_now(i)
 end do
  do while (t < t1)
    call ctcs_sub(N,c,phi_old,phi_now,phi_b)
    phi_new = phi_b
    call filter(alpha,beta,N,phi_old,phi_now,phi_new,phi_old_o,phi_now_o)
    phi_old = phi_old_o
    phi_now = phi_now_o
    t = t + dt
    if (mod(t, tp) < dt) then
      do j = 1,N
         write(10,*) x_phi(j), phi_new(j)
      end do
    end if
   end do
 else 
    print*,'error, can not write'
 End if
 close(10)
 
























end program ass03