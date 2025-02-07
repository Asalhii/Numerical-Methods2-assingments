!defining the ODE
REAL(8) FUNCTION F(x,y) RESULT(r)
 real(8), intent(in) :: x , y
 r = -0.5 * y + 4.0 * exp(-0.5 * x) * cos(4.0 * x)
end function
!definig the actule solution
REAL(8) FUNCTION Fs(x) RESULT(rs)
 real(8), intent(in) :: x
 rs = exp(-0.5 * x) * sin(4.0 * x)
end function
!the solution using the euler scheme
REAL(8) FUNCTION euler(x,y,dx) RESULT(re)
 real(8), intent(in) :: x,y,dx
 real(8), external :: F
 re = y + dx * F(x,y)
end function
!the solution using the heun scheme
REAL(8) FUNCTION heun(x,y,dx) RESULT(rh)
 real(8), intent(in) :: x,y,dx
 real(8),external :: F
 real(8) :: f0, ystar, xp
 f0 = F(x,y)
 ystar = y + dx * f0
 xp = x + dx
 rh = y + 0.5 * dx * (f0 + F(xp,ystar))
end function
! a subroutine that calculates the results for both schemes
subroutine integration(x0, y0, dx, xlist, ylist_euler ,ylist_heun)
 real(8), intent(in) :: x0, dx, y0
 real(8), intent(out), dimension(101) :: xlist, ylist_euler, ylist_heun
 integer :: n   
 real(8) :: x, y_e, y_h, y_sol
 real(8), external :: F, Fs, euler, heun
 x = x0
 xlist(1) = x0
 ylist_euler(1) = y0
 ylist_heun(1) = y0
 y_e = y0
 y_h = y0
 do i = 2,101 
    y_e = euler(x,y_e,dx)
    y_h = heun(x,y_h,dx)
    x = x + dx
    y_sol = Fs(x)
    xlist(i) = x
    ylist_euler(i) = y_e - y_sol
    ylist_heun(i) = y_h - y_sol
 end do
end subroutine 

program ass1
 implicit none
 real(8) :: x0, y0, dx
 real(8), dimension(101) ::  xlist, ylist_euler ,ylist_heun
 integer :: i, ios
 !intial conditions
 x0 = 0.0
 y0 = 0.0
 dx = 0.1
 ! calling the subroutine 
 call integration(x0, y0, dx, xlist, ylist_euler ,ylist_heun)
 ! writting the results in a file
  OPEN(10, iostat=ios, FILE='integration.txt', STATUS='replace', ACTION='write')
   IF (ios==0) THEN
    WRITE(10,*) 'values of x','               ', 'values of y_euler(x)' , 'values of y_heu(x)'
    do i=1,101
      WRITE(10,*) xlist(i), ylist_euler(i), ylist_heun(i)
    end do 
   else 
    print*,'error, can not write'
   End if
  CLOSE(10)
end program ass1