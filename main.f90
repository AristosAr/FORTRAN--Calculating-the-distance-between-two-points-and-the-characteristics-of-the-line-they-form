
program main
    implicit none

    real:: x1, x2, y1, y2, f
    real:: r
    real:: l, b !l=gradient and b=ordinate

    print*, "Give the values of the vector x, x1 and x2"
    read*, x1,x2
    print*, "Give the values of the vector y, y1 and y2"
    read*, y1,y2

    f=r(x1,x2,y1,y2)

    call g(x1,x2,y1,y2,l,b)  !x1,x2,y1,y2: inputs
                             !l,b: output

    write(*,10) "R= ", f
    write(*,10) "Gradient= ", l
    write(*,10) "Ordinate= ", b

10 format (a, 2x, f7.3)

end program

real function r(x1,x2,y1,y2)
real::x1,x2,y1,y2
r=sqrt(((x2-x1)**2)+((y2-y1)**2))
end function

subroutine g(x1,x2,y1,y2,l,b)
real::x1,x2,y1,y2 !input
real::l,b !output

l=((y2-y1)/(x2-x1))
b=y1-l*x1

    !print in subroutine
    !write(*,11) "Gradient= ", l
    !write(*,11) "Ordinate= ", b

!11 format (a, 2x, f7.3)
end subroutine

