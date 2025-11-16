program Euler9
    implicit none
    integer :: a,b
    real :: c
    integer :: sum
    integer :: i,j

    print *, "input the sum of Pythagorean triplet"
    read *, sum

    aValue: do i=1, sum, 1
        a=i
        do j=1, sum, 1
            b=j
            c = ((a**2) + (b**2))**0.5
            if(c+a+b == sum)then
                print *, "A =",a
                print *, "B =",b
                print *, "C =",int(c)
                print *, "A*B*C=",a*b*int(c)
                exit aValue
            end if
        end do
    end do aValue
end program Euler9