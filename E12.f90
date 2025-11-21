function triangleTestDivisors(N) result (tn)
    implicit none
    integer, parameter :: I16 = SELECTED_INT_KIND(16)
    integer(kind=I16), intent(in) :: N
    integer(kind=I16) :: tn
    integer(kind=I16) :: i,j,a,k,c
    k = int(N*2*N/50)-10
    i=1
    tn=0
    a=1
    do 
        tn = tn+i
        if ((i >= k))then
            c=int(tn/2)
            j=1
            do while(j<c)
                if(mod(tn, j) == 0)then
                    a=a+2
                    c=int(tn/j)-1
                end if
                j=j+1
            end do
            if(a >= N)then
                print *, tn, "is the first triangle number to have over N divisors, and is the", i, "triangle number"
                exit
            end if
            a=0
        end if
        i=i+1
    end do
end function

program Euler12
    implicit none
    integer, parameter :: I16 = SELECTED_INT_KIND(16)
    integer(kind=I16) :: triangleTestDivisors
    integer(kind=I16) :: k, N
    Write(*,*) "input a N number"
    read *, N
    k = triangleTestDivisors(N)
end program Euler12