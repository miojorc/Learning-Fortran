program Euler6
    implicit none
    integer :: sumSquare, squareSum, difference
    integer :: N
    integer :: i

    print *, "input a number N"
    read *, N

    sumSquare = 0
    squareSum = 0

    do i=1, N, 1
        sumSquare = sumSquare + i**2
    end do
    print *, "The sum of the squares of the first N natural numbers is", sumSquare

    do i=1, N, 1
        squareSum = squareSum + i
    end do

    squareSum = squareSum**2
    print *, "The square of the sum of the first N natural numbers is", squareSum

    difference = squareSum-sumSquare
    print *, "The difference between the sum of the squares of the first N natural numbers and the square of the sum is", difference

end program Euler6