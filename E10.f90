program Euler10
    use primeMaker
    implicit none
    integer(Kind=I16) :: sumPrimes
    integer(Kind=I16) :: C, N
    integer(Kind=I16), allocatable :: primes(:)
    integer :: i
    sumPrimes = 0

    print *, "input a number N"
    read *, C

    primes = primeNumbers(C)
    print *, primes

    do i=2, size(primes), 1
        sumPrimes = sumPrimes+primes(i) !C+1 because I, unfortunately, considered 1 a prime number
    end do

    print *, "the sum of primes is", sumPrimes 
end program Euler10