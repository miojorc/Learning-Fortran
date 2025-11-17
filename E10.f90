program Euler10
    use SieveOfEratosthenes
    implicit none
    integer(Kind=I16) :: sumPrimes
    integer(Kind=I16) :: C
    integer(Kind=I16), allocatable :: primes(:)
    integer :: i
    sumPrimes = 0

    print *, "input a number N"
    read *, C

    primes = primeNumbers(C)

    do i=1, size(primes), 1
        sumPrimes = sumPrimes+primes(i)
    end do

    print *, "the sum of primes is", sumPrimes 
end program Euler10