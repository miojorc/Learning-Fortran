program Euler7
    use primeMaker
    implicit none
    integer(Kind=I16), allocatable :: primes(:)
    integer(Kind=I16) :: N, C

    print *, "input a number N"
    read *, C

    allocate(primes(C))
    N = int(((C**1.5)/3) + 30)

    primes(:) = 0
    primes = primeNumbers(N)
    print *, "the N'st prime is", primes(C+1) !C+1 because I, unfortunately, considered 1 a prime number
    
end program Euler7