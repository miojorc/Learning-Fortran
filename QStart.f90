! program Variables
!     implicit none !faz com que as variaveis tenham que ser declaradas e não definidas pela sua letra de início
!     use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64 !defino os "bytes"
!     ! variaveis devem ser declaradas antes de seus valores serem deficidos

!     ! "integer" serve para qualquer tipo de número inteiro, se for uma constante pode ser declarada junto
!     integer :: n

!     ! "real" serve para numeros 'quebrados'
!     real :: pi

!     ! "complex" serve para números pares (um real e outro imaginário) ps:n sei onde usa ainda
!     complex :: frequency ! exemplo dado no fortran guide
    
!     ! "character" serve para letras/palavras, coloque um * (vezes) e o número de letras da palavra
!     character *6:: estrela !6 é o tamanho maximo do "nome"

!     ! "logical" serve para boleanos
!     logical :: funcionou

!     n = 20
!     pi = 3.1415926
!     frequency = (1.0, -0.5)
!     estrela = "mimosa" !pode ser tanto ' quanto "
!     funcionou = .true. !entre pontos, as condiçoes são true(verdadeiro) e false(falso)
! end program Variables

function primeNumbers(k) result(PN)
    implicit none
    integer, parameter :: TEST = SELECTED_INT_KIND(16)
    integer(Kind=(TEST)), intent(in) :: k
    integer(Kind=(TEST)), allocatable :: PN(:)
    integer(Kind=(TEST)), allocatable :: PNM(:) !M stands for Maker
    integer(Kind=(TEST)) :: i, ii, j

    allocate(PNM(k))!F processamento
    j=2
    PNM(1) = 1
    PNM(2) = 2

    primeConstructor: do i = 2, k, 1
        primecomputor: do ii = 2, j, 1
            if((mod(i, PNM(ii)) == 0 .and. i>=PNM(ii)))then
                cycle primeConstructor
            end if 
        end do primecomputor
        !if(mod(j,1000) == 0)then
            !print *, "to em", i
        !end if 
        j = j+1
        PNM(j) = i
    end do primeConstructor
    print *, j

    allocate(PN(j))
    do i=1, size(PN), 1
        PN(i) = PNM(i)
    end do
end function primeNumbers

function factors(N,D) result(F) !Faz isso
    implicit none
    integer, parameter :: TEST = SELECTED_INT_KIND(16)
    integer(Kind=(TEST)), intent(in) :: n
    integer(Kind=(TEST)), allocatable, intent(in) :: D(:)
    integer(Kind=(TEST)), allocatable :: F(:)
    integer(Kind=(TEST)), allocatable :: FM(:)
    integer(Kind=(TEST)) :: i,j
    allocate(FM(size(D)))
    FM(:) = 0
    j=0

    do i=1, size(D), 1
        if(mod(n, D(i))  == 0 .and. n>D(i))then
            j=j+1
            FM(j) = D(i)
        end if
    end do

    allocate(F(j))
    do i=1, size(F), 1
        F(i) = FM(i)
    end do
end function

program Euler3 !maior número primo divisor de 600851475143, de 13195 é 5, 7, 13, 29, eu adicionei 1, pq sim
    implicit none
    integer, parameter :: TEST = SELECTED_INT_KIND(16)
    integer(Kind=(TEST)) :: number
    integer(Kind=(TEST)), allocatable :: primes(:), primeFactors(:)
    interface
        function primeNumbers(k) result(PN)
            integer, parameter :: TEST = SELECTED_INT_KIND(16)
            integer(Kind=(TEST)), intent(in) :: k
            integer(Kind=(TEST)), allocatable :: PN(:)
            integer(Kind=(TEST)), allocatable :: PNM(:)
            integer(Kind=(TEST)) :: i, ii, j
        end function
    end interface
    interface 
        function factors(N,D) result(F)
            integer, parameter :: TEST = SELECTED_INT_KIND(16)
            integer(Kind=(TEST)), intent(in) :: n
            integer(Kind=(TEST)), allocatable ,intent(in) :: D(:)
            integer(Kind=(TEST)), allocatable :: F(:)
            integer(Kind=(TEST)) :: i,j
        end function
    end interface

    number = 600851475143_TEST
    primes = primeNumbers(int8(number**0.5))
    print *, factors(number, primes)
end program Euler3
