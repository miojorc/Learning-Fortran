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

function fibonnaciSequence(k) result(fibonnaci)
    implicit none
    integer, intent(in) :: k
    integer :: last, sLast, current, i
    integer, dimension(int(((k)**0.25))+11) :: fibonnaci
    i=1
    current = 1
    last = 1
    sLast = 0
    fibonnaci(:) = 0
    fibonnaciConstructor: do
        if(current > k) then
            exit fibonnaciConstructor
        end if
        fibonnaci(i) = current
        current = sLast + last
        sLast = last
        last = current
        i = i+1
    end do fibonnaciConstructor
end function fibonnaciSequence

program Euler2 !soma dos números pares da sequencia de fibonnaci até 4*(10**6) (4.000.000)
    implicit none !é tipo um cabeçalho, quase sempre tem
    integer, allocatable :: fibonnaciEven(:)
    integer, allocatable :: fibonnaci(:)
    integer :: i, sum, k
    
    interface
        function fibonnaciSequence(k) result(fibonnaci)
            integer, intent(in) :: k
            integer :: Last, current, i
            integer, dimension(int(((k)**0.25))+11) :: fibonnaci
        end function
    end interface
    sum = 0
    print *, "how many even fibonnaci nunbers you want to sum?"
    read *, k

    fibonnaci = fibonnaciSequence(k)
    fibonnaciEven = fibonnaci
    fibonnaciEven(:) = 0 !muito mal otimizado, mas foi o que consegui pensar
    !print *, fibonnaci

    do i=1, size(fibonnaci)
        if(mod(fibonnaci(i), 2) == 0 .and. fibonnaci(i) /= 0)then
            !print *, fibonnaci(i)
            fibonnaciEven(i) = fibonnaci(i)
        end if
    end do

    do i=1, size(fibonnaciEven)
        sum = sum + fibonnaciEven(i)
    end do
    print *,sum
end program Euler2
