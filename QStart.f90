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

program Euler1 !multiplos de 3 ou 5 até 1000
    implicit none !é tipo um cabeçalho, quase sempre tem
    integer :: i, k
    integer, allocatable  :: N(:)
    integer, allocatable :: M3(:), M5(:), M15(:)
    integer :: sum
    print *, "you gonna find the sum of numbers bellow?"
    read *, k
    allocate(N(K))
    N = [(i, i = 1, k)]
    M3 = (N(3:k-1:3))
    M5 = (N(5:k-1:5))
    M15 = (N(15:k-1:15))
    sum=0
    do i = 1, size(M3)
        sum = sum + M3(i)
    end do
    do i = 1, size(M5)
        sum = sum + M5(i)
    end do
    do i = 1, size(M15)
        sum = sum - M15(i)
    end do
    print *,"multiples of three below", K ,": "
    print *,M3
    print *,"multiples of five below", K ,": "
    print *,M5
    print *,"multiples of three and five below", K ,": "
    print *,M15
    print *, "sum of multiples of three or five below ", K ,":", sum
end program Euler1
