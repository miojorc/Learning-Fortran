! program Variables
!     implicit none !faz com que as variaveis tenham que ser declaradas e não definidas pela sua letra de início

!     ! variaveis devem ser declaradas antes de seus valores serem deficidos

!     ! "integer" serve para qualquer tipo de número inteiro, se for uma constante pode ser declarada junto
!     integer :: n

!     ! "real" serve para numeros 'quebrados'
!     real :: pi

!     ! "complex" serve para números pares (um real e outro imaginário) ps:n sei onde usa ainda
!     complex :: frequency ! exemplo dado no fortran guide
    
!     ! "character" serve para letras/palavras, coloque um * (vezes) e o número de letras da palavra
!     character *6:: estrela

!     ! "logical" serve para boleanos
!     logical :: funcionou

!     n = 20
!     pi = 3.1415926
!     frequency = (1.0, -0.5)
!     estrela = "mimosa" !pode ser tanto ' quanto "
!     funcionou = .true. !entre pontos, as condiçoes são true(verdadeiro) e false(falso)
! end program Variables

program read_values
    implicit none !é tipo um cabeçalho, quase sempre tem
    real :: x,y,soma,multiplicacao

    print *, "digite dois numeros"
    read (*,*) x,y
    soma = x+y
    multiplicacao= x*y
    print *, "a soma e: ", soma, " e a multiplicacao: ", multiplicacao
end program read_values
