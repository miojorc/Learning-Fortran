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
!     character *6:: estrela !6 é o tamanho maximo do "nome"

!     ! "logical" serve para boleanos
!     logical :: funcionou

!     n = 20
!     pi = 3.1415926
!     frequency = (1.0, -0.5)
!     estrela = "mimosa" !pode ser tanto ' quanto "
!     funcionou = .true. !entre pontos, as condiçoes são true(verdadeiro) e false(falso)
! end program Variables

program Cone
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none !é tipo um cabeçalho, quase sempre tem
    real(sp) :: R, H, G !R->raio H->altura G-> geratriz
    real(sp) :: pi = 3.1415926
    G = (R**2 +H**2)**(0.5)
    print *, "coloque o raio e a altura do cone"
    read *, R, H
    print *,"o volume e:", R*R*H*3.1415926/3, "a area e:", pi*R*(G+R)
end program Cone
