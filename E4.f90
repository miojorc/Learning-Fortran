function testPalindrome(m1,m2) result (YN)
    integer, intent(in) :: m1,m2
    logical :: YN
    integer :: NT !NT = Number Test
    character(1), allocatable :: palindrome(:), Ipalindrome(:)
    integer :: i, CS, k !CS = Character Size 
    YN = .true.
    k=0

    NT = m1*m2

    do i=1, 6, 1
        if((NT/(10**i) < 1))then
            CS=i
            exit
        end if
    end do

    allocate(palindrome(CS))
    allocate(Ipalindrome(CS))
    palindrome(:) = ""
    Ipalindrome(:) = ""

    do i=1, CS, 1
        write ( palindrome(i), "(I0)" ) int((NT/(10**(CS-i)) - k*(10)))
        k = int(NT/(10**(CS-i)))
    end do

    print *, NT, palindrome

    do i=1, size(palindrome), 1
        Ipalindrome(i) = palindrome(size(palindrome)-(i-1))
    end do
    print *, Ipalindrome, "I P"

    do i=1, size(palindrome), 1
        if(palindrome(i) /= Ipalindrome(i))then
            print *, palindrome(i)," ", Ipalindrome(i)
            YN=.false.
        end if
    end do
end function

program Euler4
    implicit none
    integer :: m1, m2
    integer :: i
    logical testPalindrome
    m1=100
    m2=100

    if(testPalindrome(91,99) .eqv. .true.)then
        print *, 91*99
    end if

    ! do i =1, 899, 1
    !     if(testPalindrome(m1,m2) .eqv. .true.)then
    !         print *, m1*m2
    !         exit
    !     end if
    !     m1 = m1+1
    ! end do
end program Euler4