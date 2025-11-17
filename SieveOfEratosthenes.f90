module SieveOfEratosthenes
    implicit none
    integer, parameter :: I16 = SELECTED_INT_KIND(16)
    public :: primeNumbers, I16
    private

contains
    function primeNumbers(k) result(PN)
        implicit none
        integer, parameter :: I16 = SELECTED_INT_KIND(16)
        integer(Kind=(I16)), intent(in) :: k
        integer(Kind=(I16)), allocatable :: PN(:)
        integer(Kind=(I16)), allocatable :: AN(:) !All numbers below k
        integer(Kind=(I16)) :: i, j, B
        B=0

        allocate(AN(k-1))
        do i=2, k, 1
            AN(i-1) = i
        end do

        do i=2, k, 1
            if(AN(i-1) /= -1)then
                do j=(i*2), k, i
                    AN(j-1) = -1
                end do
            end if
        end do

        do i=1, k, 1
            if(AN(i) /= -1)then
                B=B+1
            end if
        end do

        allocate(PN(B-1))

        B=0

        do i=1, size(AN), 1
            if(AN(i) /= -1)then
                B=B+1
                PN(B) = AN(i)
            end if
        end do
    end function

end module