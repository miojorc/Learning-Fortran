module primeMaker
    implicit none
    integer, parameter :: I16 = SELECTED_INT_KIND(16)
    public :: primeNumbers, I16
    private

    ! interface
    !     function primeNumbers(k) result(PN)
    !         integer(Kind=(I16)), intent(in) :: k
    !         integer(Kind=(I16)), allocatable :: PN(:)
    !         integer(Kind=(I16)), allocatable :: PNM(:)
    !         integer(Kind=(I16)) :: i, ii, j
    !     end function
    ! end interface
    
contains

    function primeNumbers(k) result(PN)
        implicit none
        integer, parameter :: I16 = SELECTED_INT_KIND(16)
        integer(Kind=(I16)), intent(in) :: k
        integer(Kind=(I16)), allocatable :: PN(:)
        integer(Kind=(I16)), allocatable :: PNM(:) !M stands for Maker
        integer(Kind=(I16)) :: i, ii, j

        allocate(PNM(k)) !F processamento
        j=2
        PNM(1) = 1
        PNM(2) = 2

        primeConstructor: do i = 2, k, 1
            primeComputor: do ii = 2, j, 1
                if((mod(i, PNM(ii)) == 0 .and. i>=PNM(ii)))then
                    cycle primeConstructor
                end if 
            end do primeComputor
            j = j+1
            PNM(j) = i
        end do primeConstructor
        
        !print *, j

        allocate(PN(j))
        do i=1, size(PN), 1
            PN(i) = PNM(i)
        end do
    end function primeNumbers

end module primeMaker