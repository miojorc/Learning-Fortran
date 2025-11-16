program Euler5
    implicit none
    integer :: i, j, n, result
    integer, allocatable :: multipleCreator(:), organizer(:) ! numbersNB = numbers below number
    logical, allocatable :: KM(:)
    integer :: PN, PrN, TP !PN = possible number PrN = previous number TP = tests passed
    logical :: DN !DN = distinct numbers

    result = 1
    print *, "input a number"
    read *, n

    n = 20
    PN = 0
    PrN = 0
    TP = 0
    DN = .false.
    allocate(multipleCreator(n))
    allocate(organizer(n))
    allocate(KM(n))
    KM(:) = .true.
    multipleCreator(:) = n+1
    organizer(:) = n+1

    do i=1, n, 1
        multipleCreator(i) = i
        PN = i

        do j=1, i-1, 1
            if(mod(multipleCreator(i), multipleCreator(j)) == 0 .and. multipleCreator(j) /= 1)then
                if(mod(multipleCreator(i), multipleCreator(j)**2) == 0)then
                    PN = multipleCreator(j)
                end if
                if(multipleCreator(j) /= PrN .and. TP >=1)then
                    DN = .true.
                end if
                PrN = multipleCreator(j)
                TP = TP+1
            end if
        end do

        if(DN .eqv. .true.)then
            multipleCreator(i) = 1
        else 
            multipleCreator(i) = PN
        end if

        result = result*multipleCreator(i)
        !print *, multipleCreator, result

        KM(:) = .true.
        do j=1, i, 1
            organizer(j) = minval(multipleCreator, KM)
            KM(minloc(multipleCreator,KM)) = .FALSE.
        end do
        multipleCreator = organizer
        DN = .false.
        TP = 0
    end do
    
    print *, result, "that's the smallest that can by divided by each number from 1 to ", N
end program Euler5