
module LexicalSort

  implicit none

  public :: ch_sort
  private :: partition, quicksort, swap, stringComp, UpperCase

  integer, dimension (:), allocatable, private :: indexarray
  logical, private :: CaseSensitive

contains

!  Subroutine sort uses the quicksort algorithm.
!  On input, StringArray is a one-dimensional array of character strings
!  to be sorted in ascending lexical order.
!  On output, StringArray is the sorted array.
!  If the optional argument CaseInsensitive is present and .true.,
!  the sort is case-insensitive. If CaseInsensitive is absent or
!  if it is .false., the sort is case-sensitive.
!  The characters of the elements of the string array are not modified,
!  so that if blanks or punctuation characters are to be ignored,
!  for instance, this needs to be done before calling sort.

  subroutine ch_sort(StringArray, CaseInsensitive)

    character (len = *), dimension (:), intent (inout) :: StringArray
    logical, intent (in), optional :: CaseInsensitive
    integer :: low, high, ios, k

    if (present(CaseInsensitive)) then
      CaseSensitive = .not. CaseInsensitive
    else
      CaseSensitive = .true.
    end if

    low = 1
    high = size(StringArray)

    allocate(indexarray(high), stat = ios)

    if (ios /= 0) then
      print *, "Error allocating indexarray in LexicalSort::sort"
      stop
    end if

    indexarray = (/ (k, k = low, high) /)

    call quicksort(StringArray, low, high)
    StringArray = StringArray(indexarray)

    deallocate(indexarray, stat = ios)
    if (ios /= 0) then
      print *, "Error deallocating indexarray in LexicalSort::sort"
      stop
    end if

  end subroutine ch_sort

  recursive subroutine quicksort(StringArray, low, high)
    character (len = *), dimension (:), intent (inout) :: StringArray
    integer, intent (in) :: low, high
    integer :: pivotlocation
    if (low < high) then
      call partition(StringArray, low, high, pivotlocation)
      call quicksort(StringArray, low, pivotlocation - 1)
      call quicksort(StringArray, pivotlocation + 1, high)
    end if
  end subroutine quicksort

  subroutine partition(StringArray, low, high, pivotlocation)
    character (len = *), dimension (:), intent (inout) :: StringArray
    integer, intent (in) :: low, high
    integer, intent (out) :: pivotlocation
    integer :: k, lastsmall
    call swap(indexarray(low), indexarray((low + high)/2))
    lastsmall = low
    do k = low + 1, high
      if (stringComp(StringArray(indexarray(k)), StringArray(indexarray(low)))) then
        lastsmall = lastsmall + 1
        call swap(indexarray(lastsmall), indexarray(k))
      end if
    end do
    call swap(indexarray(low), indexarray(lastsmall))
    pivotlocation = lastsmall
  end subroutine partition

  subroutine swap(m, n)
    integer, intent (inout) :: m, n
    integer :: temp
    temp = m
    m = n
    n = temp
  end subroutine swap

  function stringComp(p, q) result(lexicalLess)
    character (len = *), intent (in) :: p, q
    logical :: lexicalLess
    integer :: kq, k
    if (CaseSensitive) then
      lexicalLess = p < q
    else
      kq = 1
      do k = 1, max(len_trim(p), len_trim(q))
        if (UpperCase(p(k:k)) == UpperCase(q(k:k)) ) then
          cycle
        else
          kq = k
          exit
        end if
      end do
      lexicalLess = UpperCase(p(kq:kq)) < UpperCase(q(kq:kq))
    end if
  end function stringComp

  function UpperCase(letter) result(L)
    character (len = *), intent (in) :: letter
    character (len = 1) :: L
    character (len = 26), parameter :: Lower = "abcdefghijklmnopqrstuvwxyz", &
       Upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer :: k
    k = index(Lower, letter)
    if (k > 0) then
      L = Upper(k:k)
    else
      L = letter
    end if
  end function UpperCase

end module LexicalSort 
