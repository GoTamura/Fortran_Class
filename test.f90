program main
  use point_m
  implicit none

type( point ) :: p
type( point_3d ) :: p3
real :: ans

p % x = 1
p % y = 2

p3 % x = 1
p3 % y = 2
p3 % z = 3

print *, 'Hello World!'

ans = test(p)
ans = test(p3)

! pointの派生typeは渡せるが,他の型は渡せない
! compile error
! ans = test(ans)

contains

real function test(a)
  class(point) a
  call a % hello()
  call point_select_type(a)
  test = a % length()
  print *, test
  test = a % length(1.)
  print *, test
end

end program main
