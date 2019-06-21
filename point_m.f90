module point_m
  implicit none 

  public :: point
  type point
    real x,y

  contains
    procedure :: hello => point_hello
    procedure :: length => point_length
  end type point

  public :: point_3d
  ! 継承
  type,extends(point) :: point_3d
    real z

  contains
    procedure :: length => point_3d_length
  end type point_3d

contains
  subroutine point_hello(this)
    implicit none
    ! 親のtypeを指定することで派生クラスを受け取ることができる
    ! それ以外の型はコンパイルエラー
    class(point) this
    print *, 'hello'
  end subroutine point_hello


  ! オーバーライド 
  ! 引数と返り値を合わせる必要があるので, 特定の関数だけで使う場合はoptionalで宣言する
  ! presentでoptinal変数が存在するかを確認する
  function point_length(p, opt)
    implicit none
    class (point) p
    real, optional :: opt
    real :: point_length
    point_length = sqrt(p % x * p % x + p % y + p % y)
  end function point_length

  function point_3d_length(p, opt)
    implicit none
    class (point_3d) p
    real, optional :: opt
    real :: point_3d_length

    if (present(opt)) then
      point_3d_length = sqrt(p % x * p % x + p % y + p % y + p % z * p % z) + opt
    else
      point_3d_length = sqrt(p % x * p % x + p % y + p % y + p % z * p % z)
    endif

  end function point_3d_length

  ! 型ごとに実行時に動作を変化させる
  subroutine point_select_type( p )
    implicit none
    class(point) p
    ! 
    select type(a => p)
      type is (point)
        print *, 'this is point'
        print *, a
      type is (point_3d)
        print *, 'this is point_3d'
        print *, a
    end select
  end subroutine point_select_type
end module point_m
