module cvfortran
    use,intrinsic :: iso_fortran_env
    use,intrinsic :: iso_c_binding
    implicit none
    !This library consists of
    
    !(1) https://github.com/sage-git/forcv
    !(2) original wrapper

    ! >>> cited from forCV
    type,bind(C) :: CvMat
        integer(c_int) :: type
        integer(c_int) :: step
        type(c_ptr) :: refcount
        integer(c_int) :: hdr_refcount
        type(c_ptr) :: data
        integer(c_int) :: rows
        integer(c_int) :: cols
    end type

    type,bind(C) :: CvSize
        integer(c_int) :: width
        integer(c_int) :: height
    end type
    
    type,bind(C) :: CvScalar
        real(c_double) :: val(4)
    end type
    
    type,bind(C) :: CvPoint
        integer(c_int) :: x
        integer(c_int) :: y
    end type
    
    integer(c_int), parameter :: CV_16S = 3
    integer(c_int), parameter :: CV_16U = 2
    integer(c_int), parameter :: CV_32F = 5
    integer(c_int), parameter :: CV_32S = 4
    integer(c_int), parameter :: CV_64F = 6
    integer(c_int), parameter :: CV_8S = 1
    integer(c_int), parameter :: CV_8U = 0
    integer(c_int), parameter :: CV_CN_SHIFT = 3
    integer(c_int), parameter :: CV_DEPTH_MAX = ishft(1, CV_CN_SHIFT)
    integer(c_int), parameter :: CV_DEPTH_MASK = CV_DEPTH_MAX - 1

    interface
        function cvCreateMat(rows, cols, type) bind(C, name="cvCreateMat")
            import :: c_ptr, c_int
            type(c_ptr) :: cvCreateMat
            integer(c_int), value :: rows, cols, type
        end function cvCreateMat
    
        subroutine cvReleaseMat(mat) bind(C, name="cvReleaseMat")
            import :: c_ptr
            type(c_ptr) :: mat
        end subroutine cvReleaseMat
        subroutine cvSet(arr, val, mask) bind(C, name="cvSet")
            import :: c_ptr, CvScalar
            type(c_ptr),value :: arr, mask
            type(CvScalar),value :: val
        end subroutine cvSet
    end interface


    interface
        subroutine cvShowImage(name, image) bind(C, name="cvShowImage")
            import :: c_ptr, CvMat
            character :: name(*)
            type(c_ptr),value :: image
        end subroutine cvShowImage
        function cvWaitKey(delay) bind(C, name="cvWaitKey")
            import :: c_int
            integer(c_int),value :: delay
            integer(c_int) :: cvWaitKey
        end function cvWaitKey
        subroutine cvDestroyAllWindows() bind(C, name="cvDestroyAllWindows")
        end subroutine cvDestroyAllWindows
    end interface

    interface
        subroutine cvCircle(img, center, radius, color, thickness, line_type, shift) bind(C, name="cvCircle")
            import :: c_ptr, c_int, CvPoint, CvScalar
            type(c_ptr), value :: img
            type(CvPoint), value :: center
            type(CvScalar), value :: color
            integer(c_int), value :: radius, thickness, line_type, shift
        end subroutine cvCircle
    end interface

    ! <<< cited from forCV

    ! >>> original wrapper
    type ::CV_
        type(c_ptr) :: p_img
        type(CvScalar) :: v, color
        type(CvPoint) :: center
        integer(c_int) :: W, H, ret
    contains
        procedure,public :: init => initCV
        procedure,public :: circle => circleCV
        procedure,public :: putPixel => putPixelCV
        procedure,public :: show => showCV
        procedure,public :: wait => waitCV
        procedure,public :: destroyWindows => destroyWindowsCV
        procedure,public :: clear => clearCV
    end type 
    ! <<< original wrapper

contains
    ! >>> cited from forCV
    function CV_MAKETYPE(depth, cn)
        integer(c_int), intent(in):: depth, cn
        integer(c_int) :: CV_MAKETYPE
        CV_MAKETYPE = iand(depth, CV_DEPTH_MASK) + ishft(cn - 1, CV_CN_SHIFT)
    end function CV_MAKETYPE
    ! <<< cited from forCV


    ! >>> original wrapper
    subroutine initCV(this,Width,Height)
        class(CV_),intent(inout) :: this
        integer(int32),intent(in) :: Width,Height
        
        this%p_img = cvCreateMat(Height, Width, CV_MAKETYPE(CV_64F, 3))
        this%v%val = 0.0d0
        this%color%val = [0.d0, 1.d0, 0.d0, 0.d0]
        this%center%x = Width/2
        this%center%y = Height/2
        
        call cvSet(this%p_img, this%v, c_null_ptr)

    end subroutine

    subroutine circleCV(this,center,color,radius,width)
        class(CV_),intent(inout) :: this
        integer(int32),intent(in) :: center(2),radius,width
        type(CvPoint) :: center_cv
        integer(int32),intent(in) :: color(4)
        type(CvScalar) :: color_cv
        
        center_cv%x = center(1)
        center_cv%y = center(2)
        color_cv%val = dble(color(1:4))
        call cvCircle(this%p_img, center_cv, radius, color_cv, width, 1, 0)

    end subroutine


    subroutine putPixelCV(this,center,color)
        class(CV_),intent(inout) :: this
        integer(int32),intent(in) :: center(2)
        type(CvPoint) :: center_cv
        integer(int32),intent(in) :: color(4)
        type(CvScalar) :: color_cv
        
        center_cv%x = center(1)
        center_cv%y = center(2)
        call cvCircle(this%p_img, center_cv, 1, color_cv, 1, 1, 0)
        
    end subroutine

    subroutine showCV(this)
        class(CV_),intent(in) :: this
        call cvShowImage("hoge", this%p_img)
    end subroutine

    subroutine waitCV(this,duration)
        class(CV_),intent(in) :: this
        integer(int32),intent(in) :: duration
        integer(c_int) :: ret, dr
        dr = duration
        ret = cvWaitKey(dr)

    end subroutine

    subroutine destroywindowsCV(this)
        class(CV_),intent(in) :: this
        call cvDestroyAllWindows()
    end subroutine

    subroutine clearCV(this)
        class(CV_),intent(inout) :: this

        call cvReleaseMat(this%p_img)

    end subroutine
    ! <<< original wrapper

end module cvfortran