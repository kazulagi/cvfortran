program test
    use cvFortran
    implicit none
    type(CV_) :: cv 
    integer(int32) :: i
    
    call cv%init(width=840,height=840)
    call cv%circle(center=[200,200], color=[300,30,2,0], radius=1000,width=2000)
    
    do i=1,2000
        call cv%putPixel(center=[i,i], color=[255,255,255,255] )
        call cv%putPixel(center=[i-300,i-100], color=[255,255,255,255] )
        call cv%putPixel(center=[-i+300,i-100], color=[255,255,255,255] )
        call cv%putPixel(center=[i-200,-i+1500], color=[0,255,255,255] )
        call cv%show()
        call cv%wait(1)
    enddo
    
    call cv%destroyWindows()
    call cv%clear()
    

end program test