Module data_module

  Implicit None

  Type, Public, Abstract :: data
     !! Base data type
   Contains
     ! Public Methods
     Generic, Public :: Operator( * ) => multiply
     ! Private implementations
     Procedure( multiply_interface     ), Deferred, Private :: multiply
     Procedure( mult_real_dd_interface ), Deferred, Private :: mult_real_dd
     Procedure( mult_comp_dd_interface ), Deferred, Private :: mult_comp_dd
  End type data

  Type, Public, Extends( data ) :: real_data
     !! Real data type
     Real, Private :: values
   Contains
     Procedure, Private :: multiply     => multiply_real_real
     Procedure, Private :: mult_real_dd => multiply_real_real_dd
     Procedure, Private :: mult_comp_dd => multiply_complex_real_dd
  End type real_data

  Type, Public, Extends( data ) :: complex_data
     !! Complex data type
     Complex, Private :: values
   Contains
     Procedure, Private :: multiply     => multiply_complex_complex
     Procedure, Private :: mult_real_dd => multiply_real_complex_dd
     Procedure, Private :: mult_comp_dd => multiply_complex_complex_dd
  End type complex_data

  Private

  Abstract Interface
     Function multiply_interface( a, b ) Result( r )
       Import data
       Implicit None
       Class( data ), Allocatable  :: r
       Class( data ), Intent( In ) :: a
       Class( data ), Intent( In ) :: b
     End Function multiply_interface
     Function mult_real_dd_interface( b, a ) Result( r )
       Import data
       Import real_data
       Implicit None
       Class( real_data ), Allocatable  :: r
       Class( data      ), Intent( In ) :: b
       Class( real_data ), Intent( In ) :: a
     End Function mult_real_dd_interface
     Function mult_comp_dd_interface( b, a ) Result( r )
       Import data
       Import complex_data
       Implicit None
       Class( complex_data ), Allocatable  :: r
       Class( data         ), Intent( In ) :: b
       Class( complex_data ), Intent( In ) :: a
     End Function mult_comp_dd_interface
  End Interface
 
Contains

  Function multiply_real_real( a, b ) Result( r )

    !! Multiply a real by something

    Implicit None

    Class(      data ), Allocatable :: r

    Class( real_data ), Intent( In ) :: a
    Class(      data ), Intent( In ) :: b

    Allocate( real_data :: r )

    ! Implement double dispatch
    r = B%mult_real_dd( a )

  End Function multiply_real_real
  
  Function multiply_real_real_dd( b, a ) Result( r )

    !! Multiply two real pieces of data together

    Implicit None

    Class( real_data ), Allocatable :: r

    Class( real_data ), Intent( In ) :: b
    Class( real_data ), Intent( In ) :: a

    Allocate( real_data :: r )
    
    r%values = a%values * b%values

  End Function multiply_real_real_dd
  
  Function multiply_complex_real_dd( b, a ) Result( r )

    Implicit None

    Class( complex_data ), Allocatable :: r

    Class(    real_data ), Intent( In ) :: b
    Class( complex_data ), Intent( In ) :: a

    ! Could implement this but for what we want to do ....
    Stop "Error: multiplying a real by a complex"
    
  End Function multiply_complex_real_dd
  
  Function multiply_real_complex_dd( b, a ) Result( r )

    !! Multiply two real pieces of data together

    Implicit None

    Class(    real_data ), Allocatable :: r

    Class( complex_data ), Intent( In ) :: b
    Class(    real_data ), Intent( In ) :: a

    ! Could implement this but for what we want to do ....
    ! also return value a bit of a pain
    Stop "Error: multiplying a complex by a real"
    
  End Function multiply_real_complex_dd

  Function multiply_complex_complex( a, b ) Result( r )

    !! Multiply a complex by something

    Implicit None

    Class(         data ), Allocatable :: r

    Class( complex_data ), Intent( In ) :: a
    Class(         data ), Intent( In ) :: b

    Allocate( complex_data :: r )

    ! Implement double dispatch
    r = B%mult_comp_dd( a )

  End Function multiply_complex_complex
  
  Function multiply_complex_complex_dd( b, a ) Result( r )

    !! Multiply two complex pieces of data together

    Implicit None

    Class( complex_data ), Allocatable :: r

    Class( complex_data ), Intent( In ) :: b
    Class( complex_data ), Intent( In ) :: a

    Allocate( complex_data :: r )
    
    r%values = a%values * b%values
    
  End Function multiply_complex_complex_dd
  
End Module data_module
