Module data_module

  Implicit None

  Type, Public, Abstract :: data
     !! Base data type
   Contains
     ! Public Methods
     Procedure( create_interface       ), Deferred, Public  :: create
     Procedure( print_interface        ), Deferred, Public  :: print
     Generic                                      , Public  :: put           => put_real, put_complex
     Generic                                      , Public  :: get           => get_real, get_complex
     Generic                                      , Public  :: Operator( * ) => multiply
     ! Private implementations
     Procedure( multiply_interface     ), Deferred, Private :: multiply
     Procedure( mult_real_dd_interface ), Deferred, Private :: mult_real_dd
     Procedure( mult_comp_dd_interface ), Deferred, Private :: mult_comp_dd
     Procedure( put_real_interface     ), Deferred, Private :: put_real
     Procedure( put_complex_interface  ), Deferred, Private :: put_complex
     Procedure( get_real_interface     ), Deferred, Private :: get_real
     Procedure( get_complex_interface  ), Deferred, Private :: get_complex
  End type data

  Type, Public, Extends( data ) :: real_data
     !! Real data type
     Real, Allocatable, Private :: values
   Contains
     ! Public Methods
     Procedure, Public  :: create       => create_real
     Procedure, Public  :: print        => print_real
     ! Private implementations
     Procedure, Private :: multiply     => multiply_real_real
     Procedure, Private :: mult_real_dd => multiply_real_real_dd
     Procedure, Private :: mult_comp_dd => multiply_complex_real_dd
     Procedure, Private :: put_real     => real_put_real
     Procedure, Private :: put_complex  => real_put_complex
     Procedure, Private :: get_real     => real_get_real
     Procedure, Private :: get_complex  => real_get_complex
  End type real_data

  Type, Public, Extends( data ) :: complex_data
     !! Complex data type
     Complex, Allocatable, Private :: values
   Contains
     ! Public Methods
     Procedure, Public  :: create       => create_complex
     Procedure, Public  :: print        => print_complex
     ! Private implementations
     Procedure, Private :: multiply     => multiply_complex_complex
     Procedure, Private :: mult_real_dd => multiply_real_complex_dd
     Procedure, Private :: mult_comp_dd => multiply_complex_complex_dd
     Procedure, Private :: put_real     => complex_put_real
     Procedure, Private :: put_complex  => complex_put_complex
     Procedure, Private :: get_real     => complex_get_real
     Procedure, Private :: get_complex  => complex_get_complex
  End type complex_data

  Private

  Abstract Interface
     Subroutine create_interface( a )
       Import data
       Implicit None
       Class    ( data ), Intent( Out ) :: a
     End Subroutine create_interface
     Subroutine print_interface( a, title )
       Import data
       Implicit None
       Class    ( data    ), Intent( In )           :: a
       Character( Len = * ), Intent( In ), Optional :: title
     End Subroutine print_interface
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
     Subroutine put_real_interface( a, v )
       Import data
       Implicit None
       Class( data ), Intent( InOut ) :: a
       Real         , Intent( In    ) :: v
     End Subroutine put_real_interface
     Subroutine put_complex_interface( a, v )
       Import data
       Implicit None
       Class( data ), Intent( InOut ) :: a
       Complex      , Intent( In    ) :: v
     End Subroutine put_complex_interface
     Subroutine get_real_interface( a, v )
       Import data
       Implicit None
       Class( data ), Intent( In    ) :: a
       Real         , Intent(   Out ) :: v
     End Subroutine get_real_interface
     Subroutine get_complex_interface( a, v )
       Import data
       Implicit None
       Class( data ), Intent( In    ) :: a
       Complex      , Intent(   Out ) :: v
     End Subroutine get_complex_interface
  End Interface
 
Contains

  Subroutine create_real( a )

    Class( real_data ), Intent( Out ) :: a

    Allocate( a%values )

  End Subroutine create_real

  Subroutine create_complex( a )

    Class( complex_data ), Intent( Out ) :: a

    Allocate( a%values )

  End Subroutine create_complex

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

  Subroutine print_real( a, title )

    Class( real_data )  , Intent( In )           :: a
    Character( Len = * ), Intent( In ), Optional :: title

    If( Present( title ) ) Then
       Write( *, * ) title
    End If
    Write( *, * ) a%values

  End Subroutine print_real
  
  Subroutine print_complex( a, title )

    Class( complex_data ), Intent( In )           :: a
    Character( Len = *  ), Intent( In ), Optional :: title

    If( Present( title ) ) Then
       Write( *, * ) title
    End If
    Write( *, * ) a%values

  End Subroutine print_complex

  Subroutine real_put_real( a, v )

    Class( real_data ), Intent( InOut ) :: a
    Real              , Intent( In    ) :: v

    a%values = v
    
  End Subroutine real_put_real
  
  Subroutine real_put_complex( a, v )

    Class( real_data ), Intent( InOut ) :: a
    Complex           , Intent( In    ) :: v

    Stop "Putting a complex into real"
    
  End Subroutine real_put_complex
  
  Subroutine complex_put_complex( a, v )

    Class( complex_data ), Intent( InOut ) :: a
    Complex              , Intent( In    ) :: v

    a%values = v
    
  End Subroutine complex_put_complex
  
  Subroutine complex_put_real( a, v )

    Class( complex_data ), Intent( InOut ) :: a
    Real                 , Intent( In    ) :: v

    Stop "Putting a real into complex"
    
  End Subroutine complex_put_real
  
  Subroutine real_get_real( a, v )

    Class( real_data ), Intent( In    ) :: a
    Real              , Intent(   Out ) :: v

    v = a%values
    
  End Subroutine real_get_real
  
  Subroutine real_get_complex( a, v )

    Class( real_data ), Intent( In    ) :: a
    Complex           , Intent(   Out ) :: v

    Stop "Getting a complex into real"
    
  End Subroutine real_get_complex
  
  Subroutine complex_get_complex( a, v )

    Class( complex_data ), Intent( In    ) :: a
    Complex              , Intent(   Out ) :: v

    v = a%values
    
  End Subroutine complex_get_complex
  
  Subroutine complex_get_real( a, v )

    Class( complex_data ), Intent( In    ) :: a
    Real                 , Intent(   Out ) :: v

    Stop "Getting a real into complex"
    
  End Subroutine complex_get_real
  
End Module data_module
