Module ks_data_module

  !! Module for unifying interface ro real and complex data
  
  Use         data_module, Only : data, real_data, complex_data

  Implicit None
  
  Type, Public ::  ks_data
     !! Wrapper for data types so that each member of an array of these
     !! things can have a different dynamic type
     Class( data ), Allocatable, Private :: ks
   Contains
     ! Public Methods
     Generic  , Public  :: Operator( * ) => mult_ks_by_ks
     Procedure, Public  :: create
     Generic  , Public  :: put => put_real, put_complex
     Generic  , Public  :: get => get_real, get_complex
     Procedure, Public  :: print
     ! Private implementations
     Procedure, Private :: mult_ks_by_ks
     Procedure, Private :: put_real
     Procedure, Private :: put_complex
     Procedure, Private :: get_real
     Procedure, Private :: get_complex
  End type ks_data

  Private

Contains

  Subroutine create( a, is_complex )

    !! Create A, holding complex data if IS_COMPLEX is true, otherwise real data

    Class( ks_data ), Intent(   Out ) :: a
    Logical         , Intent( In    ) :: is_complex

    If( is_complex ) Then
       Allocate( complex_data :: a%ks )
    Else
       Allocate( real_data :: a%ks )
    End If
    
!!$    Call a%ks%create( is_complex )
    Call a%ks%create
    
  End Subroutine create

  Function mult_ks_by_ks( a, b ) Result( r )

    !! Multiply A by B, returning the result in R

    Implicit None
    Type ( ks_data ), Allocatable :: r
    Class( ks_data ), Intent( In ) :: a
    Type ( ks_data ), Intent( In ) :: b

    Allocate( r )
    Allocate( r%ks, source = a%ks )

    ! This line is F2008 - can;t think of a neat way around it.
    ! Alternatives:
    ! 1) Select Type - trying to avoid this as horrible
    ! 2) Defined, overloaded assignment - horrible and maintenance overhead
    r%ks = a%ks * b%ks
    
  End Function mult_ks_by_ks

  Subroutine put_real( a, v )

    !! Put a real value into the container within A

    Class( ks_data ), Intent( InOut ) :: a
    Real            , Intent( In    ) :: v

    Call a%ks%put( v )
    
  End Subroutine put_real
  
  Subroutine put_complex( a, v )

    !! Put a complex value into the container within A

    Class( ks_data ), Intent( InOut ) :: a
    Complex         , Intent( In    ) :: v

    Call a%ks%put( v )
    
  End Subroutine put_complex
  
  Subroutine get_real( a, v )

    !! Get a real value from the container within A

    Class( ks_data ), Intent( InOut ) :: a
    Real            , Intent(   Out ) :: v

    Call a%ks%get( v )
    
  End Subroutine get_real
  
  Subroutine get_complex( a, v )

    !! Get a complex value from the container within A

    Class( ks_data ), Intent( InOut ) :: a
    Complex         , Intent(   Out ) :: v

    Call a%ks%get( v )
    
  End Subroutine get_complex

  Subroutine print( a, title )

    !! Print the value held within A, with an optional title

    Class( ks_data )    , Intent( In )           :: a
    Character( Len = * ), Intent( In ), Optional :: title

    If( Present( title ) ) Then
       Call a%ks%print( title )
    Else
       Call a%ks%print
    End If
    
  End Subroutine print
  
End Module ks_data_module
