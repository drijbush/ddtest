Program test

  Use ks_array_module, Only : ks_array

  Implicit None

  Integer, Parameter :: n_ks = 8
  Logical, Parameter :: KS_COMPLEX = .True.
  
  Complex :: ca, cb
  
  Real :: r1, r2
  Real :: ra, rb
  
  Logical, Dimension( 1:n_ks ) :: ks_type

  Integer :: ks
  
  Type( ks_array ) :: A
  Type( ks_array ) :: B

  Do ks = 1, n_ks
     Call Random_number( r1 )
     ks_type( ks ) = r1 > 0.4
  End Do

  Call A%create( ks_type )
  Call B%create( ks_type )

  Do ks = 1, n_ks
     Call Random_number( r2 )
     If( ks_type( ks ) .Eqv. KS_COMPLEX ) Then
        Call Random_number( r1 )
        Call Random_number( r2 )
        ca = Cmplx( r1, r2 )
        Call A%put( ks, ca )
        Call Random_number( r1 )
        Call Random_number( r2 )
        cb = Cmplx( r1, r2 )
        Call B%put( ks, cb )
        Write( *, * ) 'Correct result #', ks,' = ', ca * cb
     Else
        Call Random_number( ra )
        Call A%put( ks, ra )
        Call Random_number( rb )
        Call B%put( ks, rb )
        Write( *, * ) 'Correct result #', ks,' = ', ra * rb
     End If
  End Do
  Write( *, * )
     
  Call A%print( 'A' )
  Call B%print( 'B' )

  A = A * B

  Write( *, * )
  Call A%print( 'A * B' )

End Program test
