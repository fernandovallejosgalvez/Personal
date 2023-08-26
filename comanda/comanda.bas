%Environ T

%Include EAMITEMR.J86

INTEGER*1 GLOBAL PRT4610.ENABLE, Pos.Chester, Articulo.Chester
Integer*2 Global Contador.Codigos.Chester
String Global Dtv.Chester$(1), nombre.cliente$
real global num.fac, \
            nit.cli, \
            cod.aut
            
Real    Global                  \
        fecha.transac          \fecha de transaccion (fecha actual)

Integer*2 Global								\
				Record.Len           
				
%Include EAMTRANS.J86
%Include EAMTSWKG.J86
%Include EAMTOPTS.J86
%Include EAMASMCT.J86

Sub Graba.Trace.fidcom(texto$) external
String texto
End Sub

Function FORMAT.AMOUNT (AMT1) EXTERNAL
Integer*4 AMT1
End Function

Function Center$(In$,Lth) External
String In$,Center$
Integer Lth
End Function

Sub Elite.PrintLine(texto$,valor,largo,tip.fmt,aline,lf,enfa,est.cr,est.sj,est.di) external
Integer *4 valor
Integer *1 est.cr,est.sj,est.di,largo,tip.fmt,aline,lf,enfa
String texto$
End Sub

! --------------------------------------------------------------------------
! Anulo accion, realizo secuencia borrar y envio al nuevo estado
!  * indic =    0 = despliega mensaje     1 = sin mensaje
! --------------------------------------------------------------------------
SUB SEC.BORRAR( indic, linea1$, linea2$, estado ) EXTERNAL
integer*1 indic			\ 0 = Mensaje detenido
				! 1 = wait
				! 2 = Mensaje con tiempo
				! 3 = Salva , despliega y restaura
integer*2 estado  		! Estado o wait
string linea1$, linea2$
End Sub

!-------------------------------------------------------------------------------
! Funcion que deja la fecha en formato dd/mm/aaaa
!-------------------------------------------------------------------------------
Function formatea.fecha$( fecha$,formato ) external
   string    formatea.fecha$, fecha$
   integer*1 formato
End Function

Sub Imprime.Ticket
Integer*1 Existe
Integer*2 i,j,ue.for
   
   A$ = "U20 UTKCMD NUM.FAC: " + Str$( num.fac )
   Call Graba.Trace.fidcom(A$)
   A$ = "U20 UTKCMD COD.AUT: " + Str$( cod.aut )
   Call Graba.Trace.fidcom(A$)
   A$ = "U20 UTKCMD nombre.cliente: " + nombre.cliente$
   Call Graba.Trace.fidcom(A$)
   A$ = "U20 UTKCMD nit.cli: " + str$( nit.cli )
   Call Graba.Trace.fidcom(A$)
   
   A$ = CHR$(0CH)
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 0, 0 )
     
   A$ = "          COMANDA CHESTER"
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   
   !A$ = "NUMERO DE AUTORIZACION " + Str$( cod.aut )
   !call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 1, 0 )
   A$ = "NUMERO DE FACTURA " + Str$( num.fac )
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 1, 0 )
   A$ = "FECHA: "+formatea.fecha$(Str$(fecha.transac),1)
   Call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 1, 0 )	
 
   If Record.Len = 42 Then Begin
      A$ = "NOMBRE:"+nombre.cliente$
      Call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 1, 0 )	
      Endif \
   Else Begin
    	A$ = "NOMBRE:"+left$(nombre.cliente$,30)
      Call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 1, 0 )	
      A$ = "          "+mid$(nombre.cliente$,31,30)
      Call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 1, 0 )
      Endif
           
   A$ = " "
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   A$ = center$("PEDIDO",38)
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   
   A$ = "DESCRIPCION          CANTIDAD"
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   A$ = String$( 38, "-" )
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   
   For ue.for = 1 To Contador.Codigos.Chester
   
     If Val(Mid$(Dtv.Chester$(ue.for), 31, 5 ) ) > 0 Then Begin
        A$ = Mid$( Dtv.Chester$(ue.for), 13, 18 ) + \
     		  	 "     " + \
             Str$( Val(Mid$(Dtv.Chester$(ue.for), 31, 5 ) ) )
        call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
     
        A$ = "U20 PRINT Dtv.Chester$(ue.for): ue.for " + str$( ue.for ) + " string " + Dtv.Chester$(ue.for)
        Call Graba.Trace.fidcom(A$)
        Endif
      
   Next ue.for
   
   A$ = " "
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   
   A$ = "NO VALIDO COMO FACTURA DE COMPRA" 
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   
   A$ = "FAVOR RECOJA SU PEDIDO" 
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   A$ = "CON EL NUMERO DE FACTURA" 
   call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 1, 0 )
   
   !A$ = CHR$(0CH)
   !call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 0, 1, 0, 0 )
   
   Contador.Codigos.Chester = 0
   
   Exit Sub
     
End Sub

Sub Comanda.Ts02
  Contador.Codigos.Chester = 0
  Dim Dtv.Chester$(500)
  Articulo.Chester = 0
End Sub

Sub Comanda.Ts07

  Contador.Codigos.Chester = 0
  Pos.Chester = 0
  If TO.USEROPTS(6) Then Pos.Chester = 1
  
  If Pos.Chester = 1 Then Begin
     Dim Dtv.Chester$(500)
     A$ = "COMANDA ACTIVA"
     call Elite.PrintLine( A$, 0, 0, 0, 0, 1, 1, 1, 0, 0 )
     A$ = "U07CMD VERSION 7.4"
     Call Graba.Trace.fidcom(A$)
     Endif
   
End Sub

Sub Comanda.Ts08

  A$ = "V 7.5"
  Call Graba.Trace.fidcom(A$)
  A$ = "U08CMD IR.USEREXIT1   : " + Str$( IR.USEREXIT1 )
  Call Graba.Trace.fidcom(A$)
  A$ = "U08CMD IR.ITEMCODE$   : " + Unpack$(IR.ITEMCODE$)
  Call Graba.Trace.fidcom(A$)
  A$ = "U08CMD IR.ITEMNAME$   : " + IR.ITEMNAME$
  Call Graba.Trace.fidcom(A$)
  A$ = "U08CMD SL.IE.QTYORWGT : " + Str$( SL.IE.QTYORWGT )
  Call Graba.Trace.fidcom(A$)
  
  A$ = "----------------------------------------------------------"
  Call Graba.Trace.fidcom(A$)
  
  !If IR.USEREXIT1 = 64 Then Begin
  !	 Contador.Codigos.Chester = Contador.Codigos.Chester + SL.IE.QTYORWGT
  !	 A$ = Right$( "000000000000" + Unpack$(IR.ITEMCODE$), 12 ) + \
  !	      IR.ITEMNAME$ + \
  !	      Right$( "00000" + Str$( SL.IE.QTYORWGT ), 5 )
  !	       
  !	 Dtv.Chester$(Contador.Codigos.Chester) = A$  	 
  !	 Endif \
  !Else Begin
  !	 Call SEC.BORRAR( 0, "CODIGO NO CHESTER", "PRESIONE BORRAR",10)
  !   IR.INDICAT0 = 4
  !   !IR.DEPARTME$ = Pack$( "0000" )
  !	 Endif 
  
  If IR.USEREXIT1 = 64 Then Begin
  	 Articulo.Chester = 1                                        
	   Endif \                                                              
  Else Begin                                                             
	   Call SEC.BORRAR( 0, "CODIGO NO CHESTER", "PRESIONE BORRAR",10)       
     IR.INDICAT0 = 4        
     Articulo.Chester = 0                                             
     !IR.DEPARTME$ = Pack$( "0000" )                                     
	   Endif                                                                

End Sub

Sub Comanda.Ts14
   
   ! Secuencia Anulacion Total se debe dejar en cero el acumulador
   IF TS.IO.KEYS(1) = 70 AND TS.IO.MOTORKEY=81 Then Begin
   	  if mid$( TS.IO.HDR$, 11, 1 ) = "0" Then Begin
         Call SEC.BORRAR( 1, "Requiere", "LLave de Supervisor", 10 )
         Endif \
      Else Begin
      	 Contador.Codigos.Chester = 0
         Dim Dtv.Chester$(500)
   	     Endif
   	  Endif
   
   
End Sub

Sub Comanda.Ts20
Integer*2 i, Cant, ue.for
Integer*1 Flag.Found

  A$ = "U20 TS.LINETYPE                : " + Str$( TS.LINETYPE )
  Call Graba.Trace.fidcom(A$)
  A$ = "U20 TS.LINEDATA                : " + Str$( TS.LINEDATA )
  Call Graba.Trace.fidcom(A$)
  A$ = "U20 Articulo.Chester           : " + Str$( Articulo.Chester )
  Call Graba.Trace.fidcom(A$)
  A$ = "U20CMD Contador.Codigos.Chester: " + Str$( Contador.Codigos.Chester )
  Call Graba.Trace.fidcom(A$)
  A$ = "U20CMD SL.IE.QTYORWGT          : " + Str$( SL.IE.QTYORWGT )
  Call Graba.Trace.fidcom(A$)
  A$ = "U20 Ts.Io.Keys(1)              : " + Str$( Ts.Io.Keys(1) )
  Call Graba.Trace.fidcom(A$)
  A$ = "U20 IR.ITEMCODE$               : " + Unpack$( IR.ITEMCODE$ )
  Call Graba.Trace.fidcom(A$)
  
  If (TS.LINETYPE = 6 AND TS.LINEDATA = 1 ) Then BEGIN
  	 A$ = "U20 LT6 LD1 Contador.Codigos.Chester: " + Str$( Contador.Codigos.Chester )
     Call Graba.Trace.fidcom(A$)
     If Contador.Codigos.Chester > 0 Then Begin
  	    Call Imprime.Ticket
  	    Endif
  	 A$ = "---------------------------------------------------------"
     Call Graba.Trace.fidcom(A$)
  	 Exit Sub
  	 Endif
  	 
  If Articulo.Chester = 0 Then Begin
  	 A$ = "---------------------------------------------------------"
     Call Graba.Trace.fidcom(A$) 
  	 Exit Sub
  	 Endif
  	
  If ( TS.LINETYPE = 0 And TS.LINEDATA = 0 ) Or \
     ( TS.LINETYPE = 1 And TS.LINEDATA = 0 ) Or \
     ( TS.LINETYPE = 1 And TS.LINEDATA = 1 ) Then Begin
     
     If Articulo.Chester = 1 Then Begin
     	    If Contador.Codigos.Chester = 0 Then Begin
     	    	 Gosub Acumula.Codigo  	                      
     	    	 Endif \
     	    Else Begin 	
     	    	 flag.found = 0
     	    	 For ue.for = 1 to Contador.Codigos.Chester
     	  	   
     	  	       A$ = "U20 Unpack$(IR.ITEMCODE$): " + Unpack$(IR.ITEMCODE$)
              	 Call Graba.Trace.fidcom(A$)
     	  	   		 A$ = "U20 Mid$( Dtv.Chester$(i), 1, 12 ): " + Mid$( Dtv.Chester$(ue.for), 1, 12 )
              	 Call Graba.Trace.fidcom(A$)
              	 
     	  	    	 If Unpack$(IR.ITEMCODE$) = Mid$( Dtv.Chester$(ue.for), 1, 12 ) Then Begin
     	  	    	    
     	  	    	    A$ = "U20 Mid$(  Mid$( Dtv.Chester$(i), 31, 5 ): " +  Mid$( Dtv.Chester$(ue.for), 31, 5 )
              	    Call Graba.Trace.fidcom(A$)
              	 
     	  	    	    Cant = Val( Mid$( Dtv.Chester$(ue.for), 31, 5 ) )
     	  	    	    If Ts.Io.Keys(1) = 70 Then \
     	  	    	 	     Cant = Cant - SL.IE.QTYORWGT \	  	    	 
     	  	    	    Else \
     	  	    	 	     Cant = Cant + SL.IE.QTYORWGT
                    
                    A$ = "U20 Cant: " + Str$( Cant )
              	    Call Graba.Trace.fidcom(A$)
              	    
     	  	    	    A$ = Mid$( Dtv.Chester$(ue.for), 1, 12 ) + \        
	                       Mid$( Dtv.Chester$(ue.for), 13, 18 ) + \                                                
	                       Right$( "00000" + Str$( Cant ), 5 )                                                                      
	                  Dtv.Chester$(ue.for) = A$
	                  
	                  Articulo.Chester = 0
	                  
	                  i=Contador.Codigos.Chester
	                  Flag.Found = 1   
	                  
	                  A$ = "U20 Posicion : " + Str$( i )
                    Call Graba.Trace.fidcom(A$)
  
                    A$ = "U20 Dtv.Chester$(i): " + Dtv.Chester$(i)
                    Call Graba.Trace.fidcom(A$)
	                  
     	  	          Endif 
	             
     	  	    Next ue.for
     	  
     	     If Flag.Found = 0 Then Begin
     	     	  Gosub Acumula.Codigo
     	       	Endif
     	     	
     	     Endif 
     	  Endif
     A$ = "---------------------------------------------------------"
     Call Graba.Trace.fidcom(A$)
     Exit Sub	
     Endif
  
  Exit Sub
 
Acumula.Codigo:
  
  A$ = "U20 Acumulo Codigo --------------------"
  Call Graba.Trace.fidcom(A$)
  
  Contador.Codigos.Chester = Contador.Codigos.Chester + 1 
	A$ = Right$( "000000000000" + Unpack$(IR.ITEMCODE$), 12 ) + \        
	     IR.ITEMNAME$ + \                                                
       Right$( "00000" + Str$( SL.IE.QTYORWGT ), 5 )                   
	                                                             
	Dtv.Chester$(Contador.Codigos.Chester) = A$
	Articulo.Chester = 0
	
	A$ = "U20 Contador.Codigos.Chester              : " + Str$( Contador.Codigos.Chester )
  Call Graba.Trace.fidcom(A$)
  
  A$ = "U20 Dtv.Chester$(Contador.Codigos.Chester): " + Dtv.Chester$(Contador.Codigos.Chester)
  Call Graba.Trace.fidcom(A$)
  
  A$ = "U20 Fin Acumulo Codigo ----------------"
  Call Graba.Trace.fidcom(A$)
  
  Return
  
End Sub

Sub Comanda(User) Public
Integer*2 User
 
  If User = 2 And Pos.Chester Then Begin
  	 Call Comanda.Ts02
  	 Endif \
  Else \
  If User = 7 Then Begin
		 Call Comanda.Ts07
		 Endif \
  Else \
  If User = 8 And Pos.Chester Then Begin
  	 Call Comanda.Ts08
  	 Endif \		 
  Else \
  If User = 14 Then Begin
  	 If Pos.Chester = 1 Then Begin
  	 	  Call Comanda.Ts14
  	 	  Endif
  	 Endif \
  Else \
	If User = 20 And Pos.Chester Then Begin
		 Call Comanda.Ts20
		 Endif
	
End Sub
