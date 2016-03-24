' Name : Sub_routine
' Author : Louis de Charsonville
' Date created : 19/11/2015
' Last modified : 21/01/2015  ---- Change %debcontrib to prevent sample errors if user entered %debcontrib
' Description : calculate the infinite moving average decomposition of the ratio B(L)/A(L) where A,B are two finite polynoms


subroutine inf_ma(string %vector_A, string %vector_B, string %vector_C)
	'INPUT 
	' %vector_A : vector of coefficient if A=1+2x+3x^2 then vector_A=(1,2,3)
	' %vector_B : vector of coefficient of B
	' %vector_C : unfilled vector which the program fills with the "dim of %vector_C" first terms of B(L)/A(L)
	
	'OUTPUT 
	'%vector_C filled with the first "dim of %vector_C" of the ma decomposition of B/A
	
	'data
	!degree_A=@rows({%vector_A})-1
	!degree_B=@rows({%vector_B})-1
	!degree_C=@rows({%vector_C})-1
	
	'vector({%nb_terms}) {%vector_C}

	'calculations
	{%vector_C}(1)={%vector_B}(1)
	if !degree_C>!degree_B then 
		for !i=1 to !degree_B
			!buffer=0 
			!end_loop=@round(1/2*(-@abs(!degree_A-!i)+!degree_A+!i)) '=min(degree_A, i-1)
			for !j=1 to  !end_loop
				!buffer=!buffer+{%vector_A}(!j+1)*{%vector_C}(!i-!j+1)
			next
			{%vector_C}(!i+1)={%vector_B}(!i+1)-!buffer
		next
		for !i=!degree_B+1 to !degree_C 
			!buffer=0
			for !j=1 to 1/2*(-@abs(!degree_A-!i)+!degree_A+!i)
				!buffer=!buffer+{%vector_A}(!j+1)*{%vector_C}(!i-!j+1)
			next
			{%vector_C}(!i+1)=-!buffer
		next
	else
		for !i=1 to !degree_C
			!buffer=0 
			for !j=1 to 1/2*(-@abs(!degree_A-!i)+!degree_A+!i)
				!buffer=!buffer+{%vector_A}(!j+1)*{%vector_C}(!i-!j+1)
			next
			{%vector_C}(!i+1)={%vector_B}(!i+1)-!buffer
		next
	endif
endsub


subroutine flipud(string %vect)
	' flip up down a given vector so that new_v(i) = old_v(n-i+1) avec n=length(old_v)
	
	'data
	!dim_vect=@rows({%vect})
	vector(!dim_vect) v_temp
	'computation
	for !k_flip=1 to !dim_vect
		v_temp(!k_flip)={%vect}(!dim_vect-!k_flip+1)
	next
	{%vect}=v_temp
	delete v_temp
endsub



subroutine contrib(string %var_x, string %vect_a, string %vect_b, string %deb_contrib, string %fin_contrib)
	' We have a(L)*y=b(L)*x => we want to compute the econometric contribution of dx on dy.
	
	' INPUT
	' %var_x : variable of which we compute the econometric contribution
	' %vect_a : vector of coefficient of A
	' %vect_b : vector of coefficient of B
	' %deb_contrib : date to begin the computation of econometric contributions
	' %fin_contrib : last date of econometric contributions
	
	' OUTPUT 
	' contrib_var_x : timeseries of econometric contributions
	
	'data
	smpl @all
	!first_obs=@ifirst({%var_x})+1
	%date=@otod(!first_obs)
	series myD_var_x=d({%var_x})
	'If debcontrib is before the date of the first available data then we change %debcontrib accordingly
	if @dtoo(%deb_contrib)<!first_obs then
		%deb_contrib=%date
	endif
	
	smpl %deb_contrib %fin_contrib
	genr contrib_{%var_x}=0
	!nb_iter=@obssmpl
	
	'computation
	for !kk=0 to !nb_iter-1
		smpl %date %deb_contrib+!kk
		vector vect=@convert(myD_var_x)
		call flipud("vect") 'we flip up down vect so that the most recent value appears first in the vector
		vector(@rows(vect)) vect_c 'vector of "infinite" moving average
		rowvector tvect=@transpose(vect)
		call inf_ma(%vect_a,%vect_b,"vect_c")
		!buffer=tvect*vect_c
		delete vect tvect
		smpl %deb_contrib+!kk %deb_contrib+!kk
		series contrib_{%var_x}=!buffer
	next
	delete myD_var_x
	'smpl @all
	'genr contrib_{%var_x}=d(contrib_{%var_x}_0)
	'delete contrib_{%var_x}_0
endsub







'#################################################################


subroutine contrib_d(string %var_x, string %vect_a, string %vect_b, string %deb_contrib, string %fin_contrib)
	' We have a(L)*dy=b(L)*dx => we want to compute the econometric contribution of dx on dy.
	
	' INPUT
	' %var_x : variable of which we compute the econometric contribution
	' %vect_a : vector of coefficient of A
	' %vect_b : vector of coefficient of B
	' %deb_contrib : date to begin the computation of econometric contributions
	' %fin_contrib : last date of econometric contributions
	
	' OUTPUT 
	' contrib_var_x : timeseries of econometric contributions
	
	'data
	'Preambule
	smpl %deb_contrib %fin_contrib
	genr contrib_{%var_x}=0
	!nb_iter=@obssmpl	
	smpl @all
	!first_obs=@ifirst({%var_x})
	%date=@otod(!first_obs)
	
	'If debcontrib is before the date of the first available data then we change %debcontrib accordingly
	if @dtoo(%deb_contrib)<!first_obs then
		%deb_contrib=%date
	endif
	
	'computation
	for !kk=0 to !nb_iter-1
		smpl %date %deb_contrib+!kk
		vector vect=@convert({%var_x})
		call flipud("vect") 'we flip up down tvect so that the most recent value appears first in the vector
		vector(@rows(vect)) vect_c 'vector of "infinite" moving average
		rowvector tvect=@transpose(vect)
		call inf_ma(%vect_a,%vect_b,"vect_c")
		!buffer=tvect*vect_c
		delete vect tvect
		smpl %deb_contrib+!kk %deb_contrib+!kk
		series contrib_{%var_x}=!buffer
	next
endsub



subroutine balance_identity(string %var_y, string %group_x,string %prefix)
	' Description : 
	' Balance identity will create new variables from variables of %group_x so that sum of these variables equals %y.
	' The new variables are create using group_x variables and split the discrepancy proportionnaly to the absolute value of each contribution
	
	' INPUT
	' %var_y : a variable
	' %group_x : a group of variable
	' %prefix : prefix of the new variables created
	
	' OUTPUT 
	' a group composed by prefix_x variables that sum up to %var_y
	
	' computation
	smpl @otod(@ifirst({%var_y})) @otod(@ifirst({%var_y}))
	genr epsilon={%var_y}
	genr abs_x 'sum of absolute values of variables in group_x
	' we compute epsilon = y - sum of var in group_x
	for !i=1 to {%group_x}.@count
		%name={%group_x}.@seriesname(!i)
		series epsilon=epsilon-{%name}
		series abs_x=abs_x+@abs({%name})
	next
	for !i=1 to {%group_x}.@count
		%name={%group_x}.@seriesname(!i)
		genr {%prefix}_{%name}={%name}+epsilon*@abs({%name})/abs_x
	next
endsub


subroutine calcul_contrib(string %equation, string %debEstim, string %finEstim, string %debContrib, string %finContrib,string %ecm, string %graph)
	
	' Pour toute équation de la forme a(L)y=b(L)x, la subroutine calcul les contributions de dx à dy
	
	
	' INPUT 
	' %equation : list of spaced variable. The first one should be the endogenous. Example d(y) y(-1) x(-1) d(x(-1))
	' log are currently not supported so dlog(x) will not work. 
	' %debEstim : date to begin estimation
	' %finEstim : end of estimation
	' %debContrib : date to begin the computation of econometric contributions
	' %finContrib : last date of econometric contributions
	' %ecm : does the equation got a long-term component ? -> yes : 1 // no : 0
	' %graph : include a graph -> include a graph : 1 // do not include a graph : 0
	

	
	
	!_nbwords=@wcount({%equation})
	svector vecteurvar=@wsplit({%equation})
	matrix(!_nbwords,2) _infovar0 'ligne : liste des variables \\ colonne : diff (0: non, 1:oui) / retard   
	for !_kk=1 to !_nbwords
		%mot=vecteurvar(!_kk)
		if @upper(@left(%mot,2))="D(" then
			%mot=@right(%mot,@len(%mot)-2)
			%mot=@left(%mot,@len(%mot)-1)
			_infovar0(!_kk,1)=1
		endif
		for !_retard=1 to 99
			%_compar="(-"+@str(!_retard)+")"
			if @upper(@right(%mot,4))=%_compar then
				%mot=@left(%mot,@len(%mot)-4)
				_infovar0(!_kk,2)=!_retard
			endif
		next
		vecteurvar(!_kk)=%mot
	next
	%listvar0=@wjoin(vecteurvar)
	%listvar=@wunique(%listvar0)
	'string _listvar=%listvar

	' Pour chaque variable, on créé une liste (un vecteur) comportant les positions de la variable dans l'équation
	!_nbvar=@wcount(%listvar)
	for !_kk=1 to !_nbvar
		%var=@word(%listvar,!_kk)	
		!_nboccur=@wcount(@wkeep(%listvar0,%var)) 'nombre d'occurences de la variable %var	
		vector(!_nboccur) _infovariable{!_kk} 'on stocke dans ce vecteur les positions de la variable dans _infovar0
		!_compt=1
		for !_kkk=1 to @wcount(%listvar0)
			%var0=@word(%listvar0,!_kkk)
			if %var=%var0 then
				_infovariable{!_kk}(!_compt)=!_kkk
				!_compt=!_compt+1
			endif
		next
	next

	' Estimation par OLS
	smpl {%debEstim} {%finEstim}
	'coef(!_nbwords) _ccc
	equation _eqols_.ls {{%equation}} c
	 _eqols_.makeresids _mesResidus
	'------------------------------

	'Si %finContrib est superieure à %finEstim prolonger les résidus à 0
	if @dtoo(%finContrib)>@dtoo(%finEstim) then
		smpl %finEstim+1 %finContrib
		series _mesResidus=0
	endif

	'On crée la liste des retards max
	for !_kk=1 to !_nbvar
		%var=@word(%listvar,!_kk)
		vector(@rows(_infovariable{!_kk})) _maxlag{!_kk}v
		for !_kkk=1 to @rows(_infovariable{!_kk})
			_maxlag{!_kk}v(!_kkk)=_infovar0(_infovariable{!_kk}(!_kkk),2)
		next
		!_maxlag{!_kk}=@max(_maxlag{!_kk}v)
	next
	
	vector(!_maxlag1+2) _vecEndog=0
	_vecEndog(1)=1
	if _infovar0(1,1)=1 then _vecEndog(2)=-1 endif
	if @rows(_infovariable1)>1 then
		for !_kk=2 to @rows(_infovariable1)
			!_pos=_infovariable1(!_kk)
			!_lag=_infovar0(!_pos,2)
			_vecEndog(!_lag+1)=_vecEndog(!_lag+1)-c(!_pos-1)
			if _infovar0(!_pos,1)=1 then _vecEndog(!_lag+2)=_vecEndog(!_lag+2)+c(!_pos-1) endif
		next
	endif		
	
	' Pour chaque variable exogène, on calcule le vecteur associé au polynôme retard et la contribution
	for !_vv=2 to !_nbvar
		%var=@word(%listvar,!_vv)
		vector(!_maxlag{!_vv}+2) _vecExog{!_vv}
		for !_kk=1 to @rows(_infovariable{!_vv})
			!_pos=_infovariable{!_vv}(!_kk)
			!_lag=_infovar0(!_pos,2)
			_vecExog{!_vv}(!_lag+1)=_vecExog{!_vv}(!_lag+1)+c(!_pos-1)
			if _infovar0(!_pos,1)=1 then _vecExog{!_vv}(!_lag+2)=_vecExog{!_vv}(!_lag+2)-c(!_pos-1) endif
		next
		
		if %ecm="1" then
			call contrib(%var,"_vecEndog","_vecExog{!_vv}",%debContrib,%finContrib)
		else
			call contrib_d(%var,"_vecEndog","_vecExog{!_vv}",%debContrib,%finContrib)
		endif
	next
	
	vector(1) _vecResid=1
	if %ecm="1" then
		call contrib("_mesresidus","_vecEndog","_vecResid",%debContrib,%finContrib)
	else
		call contrib_d("_mesresidus","_vecEndog","_vecResid",%debContrib,%finContrib)
		vector(1) _vecConst=1
		smpl @all
		series _maConstante=c(!_nbwords)
		call contrib_d("_maConstante","_vecEndog","_vecConst",%debContrib,%finContrib)
	endif 

	if %graph="1" then
		group _forGraph 
		if %ecm="1" then svector(!_nbvar) _stackedList else svector(!_nbvar+1) _stackedList endif
		for !_kk=2 to !_nbvar
			%var=@word(%listvar,!_kk)
			_forGraph.add contrib_{%var}
			%name="contrib_"+%var
			_stackedList(!_kk-1)=%name
		next
		_stackedList(!_nbvar)="contrib__mesResidus"
		_forGraph.add contrib__mesResidus
		if %ecm="0" then
			_stackedList(!_nbvar+1)="contrib__maConstante"
			_forGraph.add contrib__maConstante
		endif
		%_stackedList=@wjoin(_stackedList)
		%_stackedList=@wdelim(%_stackedList," ",",")
		%_monEndog=@word({%equation},1)
		_forGraph.add {%_monEndog}
		'%_monEndog="d("+%_monEndog+")"
		
		smpl {%debContrib} {%finContrib}
		freeze(_z_{%equation}) _forGraph.mixed stackedbar({%_stackedList}) line({%_monEndog})
		_z_{%equation}.options stackposneg
	endif
	delete _maxlag* _infovar* _stackedlist vecteurvar
endsub


