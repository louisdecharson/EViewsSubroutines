' =======================
'    T O O L B O X 
' =======================

' Cr�ation de page qui supprime la page s'elle existe d�j�
subroutine creationPage(string %_nom, string %_freq, string %_debPage, string %_finPage)
	if @pageexist(%_nom) then pagedelete {%_nom} endif
	pagecreate(page=%_nom) {%_freq} {%_debPage} {%_finPage}
endsub

' Supprime un objet uniquement s'il existe
subroutine supprime(string %objs)
	svector ___objs = @wsplit(%objs)
	for !___k___ = 1 to @rows(___objs) 
		%___obj = ___objs(!___k___)
		if @isobject(%___obj) then delete %___obj endif
	next
	delete ___objs
endsub

' Cr�e une base qui �crase la base existante
subroutine creationBase(string %_nom)
	if @fileexist(%_nom+".edb") then
		dbdelete %_nom
		dbcreate %_nom
	else
		dbcreate %_nom
	endif
endsub

subroutine incr(scalar !__sc)
	!__sc = !__sc + 1
endsub

