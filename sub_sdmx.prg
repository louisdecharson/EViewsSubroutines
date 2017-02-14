subroutine sdmx(string %provider, string %series, string %filters, string %rename)

        ' Api Key Quandl & BLS 
        %__quandl = "" ' PUT YOUR API KEY BETWEEN THE QUOTES
        %__bls  = ""  'PUT YOUR API KEY BETWEEN THE QUOTES
        %__fred  = ""  ' PUT YOUR API KEY BETWEEN THE QUOTES
        
        %__app = "http://sdmx.herokuapp.com/"
        %__url = %__app

	%__renlist = ""
	if @wcount(%rename) > 0 then
		!__wc = @wcount(%rename) - 1
		for !____k = 1 to !__wc
			%__renlist = %__renlist + @word(%rename, !____k) + "," 
		next
		!__wc = !__wc + 1
		%__renlist = %__renlist + @word(%rename, !__wc)
	endif
        
        if %provider = "quandl" or %provider = "bls" or %provider = "fred" then
                %__url = %__url + %provider + "/" + %__{%provider} + "/" + %series + %filters
                if @len(%rename) > 0 then
                        import(t=html) %__url names=("date",%rename)
                else
                        import(t=html) %__url
                endif
        else
                if %provider = "ecb" or %provider = "insee" or %provider = "eurostat" then
                        %__url = %__url + %provider + "/" + "series/" + %series + %filters
                        if @len(%rename) > 0 then
                                import(t=html) %__url colhead=2 namepos=none names=("date",{%__renlist})
                        else
                                import(t=html) %__url colhead=2 namepos=first
                        endif
                else
                        %__url = %__url + "req?url=" + "'"  + %series + "'"
                        if @len(%rename) > 0 then
                                import(t=html) %__url colhead=2 namepos=none names=("date",{%__renlist})
                        else
                                import(t=html) %__url colhead=2 namepos=first
                        endif
                endif
        endif       
endsub