'Name : sub_chain.prg
'Author : Louis de Charsonville, inspired from ECB subroutines
'Created : 06/11/2015
'Last modification : 06/11/2015
'-----------------------------
'Description : the subroutines is creating a aggregated chain-linking index using level series and weights.  

subroutine create_aggr(string %name_aggr, string %chain_start, string %chain_end, string %g_index, string %g_weights)
	
	'INPUT :
	' %name_aggr = name of the aggregated index
	' %chain_start = date of the beginning of aggregation
	' %chain_end = date of end of aggregation
	' %g_index = group containing the "elementary indexes"
	' %g_weights = group containing the weights. 
	
	' WARNING : %g_index and %g_weights should have the same number of
	' series and series should be put in the same order so that
	' the ith series of %g_weights is the series of weight of the ith series
	' of %g_index.
	' Workfile should be in monthly basis and be larger than the sample
	' %chain_start - %chain_end
	
	'OUTPUT :
	' %name_aggr will be added to current workfile and containing the values
	' of the aggregated index.
	
	'REMARKS : 
	'The subroutine is able to disaggregate timeseries if all the weights
	' except one are negative.
	' Intuition is the following :
	' neig_chain =  core_chain * core_w / sum(core_w, -serv_w) + serv_chain * (- serv_w / sum(core_w, -serv_w) )

	
	smpl @all
	genr month= @month 'the timeseries month is equal to the "number of the month", i.e.  month take value 5 in May

	'determine %chain_end to comply to the shortest series in the group
	vector({%g_index}.@count) vec_obs
	for !i=1 to {%g_index}.@count
		%name={%g_index}.@seriesname(!i)
		vec_obs(!i)=@ilast({%name})
	next
	%chain_end0=@otod(@min(vec_obs))
	delete vec_obs
	
	group _g_chain
	for !i=1 to {%g_index}.@count
		%name={%g_index}.@seriesname(!i)
		%name_w={%g_weights}.@seriesname(!i)
		genr {%name}_chn=na
			
		' %%% AJOUT LOUIS - 03/03/2016
		%_debser={%name}.@first
		'_decfirstyear = december of the first year of available data for the series
		%_decfirstyear=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))) 
		!_odebser=@dtoo(%_debser)
		!_ofirstdec=@dtoo(%chain_start)-month(@dtoo(%chain_start))
		
		if !_ofirstdec < !_odebser then
			smpl %_debser %_decfirstyear 
			!_meanfirstyear=@mean({%name})
			{%name}_chn={%name}/!_meanfirstyear
			
			%chain_start0=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))+1)
		else
			%chain_start0=%chain_start
		endif
		for !t=@dtoo(%chain_start0) to @dtoo(%chain_end0)
			{%name}_chn(!t)={%name}(!t)/{%name}(!t-month(!t))
		next
	
		'Calculate aggregated chain
		smpl @all
		genr {%name_aggr}_chn_{!i}={%name}_chn*{%name_w}/@rsum({%g_weights})
		_g_chain.add {%} {%name_aggr}_chn_{!i}
		 delete {%name}_chn
	next
	genr {%name_aggr}_chn=@rsum(_g_chain)
	
	'Create {%name_aggr} index
	genr {%name_aggr}=@recode({%name_aggr}_chn=na,na,1) 'this is equivalent to genr=1 except when {%name_aggr}_chn is na.
	
	%_debser={%name_aggr}_chn.@first
	%_decfirstyear=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))) 
	!_odebser=@dtoo(%_debser)
	!_ofirstdec=@dtoo(%chain_start)-month(@dtoo(%chain_start))
	
	if !_ofirstdec < !_odebser then
		smpl %_debser %_decfirstyear 
		!_meanfirstyear=@mean({%name_aggr}_chn)
		{%name_aggr}={%name_aggr}_chn*!_meanfirstyear	
		%chain_start0=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))+1)
	else
		%chain_start0=%chain_start
	endif

	for !t=@dtoo(%chain_start0) to @dtoo(%chain_end0)
		{%name_aggr}(!t)=@recode(@isna({%name_aggr}_chn(!t)),{%name_aggr}(!t),{%name_aggr}_chn(!t)*{%name_aggr}(!t-month(!t)))
	next
	
	'Set level base year
	smpl 2015 2015
	scalar mean2015 = @mean({%name_aggr})
	smpl @all
	'rebase level at 2015 prices
	genr {%name_aggr} = {%name_aggr} / mean2015 * 100
	delete mean2015 
	delete {%name_aggr}_chn {%name_aggr}_chn_* _g_chain month
endsub

subroutine create_aggr_plus(string %name_aggr, string %chain_start, string %chain_end, string %g_index, string %g_weights,string %base_year)
	
	'INPUT :
	' %name_aggr = name of the aggregated index
	' %chain_start = date of the beginning of aggregation
	' %chain_end = date of end of aggregation
	' %g_index = group containing the "elementary indexes"
	' %g_weights = group containing the weights. 
	' %base_year = base year
	
	' WARNING : %g_index and %g_weights should have the same number of
	' series and series should be put in the same order so that
	' the ith series of %g_weights is the series of weight of the ith series
	' of %g_index.
	' Workfile should be in monthly basis and be larger than the sample
	' %chain_start - %chain_end
	
	'OUTPUT :
	' %name_aggr will be added to current workfile and containing the values
	' of the aggregated index.
	
	'REMARKS : 
	'The subroutine is able to disaggregate timeseries if all the weights
	' except one are negative.
	' Intuition is the following :
	' neig_chain =  core_chain * core_w / sum(core_w, -serv_w) + serv_chain * (- serv_w / sum(core_w, -serv_w) )

	
	smpl @all
	genr month= @month 'the timeseries month is equal to the "number of the month", i.e.  month take value 5 in May

	'determine %chain_end to comply to the shortest series in the group
	vector({%g_index}.@count) vec_obs
	for !i=1 to {%g_index}.@count
		%name={%g_index}.@seriesname(!i)
		vec_obs(!i)=@ilast({%name})
	next
	%chain_end0=@otod(@min(vec_obs))
	delete vec_obs
	
	group _g_chain
	for !i=1 to {%g_index}.@count
		%name={%g_index}.@seriesname(!i)
		%name_w={%g_weights}.@seriesname(!i)
		genr {%name}_chn=na
			
		' %%% AJOUT LOUIS - 03/03/2016
		%_debser={%name}.@first
		'_decfirstyear = december of the first year of available data for the series
		%_decfirstyear=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))) 
		!_odebser=@dtoo(%_debser)
		!_ofirstdec=@dtoo(%chain_start)-month(@dtoo(%chain_start))
		
		if !_ofirstdec < !_odebser then
			smpl %_debser %_decfirstyear 
			!_meanfirstyear=@mean({%name})
			{%name}_chn={%name}/!_meanfirstyear
			
			%chain_start0=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))+1)
		else
			%chain_start0=%chain_start
		endif
		for !t=@dtoo(%chain_start0) to @dtoo(%chain_end0)
			{%name}_chn(!t)={%name}(!t)/{%name}(!t-month(!t))
		next
	
		'Calculate aggregated chain
		smpl @all
		genr {%name_aggr}_chn_{!i}={%name}_chn*{%name_w}/@rsum({%g_weights})
		_g_chain.add {%} {%name_aggr}_chn_{!i}
		 delete {%name}_chn
	next
	genr {%name_aggr}_chn=@rsum(_g_chain)
	
	'Create {%name_aggr} index
	genr {%name_aggr}=@recode({%name_aggr}_chn=na,na,1) 'this is equivalent to genr=1 except when {%name_aggr}_chn is na.
	
	%_debser={%name_aggr}_chn.@first
	%_decfirstyear=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))) 
	!_odebser=@dtoo(%_debser)
	!_ofirstdec=@dtoo(%chain_start)-month(@dtoo(%chain_start))
	
	if !_ofirstdec < !_odebser then
		smpl %_debser %_decfirstyear 
		!_meanfirstyear=@mean({%name_aggr}_chn)
		{%name_aggr}={%name_aggr}_chn*!_meanfirstyear	
		%chain_start0=@otod(@dtoo(%_debser)+12-month(@dtoo(%_debser))+1)
	else
		%chain_start0=%chain_start
	endif

	for !t=@dtoo(%chain_start0) to @dtoo(%chain_end0)
		{%name_aggr}(!t)=@recode(@isna({%name_aggr}_chn(!t)),{%name_aggr}(!t),{%name_aggr}_chn(!t)*{%name_aggr}(!t-month(!t)))
	next
	
	'Set level base year
	smpl {%base_year} {%base_year}
	scalar mean_{%base_year} = @mean({%name_aggr})
	smpl @all
	'rebase level at %base_year prices
	genr {%name_aggr} = {%name_aggr} / mean_{%base_year} * 100
	delete mean_{%base_year} 
	delete {%name_aggr}_chn {%name_aggr}_chn_* _g_chain month
endsub

subroutine create_aggr_ecb(string %name_aggr, string %chain_start, string %chain_end, string %g_index, string %g_weights)
	
	'INPUT :
	' %name_aggr = name of the aggregated index
	' %chain_start = date of the beginning of aggregation
	' %chain_end = date of end of aggregation
	' %g_index = group containing the "elementary indexes"
	' %g_weights = group containing the weights. 
	
	' WARNING : %g_index and %g_weights should have the same number of
	' series and series should be put in the same order so that
	' the ith series of %g_weights is the series of weight of the ith series
	' of %g_index.
	' Workfile should be in monthly basis and be larger than the sample
	' %chain_start - %chain_end
	
	'OUTPUT :
	' %name_aggr will be added to current workfile and containing the values
	' of the aggregated index.
	
	'REMARKS : 
	'The subroutine is able to disaggregate timeseries if all the weights
	' except one are negative.
	' Intuition is the following :
	' neig_chain =  core_chain * core_w / sum(core_w, -serv_w) + serv_chain * (- serv_w / sum(core_w, -serv_w) )

	
	smpl @all
	genr month= @month 'the timeseries month is equal to the "number of the month", i.e.  month take value 5 in May

	'determine %chain_end to comply to the shortest series in the group
	vector({%g_index}.@count) vec_obs
	for !i=1 to {%g_index}.@count
		%name={%g_index}.@seriesname(!i)
		vec_obs(!i)=@ilast({%name})
	next
	%chain_end=@otod(@min(vec_obs))
	delete vec_obs
	
	group _g_chain
	for !i=1 to {%g_index}.@count
		%name={%g_index}.@seriesname(!i)
		%name_w={%g_weights}.@seriesname(!i)
		genr {%name}_chn=na
		
		'Creating chain index
		for !t=@dtoo(%chain_start) to @dtoo(%chain_end)
			{%name}_chn(!t)={%name}(!t)/{%name}(!t-month(!t))
		next
		
		'Calculate aggregated chain
		smpl @all
		genr {%name_aggr}_chn_{!i}={%name}_chn*{%name_w}/@rsum({%g_weights})
		_g_chain.add {%} {%name_aggr}_chn_{!i}
		 delete {%name}_chn
	next
	genr {%name_aggr}_chn=@rsum(_g_chain)
	
	'Create {%name_aggr} index
	genr {%name_aggr}=@recode({%name_aggr}_chn(1)=na,na,1) 'this is equivalent to genr=1 except when {%name_aggr}_chn(+1) is na.
	for !t=@dtoo(%chain_start) to @dtoo(%chain_end)
		{%name_aggr}(!t)=@recode(@isna({%name_aggr}_chn(!t)),{%name_aggr}(!t),{%name_aggr}_chn(!t)*{%name_aggr}(!t-month(!t)))
	next
	
	'Set level base year
	smpl 2015 2015
	scalar mean2015 = @mean({%name_aggr})
	smpl @all
	'rebase level at 2005 prices
	genr {%name_aggr} = {%name_aggr} / mean2015 * 100
	delete mean2015 
	delete {%name_aggr}_chn {%name_aggr}_chn_* _g_chain month
	
endsub
