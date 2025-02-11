*===================================================================================*
* samregci: Sensitivity Analysis for the Main Regression Coefficients of Interest   *
* Authors:																			                                    *
* Pablo Glüzmann, CEDLAS-UNLP and CONICET - La Plata, Argentina - gluzmann@yahoo.com*
* Demian Panigo, Instituto Malvinas, UNLP and CONICET, Argentina - panigo@gmail.com *
*-----------------------------------------------------------------------------------*
* Version 1.0 - 19-01-2025                                                          *
*===================================================================================*

*DEFINE "samregci" PROGRAM
capture program drop samregci
program define samregci, eclass

qui {

	*Define Syntax Program

	syntax varlist(min=2) [aw fw iw pw] [if] [in],	///
	ITerateover(varlist)										///
	[														   	///
	NComb(numlist >=0 integer max=2) 			   	///
	Fixvar(varlist) 									   	///
	tsig											            ///
	tlost											            ///
	CMDEst(string) 									   	///
	CMDOptions(string) 								   	///
	CMDStat(string) 									   	///
	CMDIveq(string) 									   	///
	RESultsdta(string) 								   	///
	COUnt 												   	///
	DOuble 												   	///
	REPlace 												   	///
	SAMEsample 											   	///
	graphtype(string)											///														///
	NOGraph														///
	level(real 95)											   ///
	at(real 0)											      ///
	]														   	

	if "`weight'`exp'" != ""			loc weight " [`weight'`exp'] "
	if "`resultsdta'" == ""				loc resultsdta "samregci"
	if "`cmdest'" == ""					loc cmdest "regress"
	tempvar touse
	mark `touse' `if' `in'

	*Define Error Messages
	capture tsset
	loc time=r(timevar)
	loc panel=r(panelvar)
	if "`time'" == "."	loc time ""
	if "`panel'" == "."	loc panel ""

	if "`fixvar'" != "" {
		loc aux 0
		foreach var1 of varlist `fixvar'  {
			foreach var2 of varlist `varlist' {
				if "`var1'" == "`var2'" loc aux 1
			}
		}
		if `aux'==1 {
			display as error "Option fixvar contains at last one variable already included as main variable"
			exit 503
		}
	}

	* defino y corrigo locación de los resultados
	loc path ""
	loc revresultsdta=reverse("`resultsdta'")
	loc dta=substr("`resultsdta'",-4,.)
	loc dta=strmatch("`dta'",".dta")
	if `dta'==1 loc revresultsdta=substr("`revresultsdta'",5,.)
	loc resultsdta=reverse("`revresultsdta'")
	loc posit=strpos("`revresultsdta'","\")
	loc posit2=strpos("`revresultsdta'","/")
	loc position = max(`posit',`posit2')
	if `posit'>0 & `posit2'>0 loc position=min(`posit',`posit2')
	if `position'!=0 {
		loc revpath =substr("`revresultsdta'",`position',.)
		loc path =reverse("`revpath'")
	}
	if "`path'" == "" loc fname "`resultsdta'"
	if "`path'" != "" {
		loc posaux=`position'-1
		loc fname =substr("`resultsdta'",-`posaux',.)
	}
	loc length =length("`resultsdta'")
	loc aux 0

	if `length'>245-10 & "`path'" == "" {
		display as error "the path of the working directory is too long, change the working directory using command cd or specify shorter path using option resultsdta"
		exit 603
	}
	if `length'>245-10 & "`path'" != "" {
		display as error "the path specified in option resultsdta too long, specify a shorter path using option resultsdta"
		exit 603
	}
	preserve
	drop _all
	set obs 1
	tempname aux
	tempvar var1
	gen `var1' =1
	capture save "`resultsdta'.dta", replace
	if _rc !=0 & "`path'" == "" {
		display as error "stata cannot save files in the working directory, change the working directory using command cd or specify another path using option resultsdta"
		exit 603
	}
	if _rc !=0 & "`path'" != "" {
		display as error "stata cannot save files the path specified in option resultsdta, change the working directory using command cd or specify another path using option resultsdta"
		exit 603
	}
	capture erase "`resultsdta'.dta"
	* separo Depvar de MainVariables
	drop _all
	restore
	local wt: word 2 of `exp'
	tokenize `varlist'	
	loc depvar "`1'"
	macro shift 1
	loc MainVar "`*'"


	* separo agrego variables Instrumentales
	if "`cmdiveq'" != ""  {
			_iv_parse `depvar' (`cmdiveq')
			loc instruments "`s(inst)'"
			loc endogenous "`s(endog)'"

	}
	if "`instruments'" != "" & "`endogenous'" != "" {
		foreach var1 of local endogenous {
			local iterateover: subinstr local iterateover "`var1'" "", word
		}
		loc iterateover "`iterateover' `endogenous'"
	}
	* completo sintaxis ncomb
	if "`ncomb'" == "" {
		loc allcomb: word count `iterateover'
		loc ncomb "1 `allcomb'"
		if "`fixvar'" != "" {
			loc ncomb "0 `allcomb'"
		}
	}
	local none: word 1 of `ncomb'
	if `none' == 0 {
		if "`fixvar'" == "" {
			display as error "Combinatory 0 not allowed without fixvar"
			exit 198
		}
		local aux: word 2 of `ncomb'
		loc ncomb "1 `aux'"
			
	}
	tokenize `ncomb'	
	loc kmin `1'
	if "`2'" != "" loc kmax `2'
	if "`2'" == "" loc kmax `1'
	if `kmin'>`kmax' {
		display as error "ncomb() invalid, elements out of order"
		exit 124
	}
	loc tindep: word count `iterateover'
	loc ListaTotVar "`MainVar' `fixvar' `iterateover'"
	if "`samesample'" != "" {
		tempvar aux
		gen `aux'=0 if `touse' ==1
		foreach var of varlist `ListaTotVar' {
			replace `aux'=1 if `touse' ==1 & `var'>=. 
		}
		replace `touse' =0 if `aux' ==1
	}
	* determino el nro total de regresiones a hacer
	loc Ntotreg 0
	loc total 0
	forvalues j=`kmin'/`kmax' {
		loc n1=comb(`tindep',`j')
		loc total = `total'+`n1'
	}
	if `none' ==0 loc total = `total'+1
	if `total'<=0 | `total' >=. {
		display as error "Too few independent variables specified for selected combinatorial"
		exit 198
	}
	noi di as text "---------------------------------------------------------------------------"
	noi di as text "Total Number of Estimations: " as result "`total'"
	noi di as text "---------------------------------------------------------------------------"
	loc estcomoptions "cmde(`cmdest') cmdoptions(`cmdoptions') cmdstat(`cmdstat') resultsdta(`resultsdta') lastreg(`total') `double' `count' time(`time') panel(`panel') instruments(`instruments') endogenous(`endogenous') " 
	local hh1: word count `fixvar'
	local hh2: word count `ListaTotVar'
	loc hh 1
	if `hh2'>`kmax'+`hh1' {
		foreach var of local ListaTotVar {
			if `hh'<=(`kmax'+`hh1') loc listaaux "`listaaux' `var'"
			loc ++hh
		}
	}
	else loc listaaux "`ListaTotVar'"
	* estimo el tiempo que lleva
	timer clear 99
	timer on 99
	tempname aux
	_samregci_estcomtry `depvar' `MainVar' `listaaux' `weight' if `touse' ==1 , matres2(`aux') `estcomoptions' ordervar(`ListaTotVar') nroreg(`Ntotreg') 
	capture mat drop `aux'
	macro drop aux
	timer off 99
	timer list
	ret li
	loc time1 r(t99)
	timer clear 99
	loc timeprox =round((`time1'*`total')/50)
	if `timeprox'>=1 {
		noi di as text "---------------------------------------------------------------------------"
		noi di as text "Warning: Estimation could take about " as result "`timeprox'" as text " minutes "
		noi di as text "---------------------------------------------------------------------------"
	}
	if `timeprox'>3 {
		noi more
		noi di as text " "
	}	

	* calculo combinatorias
	noi di as text "Computing combinations..."
	forvalues combaux=1/`kmax' {
		tempfile __a_`combaux'
		loc __a_ "`__a_' `__a_`combaux''"
	}
	_samregci_combinate `__a_', nsamp(`tindep') ncomb(`kmin',`kmax')
	noi di as text "Preparing regression list..."
	if `none' ==0 loc ++Ntotreg
	if `none' ==0 loc regress`Ntotreg' "" 
	tokenize `iterateover' 
	forvalues j=`kmin'/`kmax' {
		preserve
		use `__a_`j'', clear
		erase `__a_`j''
		loc v =_N
		loc v1 "`"
		loc v2 "'"
		d _all
		forvalues i =1/`v' {
			macro drop _reg`i' 
			foreach var of varlist _all {
				loc vaux = `var'[`i']
				loc reg`i' " `reg`i'' `v1'`vaux'`v2'"
			}
		}
		restore
		forvalues i =1/`v' {
			loc ++Ntotreg
			loc regress`Ntotreg' "`reg`i''" 
			macro drop _reg_`i'
		}
	}

	noi di as text "Doing regressions..."
	
	* armo en mata la matris de resultados
	* k = nro de variables
	* 1+k más la constante
	* (1+`k')*2 = t y coef de todas más 1 por el orden
	* 1+(1+`k')*2 +2 agrego 1 para el orden + 2 para las observaciones y nro de variables
	tempname resultados matres matres1 matres2 matres1S matres2S resultadosS matresS
	local k: word count `ListaTotVar'
	mat `matres1' = J(1,(1+`k')*2,.)
	mat `matres2' = J(1,2,.)
	mata `resultados' = J(`Ntotreg',1+(1+`k')*2+2,.)

	mat `matres1S' = J(1,(1+`k')*2,.)
	mat `matres2S' = J(1,2,.)
	mata `resultadosS' = J(`Ntotreg',1+(1+`k')*2+2,.)

	* agrego constante a la lista para que después la tome
	loc aux = regexm("`cmdoptions'","nocons")
	if "`aux'" == "0" loc ListaTotVarCons "`ListaTotVar' _cons"

	* corro regresiones
	loc noestcom =0
	loc h=1
	forvalues i=1/`Ntotreg' {
		loc oreg =`i'
		loc h=`i'-`noestcom'
		noi _samregci_estcom `depvar' `MainVar' `fixvar' `regress`oreg'' `weight' if `touse' ==1 , matres1(`matres1')  matres2(`matres2') `estcomoptions' ordervar(`ListaTotVarCons') nroreg(`oreg') 
		if r(noest) ==1 | r(notest) ==1 {
			loc ++noestcom
			continue
		}
		mat `matres' = `oreg',`matres1',`matres2'
		mata `resultados'[`h',.]=st_matrix("`matres'")
		* sisters regresions
		if "`samesample'" == "" {
			noi _samregci_estcom `depvar' `MainVar' `fixvar' `weight' if `touse' ==1 & e(sample), matres1(`matres1S')  matres2(`matres2S') `estcomoptions' ordervar(`ListaTotVarCons') nroreg(`oreg') 
			mat `matresS' = `oreg',`matres1S',`matres2S'
			mata `resultadosS'[`h',.]=st_matrix("`matresS'")
		}
		if "`samesample'" != "" {
			if `i'==1 noi _samregci_estcom `depvar' `MainVar' `fixvar' `weight' if `touse' ==1 , matres1(`matres1S')  matres2(`matres2S') `estcomoptions' ordervar(`ListaTotVarCons') nroreg(`oreg') 
			mat `matresS' = `oreg',`matres1S',`matres2S'
			mata `resultadosS'[`h',.]=st_matrix("`matresS'")
		}

	}
	noi di as text "Saving results..."
	preserve
	drop _all
	getmata (v*) = `resultados'
	mata mata drop `resultados'
	loc i=1
	ren v`i' order
	label var order "Order number of estimation"
	loc i=2
	loc j=1

	foreach var of local ListaTotVarCons {
		loc h = `i'+1
		ren v`i' v_`j'_b 
		ren v`h' v_`j'_t 
		label var v_`j'_b "`var' coeff."
		label var v_`j'_t "`var' tstat."
		if "`var'" == "_cons" {
			label var v_`j'_b "Constant coeff."
			label var v_`j'_t "Constant tstat."
		}
		loc i=`i'+2
		loc ++j
	}
	foreach name in obs nvar {
		ren v`i' `name'
		loc ++i
	}
	label var obs "Observations"
	label var nvar "Number of regressors"
	*************************************************************************************************************************
	order v_*,  seq
	loc i=1
	foreach var of varlist v_*_b {
		capture ren `var' v_`i'_b 
		loc ++i
	}
	loc i=1
	foreach var of varlist v_*_t {
		capture ren `var' v_`i'_t 
		loc ++i
	}

	if r(N)==0  {
		display as error "No estimations has been stored"
		exit 
	}

	order order, first
	if "`double'" != "" {
		if "`cmdstat'" != "" {
			foreach i of local cmdstat {
				capture format `i' %20.0g
			}
			
		}
	}
	drop if order ==.
	compress
	sort order
	noi save "`resultsdta'.dta", replace
	if "`nograph'" == ""	noi _samregci_g1 `MainVar', path(`path') at(`at') level(`level')
	
	noi _samregci_t1 `MainVar', path(`path') at(`at') level(`level') resultsdta(`resultsdta')
	
	sum obs
	if r(min) != r(max) loc sameobs =0 
	if r(min) == r(max) loc sameobs =1 

	if `sameobs' !=1 noi _samregci_scatterbt `MainVar', path(`path') at(`at') level(`level')
	drop _all
	getmata (v*) = `resultadosS'
	mata mata drop `resultadosS'
	loc i=1
	ren v`i' order
	label var order "Order number of estimation"
	loc i=2
	loc j=1

	foreach var of local ListaTotVarCons {
		loc h = `i'+1
		ren v`i' v_`j'_bSIS 
		ren v`h' v_`j'_tSIS 
		label var v_`j'_bSIS "`var' coeff."
		label var v_`j'_tSIS "`var' tstat."
		if "`var'" == "_cons" {
			label var v_`j'_bSIS "Constant coeff."
			label var v_`j'_tSIS "Constant tstat."
		}
		loc i=`i'+2
		loc ++j
	}
	foreach name in obs nvar {
		ren v`i' `name'SIS
		loc ++i
	}
	label var obsSIS "Observations"
	label var nvarSIS "Number of regressors"
	*************************************************************************************************************************
	order v_*,  seq
	loc i=1
	foreach var of varlist v_*_bSIS {
		capture ren `var' v_`i'_bSIS 
		loc ++i
	}
	loc i=1
	foreach var of varlist v_*_tSIS {
		capture ren `var' v_`i'_tSIS 
		loc ++i
	}

	if r(N)==0  {
		display as error "No estimations has been stored"
		exit 
	}

	order order, first
	if "`double'" != "" {
		if "`cmdstat'" != "" {
			foreach i of local cmdstat {
				capture format `i' %20.0g
			}
			
		}
	}
	drop if order ==.
	compress
	sort order
	if `sameobs' !=1 save "`resultsdta'_Sisters.dta", replace
	if `sameobs' ==1 {
		tempfile tempsis
		save `tempsis'
	}
	drop _all
	if `sameobs' !=1 use "`resultsdta'_Sisters.dta"
	if `sameobs' ==1 use `tempsis'
	merge 1:1 order using "`resultsdta'.dta"
	if `sameobs' !=1 {
		noi _samregci_sisters `MainVar', path(`path') at(`at') level(`level') resultsdta(`resultsdta')
		noi _samregci_tSIS `MainVar', path(`path') at(`at') level(`level') resultsdta(`resultsdta') 
	}
	noi _samregci_vslist `MainVar', path(`path') at(`at') level(`level') resultsdta(`resultsdta') fixvar(`fixvar') `tsig' `tlost'
}
end



*DEFINE "_samregci_combinate" PROGRAM

capture program drop _samregci_combinate
program define _samregci_combinate

	syntax anything ,NSamp(integer) NComb(numlist >0 integer max=2) [Reps]
	preserve
	tokenize `ncomb'
	loc kmin `1'
	if "`2'" !="" loc kmax `2'
	if "`2'" == "" loc kmax `1'
	tokenize `anything'
	clear
	set obs `nsamp'
	gen aux1=_n
	tempfile temp
	save `temp', replace
	if `kmin' ==1 {
		save "`1'", replace
		count
	}
	if `kmax' >=2 {
		loc lista_ant = "aux1 "
		tempfile foto
		save `foto', replace
		forvalues j=2/`kmax' {
			tempfile temp`j'
			ren aux aux`j'
			save `temp`j'', replace
		}
		use `foto', clear
		forvalues j=2/`kmax' {
			loc j_1 =`j'-1
			cross using `temp`j''
			if "`reps'" == "" drop if aux`j_1'>=aux`j'
			if "`reps'" !="" drop if aux`j_1'>aux`j'
			if `kmin'<=`j' {
				save "``j''", replace
				count
			}
		}
	}
	restore
end


*DEFINE "_samregci_estcom" PROGRAM
capture program drop _samregci_estcom
program define _samregci_estcom, rclass
qui	{
	loc setmoreprev=c(more)
	set more off
	syntax anything [aw fw iw pw] [if], matres1(string) matres2(string) CMDEst(string) [CMDOptions(string) cmdstat(string)] resultsdta(string) ordervar(string) [nroreg(integer 0) lastreg(integer 0) double COUnt time(string) panel(string) instruments(string) endogenous(string) ] [*]
	loc weight "`weight'`exp'"
	if "`count'" != "" noi di as text "Estimation number " as result "`nroreg'" as text " of " as result "`lastreg'"
	tempvar touse
	mark `touse' `if' 
	tempvar insample
	gen `insample' =1 if `touse'==1
	if "`instruments'" != "" {
		foreach var1 of local anything {
			foreach var2 of local endogenous {
				if "`var1'" == "`var2'" loc ivendogenous " `ivendogenous' `var2' "
			}
		}
		loc anything2 "`anything'"
		foreach var1 of local ivendogenous {
			local anything2: subinstr local anything2 "`var1'" "", word
		}
		loc estim: word 2 of `cmdest'
		if ("`ivendogenous'" != "" | "`estim'" == "gmm") {
			loc dependiente: word 1 of `anything2' 
			capture `cmdest' `anything2' (`ivendogenous' =`instruments') `weight' if `insample'==1, `cmdoptions'
			if _rc !=0 & "`count'" != "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
			if _rc !=0 {
				return scalar noest =1
				exit
			}
		}
		else {
			loc dependiente: word 1 of `anything2' 
			capture cmdest' `anything2' `weight' if `insample'==1, `cmdoptions'
			if _rc !=0 & "`count'" != "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
			if _rc !=0 {
				return scalar noest =1
				exit
			}
		}
	}
	else {
		loc dependiente: word 1 of `anything' 
		capture `cmdest' `anything' `weight' if `insample'==1, `cmdoptions'
		if _rc !=0 & "`count'" != "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
		if _rc !=0 {
			return scalar noest =1
			exit
		}
	}
}

if _rc == 0 {
	tempname betas sigmas t
	mat `betas'  =e(b) 
	mat `sigmas' = e(V)
	loc obs      = e(N)
	loc rank     = e(rank)
	if `rank'    ==0 exit
	loc nvar     = colsof(`betas')
	capture mat `t'=(`betas'*inv(cholesky(diag(vecdiag(`sigmas')))))
	if _rc !=0 {
		mat `t' = `betas'
		forvalues i=1/`nvar'{
			mat `t'[1,`i'] = `betas'[1,`i'] / `sigmas'[`i',`i']^.5
		}
	}
	mat `matres1'=`matres1'*.
	loc i=1
	loc h=1
	local names : colfullnames e(b)
	foreach var1 of local ordervar {
		foreach var2 of local names {
			if "`var1'" == "`var2'" | "o.`var1'" == "`var2'" | "`dependiente':o.`var1'" == "`var2'" | "`dependiente':`var1'" == "`var2'" {
				mat `matres1'[1,`i']=`betas'[1,`h']
				mat `matres1'[1,`i'+1]=`t'[1,`h']
				loc ++h	
			} 
		}
		loc i=`i'+2
	}
	mat `matres2' = `obs',`nvar'
	if "`cmdstat'" != "" {
		tempname aux
		foreach i of local cmdstat {
			mat `aux'=e(`i')
			mat colnames `aux'= `i'
			mat `matres2'=`matres2',`aux'
		}
	}
	if "`setmoreprev'" == "on" set more on
}
else  {
	"`count'" != "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
		sleep 100
}

end

*DEFINE "_samregci_estcomtry" PROGRAM
capture program drop _samregci_estcomtry
program define _samregci_estcomtry, rclass
qui	{
	loc setmoreprev=c(more)
	set more off
	syntax anything [aw fw iw pw] [if], matres2(string) CMDEst(string) [CMDOptions(string) cmdstat(string)] resultsdta(string) ordervar(string) [nroreg(integer 0) Outsample(integer 0) compact aicbic hettest hettest_o(string) archlm  archlm_o(string) bgodfrey bgodfrey_o(string) durbinalt durbinalt_o(string) dwatson sktest sktest_o(string) swilk swilk_o(string) sfrancia testpass(numlist >0 <1 max=1 ) lastreg(integer 0) double COUnt time(string) panel(string) instruments(string) endogenous(string) ] [*]
	loc weight "`weight'`exp'"
	if "`count'" == "" noi di as text "Estimation number " as result "`nroreg'" as text " of " as result "`lastreg'"
	tempvar touse
	mark `touse' `if' 
	tempvar insample
	gen `insample' =1 if `touse'==1
	if `outsample'!=0 {
		sort `panel' `time'
		if "`panel'" == "" replace `insample' =0 if _n>_N-`outsample' & `touse'==1
		if "`panel'" !="" by `panel': replace `insample' =0 if _n>_N-`outsample' & `touse'==1
	}
	if "`instruments'" !="" {
		foreach var1 of local anything {
			foreach var2 of local endogenous {
				if "`var1'" == "`var2'" loc ivendogenous " `ivendogenous' `var2' "
			}
		}
		loc anything2 "`anything'"
		foreach var1 of local ivendogenous {
			local anything2: subinstr local anything2 "`var1'" "", word
		}
		loc estim: word 2 of `cmdest'
		if ("`ivendogenous'" !="" | "`estim'" == "gmm") {
			capture `cmdest' `anything2' (`ivendogenous' =`instruments') `weight' if `insample'==1, `cmdoptions'
			if _rc !=0 & "`count'" == "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
			if _rc !=0 {
				return scalar noest =1
				exit
			}
		}
		else {
			capture cmdest' `anything2' `weight' if `insample'==1, `cmdoptions'
			if _rc !=0 & "`count'" == "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
			if _rc !=0 {
				return scalar noest =1
				exit
			}
		}
	}
	else {
		capture `cmdest' `anything' `weight' if `insample'==1, `cmdoptions'
		if _rc !=0 & "`count'" == "" noi di as text "Error " as input "r(" _rc ")" as text " in estimation number " as result "`nroreg'" 
		if _rc !=0 {
			return scalar noest =1
			exit
		}
	}
}
capture {
	tempname betas sigmas t
	mat `betas' =e(b) 
	mat `sigmas' =e(V)
	loc obs =e(N)
	loc rank =e(rank)
	if `rank' ==0 exit
	loc nvar =colsof(`betas')
	local r_sqr_a = e(r2_a) 
	local rmse_in = e(rmse) 
	if "`cmdest'" == "logit" | "`cmdest'" == "probit" {
		local r_sqr_a = e(r2_p) 
		local rmse_in = e(ll) 		
	}
	capture mat `t'=(`betas'*inv(cholesky(diag(vecdiag(`sigmas')))))
	if _rc !=0 {
		mat `t' = `betas'
		forvalues i=1/`nvar'{
			mat `t'[1,`i'] = `betas'[1,`i'] / `sigmas'[`i',`i']^.5
		}
	}
	if `outsample'!=0 {
		loc df_r=e(df_r)
		tempvar resout resout_sq 
		if "`cmdest'" == "xtreg" predict `resout' if `insample'==0, e  
		else predict `resout' if `insample'==0, res
		gen double `resout_sq'= `resout'*`resout' 
		sum `resout_sq', mean
		tempname rmse_out
		if "`cmdest'" == "logit" | "`cmdest'" == "probit" {
			lroc if `insample'==0, nograph
			tempname rmse_out
			mat `rmse_out' = r(area)
		}
		else mat `rmse_out' =( r(sum)/`df_r' )^.5	
		mat colnames `rmse_out' = rmse_out
	}
	if "`aicbic'" !="" {
		estat ic
		tempname aicbic
		mat `aicbic'=r(S)
		mat `aicbic'=`aicbic'[1,5],`aicbic'[1,6]
		mat colnames `aicbic' = aic bic 
	}
	if "`hettest'" !="" {
		estat hettest
		tempname hettest
		mat `hettest' =r(p)
		mat colnames `hettest' = hettest
	}
	if "`archlm'" !="" {
		loc lista ""
		estat archlm, `archlm_o'
		loc aux=r(lags)
		foreach i of local aux {
			loc lista "`lista' archlm`i'"
		}
		tempname archlm
		mat `archlm' =r(p)
		mat colnames `archlm' =`lista' 
	}
	if "`bgodfrey'" !="" {
		loc lista ""
		estat bgodfrey, `bgodfrey_o'
		loc aux=r(lags)
		foreach i of local aux {
			loc lista "`lista' bgodfrey`i'"
		}
		tempname bgodfrey
		mat `bgodfrey' =r(p)
		mat colnames `bgodfrey' =`lista' 
	}
	if "`durbinalt'" !="" {
		loc lista ""
		estat durbinalt, `durbinalt_o'
		loc aux=r(lags)
		foreach i of local aux {
			loc lista "`lista' durbinalt`i'"
		}
		tempname durbinalt
		mat `durbinalt' =r(p)
		mat colnames `durbinalt' =`lista' 
	}
	if "`dwatson'" !="" {
		estat dwatson
		tempname dwatson
		mat `dwatson' =r(dw)
		mat colnames `dwatson' = dwatson
	}
	if "`sktest'" !="" | "`swilk'" !="" | "`sfrancia'" !="" {
		tempvar resxt
		if "`cmdest'" == "xtreg" predict `resxt' if e(sample), e  
		else predict `resxt' if e(sample), res
		if "`sktest'" !="" {
			sktest `resxt' ,`sktest_o'
			tempname sktest1
			mat `sktest1' =r(P_chi2)
			mat colnames `sktest1' = sktest
		}
		if "`swilk'" !="" {
			swilk `resxt' ,`swilk_o'
			tempname swilk1
			mat `swilk1' =r(p)
			mat colnames `swilk1' = swilk
		}
		if "`sfrancia'" !="" {
			sfrancia `resxt' 
			tempname sfrancia1
			mat `sfrancia1' =r(p)
			mat colnames `sfrancia1' = sfrancia
		}
	}
	mat `matres2' = `obs',`nvar',`r_sqr_a',`rmse_in'
	mat colnames `matres2'= obs nvar r_sqr_a rmse_in
	if "`cmdstat'" !="" {
		tempname aux
		foreach i of local cmdstat {
			mat `aux'=e(`i')
			mat colnames `aux'= `i'
			mat `matres2'=`matres2',`aux'
		}
	}
	if `outsample'!=0	mat `matres2' =`matres2',`rmse_out'
	if "`aicbic'" !=""	mat `matres2' =`matres2',`aicbic'
	if "`testpass'" !="" {
		if "`hettest'" !="" {
			loc aux1 =colsof(`hettest')
			forvalues i =1/`aux1' {
				loc aux = `hettest'[1,`i']
				if `hettest'[1,`i']<`testpass' continue, break
			}
		}
		if "`archlm'" !="" {
			loc aux1 =colsof(`archlm')
			forvalues i =1/`aux1' {
				loc aux = `archlm'[1,`i']
				if `archlm'[1,`i']<`testpass' continue, break
			}
		}
		if "`bgodfrey'" !="" {
			loc aux1 =colsof(`bgodfrey')
			forvalues i =1/`aux1' {
				loc aux = `bgodfrey'[1,`i']
				if `bgodfrey'[1,`i']<`testpass' continue, break
			}
		}
		if "`durbinalt'" !="" {
			loc aux1 =colsof(`durbinalt')
			forvalues i =1/`aux1' {
				loc aux = `durbinalt'[1,`i']
				if `durbinalt'[1,`i']<`testpass' continue, break
			}
		}
		if `aux'<`testpass' return scalar notest =1
		if "`sktest'" !="" {
				loc aux= `sktest1'[1,1]
				if `aux'<`testpass' return scalar notest =1
		}
		if "`swilk'" !="" {
				loc aux= `swilk1'[1,1]
				if `aux'<`testpass' return scalar notest =1
		}
		if "`sfrancia'" !="" {
				loc aux= `sfrancia1'[1,1]
				if `aux'<`testpass' return scalar notest =1
		}
	}
	if "`hettest'" !=""	mat `matres2' =`matres2',`hettest'
	if "`archlm'" !=""	mat `matres2' =`matres2',`archlm'
	if "`bgodfrey'" !=""	mat `matres2' =`matres2',`bgodfrey'
	if "`durbinalt'" !=""	mat `matres2' =`matres2',`durbinalt'
	if "`dwatson'" !=""	mat `matres2' =`matres2',`dwatson'
	if "`sktest'" !=""	mat `matres2' =`matres2',`sktest1'
	if "`swilk'" !=""	mat `matres2' =`matres2',`swilk1'
	if "`sfrancia'" !=""	mat `matres2' =`matres2',`sfrancia1'
	if "`setmoreprev'" == "on" set more on
}
end


capture program drop _samregci_g1
program define _samregci_g1, rclass
	qui	{
		*syntax anything , [level(real)] [level(numlist >=0 real max=1)]
		syntax anything , path(string) [level(real 95) at(real 0)]
		loc vt =invnorm((100-`level')/200)
		loc i=1
		foreach var in `anything' {
			kdensity v_`i'_b
			if "`graphtype'" == "" noi graph export "`path'/b_`var'.png", replace
			else if "`graphtype'" == "gph" noi graph save "`path'/b_`var'.`graphtype'", replace
			else if "`graphtype'" != "" noi graph export "`path'/b_`var'.`graphtype'", replace
			loc lx =`at'+`vt'
			loc ux =`at'-`vt'
			kdensity v_`i'_t, xline(`lx') xline(`ux')
			if "`graphtype'" == "" noi graph export "`path'/t_`var'.png", replace
			else if "`graphtype'" == "gph" noi graph save "`path'/t_`var'.`graphtype'", replace
			else if "`graphtype'" != "" noi graph export "`path'/t_`var'.`graphtype'", replace
			loc ++i
		}
	}
end

capture program drop _samregci_scatterbt
program define _samregci_scatterbt, rclass
	qui	{
		syntax anything , path(string) [level(real 95) at(real 0)]
		loc vt =invnorm((100-`level')/200)
		loc i=1
		foreach var in `anything' {
			* scatter de regresiones comunes
			scatter v_`i'_b obs
			if "`graphtype'" == "" noi graph export "`path'/scatter_b_`var'.png", replace
			else if "`graphtype'" == "gph" noi graph save "`path'/scatter_b_`var'.`graphtype'", replace
			else if "`graphtype'" != "" noi graph export "`path'/scatter_b_`var'.`graphtype'", replace
			loc lx =`at'+`vt'
			loc ux =`at'-`vt'
			scatter v_`i'_t obs , yline(`lx') yline(`ux')
			if "`graphtype'" == "" noi graph export "`path'/scatter_t_`var'.png", replace
			else if "`graphtype'" == "gph" noi graph save "`path'/scatter_t_`var'.`graphtype'", replace
			else if "`graphtype'" != "" noi graph export "`path'/scatter_t_`var'.`graphtype'", replace
			loc ++i
		}
	}
end


capture program drop _samregci_sisters
program define _samregci_sisters, rclass
	qui	{
		syntax anything , path(string) [level(real 95) at(real 0) resultsdta(string)]
		loc vt = -invnorm((100-`level')/200)
		loc i=1
		foreach var in `anything' {
			loc lx = `at' - `vt'
			loc ux = `at' + `vt'
			*Grafico de flechas separo positivos y negativos por color
			twoway (pcarrow v_`i'_tSIS obs v_`i'_t obsSIS if v_`i'_tSIS-v_`i'_t <=0, yline(`lx') yline(`ux') mstyle(p3arrow) color(green)  barbsize(1)) ///
			(pcarrow v_`i'_tSIS obs v_`i'_t obsSIS if v_`i'_tSIS-v_`i'_t >0, yline(`lx') yline(`ux') color(maroon) barbsize(1))

			if "`graphtype'" == "" noi graph export "`path'/Sisters_`var'.png", replace
			else if "`graphtype'" == "gph" noi graph save "`path'/Sisters_`var'.`graphtype'", replace
			else if "`graphtype'" != "" noi graph export "`path'/Sisters_`var'.`graphtype'", replace
			
			loc ++i
		}
	}
end

capture program drop _samregci_t1
program define _samregci_t1, rclass
	qui	{
		syntax anything , path(string) [level(real 95) at(real 0) resultsdta(string)] 
		loc vt =invnorm((100-`level')/200)
		mat t1_1 =J(1,6,.)
		loc i=1
		foreach var in `anything' {
			mat t1 =J(1,6,.)
			count if v_`i'_b >=  `at'
			mat t1[1,1] =r(N)
			count if v_`i'_b < `at'
			mat t1[1,2] =r(N)

			loc lx = round(`at' + `vt',0.01)
			loc ux = round(`at' - `vt',0.01)

			count if v_`i'_t < `lx'
			mat t1[1,3] =r(N)
			count if v_`i'_t >= `lx' & v_`i'_t <= `ux'
			mat t1[1,4] =r(N)
			count if v_`i'_t > `ux'
			mat t1[1,5] =r(N)

			count if v_`i'_t != .
			mat t1[1,6] =r(N)
			mat t1_1 = t1_1 \ t1
			loc ++i
		}

		*noi mat list t1_1 
		preserve
		drop _all
		svmat t1_1 
		drop if _n==1
		gen varname = ""
		loc i=1
		foreach var in `anything' {
			replace varname = "`var'" in `i'
			loc ++i
		}
		order varname, first
		* armo tabla de observaciones
		rename t1_11 Pos
		rename t1_12 Neg
		rename t1_13 t_Low
		rename t1_14 t_Btw
		rename t1_15 t_Up
		rename t1_16 Total

		label var Pos     "Coeff. > `at'"
		label var Neg     "Coeff. < `at'"
		label var t_Low   "t-stat < `lx'"
		label var t_Btw   "t-stat between `lx' and `ux'"
		label var t_Up    "t-stat > `ux'"
		label var Total   "Total cases"
		noi di as text "----------------------------------------------------"
		noi di as text " Observations: "
		noi di as text "----------------------------------------------------"
		noi list, noobs table ds divider
		export excel using "`resultsdta'.xlsx", sheet("Table 1", modify) firstrow(varlabels) keepcellfmt

		replace Pos     = round(Pos     / Total *100, 0.1)
		replace Neg     = round(Neg     / Total *100, 0.1)
		replace t_Low   = round(t_Low   / Total *100, 0.1)
		replace t_Btw   = round(t_Btw / Total *100, 0.1)
		replace t_Up    = round(t_Up    / Total *100, 0.1)
		drop Total
		noi di as text "----------------------------------------------------"
		noi di as text " Percentages: "
		noi di as text "----------------------------------------------------"
		noi list, noobs table ds divider
		export excel using "`resultsdta'.xlsx", sheet("Table 2", modify) firstrow(varlabels) keepcellfmt
		restore
	}

end

capture program drop _samregci_tSIS
program define _samregci_tSIS, rclass
	qui	{
		syntax anything , path(string) [level(real 95) at(real 0) resultsdta(string)] 
		loc vt = -invnorm((100-`level')/200)
		mat t1_1 =J(1,10,.)
		loc i=1
		foreach var in `anything' {
			mat t1 =J(1,10,.)
			loc lx = round(`at' - `vt',0.01)
			loc ux = round(`at' + `vt',0.01)
			----------------------------------------------------------------------------------
			arreglar, 1) no suma bien 2) no se si es lo que quiero:
			me interesan las veces q selection cambia la significatividad y eso es significativo!! 
			como en vslist??
			----------------------------------------------------------------------------------

			* Sister significativa, Original significativa en sentido contrario
			gen aux = 1 if (v_`i'_tSIS < `lx' & v_`i'_t > `ux')
			replace aux = 1 if (v_`i'_t < `lx' & v_`i'_tSIS > `ux')
			count if aux == 1
			mat t1[1,1] =r(N)
			drop aux
			* ninguna significativa= Sister no significativa, Original no significativa 
			gen aux = 1 if (v_`i'_tSIS >= `lx' & v_`i'_tSIS <= `ux') & (v_`i'_t >= `lx' & v_`i'_t <= `ux')
			count if aux == 1
			mat t1[1,2] =r(N)
			drop aux
			* Siempre significativa para el mismo lado: cambia de sig a nosig
			gen aux = 1 if (v_`i'_tSIS < `lx' ) & (v_`i'_tSIS < `lx' )  
			replace aux = 1 if (v_`i'_tSIS > `ux') & (v_`i'_tSIS > `ux')
			count if aux == 1
			mat t1[1,3] =r(N)
			drop aux
			* cambios de signivicativa a no significativa: sister signif, orig no signif
			gen aux_tlost = 1 if (v_`i'_tSIS < `lx' | v_`i'_tSIS > `ux') & (v_`i'_t >= `lx' & v_`i'_t <= `ux') 
			count if aux_tlost == 1
			mat t1[1,4] =r(N)
			drop aux_tlost
			* cambios de no signivicativa a significativa: sister no signif, orig signif
			gen aux_tgain = 1 if (v_`i'_t < `lx' | v_`i'_t > `ux') & (v_`i'_tSIS >= `lx' & v_`i'_tSIS <= `ux') 
			count if aux_tgain == 1
			mat t1[1,5] =r(N)
			drop aux_tgain
			count
			mat t1[1,6] =r(N)

			/*
			* Cambios del t-stat significativos
			gen aux_tsig = 1 if abs(v_`i'_t-v_`i'_tSIS) >= `vt' & v_`i'_t>=0 & (v_`i'_t-v_`i'_tSIS)<=0 
			replace aux_tsig = 1 if abs(v_`i'_t-v_`i'_tSIS) >= `vt' & v_`i'_t<0 & (v_`i'_t-v_`i'_tSIS)>0 


			* cambios de rango: pasan de sig a no sig
			count if (v_`i'_tSIS >= `lx' & v_`i'_tSIS <= `ux') &  (v_`i'_t < `lx' | v_`i'_t > `ux')
			mat t1[1,3] =r(N)
			* Siempre sig
			count if ///
			(	///
				(v_`i'_tSIS < `lx' | v_`i'_tSIS > `ux') &  (v_`i'_t < `lx' | v_`i'_t > `ux') ///
			) | ( ///
				v_`i'_tSIS >= `lx' & v_`i'_tSIS <= `ux' &  v_`i'_t >= `lx' & v_`i'_t <= `ux' ///
			) | ( ///
				v_`i'_t >= `lx' & v_`i'_t <= `ux' &  (v_`i'_tSIS < `lx' | v_`i'_tSIS > `ux') ///
			) 
			mat t1[1,4] =r(N)

			
			count if v_`i'_t != .
			mat t1[1,5] =r(N)
			*/
			mat t1_1 = t1_1 \ t1
			loc ++i
		}

		preserve
		drop _all
		svmat t1_1 
		drop if _n==1
		gen varname = ""
		loc i=1
		foreach var in `anything' {
			replace varname = "`var'" in `i'
			loc ++i
		}
		order varname, first
		* armo tabla de observaciones
		rename t1_11 CambioSignificativo
		rename t1_12 NingunaSignificativo
		rename t1_13 SiempreSignificativoIgual
		rename t1_14 SignNosig
		rename t1_15 NosigSig
		rename t1_16 Total
		/*
		label var Increase   "Sister Regresion Increase t-stat"
		label var Decrease   "Sister Regresion Decrease t-stat"
		label var Change     "Sister Regresion turn t-stat no significant"
		label var Remain     "Sister Regresion t-stat remain significant"
		label var Total      "Total cases"
		*/
		noi di as text "----------------------------------------------------"
		noi di as text " Observations: "
		noi di as text "----------------------------------------------------"
		noi list, noobs table ds divider
		export excel using "`resultsdta'.xlsx", sheet("Sisters 1", modify) firstrow(varlabels) keepcellfmt

		replace CambioSignificativo   = round(CambioSignificativo  / Total *100, 0.1)
		replace NingunaSignificativo   = round(NingunaSignificativo  / Total *100, 0.1)
		replace SiempreSignificativoIgual     = round(SiempreSignificativoIgual    / Total *100, 0.1)
		replace SignNosig     = round(SignNosig / Total *100, 0.1)
		replace NosigSig     = round(NosigSig / Total *100, 0.1)
		drop Total
		noi di as text "----------------------------------------------------"
		noi di as text " Percentages: "
		noi di as text "----------------------------------------------------"
		noi list, noobs table ds divider
		export excel using "`resultsdta'.xlsx", sheet("Sisters 2", modify) firstrow(varlabels) keepcellfmt
		restore
	}

end


capture program drop _samregci_vslist
program define _samregci_vslist, rclass
	qui	{
		syntax anything , path(string) [ level(real 95) at(real 0) resultsdta(string) fixvar(string) tsig tlost ] 
		loc vt = -invnorm((100-`level')/200)
		mat t1_1 =J(1,5,.)
		loc ndrop =wordcount("`anything'")+wordcount("`fixvar'")
		loc lx = round(`at' - `vt',0.01)
		loc ux = round(`at' + `vt',0.01)
		loc i=1
		foreach var in `anything' {
			preserve
			* cambios de rango: pasan de sig a no sig
			gen aux_tlost = 1 if (v_`i'_tSIS < `lx' | v_`i'_tSIS > `ux') & (v_`i'_t >= `lx' & v_`i'_t <= `ux') 
			* Cambios del t-stat significativos
			gen aux_tsig = 1 if abs(v_`i'_t-v_`i'_tSIS) >= `vt' & v_`i'_t>=0 & (v_`i'_t-v_`i'_tSIS)<=0 
			replace aux_tsig = 1 if abs(v_`i'_t-v_`i'_tSIS) >= `vt' & v_`i'_t<0 & (v_`i'_t-v_`i'_tSIS)>0 
			if "`tlost'" == "" & "`tsig'" == "" gen aux =1 if aux_tlost ==1 & aux_tsig ==1
			if "`tsig'"  != "" clonevar aux = aux_tsig 
			if "`tlost'" != "" clonevar aux = aux_tlost
			replace aux = . if v_`i'_t    ==0 | v_`i'_t    ==.
			replace aux = . if v_`i'_tSIS ==0 | v_`i'_tSIS ==.
			keep if aux == 1
			drop aux aux_tsig aux_tlost
			count
			if r(N)==0 {
				noi di as text "----------------------------------------------------"
				noi di as text " `var': No variable lost of significance "
				noi di as text "----------------------------------------------------"
				restore
				loc ++i
				continue
			}
			drop _merge nvar obs obsSIS nvarSIS
			tempfile aux
			save `aux', replace
			forvalues f =1/`ndrop' {
				drop v_`f'_t
				drop v_`f'_b
				drop v_`f'_tSIS
				drop v_`f'_bSIS
			}
			loc listav ""
			foreach cvar of varlist *_t {
				sum `cvar'
				if r(N) ==0 {
					drop `cvar'
					continue
				}
				loc listav "`listav' `cvar'"
				replace `cvar' =1 if `cvar'!=0 & `cvar'!=.
			}
			merge 1:1 order using `aux'
			drop if _merge ==1
			drop if _merge ==2
			drop _merge order 
			loc nv = wordcount("`listav'")
			mat lv =J(`nv',5,.)
			loc j=1
			foreach sv of varlist `listav' {
				sum `sv'
				mat lv[`j',1]=r(sum)
				sum v_`i'_b if `sv' ==1
				mat lv[`j',2]=r(mean)
				sum v_`i'_bSIS if `sv' ==1
				mat lv[`j',3]=r(mean)
				sum v_`i'_t if `sv' ==1
				mat lv[`j',4]=r(mean)
				sum v_`i'_tSIS if `sv' ==1
				mat lv[`j',5]=r(mean)
				loc ++j
			}
			keep `listav'
			d, replace clear
			count
			loc tt=r(N)
			forvalues j =1 / `tt' {
				loc name_`j' =varlab[`j']
			}
			drop _all
			svmat lv
			gen varname = ""
			loc j=1
			forvalues j =1 / `tt' {
				replace varname = strreverse(substr(strreverse("`name_`j''"), 8,.)) in `j'
			}
			order varname, first
			rename lv1 Cases
			rename lv2 Beta
			rename lv3 BetaSis
			rename lv4 t
			rename lv5 tSis
			drop if varname == "Constant"
			label var varname "Variable Name"
			if "`tsig'" == "" & "`tlost'" == "" label var Cases "Num. of regressions: significant t-stat reduction, main variable not significant"
			if "`tlost'" != "" label var Cases "Number of regressions where the variable turn the main variable not significant"
			if "`tsig'"  != "" label var Cases "Number of regressions with a significant reduction of the t-statistic"
			label var Beta "Average coefficient of main variable"
			label var BetaSis "Average coefficient of main variable on Sister Regression"
			label var t "Average t-statistic of main variable"
			label var tSis "Average t-statistic of main variable on Sister Regression"
			gen aux = -Cases
			sort aux
			drop aux
			noi di as text "----------------------------------------------------"
			noi di as text " `var': Lost of significance"
			noi di as text " List of covariables included in report "
			noi di as text "----------------------------------------------------"
			export excel using "`resultsdta'.xlsx", sheet("`var' List", modify) firstrow(varlabels) keepcellfmt
			restore
			loc ++i
		}
	}
end


