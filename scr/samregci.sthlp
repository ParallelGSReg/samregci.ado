{smcl}
{* *! version 1.0  13Aug2024}{...}
{hline}

{title:Title}

{p2colset 5 16 18 2}{...}
{p2col:{hi:samregci}: Sensitivity Analysis for the Main Regression Coefficients of Interest}{p_end}
{p2colreset}{...}


{title:Syntax}

{p 8 16 2}
{cmd:samregci} {depvar} {varlist:_Main} {ifin} {weight} 
{cmd:, } 
{opt IT:erateover(varlist_Selection)}
[{opt nc:omb(#1,#2)}
{opt f:ixvar(varlist_fix)}
{opt tsig}
{opt tlost}
{opt cmde:st(commandname)}
{opt cmdo:ptions(commandoptions)}
{opt cmds:tat(commandstats)}
{opt cmdi:veq(varlist_end = varlist_inst)}
{opt same:sample}
{opt co:unt}
{opt res:ultsdta(newbasename)}
{opt rep:lace}
{opt do:uble}
{opt nog:raph}
{opt graphtype}
{opt level(#)}
{opt at(#)}]

{phang2} {cmd:fweight}s, {cmd:aweight}s, and {cmd:pweight}s are allowed depending on the estimation command specified in {opt cmdest(commandname)}; see
{help weight}. {p_end}

{*:*****************************************************************************************}
{title:Sections}
{*:*****************************************************************************************}
{pstd} 
Main sections are presented under the following headings:
{p_end}

{phang2} {it:{help samregci##Description:Description}} {p_end}
{phang2} {it:{help samregci##Options:Options}} {p_end}
{phang2} {it:{help samregci##Examples:Examples}} {p_end}
{phang2} {it:{help samregci##Saved_results:Saved results}} {p_end}

{*:*****************************************************************************************}
{marker Description}{...}
{title:Description}
{*:*****************************************************************************************}
{pstd} 
{cmd:samregci} is a derivation of {cmd:gsreg} an automatic model selection command. 
By default {cmd:samregci} performs all possible regressions always including {varlist:_Main} as 
regressors, and iterating over all possible combinations among selection variables 
specified in {varlist:_Selection}. Regression results are stored in a .dta file 
named as {it:samregci.dta}. This database 
assigns one row for each regression and includes the following columns: 
{p_end}

{marker list1}{...}
{phang} 1) regression id (variable {it:order}){p_end}
{phang} 2) covariate regression coefficients (named {it:v_1_b, v_2_b... , etc.}, 
and labeled with the full covariate name plus the word "coeff.") {p_end}
{phang} 3) coefficient t-statistics (named {it:v_1_t, v_2_t..., etc.}, and labeled
with the full covariate name plus the word "tstat.") {p_end}
{phang} 4) number of observations (variable {it:obs}) {p_end}
{phang} 5) number of covariates -including the intercept- (variable {it:nvar}) {p_end}

{phang} covariates includes main and selection variables (and fix variables if it is included) {p_end}
 

{phang}{cmd:.samregci depvar main, iterateover(svar1 svar2)}{p_end}

{pstd} runs the 3 following regressions {p_end}
{pstd} {it: regress depvar main svar1} {p_end}
{pstd} {it: regress depvar main svar2} {p_end}
{pstd} {it: regress depvar main svar1 svar2} {p_end}
{pstd} and generates a {it:samregci.dta} database like: {p_end}

order	v_1_b	v_1_t	v_2_b	v_2_t	v_3_b	v_3_t	v_4_b	v_4_t	obs	nvar	
1	#	#	#	#			#	#	#	3	
2	#	#			#	#	#	#	#	3	
3	#	#	#	#	#	#	#	#	#	4	

{pstd} Below is a description of each variable of the dataset:{p_end}

{phang} {cmd:.use samregci.dta} {p_end}
{phang} {cmd:.describe} {p_end}
		
variable	variable label
---------	--------------					
order		order number of estimation
v_1_b		Main variable coeff.
v_1_t		Main variable tstat.
v_2_b		svar1 coeff.
v_2_t		svar1 tstat.
v_3_b		svar2 coeff.
v_3_t		svar2 tstat.
v_4_b		intercept coeff.
v_4_t		intercept tstat.
obs		number of observations
nvar		number of covariates

{phang} {cmd:samregci} also provides an excel file named samregci.xlsx including tables and other results. 
By default, it includes tables summarizing
the number (and percentage) of regressions in witch the main varibles coefficients are positive/negative,
significant/not significant. The excel file will also includes a list of "relevant" selection
variables for each Main variable. By default, all selection variables that holds 2 conditions; 
(i) turn the main variable coefficient from significant to non-significant, and, 
(ii) causes a significant reduction of the main variable t-statistic, will be included in the list.
{p_end}

{phang}
{cmd:samregci} draws a kernel density plots for coefficients and t-statistic of the main variables.
{p_end}

{phang}
If the number of observations changes between regressions, scatter plots of coefficients 
and t-statistic join with the number of observation will be also included. Furthermore an Excel 
file (samregci.xlsx) will 
include tables summarizing the number (and percentage) of regressions in which the coefficients 
of the main variables increase/decrease and they change or not its significance due to the inclusion 
of selection variables. Paired-coordinate plot with arrows will also be made showing the change in 
the t-statistic due to the inclusion of selection variables along with the number of observations.
{p_end}

{phang}
By default, all results including graphics and tables are saved in the working directory. 
{p_end}

{pstd} Back to {it:{help samregci}} {p_end}

{*:*****************************************************************************************}
{marker Options}{...}
{title:Options}
{*:*****************************************************************************************}
{marker genoption}{...}
{syntab:{it:General options}}

{phang} 
{opt it:erateover(varlist_Selection)} 
It is a mandatory option that contains the list of variables to iterate over and 
evaluate their incidence on the main variables' coefficients.
Variables defined in {varlist:_Selection} must not be included among the 
Main variables({it:{help samregci:varlist_Main}}). 
{p_end}

{phang} 
{opt nc:omb(#1,#2)} specifies the minimum and maximum number of variable covariates 
to be included in the selection procedure. {cmd:samregci} 
will perform all possible combinations (regressions) between selection variables taken 
from {it:#1} to {it:#2}. {it:#1} must be less or equal to {it:#2} and, additionally, 
the number of selection variables must be greater or equal to {it:#2}. If this option is not 
specified, {cmd:samregci} will run all possible combinations in sets of 3 (without repetition). 
See {it:{help samregci##egncomb:Examples of using ncomb}} 
{p_end}

{marker fixvar}{...}
{phang} 
{opt f:ixvar(varlist_fix)} allows users to specify a subset of covariates that must be 
included in all regressions. Variables defined in {varlist:_fix} must not be included 
among the standard main variables nor selection variables ({it:varlist_Main}, {it:varlist_Selection}). 
See {it:{help samregci##egfixvar:Examples of using fixvar}} 
{p_end}

{*:*****************************************************************************************}
{marker commandopt}{...}
{syntab:{it:Regression command options}}

{phang} 
{opt cmde:st(commandname)} allows choosing the regression command to be used. If the option is 
not specified, {it:commandname} default is {it:{help regress:regress}}. This option allows using
{it:{help regress:regress}}, {it:{help xtreg:xtreg}}, {it:{help probit:probit}}, 
{it:{help logit:logit}}, {it:{help areg:areg}}, {it:{help qreg:qreg}} and {it:{help plreg:plreg}},
but it additionally accept any regression command that respects the syntax of 
{it:{help regress:regress}} 
and saves results in the same way ({it:matrices e(b) and e(V)}). {it:{help ivregress:ivregress}}
is also acepted using option {opt cmdiveq(varlist_end = varlist_inst)}.
See {it:{help samregci##egcommand:Examples of using cmdest, cmdoptions, cmdstat and cmdiveq}} 
{p_end}

{phang} 
{opt cmdo:ptions(commandoptions)} allows adding supported (by {it:commandname}) additional 
options for each regression. 
See {it:{help samregci##egcommand:Examples of using cmdest, cmdoptions, cmdstat and cmdiveq}} 
{p_end}

{phang} 
{opt cmds:tat(commandstats)} enables {cmd:samregci} (which automatically saves the number of
observations -{it:{help samregci##list1:obs}}- and the number of covariates -{it:{help samregci##list1:nvar}}-) 
to save additional regression statistics saved as scalars {it:{help e():e()}} by the regression 
command.
See {it:{help samregci##egcommand:Examples of using cmdest, cmdoptions, cmdstat and cmdiveq}} 
{p_end}

{phang} 
{opt cmdi:veq(varlist_end = varlist_inst)} is a special option to include a varlist 
of endogenous variables ({it:varlist_end}) and of instruments ({it:varlist_inst}) when
the estimator command is {it:{help ivregress:ivregress}}. When using this option, 
{opt cmdest(ivregress 2sls)}, {opt cmdest(ivregress liml)} or {opt cmdest(ivregress gmm)}
must be specified. The endogenous variables must be included in {it:varlist_fix}
(see option {it:{help samregci##fixvar:fixvar}}). 
See {it:{help samregci##egcommand:Examples of using cmdest, cmdoptions, cmdstat and cmdiveq}} 
{p_end}

{phang} 
{opt same:sample} makes all regressions to be performed over the same
sample of observations, defined as the largest common sample. By default, {cmd:samregci} 
performs each regression with the maximum number of common observations available 
for the covariate subset used in each particular case. 
See {it:{help samregci##egsamesample:Examples of using samesample}} 
{p_end}

{phang} 
{opt co:unt} shows for each alternative model, the regression number 
(used for identification purposes) and the total number of regressions to be estimated. 
If this option is not specified samregci will hides from the screen the number 
of regression which is being estimated. This option increases the execution time. 
{p_end}

{*:*****************************************************************************************}
{marker posestim}{...}
{syntab:{it:Post-estimation and output options}}


{phang} 
{opt res:ultsdta(newbasename)} allows results database name to be user defined 
in {it:{help filename:newbasename}}. By default, the name will be {it:samregci.dta}.  {p_end}

{phang} {opt rep:lace} replaces the results database if it is already created (with 
the same name) in the ongoing working directory.  {p_end}

{phang} {opt do:uble} forces results to be created and saved in {it:{help data_types:double}}
format, that is, with double precision. {p_end}

{phang} {opt nog:raph} avoid the execution of kernel density graphs. {p_end}

{phang} {opt graphtype} allows you to change the format in which graphs will be exported by 
making the extension explicit; by default, graphics will be exported as {it:png}. 
Supported formats are {it: png, ps, eps, svg, emf, pdf, tif, gif} and {it: jpg}. 
graphtype(gph) will save graphs instead of exporting them.
{p_end}

{phang} {opt level(#)} set the significance level (p-value) for sensivility análisis. 
By default the level is 95%. {p_end}

{phang} {opt at(#)} allows to evaluate statistical significance of coefficients over a specific value. 
By default, the coefficient analysis evaluates the significant differences from 0. {p_end}

{phang} {opt tlost} and {opt tsig} allows to change criterium to consider by witch variables in 
it:{varilist_Selection} affect the coefficients of main variables (it:{varilist_Main}). 
The excel file (samregci.xlsx) will include a list of "relevant" selection variables for each Main variable. 
The option {opt tlost} includes all selection variables that cause the main variable to go 
from significant to non-significant; the option {opt tsig} includes instead, all variables 
that make drop t-statistic of main variable in a significant magnitude. 
By default, the list only include variables that hold both conditions. {p_end}

{*:*****************************************************************************************}
{marker Examples}{...}
{title:Examples}
{*:*****************************************************************************************}
{phang2}{cmd:. sysuse auto} {p_end}
{phang2}{cmd:. samregci price mpg, iterateover(weight foreign)} {p_end}
{pstd} In this case there is 1 main variable (mpg), samregci will perform the sensitivity
analisis of mpg coefficient {p_end}
{pstd} there are 2 selection covariates (weight and foreign), {cmd:samregci} will 
perform all possible combinations (regressions) without repetition taken from 2:{p_end}
{pstd} {it: regress price mpg weight} {p_end}
{pstd} {it: regress price mpg foreign} {p_end}
{pstd} {it: regress price mpg weight foreign} {p_end}

{marker egncomb}{...}
{dlgtab: Examples of using ncomb}
{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1,1)} {p_end}
{pstd} or equivalently {p_end}
{phang2}{cmd:. samregci depvar main svar1 svar2 svar3, ncomb(1)} {p_end}
{pstd} In this case there are 3 candidate covariates, {cmd:samregci} will perform all
possible combinations (regressions) without repetition of size 1 taken from 3:{p_end}
{pstd} {it: regress depvar main svar1} {p_end}
{pstd} {it: regress depvar main svar2} {p_end}
{pstd} {it: regress depvar main svar3} {p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1,2)} {p_end}
{pstd} will perform all possible combinations (regressions) without repetition
of size 1 and 2, taken from 3, that is, it will perform the following 6 regressions:{p_end}
{pstd} {it: regress depvar main svar1} {p_end}
{pstd} {it: regress depvar main svar2} {p_end}
{pstd} {it: regress depvar main svar3} {p_end}
{pstd} {it: regress depvar main svar1 svar2} {p_end}
{pstd} {it: regress depvar main svar1 svar3} {p_end}
{pstd} {it: regress depvar main svar2 svar3} {p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1,3)} {p_end}
{pstd} {cmd:samregci} will perform all possible combinations (regressions) without repetition
of size 1 to 3, taken from 3:{p_end}
{pstd} {it: regress depvar main svar1} {p_end}
{pstd} {it: regress depvar main svar2} {p_end}
{pstd} {it: regress depvar main svar3} {p_end}
{pstd} {it: regress depvar main svar1 svar2} {p_end}
{pstd} {it: regress depvar main svar1 svar3} {p_end}
{pstd} {it: regress depvar main svar2 svar3} {p_end}
{pstd} {it: regress depvar main svar1 svar2 svar3} {p_end}

{pstd} Back to {it:{help samregci##genoption:General options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}
{*:*****************************************************************************************}
{marker egunbalanced}{...}
{dlgtab: Examples of changes in observations due to selection variables}

{pstd} suppose {it:depvar}, {it:main} and {it:svar1} have 50 nonmissing 
observations, {it:svar2} have 46 nonmissing 
observations and {it:svar3} has only 48, then {p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1)} {p_end}

{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: regress depvar main svar1}, with 50 observations {p_end}
{pstd} {it: regress depvar main svar2}, with 46 observations {p_end}
{pstd} {it: regress depvar main svar3}, with 48 observations {p_end}

{pstd} samregci additionally will perform the following 3 "sisters" 
regressions (one for each original regressions): {p_end}

{pstd} {it: regress depvar svar1}, with 50 observations {p_end}
{pstd} {it: regress depvar svar2}, with 46 observations {p_end}
{pstd} {it: regress depvar svar3}, with 48 observations {p_end}

{pstd} The results of sisters regressions will be stored in a .dta file 
named as {it:samregci_sisters.dta}. Additional tables and figures will be show and stored:
{p_end}

{pstd} - One table for each main variable summarizing the number (and percentage) of regressions in which the
coefficients of the main variables increase/decrease and they change or not its significance due to the 
inclusion of selection variables (depending of the options {it: tlost} and {it: tsig} ). {p_end}

{pstd} - Paired-coordinate plot with arrows will also be made for each main variable showing the change 
in the t-statistic due to the inclusion of selection variables along with the number of observations
(i.e. comparing each main coefficient of samregci.dta and samregci_sisters.dta).
{p_end}

{pstd} Back to {it:{help samregci##genoption:General options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}

{*:*****************************************************************************************}
{marker egsamesample}{...}
{dlgtab: Examples of using samesample}

{pstd} suppose {it:depvar}, {it:main}, {it:svar1} and {it:svar2} have 50 nonmissing 
observations but {it:svar3} has only 48, then {p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1)} {p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: regress depvar main svar1}, with 50 observations {p_end}
{pstd} {it: regress depvar main svar2}, with 50 observations {p_end}
{pstd} {it: regress depvar main svar3}, with 48 observations {p_end}

{pstd} while: {p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1)} samesample {p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: regress depvar main svar1}, with 48 observations {p_end}
{pstd} {it: regress depvar main svar2}, with 48 observations {p_end}
{pstd} {it: regress depvar main svar3}, with 48 observations {p_end}

{pstd} Back to {it:{help samregci##genoption:General options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}

{*:*****************************************************************************************}
{marker egfixvar}{...}
{dlgtab: Examples of using fixvar}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2) ncomb(1) fixvar(fixvar1 fixvar2)} {p_end}
{pstd} will perform the following 2 regressions: {p_end}
{pstd} {it: regress depvar main svar1 fixvar1 fixvar2} {p_end}
{pstd} {it: regress depvar main svar2 fixvar1 fixvar2} {p_end}

{pstd} 
when using fixvar without the {opt ncomb} option, {cmd:samregci} will also evaluate regressions 
without any variable candidate covariate:
{p_end}

{pstd} Back to {it:{help samregci##fixvar:Fixed variable options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}

{*:*****************************************************************************************}
{marker egcommand}{...}
{dlgtab: Examples of using cmdest, cmdoptions, cmdstat and cmdiveq}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2) cmdest(probit) cmdstat(r2_p)} {p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: probit depvar main svar1} {p_end}
{pstd} {it: probit depvar main svar2} {p_end}
{pstd} {it: probit depvar main svar1 svar2} {p_end}
{pstd} and the Pseudo R2 of each estimation (e(r2_p)) will be saved in {it:samregci.dta}
{p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2) cmdoptions(robust) } {p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: regress depvar main svar1, robust} {p_end}
{pstd} {it: regress depvar main svar2, robust} {p_end}
{pstd} {it: regress depvar main svar1 svar2, robust} {p_end}


{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2) ncomb(1,2) cmdest(xtreg) cmdoptions(fe vce(robust)) 
cmdstat(sigma_u sigma_e rho r2_w r2_b r2_o) } {p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: xtreg depvar main svar1, fe vce(robust)} {p_end}
{pstd} {it: xtreg depvar main svar2, fe vce(robust)} {p_end}
{pstd} {it: xtreg depvar main svar1 svar2, fe vce(robust)} {p_end}
{pstd} and the scalars stored in e(sigma_u), e(sigma_e), e(r2_p), e(rho), e(r2_w), e(r2_b) and e(r2_o) 
will be saved in {it:samregci.dta} {it:{help xtreg:xtreg}}
{p_end}

{phang2}
{cmd:. samregci depvar main, iterateover(svar1 svar2) cmdest(ivregress liml) cmdiveq(svar2= ivar1 ivar2)} 
{p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: ivregress liml depvar main svar1} {p_end}
{pstd} {it: ivregress liml depvar main svar2 (svar2= ivar1 ivar2)} {p_end}
{pstd} {it: ivregress liml depvar main svar1 svar2 (svar2= ivar1 ivar2)} {p_end}
{pstd} (the first regression is equivalent to {it:regress depvar svar1}) {p_end}

{phang2}
{cmd:. samregci depvar main, iterateover(svar1 svar2) cmdest(ivregress gmm) cmdiveq(svar2= ivar1 ivar2)} 
{p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: ivregress gmm depvar main svar1 (=ivar1 ivar2)} {p_end}
{pstd} {it: ivregress gmm depvar main svar2 (svar2= ivar1 ivar2)} {p_end}
{pstd} {it: ivregress gmm depvar main svar1 svar2 (svar2= ivar1 ivar2)} {p_end}

{phang2}
{cmd:. samregci depvar main, iterateover(svar1 svar2) cmdest(ivregress 2sls) cmdiveq(fixvar1 fixvar2= ivar1 ivar2) fixvar(fixvar1 fixvar2)} 
{p_end}
{pstd} will perform the following 3 regressions: {p_end}
{pstd} {it: ivregress 2sls depvar main svar1 (fixvar1 fixvar2= ivar1 ivar2)} {p_end}
{pstd} {it: ivregress 2sls depvar main svar2 (fixvar1 fixvar2= ivar1 ivar2)} {p_end}
{pstd} {it: ivregress 2sls depvar main svar1 svar2 (fixvar1 fixvar2= ivar1 ivar2)} {p_end}

{pstd} Back to {it:{help samregci##commandopt:Regressions command options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}

{pstd} Back to {it:{help samregci##posestim:Post-estimation options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}
{*:*****************************************************************************************}
{marker egoutputopt}{...}
{dlgtab: Examples of using resultsdta, replace, double}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2) resultsdta(myresults) } {p_end}
{pstd} generates a results database named {it:myresults.dta} instead of {it:samregci.dta}
{p_end}

{phang2}{cmd:. samregci depvar main, iterateover(svar1 svar2) resultsdta(myresults) replace } {p_end}
{pstd} will replaced {it:myresults.dta} if it has already been created in the ongoing working 
directory. All results of {it:myresults.dta} will be stored with double precision
{p_end}

{pstd} Back to {it:{help samregci##outputopt:Output options}} {p_end}
{pstd} Back to {it:{help samregci:Top}} {p_end}

{*:*****************************************************************************************}
{marker Saved_results}{...}
{title:Saved results}
{*:*****************************************************************************************}
{pstd} 
{cmd:samregci} creates a .dta file with outcome information for all estimated 
alternatives. By default, it includes the following columns for each regression: 
{p_end}

{phang} 1) regression id (variable {it:order}) {p_end}
{phang} 2) covariate regression coefficients (named {it:v_1_b, v_2_b... , etc.}, 
and labeled with the full covariate name plus the word "coeff.")
{p_end}
{phang} 3) coefficient t-statistics (named {it:v_1_t, v_2_t..., etc.}, and labeled
with the full covariate name plus the word "tstat.") 
{p_end}
{phang} 4) number of observations (variable {it:obs}) {p_end}
{phang} 5) number of covariates (variable {it:nvar}) {p_end}
{p_end}
{phang} 6) additional user specified statistics (if the {opt cmdstat} option is specified)
{p_end}

{pstd} Back to {it:{help samregci:Top}} {p_end}

{*:*****************************************************************************************}
{marker Authors}{...}
{title:Authors}

{pstd}Pablo Gluzmann{p_end}
{pstd}CEDLAS-fce-UNLP and CONICET{p_end}
{pstd}La Plata, Argentina{p_end}
{pstd}gluzmann@yahoo.com{p_end}

{pstd}Demian Panigo{p_end}
{pstd}Instituto Malvinas, UNLP and CONICET{p_end}
{pstd}La Plata, Argentina{p_end}
{pstd}panigo@gmail.com{p_end}

{pstd}Back to {it:{help samregci:Top}} {p_end}

{*:*****************************************************************************************}
{title:Also see}

{p 7 14 2}Help: {it:{help gsreg:gsreg}}

