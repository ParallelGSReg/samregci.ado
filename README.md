# samregci: Sensitivity Analysis for the Main Regression Coefficients of Interest


## Syntax

        samregci depvar varlist_Main [if] [in] [weight] , ITerateover(varlist_Selection)
                [ncomb(#1,#2) fixvar(varlist_fix) tsig tlost cmdest(commandname)
                cmdoptions(commandoptions) cmdstat(commandstats) cmdiveq(varlist_end = varlist_inst)
                samesample count resultsdta(newbasename) replace double nograph graphtype level(#)
                at(#)]

        fweights, aweights, and pweights are allowed depending on the estimation command specified in
            cmdest(commandname); see weight.


## Sections

    Main sections are presented under the following headings:

        Description
        Options
        Examples
        Saved results


## Description

    samregci performs a sensitivity analysis of the coefficients of the main variables of interest
    for regression models, to changes in the available covariates or control variables.  By default
    samregci performs all possible regressions always including varlist_Main as regressors, and
    iterating over all possible combinations among selection variables specified in 
    varlist_Selection. Regression results are stored in a .dta file named as samregci.dta. This
    database assigns one row for each regression and includes the following columns:

    1) regression id (variable order)
    2) covariate regression coefficients (named v_1_b, v_2_b... , etc., and labeled with the full
        covariate name plus the word "coeff.")
    3) coefficient t-statistics (named v_1_t, v_2_t..., etc., and labeled with the full covariate
        name plus the word "tstat.")
    4) number of observations (variable obs)
    5) number of covariates -including the intercept- (variable nvar)

    covariates includes main and selection variables (and fix variables if it is included)
 

    .samregci depvar main, iterateover(svar1 svar2)

    runs the 3 following regressions
    regress depvar main svar1
    regress depvar main svar2
    regress depvar main svar1 svar2
    and generates a samregci.dta database like:

order   v_1_b   v_1_t   v_2_b   v_2_t   v_3_b   v_3_t   v_4_b   v_4_t   obs     nvar    
1       #       #       #       #                       #       #       #       3       
2       #       #                       #       #       #       #       #       3       
3       #       #       #       #       #       #       #       #       #       4       

    Below is a description of each variable of the dataset:

    .use samregci.dta
    .describe
                
variable        variable label
---------       --------------                                  
order           order number of estimation
v_1_b           Main variable coeff.
v_1_t           Main variable tstat.
v_2_b           svar1 coeff.
v_2_t           svar1 tstat.
v_3_b           svar2 coeff.
v_3_t           svar2 tstat.
v_4_b           intercept coeff.
v_4_t           intercept tstat.
obs             number of observations
nvar            number of covariates

    samregci also provides an excel file named samregci.xlsx including tables and other results.  By
        default, it includes tables summarizing the number (and percentage) of regressions in witch
        the main varibles coefficients are positive/negative, significant/not significant. The excel
        file will also includes a list of "relevant" selection variables for each Main variable. By
        default, all selection variables that holds 2 conditions; (i) turn the main variable
        coefficient from significant to non-significant, and, (ii) causes a significant reduction of
        the main variable t-statistic, will be included in the list.

    samregci draws a kernel density plots for coefficients and t-statistic of the main variables.

    If the number of observations changes between regressions, scatter plots of coefficients and
        t-statistic join with the number of observation will be also included. Furthermore an Excel
        file (samregci.xlsx) will include tables summarizing the number (and percentage) of
        regressions in which the coefficients of the main variables increase/decrease and they change
        or not its significance due to the inclusion of selection variables. Paired-coordinate plot
        with arrows will also be made showing the change in the t-statistic due to the inclusion of
        selection variables along with the number of observations.

    By default, all results including graphics and tables are saved in the working directory.

    Back to samregci


## Options

  ### General options

    iterateover(varlist_Selection) It is a mandatory option that contains the list of variables to
        iterate over and evaluate their incidence on the main variables' coefficients.  Variables
        defined in varlist_Selection must not be included among the Main variables(varlist_Main).

    ncomb(#1,#2) specifies the minimum and maximum number of variable covariates to be included in
        the selection procedure. samregci will perform all possible combinations (regressions)
        between selection variables taken from #1 to #2. #1 must be less or equal to #2 and,
        additionally, the number of selection variables must be greater or equal to #2. If this
        option is not specified, samregci will run all possible combinations in sets of 3 (without
        repetition).  See Examples of using ncomb

    fixvar(varlist_fix) allows users to specify a subset of covariates that must be included in all
        regressions. Variables defined in varlist_fix must not be included among the standard main
        variables nor selection variables (varlist_Main, varlist_Selection).  See Examples of using
        fixvar


  ### Regression command options

    cmdest(commandname) allows choosing the regression command to be used. If the option is not
        specified, commandname default is regress. This option allows using regress, xtreg, probit,
        logit, areg, qreg and plreg, but it additionally accept any regression command that respects
        the syntax of regress and saves results in the same way (matrices e(b) and e(V)). ivregress
        is also acepted using option cmdiveq(varlist_end = varlist_inst).  See Examples of using
        cmdest, cmdoptions, cmdstat and cmdiveq

    cmdoptions(commandoptions) allows adding supported (by commandname) additional options for each
        regression.  See Examples of using cmdest, cmdoptions, cmdstat and cmdiveq

    cmdstat(commandstats) enables samregci (which automatically saves the number of observations -
        obs- and the number of covariates -nvar-) to save additional regression statistics saved as
        scalars e() by the regression command.  See Examples of using cmdest, cmdoptions, cmdstat and
        cmdiveq

    cmdiveq(varlist_end = varlist_inst) is a special option to include a varlist of endogenous
        variables (varlist_end) and of instruments (varlist_inst) when the estimator command is
        ivregress. When using this option, cmdest(ivregress 2sls), cmdest(ivregress liml) or
        cmdest(ivregress gmm) must be specified. The endogenous variables must be included in
        varlist_fix (see option fixvar).  See Examples of using cmdest, cmdoptions, cmdstat and
        cmdiveq

    samesample makes all regressions to be performed over the same sample of observations, defined as
        the largest common sample. By default, samregci performs each regression with the maximum
        number of common observations available for the covariate subset used in each particular
        case.  See Examples of using samesample

    count shows for each alternative model, the regression number (used for identification purposes)
        and the total number of regressions to be estimated.  If this option is not specified
        samregci will hides from the screen the number of regression which is being estimated. This
        option increases the execution time.


  ### Post-estimation and output options


    resultsdta(newbasename) allows results database name to be user defined in newbasename. By
        default, the name will be samregci.dta.

    replace replaces the results database if it is already created (with the same name) in the
        ongoing working directory.

    double forces results to be created and saved in double format, that is, with double precision.

    nograph avoid the execution of kernel density graphs.

    graphtype allows you to change the format in which graphs will be exported by making the
        extension explicit; by default, graphics will be exported as png.  Supported formats are png,
        ps, eps, svg, emf, pdf, tif, gif and jpg.  graphtype(gph) will save graphs instead of
        exporting them.

    level(#) set the significance level (p-value) for sensivility análisis.  By default the level is
        95%.

    at(#) allows to evaluate statistical significance of coefficients over a specific value.  By
        default, the coefficient analysis evaluates the significant differences from 0.

    tlost and tsig allows to change criterium to consider by witch variables in
        it:{varilist_Selection} affect the coefficients of main variables (it:{varilist_Main}).  The
        excel file (samregci.xlsx) will include a list of "relevant" selection variables for each
        Main variable.  The option tlost includes all selection variables that cause the main
        variable to go from significant to non-significant; the option tsig includes instead, all
        variables that make drop t-statistic of main variable in a significant magnitude.  By
        default, the list only include variables that hold both conditions.


## Examples

        . sysuse auto
        . samregci price mpg, iterateover(weight foreign)
    In this case there is 1 main variable (mpg), samregci will perform the sensitivity analisis of
    mpg coefficient
    there are 2 selection covariates (weight and foreign), samregci will perform all possible
    combinations (regressions) without repetition taken from 2:
    regress price mpg weight
    regress price mpg foreign
    regress price mpg weight foreign

        +--------------------------+
    ----+  Examples of using ncomb +-----------------------------------------------------------------
        . samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1,1)
    or equivalently
        . samregci depvar main svar1 svar2 svar3, ncomb(1)
    In this case there are 3 candidate covariates, samregci will perform all possible combinations
    (regressions) without repetition of size 1 taken from 3:
    regress depvar main svar1
    regress depvar main svar2
    regress depvar main svar3

        . samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1,2)
    will perform all possible combinations (regressions) without repetition of size 1 and 2, taken
    from 3, that is, it will perform the following 6 regressions:
    regress depvar main svar1
    regress depvar main svar2
    regress depvar main svar3
    regress depvar main svar1 svar2
    regress depvar main svar1 svar3
    regress depvar main svar2 svar3

        . samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1,3)
    samregci will perform all possible combinations (regressions) without repetition of size 1 to 3,
    taken from 3:
    regress depvar main svar1
    regress depvar main svar2
    regress depvar main svar3
    regress depvar main svar1 svar2
    regress depvar main svar1 svar3
    regress depvar main svar2 svar3
    regress depvar main svar1 svar2 svar3

    Back to General options
    Back to Top

        +-----------------------------------------------------------------+
    ----+  Examples of changes in observations due to selection variables +--------------------------

    suppose depvar, main and svar1 have 50 nonmissing observations, svar2 have 46 nonmissing
    observations and svar3 has only 48, then

        . samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1)

    will perform the following 3 regressions:
    regress depvar main svar1, with 50 observations
    regress depvar main svar2, with 46 observations
    regress depvar main svar3, with 48 observations

    samregci additionally will perform the following 3 "sisters" regressions (one for each original
    regressions):

    regress depvar svar1, with 50 observations
    regress depvar svar2, with 46 observations
    regress depvar svar3, with 48 observations

    The results of sisters regressions will be stored in a .dta file named as samregci_sisters.dta.
    Additional tables and figures will be show and stored:

    - One table for each main variable summarizing the number (and percentage) of regressions in
    which the coefficients of the main variables increase/decrease and they change or not its
    significance due to the inclusion of selection variables (depending of the options tlost and tsig
    ).

    - Paired-coordinate plot with arrows will also be made for each main variable showing the change
    in the t-statistic due to the inclusion of selection variables along with the number of
    observations (i.e. comparing each main coefficient of samregci.dta and samregci_sisters.dta).

    Back to General options
    Back to Top


        +-------------------------------+
    ----+  Examples of using samesample +------------------------------------------------------------

    suppose depvar, main, svar1 and svar2 have 50 nonmissing observations but svar3 has only 48, then

        . samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1)
    will perform the following 3 regressions:
    regress depvar main svar1, with 50 observations
    regress depvar main svar2, with 50 observations
    regress depvar main svar3, with 48 observations

    while:

        . samregci depvar main, iterateover(svar1 svar2 svar3) ncomb(1) samesample
    will perform the following 3 regressions:
    regress depvar main svar1, with 48 observations
    regress depvar main svar2, with 48 observations
    regress depvar main svar3, with 48 observations

    Back to General options
    Back to Top


        +---------------------------+
    ----+  Examples of using fixvar +----------------------------------------------------------------

        . samregci depvar main, iterateover(svar1 svar2) ncomb(1) fixvar(fixvar1 fixvar2)
    will perform the following 2 regressions:
    regress depvar main svar1 fixvar1 fixvar2
    regress depvar main svar2 fixvar1 fixvar2

    when using fixvar without the ncomb option, samregci will also evaluate regressions without any
    variable candidate covariate:

    Back to Fixed variable options
    Back to Top


        +------------------------------------------------------------+
    ----+  Examples of using cmdest, cmdoptions, cmdstat and cmdiveq +-------------------------------

        . samregci depvar main, iterateover(svar1 svar2) cmdest(probit) cmdstat(r2_p)
    will perform the following 3 regressions:
    probit depvar main svar1
    probit depvar main svar2
    probit depvar main svar1 svar2
    and the Pseudo R2 of each estimation (e(r2_p)) will be saved in samregci.dta

        . samregci depvar main, iterateover(svar1 svar2) cmdoptions(robust)
    will perform the following 3 regressions:
    regress depvar main svar1, robust
    regress depvar main svar2, robust
    regress depvar main svar1 svar2, robust


        {cmd:. samregci depvar main, iterateover(svar1 svar2) ncomb(1,2) cmdest(xtreg) cmdoptions(fe
            vce(robust)) cmdstat(sigma_u sigma_e rho r2_w r2_b r2_o) }
    will perform the following 3 regressions:
    xtreg depvar main svar1, fe vce(robust)
    xtreg depvar main svar2, fe vce(robust)
    xtreg depvar main svar1 svar2, fe vce(robust)
    and the scalars stored in e(sigma_u), e(sigma_e), e(r2_p), e(rho), e(r2_w), e(r2_b) and e(r2_o)
    will be saved in samregci.dta xtreg

        . samregci depvar main, iterateover(svar1 svar2) cmdest(ivregress liml) cmdiveq(svar2= ivar1
            ivar2)
    will perform the following 3 regressions:
    ivregress liml depvar main svar1
    ivregress liml depvar main svar2 (svar2= ivar1 ivar2)
    ivregress liml depvar main svar1 svar2 (svar2= ivar1 ivar2)
    (the first regression is equivalent to regress depvar svar1)

        . samregci depvar main, iterateover(svar1 svar2) cmdest(ivregress gmm) cmdiveq(svar2= ivar1
            ivar2)
    will perform the following 3 regressions:
    ivregress gmm depvar main svar1 (=ivar1 ivar2)
    ivregress gmm depvar main svar2 (svar2= ivar1 ivar2)
    ivregress gmm depvar main svar1 svar2 (svar2= ivar1 ivar2)

        . samregci depvar main, iterateover(svar1 svar2) cmdest(ivregress 2sls) cmdiveq(fixvar1
            fixvar2= ivar1 ivar2) fixvar(fixvar1 fixvar2)
    will perform the following 3 regressions:
    ivregress 2sls depvar main svar1 (fixvar1 fixvar2= ivar1 ivar2)
    ivregress 2sls depvar main svar2 (fixvar1 fixvar2= ivar1 ivar2)
    ivregress 2sls depvar main svar1 svar2 (fixvar1 fixvar2= ivar1 ivar2)

    Back to Regressions command options
    Back to Top

    Back to Post-estimation options
    Back to Top

        +------------------------------------------------+
    ----+  Examples of using resultsdta, replace, double +-------------------------------------------

        . samregci depvar main, iterateover(svar1 svar2) resultsdta(myresults)
    generates a results database named myresults.dta instead of samregci.dta

        . samregci depvar main, iterateover(svar1 svar2) resultsdta(myresults) replace
    will replaced myresults.dta if it has already been created in the ongoing working directory. All
    results of myresults.dta will be stored with double precision

    Back to Output options
    Back to Top


## Saved results

    samregci creates a .dta file with outcome information for all estimated alternatives. By default,
    it includes the following columns for each regression:

    1) regression id (variable order)
    2) covariate regression coefficients (named v_1_b, v_2_b... , etc., and labeled with the full
        covariate name plus the word "coeff.")
    3) coefficient t-statistics (named v_1_t, v_2_t..., etc., and labeled with the full covariate
        name plus the word "tstat.")
    4) number of observations (variable obs)
    5) number of covariates (variable nvar)
    6) additional user specified statistics (if the cmdstat option is specified)

    Back to Top


## Authors

    Pablo Gluzmann
    CEDLAS-FCE-UNLP and CONICET
    La Plata, Argentina
    gluzmann@yahoo.com

    Demian Panigo
    Instituto Malvinas - UNLP y CONICET
    La Plata, Argentina
    dpanigo@gmail.com

    Back to Top
