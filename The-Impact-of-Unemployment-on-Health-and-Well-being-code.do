
clear all
set more off
*---------------------------------------------*
* LOAD DATASET
*---------------------------------------------*
use "/Users/zixuandong/Desktop/HP426 summative/Summative Instructions-20241214/Project 2 - British Household Panel Survey  dataset-20241214/bhps.dta", clear
*---------------------------------------------*
* GENERATE KEY VARIABLES
*---------------------------------------------*
* Describe and summarize the dataset
describe 
codebook 
summarize 

* Generate key variables
gen health_status = khlstat                      // Health over last 12 months
gen gp_visits = khl2gp                           // Number of visits to GP
gen wellbeing = khlghq2                          // Subjective wellbeing (GHQ Caseness)

* Main explanatory variable: Unemployment status
gen unemployed = (kjbstat == 3)  


* Control variables
gen male = (sex == 1)             // Gender: Male = 1, Female = 0
gen age = kage                    // Age
gen education = kqfedhi           // Education level
gen financial_situation = kfisit  // Financial situation
gen smoker = (ksmoker == 1)       // Smoking status: Smoker = 1
gen disabled = (khldsbl == 1)     // Registered disabled: Yes = 1
gen marital_status = kmlstat      // Marital status

* Create age groups
gen age_group = .
replace age_group = 1 if kage >= 25 & kage <= 34
replace age_group = 2 if kage >= 35 & kage <= 44
replace age_group = 3 if kage >= 45 & kage <= 54
replace age_group = 4 if kage >= 55 & kage <= 64
replace age_group = 5 if kage >= 65
label define age_group_lbl 1 "25-34" 2 "35-44" 3 "45-54" 4 "55-64" 5 "65+"
label values age_group age_group_lbl

* Handle missing values
foreach var of varlist health_status gp_visits wellbeing unemployed ///
    male age education financial_situation smoker disabled marital_status {
    replace `var' = . if `var' < 0   // Replace negative values with missing
}

*---------------------------------------------*
* DESCRIPTIVE STATISTICS
*---------------------------------------------*
summarize health_status gp_visits wellbeing unemployed ///
    male age education financial_situation smoker disabled marital_status

* Cross-tabulation
tab unemployed health_status, row    // Unemployment vs Health Status
tab unemployed gp_visits, row        // Unemployment vs GP visits
tab unemployed wellbeing, row        // Unemployment vs Wellbeing
 
*------------------------------------------*
* OLS Regression                           *
*------------------------------------------*
* Use Ordered Logistic Regression for Ordered Health Status
ologit health_status unemployed male i.age_group education financial_situation smoker disabled, robust 
estimates store ologit_health
margins, at(unemployed=(0 1)) predict(outcome(1)) // Probability of Excellent health
margins, at(unemployed=(0 1)) predict(outcome(5)) // Probability of Very Poor health

* Linear Regression for GP_visits
regress gp_visits unemployed male i.age_group education financial_situation smoker disabled, robust
margins, at(unemployed=(0 1)) // Marginal effects of unemployment on GP visits

* Use Ordered Logistic Regression for Wellbeing
ologit wellbeing unemployed male i.age_group education financial_situation smoker disabled, robust
margins, at(unemployed=(0 1)) // Marginal effects of unemployment on wellbeing

*------------------------------------------*
* Instrumental Variables (IV) Regression   *
*------------------------------------------*
*First Stage: Predicting unemploymet for Health status
reg unemployed kjbsic kjbpen male i.age_group education financial_situation smoker disabled
test kjbsic kjbpen    //Test for instrument relevance

* IV Regression for Health Status
ivregress 2sls health_status (unemployed = kjbsic kjbpen) male i.age_group education financial_situation smoker disabled, robust
estat overid          //overidentification test
estat firststage      //First-stage diagnostics
estat endogenous      //Test for endogeneity
margins, at(unemployed=(0 1)) // Marginal Effects 
marginsplot, title("Marginal Effects of Unemployment on Health Status") ytitle("Predicted Health Status") xtitle("Unemployment Status")

*First Stage: Predicting Unemployment for GP_visits
reg unemployed kjbsic ktujbpl male i.age_group education financial_situation smoker disabled
test kjbsic ktujbpl   //Test for instrument relevance

* IV Regression for GP Visits
ivregress 2sls gp_visits (unemployed = kjbsic ktujbpl) male i.age_group education financial_situation smoker disabled, robust
estat overid          //overidentification test
estat firststage      //First-stage diagnostics
estat endogenous      //Test for endogeneity
margins, at(unemployed=(0 1)) // Marginal Effects 
marginsplot, title("Marginal Effects of Unemployment on GP Visits") ytitle("Predicted GP Visits") xtitle("Unemployment Status")


*First Stage: Predicting Unemployment for Wellbeing
reg unemployed kjbsic kjbsect male i.age_group education financial_situation smoker disabled
test kjbsic kjbsect   //Test for instrument relevance

* IV Regression for Wellbeing
ivregress 2sls wellbeing (unemployed = kjbsic kjbsect) male i.age_group education financial_situation smoker disabled, robust
estat overid          //overidentification test
estat firststage      //First-stage diagnostics
estat endogenous      //Test for endogeneity
margins, at(unemployed=(0 1)) // Marginal Effects 
marginsplot, title("Marginal Effects of Unemployment on Wellbeing") ytitle("Predicted Wellbeing") xtitle("Unemployment Status")

*---------------------------------------------*
* INTERACTION EFFECTS ANALYSIS
*---------------------------------------------*
* Create interaction terms
gen unemployed_male = unemployed * male
gen unemployed_age = unemployed * age
gen unemployed_financial = unemployed * financial_situation
gen unemployed_edu = unemployed * education


* IV regression with interaction terms for Health status
ivregress 2sls health_status (unemployed = kjbsic kjbpen) male i.age_group education financial_situation smoker disabled unemployed_age unemployed_financial, robust
test unemployed_age unemployed_financial  // Test interaction terms
* Run first-stage regression
reg unemployed kjbsic kjbpen male i.age_group education financial_situation smoker disabled unemployed_age unemployed_financial
estat vif                                 // Check for multicollinearity                
* Check for heteroskedasticity
estat hettest
* Check for omitted variables (Ramsey RESET test)
estat ovtest


* IV regression with interaction terms(gp_visits)
ivregress 2sls gp_visits (unemployed = kjbsic ktujbpl) male i.age_group education financial_situation disabled unemployed_male unemployed_age unemployed_financial, robust
test unemployed_male unemployed_age unemployed_financial
* Run first-stage regression
reg unemployed kjbsic ktujbpl male i.age_group education financial_situation smoker disabled unemployed_male unemployed_age unemployed_financial
* Calculate VIFs
estat vif
* Check for heteroskedasticity
estat hettest
* Check for omitted variables (Ramsey RESET test)
estat ovtest


* IV regression with interaction terms(wellbeing)
ivregress 2sls wellbeing (unemployed = kjbsic kjbsect) male i.age_group education financial_situation disabled unemployed_male unemployed_edu, robust
test unemployed_male unemployed_edu
* Run first-stage regression
reg unemployed kjbsic kjbsect male i.age_group education financial_situation smoker disabled unemployed_male unemployed_edu
* Calculate VIFs
estat vif
* Check for heteroskedasticity
estat hettest
* Check for omitted variables (Ramsey RESET test)
estat ovtest












