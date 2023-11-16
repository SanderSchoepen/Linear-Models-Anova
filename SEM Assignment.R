install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
install_github( "ajdamico/lodown" , dependencies = TRUE )

install.packages("haven")                 # data import from spss
install.packages("dplyr")                 # data manipulation
install.packages("psych")                 # descriptives
install.packages("stringr")               # string manipulation

# ### MODELING ###
install.packages("lavaan")                # SEM modelling
install.packages("lavaan.survey")         # Wrapper around packages lavaan and survey

# ### VISUALIZATION ###
install.packages("tidySEM")               # plotting SEM models
install.packages("corrplot")              # correlation plots


# Load the packages 
library(lodown)
library(devtools)

### DATA MANIPULATION ###
library("haven")        
library("dplyr")      
library("psych")
library('stringr')

### MODELING ###
library("lavaan")       

### VISUALIZATION ###
library("tidySEM")
library("corrplot")  

### Getting the data ###

# examine all available SCF microdata files and create path
scf_cat <-
  get_catalog( "scf" ,
               output_dir = file.path( path.expand( "~" ) , "SCF" ) )

# 2019 only
scf_cat <- subset( scf_cat , year == 2019 )

# download the microdata to your local computer
scf_cat2 <- lodown( "scf" , scf_cat )

#read microdata into R
df1<- readRDS( file.path( path.expand( "~" ) , "SCF" , "scf 2019.rds" ) )

#above is a list of 5 dataframes, each with different imputations for missing values=> select the first
df<-df1[[1]]

### variable selection ###

#selecting variables that I need
select1<-select(df, x14,x8021,x5931,x5729,x3014,x7557,x5712,x5710,x5708,x5706,x7556)

head(select1)

unique(select1$x5708)

#rename the variables
df_renamed<-transmute(select1,
                      age=x14,
                      #sex: 1=male, 2=female,0=inap=> do minus 1 to dumme encode=> 0=male, 1=female
                      sex=x8021-1,
                      #low to high: 1-15;-1=lower than first grade,0=inap
                      education=x5931,
                      #-1=no knowledge at all=> add 1 such that all values are between 0a
                      fin_lit=x7556+1,
                      #-1=nothing
                      #-9=negative
                      #income=x5729,
                      #ristk1: high score = low risk
                      risk1=x3014,
                      #risk2: high score=high risk
                      #-1= no risk at all-10: very willing to take risks=> change the sign!!!
                      risk2=-x7557,
                      #financial return values:
                      income_interest=x5708+x5706,
                      #income_dividends=x5710,
                      income_capital=x5712
                      )
max(risk1)
#scaling specific columns
df_renamed[, c("income_capital","income_interest")] <- scale(df_renamed[, c("income_capital", "income_interest")])
#scaling the whole dataset

unique(df_renamed$risk1)
#education and risk2: replace -1 by 0
#income: divide by max income
head(df_renamed)
#check what the variables mean and how they are being used in the model

### EDA ###

# let's calculate the sample implied covariance matrix 
df_cov <- cov(df_renamed,          # data frame 
                        use = "pairwise.complete.obs" # remove NAs 
)

df_cor <- cov2cor(df_cov)
df_cor

corrplot::corrplot(df_cor, 
                   is.corr = FALSE,       # whether is a correlation matrix 
                   method = "circle",     # magnitude of covariances as circles 
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
                   )
#income is not relevant in the model=> drop it

### model 1 ### risk behaviour and sex, education, etc

model_ws_mimic <-'risk_behavior =~ risk1 + risk2 +age
risk_behavior ~  sex + education
'

fit_ws_mimic <- cfa(model_ws_mimic, # model formula
                    data=df_renamed      # data frame
)

summary(fit_ws_mimic, standardized=TRUE)

#model2
model_ws_mimic_2 <-'risk_behavior =~ risk1 + risk2
risk_behavior ~  sex + education +age
'

fit_ws_mimic_2 <- cfa(model_ws_mimic_2, # model formula
                    data=df_renamed      # data frame
)

summary(fit_ws_mimic_2, fit.measures=TRUE)

standardizedsolution(fit_ws_mimic_2)



fitm_model_ws_mimic_2<-fitMeasures(fit_ws_mimic_2, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")

#comparing model fit indices
data.frame(
  "model_ws_eg" = round(fitm_model_ws_mimic_2[,1],2)
)
# Modification indices
modificationindices(fit_ws_mimic_2)

### Model 2: risk behaviour and investment returns mediated through sex, education, financial literacy, etc ###

sem2<-'
#risk
risk_behavior =~ risk1 + risk2
#investment income
investment_income=~ income_capital + income_dividends + income_interest
#regressions
risk_behavior~a*education
investment_income~ b*education + c*risk_behavior
#indirect effect of education on investment_income:
indirect:=a*c
#total effect of education on investment_income
total:=b+a*c
'

fitsem2<-sem(sem2, data=df_renamed)

summary(fitsem2,fit.measures=TRUE, standardized=TRUE)

fitm_sem2<-fitMeasures(fitsem2, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")
fitm_sem2


modificationindices(fitsem2)

### adjust model ###

sem3<-'
#risk
risk_behavior =~ risk1 + risk2
#investment income
investment_income=~ income_capital + income_dividends + income_interest
#regressions
risk_behavior~a*education
investment_income~ b*education + c*risk_behavior
#indirect effect of education on investment_income:
indirect:=a*c
#total effect of education on investment_income
total:=b+a*c

#residual covariances
income_capital ~~ income_dividends
income_dividends ~~  income_interest


'

fitsem3<-sem(sem3, data=df_renamed)

#cov matrix not positive definite!!!=> drop income_dividends ~~  income_interest

summary(fitsem3,fit.measures=TRUE, standardized=TRUE)

fitm_sem3<-fitMeasures(fitsem3, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")
fitm_sem3

### adjust model again: drop income_dividends ~~  income_interest ###


sem4<-'
#risk
risk_behavior =~ risk1 + risk2
#investment income
investment_income=~ income_capital + income_dividends + income_interest
#regressions
risk_behavior~a*education
investment_income~ b*education + c*risk_behavior
#indirect effect of education on investment_income:
indirect:=a*c
#total effect of education on investment_income
total:=b+a*c

#residual covariances
income_capital ~~ income_dividends


'

fitsem4<-sem(sem4, data=df_renamed)

#cov matrix not positive definite!!!=> drop income_dividends ~~  income_interest

summary(fitsem4,fit.measures=TRUE, standardized=TRUE)

fitm_sem4<-fitMeasures(fitsem4, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")
fitm_sem4

### keep only the income_dividends ~~  income_interest ###

sem5<-'
#risk
risk_behavior =~ risk1 + risk2
#investment income
investment_income=~ income_capital + income_dividends + income_interest
#regressions
risk_behavior~a*education
investment_income~ b*education + c*risk_behavior
#indirect effect of education on investment_income:
indirect:=a*c
#total effect of education on investment_income
total:=b+a*c

#residual covariances
income_dividends ~~  income_interest
'

fitsem5<-sem(sem5, data=df_renamed)

#cov matrix not positive definite!!!=> drop income_dividends ~~  income_interest

summary(fitsem5,fit.measures=TRUE, standardized=TRUE)

fitm_sem5<-fitMeasures(fitsem5, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")
fitm_sem5

### drop dividends from the equation ###

sem6<-'
#risk
risk_behavior =~ risk1 + risk2
#investment income
investment_income=~ income_capital + income_interest
#regressions
risk_behavior~a*education
investment_income~ b*education + c*risk_behavior
#indirect effect of education on investment_income:
indirect:=a*c
#total effect of education on investment_income
total:=b+a*c
'

fitsem6<-sem(sem6, data=df_renamed)

#cov matrix not positive definite!!!=> drop income_dividends ~~  income_interest

summary(fitsem6,fit.measures=TRUE, standardized=TRUE)

fitm_sem6<-fitMeasures(fitsem6, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")
fitm_sem6

### adding financial literacy ###

sem7<-'
#risk
risk_behavior =~ risk1 + risk2
#investment income
investment_income=~ income_capital + income_interest
#regressions
risk_behavior~a*fin_lit 
investment_income~ b*fin_lit  + c*risk_behavior
#indirect effect of fin_lit  on investment_income:
indirect:=a*c
#total effect of fin_lit  on investment_income
total:=b+a*c
'

fitsem7<-sem(sem7, data=df_renamed)

#cov matrix not positive definite!!!=> drop income_dividends ~~  income_interest

summary(fitsem7,fit.measures=TRUE, standardized=TRUE)

fitm_sem7<-fitMeasures(fitsem7, c("chisq", "df", "pvalue", "cfi", "tli","rmsea","srmr"), output = "matrix")
fitm_sem7



