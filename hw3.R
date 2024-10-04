
# Probmlem 1 - Vision 

# a. Merge the two files, VIX_D and DEMO_D

install.packages("foreign")
library(foreign) # a package to import ".XPT" file

demo <- read.xport("DEMO_D.XPT") # DEMO_D file import
vix <- read.xport("VIX_D.XPT") # VIX_D file import

merge_data <- merge(demo, vix, by = "SEQN", all = FALSE) # merge data 
nrow(merge_data)
  

# b. The proportion of respondents who wear glasses/contact lenses for distance vision

install.packages("stargazer")
library(stargazer) # a package to generate HTML/LaTeX tables

# wear glasses/lens vix.VIQ220 and age demo.RIDAGEYR

table(merge_data$VIQ220) # check the count of VIQ220 with the description
sum(merge_data$RIDAGEYR >= 1) # check the count of RIDAGEYR with the description

n <- nrow(merge_data) 

# Empty matrix only with a columen of age ranges 

mat_glasses <- matrix(nrow = 10, ncol = 3)
colnames(mat_glasses) <- c("AgeRange", "Count", "Proportion")
mat_glasses[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", 
                     "70-79", "over 80", "Total")

# Populate the count of people who wear glasses/contact lenses for distance vision 
for (i in 1:10) { 
  if(i<10) { 
    mat_glasses[i,2] <- sum(merge_data$VIQ220 == 1 &
        merge_data$RIDAGEYR < 10*i & merge_data$RIDAGEYR >= 10*(i-1), na.rm = TRUE)
    }
  else if(i==10) {
    mat_glasses[i,2] <- sum(merge_data$VIQ220 == 1, na.rm = TRUE)
  }
}

# Populate the proportion of people in each age group 
for (i in 1:10) { 
    each_group <- as.numeric(mat_glasses[i,2])
    total_group <- as.numeric(mat_glasses[10,2])
    mat_glasses[i,3] <- each_group / total_group
}

mat_glasses

# Change the mat_glasses as a data.frame with numeric values 
df_glasses <- as.data.frame(mat_glasses)
df_glasses[,2] <- as.numeric(df_glasses[,2])
df_glasses[,3] <- as.numeric(df_glasses[,3])
df_glasses

#  Table by ussing stargazer
table_glasses <- stargazer(df_glasses, type = "text", summary = FALSE,
  rownames = TRUE, title = "Glasses and Contact lenses Usage for Distance Vision")

# c. Fit three logistic regression models predicting whether a respondent wears glasses/contact lenses for distance vision

merge_data$VIQ220_BI <- merge_data$VIQ220 == 1 # Binomial data for VIQ220

# 1) the 1st logistic regression with age as a predictor
 
glm1 <- glm(VIQ220_BI ~ RIDAGEYR, data = merge_data, family = binomial) 
odd_1 <- exp(coef(glm1)) # the oods ratios 
num1 <- nobs(glm1) # sample size
pseudo_r1 <- 1 - (glm1$deviance / glm1$null.deviance) # pseudo-R^2
aic1 <- AIC(glm1) # AIC value

# 2) the 2nd logistic regression with age, race and gender as predictors  
glm2 <- glm(VIQ220_BI ~ RIDAGEYR+RIDRETH1+RIAGENDR, data=merge_data, family = binomial)
odd_2 <- exp(coef(glm2)) # the oods ratios 
num2 <- nobs(glm2) # sample size
pseudo_r2 <- 1 - (glm2$deviance / glm2$null.deviance) # pseudo-R^2
aic2 <- AIC(glm2) # AIC value

# 3) the 3rd logistic regression with age, race, gender and poverty income ratio as predictors 
glm3 <- glm(VIQ220_BI ~ RIDAGEYR+RIDRETH1+RIAGENDR+INDFMPIR, data=merge_data, family = binomial)
odd_3 <- exp(coef(glm3)) # the oods ratios 
num3 <- nobs(glm3) # sample size
pseudo_r3 <- 1 - (glm2$deviance / glm2$null.deviance) # pseudo-R^2
aic3 <- AIC(glm3) # AIC value

# Create a summary table of the three logistic models 

summary_table <- data.frame(
  Model_1 = c(odd_1[2], NA, NA, NA, num1, pseudo_r1, aic1),
  Model_2 = c(odd_2[2], odd_2[3], odd_2[4], NA, num2, pseudo_r2, aic2),
  Model_3 = c(odd_3[2], odd_3[3], odd_3[4], odd_3[5], num3, pseudo_r3, aic3),
  row.names = c("Odd_Ratio_Age", "Odd_Ratio_Race", "Odd_Ratio_Gender", "Odd_Ratio_PIR", "Sample_Size", "Pseudo_R2", "AIC")
)

stargazer(summary_table, type = "text", summary = FALSE, title = "Summary of the three Logistic Regression Models")


#d. From glm3, test and interpretation 

# 1) Test whether the odds of men and women being wears of glasess/contact lenses for distance vision differs. 

# the p-value of t-test for the coefficient for the gender variable(RIAGENDR) is very small(p-value = 1.153700e-21. This means that the odds between men and women are significantly different. 

summary(glm3)$coeff["RIAGENDR", ] # the test of the coefficient for RIAGENDR 

# 2) Test whether the proportion of wearers of glasses/contact lenses for distance vision differs between men and women. 

# the p-value of chi-squared test for the proportion between males and females is very small(p-value < 2e-16). This means that the proportions between men and women are significantly different. 

pp_table <- table(merge_data$RIAGENDR, merge_data$VIQ220_BI)
pp_table # "1" means male and "2" means female
pp_table[1,2]/sum(pp_table[1,]) # the proportion of male 
pp_table[2,2]/sum(pp_table[2,]) # the proportion of female
chi_test <- chisq.test(pp_table) # the chi-squared test for the proportion difference
chi_test 
