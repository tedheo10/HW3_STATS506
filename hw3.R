
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

# 1) Predictors: age
lm1 <- lm(VIQ220~RIDAGEYR, data=merge_data)
summary(lm1)

# 2) Predictors: age, race, gender
lm2 <- lm(VIQ220~RIDAGEYR+RIDRETH1+RIAGENDR, data=merge_data)
summary(lm2)

# 3) Predictors: age, race, gender, Poverty Income ratio
lm3 <- lm(VIQ220~RIDAGEYR+RIDRETH1+RIAGENDR+INDFMPIR, data=merge_data)
summary(lm3)

