
install.packages("stargazer")
library(stargazer)

install.packages("foreign")
library(foreign)

demo <- read.xport("DEMO_D.XPT")
colnames(demo)

vix <- read.xport("VIX_D.XPT")
colnames(vix)
