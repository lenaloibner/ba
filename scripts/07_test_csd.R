
setwd("C:/Users/lenal/Documents/05_R/data_clean")

library(plm)  
library(readr)  

data <- read_csv("data_all.csv")  

pdata <- pdata.frame(data, index = c("country", "date"))  

cd_results <- data.frame(Variable = character(), z_Statistic = numeric(), p_Value = numeric())  

for (var in c("stability", "volatility", "spread", "inflation", "reserves", "ex_rate")) {  
  test_cd <- pcdtest(as.formula(paste(var, "~ 1")), data = pdata, test = "cd")  
  cd_results <- rbind(cd_results, data.frame(Variable = var, z_Statistic = test_cd$statistic, p_Value = test_cd$p.value))  
}
 
print(cd_results) 


write_csv(cd_results, "cd_results.csv") 