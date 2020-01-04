setwd('C:/Users/hzhou/Desktop/Acumen,LLC/572')
df = read.csv('default of credit card clients.csv', header = TRUE, skip = 1)

data_bill_stat = df[,13:18]

data_bill_stat$X12 = as.numeric(data_bill_stat$X12)

library(ggplot2)


boxplot(data_bill_stat$BILL_AMT1, data_bill_stat$BILL_AMT2, data_bill_stat$BILL_AMT3, 
        data_bill_stat$BILL_AMT4,data_bill_stat$BILL_AMT5, data_bill_stat$BILL_AMT6)
#conduct F-test to check whether the variances are equal or not for bill amount of different months
for (i in 1:5){
  for (j in (i+1):6){
    print(var.test(data_bill_stat[,i], data_bill_stat[,j], 
             alternative = c('two.sided'), conf.level = 0.95))
  }
}

#P-value is very small, reject the H0, different variance

#Since variances are not same, we cannot use ANOVA method. Using Pairwise t-test to check equal means
for (i in 1:5){
  for (j in (i+1):6){
    print(t.test(data_bill_stat[,i], data_bill_stat[,j], 
                   alternative = c('two.sided'), var.equal = FALSE, conf.level = 0.95))
  }
}

#different means, reject H0

