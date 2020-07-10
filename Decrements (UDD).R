### Pension Modeling
##Anil

#4-year decrements
rates <- c(0.1,0.1,0.1,0.1)
n <- 4
x <- rates[1]*(1 - (rates[2]+rates[3]+rates[4])/2 + 
            sum(prod(rates[2], rates[3]), prod(rates[2], rates[4]), prod(rates[3], rates[4]))/3 - 
            prod(rates[2],rates[3],rates[4])/4)
x  
#Approximation         
y <- prod(rates[1], (1-rates[2]/2), (1-rates[3]/2), (1-rates[4]/2))
y
#Diff
x-y

#Mortality RP-2014 (Male 20-25)
#Mortality rate over next year
mort <-c(0.000406, 0.000449,0.000488,0.000509,0.000516,0.000484)
#Probability of Survival to 26
prod(1-mort)
####