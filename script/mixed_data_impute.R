library(MixedDataImpute)
library(mice)
library(dplyr)
data("sipp08")


#sipp08 %>% View
set.seed(1)
n = 2000
s = sample(1:nrow(sipp08), n)

Y = sipp08[s,1:2]
Y[,1] = log(Y[, 1] + 1)
X = sipp08[s,-c(1:2,9)]

original <- cbind(X, Y)

seq <- rbinom(n, 1, 0.5)

seq_bool <- ifelse(seq == 1, TRUE, FALSE)

Y[seq_bool,] <- NA
X[!seq_bool, c(2,3, 6)] <- NA


train <- cbind(X, Y)


kz = 15
ky = 60
kx = 90

num.impute = 5
num.burnin = 10000
num.skip = 1000
thin.trace = 10

imp = hcmm_impute(X, Y, kz=kz, kx=kx, ky=ky,
                  num.impute=num.impute, num.burnin=num.burnin,
                  num.skip=num.skip, thin.trace=thin.trace)

form = total_earnings~age+I(age^2) + sex*I(own_kid!=0)
fits = lapply(imp$imputations, function(dat) lm(form, data=dat))
pooled_ests = pool(as.mira(fits))
summary(pooled_ests)


# original, complete data estimates for comparison
comdat = sipp08[s,]
comdat[,1] = log(comdat[,1]+10)
summary(lm(form, data=comdat))

# true population values for comparison
pop = sipp08
pop[,1] = log(pop[,1]+10)
summary(lm(form, data=pop))

Y

