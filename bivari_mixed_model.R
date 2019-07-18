
# Loading wages data
wages <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/wages_pp-1.txt",head=TRUE)
head(wages)
summary(wages)


# Work with variables lnw and exper predicted from uerate all nested within id

with(wages, {
  ## find the mean of each variable by id
  tmp <- by(cbind(lnw, exper, uerate), id, colMeans, na.rm = TRUE)
  ## view summary informaiton the means by id for each variable
  summary(do.call("rbind", tmp))
})


# Using standard deviation

with(wages, {
  ## find the variance of each variable by id
  tmp <- by(cbind(lnw, exper, uerate), id, function(x) sapply(x, sd, na.rm = TRUE))
  ## view summary informaiton the variances by id for each variable
  summary(do.call("rbind", tmp))
})

# Creating data to work on using id uerate lmw exper variables
# To do Bivariate linear mixed model need to first melt the data

require(dplyr)
require(reshape2)
## Loading required package: reshape2
swages <- melt(wages[, c("id", "uerate", "lnw", "exper")], 
               measure.vars = c("lnw","exper"))
head(swages)

# Creating dummy variables for lnw and exper
swages <- within(swages, {
  Dl <- as.integer(variable == "lnw")
  De <- as.integer(variable == "exper")
})
head(swages)


## print the data for variables of interest for id 31
swages[swages$id == 31, ]


require(nlme)
## fit the model
# using the 0 notation to allow for a separate intercept for each outcome indicate 
# by the effect of the two dummy variables, De and Dl.

m <- lme(value ~ 0 + Dl + Dl:uerate + De + De:uerate, data = swages,
         random = ~0 + Dl + Dl:uerate + De + De:uerate | id, 
         weights = varIdent(form = ~1 |variable), 
         control = lmeControl(maxIter = 200, msMaxIter = 200, niterEM = 50,msMaxEval = 400))

require(lattice)
## Loading required package: lattice
## create data frame of residuals, fitted values, and variable
diagnos <- data.frame(Resid = resid(m, type = "normalized"), Fitted = fitted(m),
                      Variable = swages$variable)


## overal QQ normal plot
qqmath(~Resid, data = diagnos, distribution = qnorm)


## separate by variable
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm)


## we could add a line to indicate 'normal' with a bit more work
qqmath(~Resid | Variable, data = diagnos, distribution = qnorm, prepanel = prepanel.qqmathline,
       panel = function(x) {
         panel.qqmathline(x)
         panel.qqmath(x)
       })


## overall plot
xyplot(Resid ~ Fitted, data = diagnos)


## separate by variable
xyplot(Resid ~ Fitted | Variable, data = diagnos)


## Centile curves using BCT
library(gamlss)
h<-gamlss(uerate ~cs(lnw), sigma.formula=~cs(lnw), nu.formula = ~cs(lnw), 
          family=BCT, data=wages) 

centiles(h, xvar=wages$lnw, cent=c(10,20,50,95,99), col.cent=c(2,3,4,5,1), 
         lwd.cent=c(1,1,1,1,2,1,1,1,1), xlab = "lnw", ylab = "uerate")


h1<-gamlss(uerate ~cs(exper), sigma.formula=~cs(exper), nu.formula = ~cs(exper), 
          family=BCT, data=wages) 


centiles(h1, xvar=wages$exper, col.cent=1, cent=c(10,20,50,95,99), 
         lty.centiles=c(3,2,1,2,3),lwd.cent=c(1,1,2,1,1), xlab = "exper", 
         ylab = "uerate")


getVarCov(m)

VarCorr(m)

v <- as.numeric(VarCorr(m)[1:4])
ICC <- v[1]/sum(v)
