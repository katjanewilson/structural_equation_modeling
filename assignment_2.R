setwd("e:/EDUC777 spring 2021")
getwd()
# get raw data set
frstreg1 <- read.table("raw/FRSTREG.with.header.dat", header=TRUE, na.strings=".")
head(frstreg1)                    # lists first 6 cases
tail(frstreg1)                    # lists last 6 cases
attributes(frstreg1)              # tells you what kind of object frstreg1 is
is.data.frame(frstreg1)           # is it a data frame
is.matrix(frstreg1)               # is it a matrix

id <- frstreg1$id                 # simplifies name
phlove <- frstreg1$phlove
phconf <- frstreg1$phconf
xhachfe <- frstreg1$xhachfe
xhdepres <- frstreg1$xhdepres

# bind to data set
frstreg1dataset1 <- cbind(phlove,phconf,xhachfe,xhdepres)     # creates a matrix
attributes(frstreg1dataset1)                                     
is.matrix(frstreg1dataset1)
is.data.frame(frstreg1dataset1)

regmodel1 <- lm(xhachfe ~ phlove + phconf)                        # regression equation
summary(regmodel1)                                                # regression output

regmodel1st <- lm(scale(xhachfe) ~ scale(phlove) + scale(phconf)) # standardized equation
summary(regmodel1st)                                              # standardized output

regmodel2 <- lm(xhdepres ~ xhachfe)
summary(regmodel2)

regmodel2st <- lm(scale(xhdepres) ~ scale(xhachfe))
summary(regmodel2st)

cor(frstreg1dataset1, use = "everything",
    method = "pearson")                   # creates a correlation matrix

firstPathCorr <- cor(frstreg1dataset1, use = "everything",
                     method = "pearson")                   # puts the correlation matrix into an object


# the same thing in LAVAAN

require("lavaan")

first.path.model <- '
xhachfe ~ b31*phlove + b32*phconf
xhdepres ~ b43*xhachfe
phlove   ~~ 1*phlove
phconf   ~~ 1*phconf
xhachfe  ~~ p33*xhachfe
xhdepres ~~ p44*xhdepres
phconf   ~~ p21*phlove
'
first.path.model.fit <- sem(first.path.model, sample.cov=firstPathCorr, sample.nobs=126)
summary(first.path.model.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(first.path.model.fit, what="est") # LISREL estimates
lavInspect(first.path.model.fit)             # which parameters are estimated

# you can get the additional statistics and matrices as shown below

# get fit measures
fitMeasures(first.path.model.fit)

# model implied covariance matrix
fitted(first.path.model.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(first.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(first.path.model.fit, type="cor")

# standardized residual correlations
residuals(first.path.model.fit, type="standardized")

# get modification indices
modificationIndices(first.path.model.fit)


# indirect effects model

first.path.model.indirect <- '
phconf ~ b21*phlove
xhachfe ~  b32*phconf
xhdepres ~ b43*xhachfe
phlove   ~~ 1*phlove
phconf   ~~ p22*phconf
xhachfe  ~~ p33*xhachfe
xhdepres ~~ p44*xhdepres
'
first.path.model.indirect.fit <- sem(first.path.model.indirect, 
                                     sample.cov=firstPathCorr, sample.nobs=126)
summary(first.path.model.indirect.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(first.path.model.indirect.fit, what="est") # LISREL estimates
lavInspect(first.path.model.indirect.fit)             # which parameters are estimated


# you can get the additional statistics and matrices as shown below

# get fit measures
fitMeasures(first.path.model.indirect.fit)

# model implied covariance matrix
fitted(first.path.model.indirect.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(first.path.model.indirect.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(first.path.model.indirect.fit, type="cor")

# standardized residual correlations
residuals(first.path.model.indirect.fit, type="standardized")

# get modification indices
modificationIndices(first.path.model.indirect.fit)