# setwd("e:/EDUC777 spring 2020")
# getwd()
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
    method = "pearson")

firstPathCorr <- cor(frstreg1dataset1, use = "everything",
    method = "pearson")


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

lavInspect(first.path.model.fit, what="est")




# second path model


require("lavaan")

second.path.model <- '
xhachfe ~  b32*phconf
xhdepres ~ b43*xhachfe
phconf ~ b12*phlove
phlove   ~~ 1*phlove
xhachfe  ~~ p33*xhachfe
xhdepres ~~ p44*xhdepres
phconf   ~~ p22*phconf
'
second.path.model.fit <- sem(second.path.model, sample.cov=firstPathCorr, sample.nobs=126)
summary(second.path.model.fit, rsquare=TRUE)

lavInspect(second.path.model.fit, what="est")







