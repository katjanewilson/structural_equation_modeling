setwd("e:/EDUC777 spring 2021")
getwd()
# get raw data set
frstreg1 <- read.table("raw/FRSTREG.with.header.dat", header=TRUE, na.strings=".")
head(frstreg1)
tail(frstreg1)
attributes(frstreg1)
is.data.frame(frstreg1)
is.matrix(frstreg1)

id <- frstreg1$id
phlove <- frstreg1$phlove
phconf <- frstreg1$phconf
xhachfe <- frstreg1$xhachfe
xhdepres <- frstreg1$xhdepres

# bind to data set
frstreg1dataset1 <- cbind(id,phlove,phconf,xhachfe,xhdepres)
attributes(frstreg1dataset1)
is.matrix(frstreg1dataset1)
is.data.frame(frstreg1dataset1)


##regressions

mod1 <- lm(scale(xhachfe) ~ scale(phlove) + scale(phconf), data = frstreg1)
summary(mod1)
mod2 <- lm(xhdepres ~ xhachfe, data = frstreg1)
summary(mod2)
