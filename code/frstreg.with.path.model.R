setwd("e:/EDUC777 spring 2021")
getwd()
# get raw data set
frstreg1 <- read.table("FRSTREG.with.header.dat", header=TRUE, na.strings=".")
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

# bind to matrix
frstreg1dataset1 <- cbind(phlove,phconf,xhachfe,xhdepres)
attributes(frstreg1dataset1)
is.matrix(frstreg1dataset1)
is.data.frame(frstreg1dataset1)

regpath1 <- lm(xhachfe ~ phlove + phconf)
summary(regpath1)
regpath1st <- lm(scale(xhachfe) ~ scale(phlove) + scale(phconf))
summary(regpath1st)

regpath2 <- lm(xhdepres ~ xhachfe)
summary(regpath2)
regpath2st <- lm(scale(xhdepres) ~ scale(xhachfe))
summary(regpath2st)

# create Beta matrix

Beta_mat <- matrix(data=c( 0,      0      ,0,      0,
                           0,      0,      0,      0,
                          -0.1584, 0.1393, 0,      0,
                           0,      0,      0.2828, 0), 
                            nrow=4, ncol=4, byrow=TRUE)
Beta_mat

# create Psi matrix

Psi_mat <- matrix(data=c(1,     -0.3243, 0,      0,
                        -0.3243, 1,      0,      0,
                         0,      0,      0.9412, 0,
                         0,      0,      0,      0.9200),
                           nrow=4,ncol=4, byrow=TRUE)
Psi_mat

# create identity matrix

Ident_mat <- diag(4)
Ident_mat

IminusBeta <- Ident_mat-Beta_mat
IminusBeta

# calculate expected correlation matrix

Sigma_hat <- solve(IminusBeta)%*%Psi_mat%*%t(solve(IminusBeta))
Sigma_hat

# calculate observed correlation matrix

cor(frstreg1dataset1,use = "everything", method = "pearson")
corrmat1 <- cor(frstreg1dataset1,use = "everything", method = "pearson")

# Calculate residual correlation matrix (aka fit)

Resid_corr <- corrmat1-Sigma_hat
Resid_corr

# calculated observed correlation matrix

cov(frstreg1dataset1,use = "everything")

# bind to matrix add constant

constant <- 1

frstreg1withconstant <- cbind(constant,phlove,phconf,xhachfe,xhdepres)
frstreg1withconstant

# calculate SSCP matrix

sscp <- t(frstreg1withconstant)%*%(frstreg1withconstant)  # sscp matrix
sscp



