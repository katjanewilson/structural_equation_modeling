require("lavaan")    

# read in necessary descriptive information
nvars <- 8
nobs <- 126
# I'm reading in the correlation matrix
MySecondRegCorr <- matrix(
  c(1.00000, -0.32746, -0.73101,  0.51345, -0.08101, -0.20184,  0.07077, -0.08081,
    -0.32746,  1.00000,  0.47820,  0.11361,  0.05331,  0.19966,  0.23657,  0.16899,  
    -0.73101,  0.47820,  1.00000, -0.28576,  0.12895,  0.26133,  0.04165,  0.17382, 
    0.51345,  0.11361, -0.28576,  1.00000, -0.10989, -0.12622,  0.22026,  0.07362,
    -0.08101,  0.05331,  0.12895, -0.10989,  1.00000,  0.66456,  0.22366,  0.20519,
    -0.20184,  0.19966,  0.26133, -0.12622,  0.66456,  1.00000,  0.20576,  0.28512,
    0.07077,  0.23657,  0.04165,  0.22026,  0.22366,  0.20576,  1.00000,  0.60096,
    -0.08081,  0.16899,  0.17382,  0.07362,  0.20519,  0.28512,  0.60096,  1.00000),
  nrow=8,
  dimnames=list(
    c("PHLOVE","PHCONF","PHAMB","PHMAINT","XHACHFI","XHACHFE","XHTENANX","XHDEPRES"),
    c("PHLOVE","PHCONF","PHAMB","PHMAINT","XHACHFI","XHACHFE","XHTENANX","XHDEPRES"))
)

MySecondRegCorr   # check the correlation matrix

# reorder the variables so they are in the same sequence as LISREL
# this is important since I want indices to reflect the order of the variables

MySecondRegCorrRev <- MySecondRegCorr[c(1,2,3,4,7,8,5,6),c(1,2,3,4,7,8,5,6)]
MySecondRegCorrRev   # check the correlation matrix

require("lavaan")

first.path.model <- '
XHTENANX ~ b51*PHLOVE + b52*PHCONF +b53*PHAMB + b54*PHMAINT
XHDEPRES ~ b61*PHLOVE + b62*PHCONF +b63*PHAMB + b64*PHMAINT
XHACHFI ~ b75*XHTENANX + b76*XHDEPRES 
XHACHFE ~ b85*XHTENANX + b86*XHDEPRES 
PHLOVE   ~~ 1*PHLOVE
PHCONF   ~~ 1*PHCONF
PHAMB   ~~ 1*PHAMB
PHMAINT  ~~ 1*PHMAINT
XHTENANX ~~ p55*XHTENANX
XHDEPRES ~~ p66*XHDEPRES
XHACHFI ~~ p77*XHACHFI
XHACHFE ~~ p88*XHACHFE
PHCONF   ~~ p21*PHLOVE
PHAMB   ~~ p31*PHLOVE
PHMAINT   ~~ p41*PHLOVE
PHAMB ~~ p32*PHCONF
PHMAINT ~~ p43*PHAMB
PHMAINT ~~ p42*PHCONF
'

first.path.model.fit <- sem(first.path.model, sample.cov=MySecondRegCorrRev, sample.nobs=126)
summary(first.path.model.fit, rsquare=TRUE)

lavInspect(first.path.model.fit, what="est")

# to get LISREL type pattern and parameter estimate matrices

lavInspect(first.path.model.fit, what="est") # LISREL estimates
lavInspect(first.path.model.fit)             # which parameters are estimated

# you can get the additional statistics and matrices as shown below

# get fit measures
fitMeasures(first.path.model.fit)

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




first.path.model <- '
XHTENANX ~  b52*PHCONF + b54*PHMAINT
XHDEPRES ~   b63*PHAMB 
XHACHFI ~ b75*XHTENANX 
XHACHFE ~ b86*XHDEPRES 
PHLOVE   ~~ 1*PHLOVE
PHCONF   ~~ 1*PHCONF
PHAMB   ~~ 1*PHAMB
PHMAINT  ~~ 1*PHMAINT
XHTENANX ~~ p55*XHTENANX
XHDEPRES ~~ p66*XHDEPRES
XHACHFI ~~ p77*XHACHFI
XHACHFE ~~ p88*XHACHFE
PHCONF   ~~ p21*PHLOVE
PHAMB   ~~ p31*PHLOVE
PHMAINT   ~~ p41*PHLOVE
PHAMB ~~ p32*PHCONF
PHMAINT ~~ p43*PHAMB
PHMAINT ~~ p42*PHCONF
XHACHFE ~~ p78*XHACHFI
'

first.path.model.fit <- sem(first.path.model, sample.cov=MySecondRegCorrRev, sample.nobs=126)
summary(first.path.model.fit, rsquare=TRUE)

lavInspect(first.path.model.fit, what="est")
lavInspect(first.path.model.fit)

# to get LISREL type pattern and parameter estimate matrices

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




first.path.model <- '
XHTENANX ~  b52*PHCONF + b54*PHMAINT
XHDEPRES ~   b63*PHAMB 
XHACHFI ~ b75*XHTENANX 
XHACHFE ~ b86*XHDEPRES 
PHLOVE   ~~ 1*PHLOVE
PHCONF   ~~ 1*PHCONF
PHAMB   ~~ 1*PHAMB
PHMAINT  ~~ 1*PHMAINT
XHTENANX ~~ p55*XHTENANX
XHDEPRES ~~ p66*XHDEPRES
XHACHFI ~~ p77*XHACHFI
XHACHFE ~~ p88*XHACHFE
PHCONF   ~~ p21*PHLOVE
PHAMB   ~~ p31*PHLOVE
PHMAINT   ~~ p41*PHLOVE
PHAMB ~~ p32*PHCONF
PHMAINT ~~ p43*PHAMB
PHMAINT ~~ p42*PHCONF
XHACHFE ~~ p87*XHACHFI
XHDEPRES ~~ p65*XHTENANX
'

first.path.model.fit <- sem(first.path.model, sample.cov=MySecondRegCorrRev, sample.nobs=126)
summary(first.path.model.fit, rsquare=TRUE)

lavInspect(first.path.model.fit, what="est")
lavInspect(first.path.model.fit)

# to get LISREL type pattern and parameter estimate matrices

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

