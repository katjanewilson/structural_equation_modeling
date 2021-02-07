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

# create the path model notice that the model is in single quotes as this
# info is passed along to lavaan as text
# regressions will be indicated using ~
# correlations or covariances will be indicated using ~~
# parameter names are arbitrary but I tried to make them conform to the LISREL
#   model  e.g. b51

# PHLOVE   ~~ 1*PHLOVE fixes the psi 1 1 value at 1
# XHTENANX ~~ p55*XHTENANX estimates the residual variance
# PHCONF   ~~ p21*PHLOVE  estimates the correlation between PHCONF and PHLOVE
#  make sure to specify these as lower diagonal values i.e. PHCONF appears
#  after PHLOVE in the correlation matrix  this will save you some grief
#  down the road
# all unspecified coefficients are fixed to 0


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

first.path.model.fit <- sem(first.path.model, sample.cov=firstPathCorr, sample.nobs=126)
summary(first.path.model.fit, rsquare=TRUE)

lavInspect(first.path.model.fit, what="est")

