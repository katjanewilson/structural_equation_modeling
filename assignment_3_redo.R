
###### REDO

require("lavaan")

nvars <- 6
nobs <- 126
# I'm reading in the correlation matrix
MyFirstLongRegCorr <- matrix(
  c(1.00000,   0.73859,   0.64878,   0.42438,   0.42894,   0.33610,
    0.73859,   1.00000,   0.61387,   0.33092,   0.38642,   0.33697,
    0.64878,   0.61387,   1.00000,   0.35473,   0.34219,   0.45285,
    0.42438,   0.33092,   0.35473,   1.00000,   0.64199,   0.68002,
    0.42894,   0.38642,   0.34219,   0.64199,   1.00000,   0.70235,
    0.33610,   0.33697,   0.45285,   0.68002,   0.70235,   1.00000),
  nrow=6,
  dimnames=list(
    c("h1love", "h2love", "h3love", "w1love", "w2love", "w3love"),
    c("h1love", "h2love", "h3love", "w1love", "w2love", "w3love"))
)

MyFirstLongRegCorr


######## Model 1



htow.path.model <- '
h2love ~ b21*h1love
h3love ~ b32*h2love
w2love ~ b54*w1love
w3love ~ b65*w2love
h1love ~~ ps11*h1love    
w1love ~~ ps44*w1love    
w1love ~~ p41*h1love     
h2love ~~ p22*h2love     
h3love ~~ p33*h3love
w2love ~~ p55*w2love
w3love ~~ p66*w3love
'
htow.path.model.fit <- sem(htow.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(htow.path.model.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(htow.path.model.fit)
57-48
lavInspect(htow.path.model.fit, what="est")
lavInspect(htow.path.model.fit, what="se")

# get fit measures
fitMeasures(htow.path.model.fit)

# model implied covariance matrix
fitted(htow.path.model.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(htow.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(htow.path.model.fit, type="cor")

# standardized residual correlations
residuals(htow.path.model.fit, type="standardized")

# get modification indices
modificationIndices(htow.path.model.fit)

exogcorr <- matrix(c(0.992,0.421,0.421,0.992), nrow=2,
                   dimnames=list(
                     c('zeta1','zeta2'),
                     c('zeta1','zeta2')))
exogcorr
cov2cor(exogcorr)  # to match exogenous correlations with LISREL




######## Model 2



htow.path.model <- '
h2love ~ b21*h1love
h3love ~ b32*h2love
w2love ~ b51*h1love + b54*w1love
w3love ~ b62*h2love + b65*w2love
h1love ~~ ps11*h1love    
w1love ~~ ps44*w1love    
w1love ~~ p41*h1love     
h2love ~~ p22*h2love     
h3love ~~ p33*h3love
w2love ~~ p55*w2love
w3love ~~ p66*w3love
'
htow.path.model.fit <- sem(htow.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(htow.path.model.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(htow.path.model.fit)
lavInspect(htow.path.model.fit, what="est")
lavInspect(htow.path.model.fit, what="se")

# get fit measures
fitMeasures(htow.path.model.fit)

# model implied covariance matrix
fitted(htow.path.model.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(htow.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(htow.path.model.fit, type="cor")

# standardized residual correlations
residuals(htow.path.model.fit, type="standardized")

# get modification indices
modificationIndices(htow.path.model.fit)

exogcorr <- matrix(c(0.992,0.421,0.421,0.992), nrow=2,
                   dimnames=list(
                     c('zeta1','zeta2'),
                     c('zeta1','zeta2')))
exogcorr
cov2cor(exogcorr)  # to match exogenous correlations with LISREL







######## Model 3



htow.path.model <- '
h2love ~ b21*h1love + b42*w1love
h3love ~ b32*h2love + b53*w2love
w2love ~ b54*w1love
w3love ~ b65*w2love
h1love ~~ ps11*h1love    
w1love ~~ ps44*w1love    
w1love ~~ p41*h1love     
h2love ~~ p22*h2love     
h3love ~~ p33*h3love
w2love ~~ p55*w2love
w3love ~~ p66*w3love
'
htow.path.model.fit <- sem(htow.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(htow.path.model.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(htow.path.model.fit)
lavInspect(htow.path.model.fit, what="est")
lavInspect(htow.path.model.fit, what="se")

# get fit measures
fitMeasures(htow.path.model.fit)

# model implied covariance matrix
fitted(htow.path.model.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(htow.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(htow.path.model.fit, type="cor")

# standardized residual correlations
residuals(htow.path.model.fit, type="standardized")

# get modification indices
modificationIndices(htow.path.model.fit)

exogcorr <- matrix(c(0.992,0.421,0.421,0.992), nrow=2,
                   dimnames=list(
                     c('zeta1','zeta2'),
                     c('zeta1','zeta2')))
exogcorr
cov2cor(exogcorr)  # to match exogenous correlations with LISREL







######## Model 4



htow.path.model <- '
h2love ~ b21*h1love + b42*w1love
h3love ~ b32*h2love + b53*w2love
w2love ~ b51*h1love + b54*w1love
w3love ~ b62*h2love + b65*w2love
h1love ~~ ps11*h1love    
w1love ~~ ps44*w1love    
w1love ~~ p41*h1love     
h2love ~~ p22*h2love     
h3love ~~ p33*h3love
w2love ~~ p55*w2love
w3love ~~ p66*w3love
'
htow.path.model.fit <- sem(htow.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(htow.path.model.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(htow.path.model.fit)
lavInspect(htow.path.model.fit, what="est")
lavInspect(htow.path.model.fit, what="se")

# get fit measures
fitMeasures(htow.path.model.fit)

# model implied covariance matrix
fitted(htow.path.model.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(htow.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(htow.path.model.fit, type="cor")

# standardized residual correlations
residuals(htow.path.model.fit, type="standardized")

# get modification indices
modificationIndices(htow.path.model.fit)

exogcorr <- matrix(c(0.992,0.421,0.421,0.992), nrow=2,
                   dimnames=list(
                     c('zeta1','zeta2'),
                     c('zeta1','zeta2')))
exogcorr
cov2cor(exogcorr)  # to match exogenous correlations with LISREL





######## Model 2



htow.path.model <- '
h2love ~ b21*h1love
h3love ~ b32*h2love
w2love ~ b51*h1love + b54*w1love
w3love ~  b65*w2love
h1love ~~ ps11*h1love    
w1love ~~ ps44*w1love    
w1love ~~ p41*h1love     
h2love ~~ p22*h2love     
h3love ~~ p33*h3love
w2love ~~ p55*w2love
w3love ~~ p66*w3love
'
htow.path.model.fit <- sem(htow.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(htow.path.model.fit, rsquare=TRUE)

# to get LISREL type pattern and parameter estimate matrices

lavInspect(htow.path.model.fit)
lavInspect(htow.path.model.fit, what="est")
lavInspect(htow.path.model.fit, what="se")

# get fit measures
fitMeasures(htow.path.model.fit)

# model implied covariance matrix
fitted(htow.path.model.fit)

# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(htow.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)

# residual correlations
residuals(htow.path.model.fit, type="cor")

# standardized residual correlations
residuals(htow.path.model.fit, type="standardized")

# get modification indices
modificationIndices(htow.path.model.fit)

exogcorr <- matrix(c(0.992,0.421,0.421,0.992), nrow=2,
                   dimnames=list(
                     c('zeta1','zeta2'),
                     c('zeta1','zeta2')))
exogcorr
cov2cor(exogcorr)  # to match exogenous correlations with LISREL



