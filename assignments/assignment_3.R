###### Part 1:
### Import Data

require("lavaan")
nvars <- 6
nobs <- 155
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

###### Model 1

require("lavaan")
first.path.model <- '
h2love ~ b21*h1love
h3love ~ b32*h2love
w2love ~ b54*w1love
w3love ~ b65*w2love
h1love   ~~ 1*h1love
w1love   ~~ 1*w1love
w2love ~~ p55*w2love
h2love ~~ p22*h2love
h3love ~~ p33*h3love
w3love ~~ p66*w3love
w1love ~~ p12*h1love

'
fourth.path.model.fit <- sem(first.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(fourth.path.model.fit, rsquare=TRUE)
lavInspect(fourth.path.model.fit, what="est")
# to get LISREL type pattern and parameter estimate matrices
lavInspect(fourth.path.model.fit, what="est") # LISREL estimates
lavInspect(fourth.path.model.fit)             # which parameters are estimated
# you can get the additional statistics and matrices as shown below
# get fit measures
fitMeasures(fourth.path.model.fit)


###### Model 2

require("lavaan")
second.path.model <- '
h2love ~ b21*h1love
h3love ~ b32*h2love
w2love ~ b51*h1love + b54*w1love
w3love ~ b62*h2love + b65*w2love
h1love   ~~ 1*h1love
w1love   ~~ 1*w1love
w2love ~~ p55*w2love
h2love ~~ p22*h2love
h3love ~~ p33*h3love
w3love ~~ p66*w3love
w1love ~~ p12*h1love

'
fourth.path.model.fit <- sem(second.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(fourth.path.model.fit, rsquare=TRUE)
lavInspect(fourth.path.model.fit, what="est")
# to get LISREL type pattern and parameter estimate matrices
lavInspect(fourth.path.model.fit, what="est") # LISREL estimates
lavInspect(fourth.path.model.fit)             # which parameters are estimated
# you can get the additional statistics and matrices as shown below
# get fit measures
fitMeasures(fourth.path.model.fit)


###### Model 3

require("lavaan")
third.path.model <- '
h2love ~ b21*h1love + b42*w1love
h3love ~ b32*h2love +b53*w2love
w2love ~ b54*w1love
w3love ~  b65*w2love
h1love   ~~ 1*h1love
w1love   ~~ 1*w1love
w2love ~~ p55*w2love
h2love ~~ p22*h2love
h3love ~~ p33*h3love
w3love ~~ p66*w3love
w1love ~~ p12*h1love

'
fourth.path.model.fit <- sem(third.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(fourth.path.model.fit, rsquare=TRUE)
lavInspect(fourth.path.model.fit, what="est")
# to get LISREL type pattern and parameter estimate matrices
lavInspect(fourth.path.model.fit, what="est") # LISREL estimates
lavInspect(fourth.path.model.fit)             # which parameters are estimated
# you can get the additional statistics and matrices as shown below
# get fit measures
fitMeasures(fourth.path.model.fit)

###### Model 4

require("lavaan")
fourth.path.model <- '
h2love ~ b21*h1love + b42*w1love
h3love ~ b32*h2love+ b53*w2love
w2love ~ b52*h1love + b53*w1love
w3love ~ b62*h2love+ b65*w2love
h1love   ~~ 1*h1love
w1love   ~~ 1*w1love
w2love ~~ p55*w2love
h2love ~~ p22*h2love
h3love ~~ p33*h3love
w3love ~~ p66*w3love
w1love ~~ p12*h1love

'

fourth.path.model.fit <- sem(fourth.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(fourth.path.model.fit, rsquare=TRUE)
lavInspect(fourth.path.model.fit, what="est")
# to get LISREL type pattern and parameter estimate matrices
lavInspect(fourth.path.model.fit, what="est") # LISREL estimates
lavInspect(fourth.path.model.fit)             # which parameters are estimated
# you can get the additional statistics and matrices as shown below
# get fit measures
fitMeasures(fourth.path.model.fit)


# the following will give you the tail probability of a chi-square, q
#   with degrees of freedom, df
# this formula can also give you the probability for a non-central chi-square
#   distribution which is used in certain power analyses
# we are not using that form so we set the non-centrality parameter, ncp=0
# we also don't want the log of the probability, so we set that equal to FALSE

q=3.84    # chi-square value that you provide
df=1      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

# As an example above, I picked a value of chi-square equal to a z=1.96 squared
# Squaring z gives you a chi-square with 1 df.  Notice the tail probability

#To run a chi-square difference test, you need to provide

q= <the difference in chi-square goes here>             # chi-square value that you provide
  df= <the difference in degrees of freedom goes here>    # degrees of freedom that you provide
  # the last two lines remain the same
  tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

## Model 1 vs. Model 2: nested
q= (57.306-48.962)    # chi-square value that you provide
df = 2    # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

## Model 2 vs. model 3 - non-nested
q= (54.046-48.962)    # chi-square value that you provide
df = 0    # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

#3 Model 3 vs. Model 4: nested
q= (62.906-54.046)    # chi-square value that you provide
df = 2    # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

#3 Model 1 vs. Model 4: nested
q= (62.906-57.306)    # chi-square value that you provide
df = 4    # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

#3 Model 2 vs. Model 4: nested
q= (62.906-48.962)    # chi-square value that you provide
df = 2    # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability



###### Modify Model 4


###### Model 4

require("lavaan")
fourth.path.model <- '
h2love ~ b21*h1love + b42*w1love
h3love ~ b32*h2love+ b53*w2love
w2love ~ b52*h1love + b53*w1love
w3love ~ b62*h2love+ b65*w2love
h1love   ~~ 1*h1love
w1love   ~~ 1*w1love
w2love ~~ p55*w2love
h2love ~~ p22*h2love
h3love ~~ p33*h3love
w3love ~~ p66*w3love
w1love ~~ p12*h1love

'

fourth.path.model.fit <- sem(fourth.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(fourth.path.model.fit, rsquare=TRUE)
lavInspect(fourth.path.model.fit, what="est")
# to get LISREL type pattern and parameter estimate matrices
lavInspect(fourth.path.model.fit, what="est") # LISREL estimates
lavInspect(fourth.path.model.fit)             # which parameters are estimated
# you can get the additional statistics and matrices as shown below
# get fit measures
fitMeasures(fourth.path.model.fit)




###### Modify Model 4


###### Model 4

require("lavaan")
fourth.path.model <- '
h2love ~ b21*h1love + b42*w1love
h3love ~ b32*h2love+ b53*w2love
w2love ~ b52*h1love + b53*w1love
w3love ~ b62*h2love+ b65*w2love
h1love   ~~ 1*h1love
w1love   ~~ 1*w1love
w2love ~~ p55*w2love
h2love ~~ p22*h2love
h3love ~~ p33*h3love
w3love ~~ p66*w3love
w1love ~~ p12*h1love
h1love ~~ p31*h3love
w1love ~~ p64*w3love

'

fourth.path.model.fit <- sem(fourth.path.model, sample.cov=MyFirstLongRegCorr, sample.nobs=126)
summary(fourth.path.model.fit, rsquare=TRUE)
lavInspect(fourth.path.model.fit, what="est")
# to get LISREL type pattern and parameter estimate matrices
lavInspect(fourth.path.model.fit, what="est") # LISREL estimates
lavInspect(fourth.path.model.fit)             # which parameters are estimated
# you can get the additional statistics and matrices as shown below
# get fit measures
fitMeasures(fourth.path.model.fit)




# if a covariance matrix you can turn into a correlation matrix
impliedCovMat <- fitted(fourth.path.model.fit)$cov
cov2cor(impliedCovMat)
impliedCorrMat <- cov2cor(impliedCovMat)
# residual correlations
residuals(fourth.path.model.fit, type="cor")
# standardized residual correlations
residuals(fourth.path.model.fit, type="standardized")
# get modification indices
modificationIndices(fourth.path.model.fit)









