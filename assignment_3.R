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

To run a chi-square difference test, you need to provide

q= <the difference in chi-square goes here>             # chi-square value that you provide
  df= <the difference in degrees of freedom goes here>    # degrees of freedom that you provide
  # the last two lines remain the same
  tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability