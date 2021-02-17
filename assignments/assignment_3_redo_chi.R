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
57.299- 48.956
# Above, I picked a value of chi-square equal to a z=1.96 squared
# Squaring z gives you a chi-square with 1 df.  Notice the tail probability

q=7.8336   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

q=2.7504   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

q=2.7503   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability


q=8.3436   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

q=3.2603   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

q=2.7463   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability

48.956-46.205

q=2.751   # chi-square value that you provide
df=2      # degrees of freedom that you provide
tail_prob <- 1-(pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE))
tail_prob # tail probability
