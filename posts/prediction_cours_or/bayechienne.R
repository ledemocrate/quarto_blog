library(statsr)
library(ggplot2)
library(tidyverse)

data(tapwater)
glimpse(tapwater)

set.seed(42)
# prior hyper-parameters
m_0 = 35; n_0 = 25; s2_0 = 156.25; v_0 = n_0- 1
# sample summaries
Y = tapwater$tthm
ybar = mean(Y)
s2 = var(Y)
n = length(Y)

# posterior hyperparameters
n_n = n_0 + n
m_n = (n*ybar + n_0*m_0)/n_n
v_n = v_0 + n
s2_n = ((n-1)*s2 + v_0*s2_0 + n_0*n*(m_0- ybar)^2/n_n)/v_n

bayes_inference(tthm, data=tapwater, prior="NG",
               mu_0 = m_0, n_0=n_0, s_0 = sqrt(s2_0), v_0 = v_0,
               stat="mean", type="ci", method="theoretical",
               show_res=TRUE, show_summ=TRUE, show_plot=FALSE)

phi = rgamma(1000, shape = v_n/2, rate=s2_n*v_n/2)

df = data.frame(phi = sort(phi))
df = mutate(df,
            density = dgamma(phi,
                             shape = v_n/2,
                             rate=s2_n*v_n/2))

df = mutate(df,
            sigma =1/sqrt(phi))
            
ggplot(data=df, aes(x=phi)) +
  geom_histogram(aes(x=phi, y=..density..), bins = 50) +
  geom_density(aes(phi, ..density..), color="black") +
  geom_line(aes(x=phi, y=density), color="orange")

mean(phi)
quantile(phi, c(0.025, 0.975))
(v_n/2)/(v_n*s2_n/2)

ggplot(data=df, aes(x=sigma)) +
  geom_histogram(aes(x=sigma, y=..density..), bins = 50) +
  geom_density(aes(sigma, ..density..), color="black") 

set.seed(1234)
phi = rgamma(10000, v_0/2, s2_0*v_0/2)
sigma = 1/sqrt(phi)
mu = rnorm(10000, mean=m_0, sd=sigma/(sqrt(n_0)))
y = rnorm(10000, mu, sigma)
quantile(y, c(0.025,0.975))

set.seed(1234)
phi = rgamma(10000, v_n/2, s2_n*v_n/2)
sigma = 1/sqrt(phi)
post_mu = rnorm(10000, mean=m_n, sd=sigma/(sqrt(n_n)))
pred_y = rnorm(10000,post_mu, sigma)
quantile(pred_y, c(.025, .975))

