mu = 10
B = 10000
n_seq = c(5,15,50)
length(n_seq)

set.seed(3)
estimates_matrix = matrix(NA, nrow = B, ncol = length(n_seq))

for(j in 1:length(n_seq)){
  n = n_seq[j]
  for(b in 1:B){
    X_sample = rnorm(n = n, mean = mu, sd = sqrt(5))
    estimates_matrix[b,j] = mean(X_sample)
  }
}
par(mfrow=c(1,3))
hist(estimates_matrix[,1], main="n=5", xlab="",  xlim = range(estimates_matrix[,1]), prob = TRUE, ylim = c(0, 1.2))
lines(density(estimates_matrix[,1], bw = bw.SJ(estimates_matrix[,1])), col="blue", lwd=1.5)
hist(estimates_matrix[,2], main="n=15", xlab="", xlim = range(estimates_matrix[,1]), prob = TRUE, ylim = c(0, 1.2))
lines(density(estimates_matrix[,2], bw = bw.SJ(estimates_matrix[,3])), col="blue", lwd=1.5)
hist(estimates_matrix[,3], main="n=50", xlab="", xlim = range(estimates_matrix[,1]), prob = TRUE, ylim = c(0, 1.2))
lines(density(estimates_matrix[,2], bw = bw.SJ(estimates_matrix[,3])), col="blue", lwd=1.5)

set.seed(3)
my_estimates_generator <- function(n){
  X_sample = rnorm(n = n, mean = mu, sd = sqrt(5))
  return(mean(X_sample))
}

estimates_matrix <- cbind(
  replicate(B, my_estimates_generator(n=n_seq[1])),
  replicate(B, my_estimates_generator(n=n_seq[2])),
  replicate(B, my_estimates_generator(n=n_seq[3]))
)

par(mfrow=c(1,3))
hist(estimates_matrix[,1], main="n=5", xlab="",  xlim = range(estimates_matrix[,1]), prob = TRUE, ylim = c(0, 1.2))
lines(density(estimates_matrix[,1], bw = bw.SJ(estimates_matrix[,1])), col="blue", lwd=1.5)
hist(estimates_matrix[,2], main="n=15", xlab="", xlim = range(estimates_matrix[,1]), prob = TRUE, ylim = c(0, 1.2))
lines(density(estimates_matrix[,2], bw = bw.SJ(estimates_matrix[,3])), col="blue", lwd=1.5)
hist(estimates_matrix[,3], main="n=50", xlab="", xlim = range(estimates_matrix[,1]), prob = TRUE, ylim = c(0, 1.2))
lines(density(estimates_matrix[,2], bw = bw.SJ(estimates_matrix[,3])), col="blue", lwd=1.5)

MC_Bias_n_seq = apply(estimates_matrix, 2, mean) - mu
MC_Var_n_seq = apply(estimates_matrix, 2, var)
MC_MSE_n_seq = apply(estimates_matrix, 2, function(x){mean(x-mu)^2})
estimates = matrix(NA, ncol = 4, nrow = 4)
estimates[1,1] = "n"
estimates[1,2] = 5
estimates[1,3] = 10
estimates[1,4] = 15
estimates[2,1] = "Bias"
estimates[3,1] = "Var"
estimates[4,1] = "MSE"
estimates[2,2:4] = MC_Bias_n_seq
estimates[3,2:4] = MC_Var_n_seq
estimates[4,2:4] = MC_MSE_n_seq
Krishna