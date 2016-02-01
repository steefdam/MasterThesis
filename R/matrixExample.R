t1 <- array(1:100, dim=c(20,5))
t2 <- array(1:100, dim=c(20,5))

t3 <- sampleReshuffle(t2)

t4 <- as.numeric(t1)
t5 <- as.numeric(t3)

t6 <- matrix(NA, nrow = 100, ncol = 2)

t6[,1] <- t4
t6[,2] <- t5

