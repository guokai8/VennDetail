options(device=pdf())
A <- sample(1:100, 40, replace = FALSE);
B <- sample(1:100, 60, replace = FALSE);
C <- sample(1:100, 40, replace = FALSE);
expect_error(venndetail(list(A=A,B=B,C=C),plot=TRUE))
dev.off()
