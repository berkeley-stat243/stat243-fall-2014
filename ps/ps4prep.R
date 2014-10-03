load("~/staff/consults/fuJobQuit/arwen/simulatedRaw_clean.rdata")
load("~/staff/consults/fuJobQuit/arwen/everything.rdata")



A <- get.adjacency(g)
n <- dim(A)[1]
K <- length(truth)

A <- as.matrix(A)


save(n, K, A, file = 'ps4prob6.Rda')
