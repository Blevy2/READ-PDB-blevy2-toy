size <- 1e5
samples <- 100
vals <- sample.int(size ^ 2, samples)
All_Samples <- cbind(vals %/% size + 1, vals %% size)

anyDuplicated(All_Samples)


#testing anyDuplicated