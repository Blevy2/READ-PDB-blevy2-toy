

temp<-list(moveCov[["cov.matrix"]][[1]],moveCov[["cov.matrix"]][[1]])
 append(temp,moveCov[["cov.matrix"]]) -> new
 View(new)
 moveCov[["cov.matrix"]]<-new

 remove(temp)