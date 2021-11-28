
# Task 1 ------------------------------------------------------------------

jf.clusterSample <- function (l = 2, n = 50, sd = 1) {
  
  if (length(n) == 1) {
    n <- rep(n, l)
  } else if (length(n) != l) {
    stop ("If n is not an atomic vector, it must be of length l.")
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, l)
  } else if (length(sd) != l) {
    stop ("If n is not an atomic vector, it must be of length l.")
  }
  
  if (l > 6) {
    stop ("More than 6 centres currently not supported")
  }
  
  pts <- list(midmid = c( 0,  0),
              botlef = c(-6, -6),
              botrig = c( 6, -6),
              toplef = c(-6,  6),
              toprig = c( 6,  6),
              midlef = c(-6,  0),
              midrig = c( 6,  0)
              )
  
  # TODO Ensure actual order of dice is observed
  ch_pts <- sample(pts, l)
  
  data <- matrix(nrow = sum(n), ncol = 2)
  
  j <- 0
  
  for (c in 1:l) {
    
    i <- j + 1
    j <- i + n[c] - 1
    
    row_idx <- i:j
    
    data[row_idx, 1] <- rnorm(n[c], mean = ch_pts[[c]][1], sd = sd[c])
    data[row_idx, 2] <- rnorm(n[c], mean = ch_pts[[c]][2], sd = sd[c])
    
  }
  
  return (data)
}


# Task 3 ------------------------------------------------------------------

# TODO Fix
jf.clusterCompare <- function (clust1, clust2) {
  
  agree <- list(SS = 0, DS = 0, SD = 0, DD = 0)
  
  for (i in 2:length(clust1)) {
    for (j in (i + 1):length(clust1) - 1) {
      if (clust1[i] == clust1[j]) {
        if (clust2[i] == clust2[j]) {
          agree[["SS"]] <- agree[["SS"]] + 1
        } else {
          agree[["SD"]] <- agree[["SD"]] + 1
        }
      } else {
        if (clust2[i] == clust2[j]) {
          agree[["DS"]] <- agree[["DS"]] + 1
        } else {
          agree[["DD"]] <- agree[["DD"]] + 1
        }
      }
    }
  }
  
  randInd <- (agree[["SS"]] + agree[["DD"]])/(sum(unlist(agree)))
  jaccard <- (agree[["SS"]]) / (agree[["SS"]] + agree[["DS"]] + agree[["SD"]])
  return (list(randInd = randInd, jaccard = jaccard))
}




