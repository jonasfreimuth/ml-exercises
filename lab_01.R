
# Task 1 ------------------------------------------------------------------

jf.na.intro <- function(x, prob = 0.05) {
  is.df <- is.data.frame(x)
  
  x_names <- dimnames(x)
  
  if (is.df) {
    x <- as.matrix(x)
  }
  
  indx_mat <- matrix(data = as.logical(rbinom(length(x), 1, prob)),
                     nrow = nrow(x), ncol = ncol(x))
  
  x[indx_mat] <- NA
  
  if (is.df) {
    x <- as.data.frame(x)
  }
  
  dimnames(x) <- x_names
  
  return(x)
}

# Task 2 ------------------------------------------------------------------


jf.impute <- function(x, method = c("mean", "median", "rpart")) {
  
  method <- match.arg(method)
  
  print(paste("Method is", method))
  
  is.df <- is.data.frame(x)
  
  x_names <- dimnames(x)
  
  if (is.df) {
    x <- as.matrix(x)
  }
  
  if (method == "mean") {
    for (i in 1:ncol(x)) {
      x[is.na(x[,i]), i] <- mean(x[, i], na.rm = TRUE)
    }
    
  } else if (method == "median") {
    for (i in 1:ncol(x)) {
      x[is.na(x[,i]), i] <- median(x[, i], na.rm = TRUE)
    }
    
  } else if (method == "rpart") {
    for (i in 1:ncol(x)) {
      
      mod_i <- rpart(formula(paste(colnames(x)[i], "~ .")),
                   data = as.data.frame(x))
      
      x[is.na(x[, i]), i] <- predict(
        mod_i,
        newdata = as.data.frame(x)
      )[is.na(x[, i])]
      
    }
  }
  
  if (is.df) {
    x <- as.data.frame(x)
  }
  
  dimnames(x) <- x_names
  
  return(x)
}

# Task 3 ------------------------------------------------------------------


jf.norm <- function(x, method = c("z", "uv", "fs", "q"), na.rm = FALSE) {
  
  method <- match.arg(method)
  
  is.df <- is.data.frame(x)
  
  x_names <- dimnames(x)
  
  if (is.df) {
    x <- as.matrix(x)
  }
  
  if (!(method %in% c("q"))) {
    
    x <- apply(x, 2, function(x) {
      if (method == "z") {
        x <- (x - mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm)
        
      } else if (method == "uv") {
        x <- (x)/sd(x, na.rm = na.rm)
        
      } else if (method == "fs") {
        x <- (x - min(x, na.rm = na.rm))/
          (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
      }
      
      return(x)
    }
    ) 
    
  } else if (method == "q") {
    # this does not work completely, i have no idea why
    x_ranks <- list(apply(x, 2, rank, ties.method = "average"), 
                    apply(x, 2, rank, ties.method = "first"))
    
    x_sort <- apply(x, 2, sort)
    x_rowavg <- sort(apply(x_sort, 1, mean))
    
    for (i in 1:ncol(x)) {
      i_ranks <- cbind(x_ranks[[1]][, i], x_ranks[[2]][, i])

      i_rowavg <- aggregate(x_rowavg,
                            by = list(rank = sort(i_ranks[, 1])),
                            mean, na.rm = na.rm)

      i_rowavg_vec_ordered <- rep(i_rowavg$x, as.vector(table(i_ranks[, 1])))

      x[, i] <- i_rowavg_vec_ordered[i_ranks[, 2]]
    }
  }
  
  if (is.df) {
    x <- as.data.frame(x)
  }
  
  dimnames(x) <- x_names
  
  return(x)
}


