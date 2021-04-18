
# Setup -------------------------------------------------------------------

library("rpart")

data("iris")

iris_mat <- as.matrix(iris[,1:4], dimnames = dimnames(iris[,1:4]))

indx_mat <- matrix(data = as.logical(rbinom(length(iris_mat), 1, 0.05)),
                   nrow = nrow(iris_mat), ncol = ncol(iris_mat))


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

na_iris_mat <- jf.na.intro(iris_mat)

# Task 2 ------------------------------------------------------------------


jf.impute <- function(x, method = "mean") {
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
      
      x[, i] <- predict(mod_i, newdata = as.data.frame(x))
      
    }
  }
  
  if (is.df) {
    x <- as.data.frame(x)
  }
  
  dimnames(x) <- x_names
  
  return(x)
}

imp_iris_mat <- jf.impute(na_iris_mat)


# Task 3 ------------------------------------------------------------------


jf.norm <- function(x, method = "z", na.rm = FALSE) {
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

z_norm_iris_mat <- jf.norm(imp_iris_mat)

uv_norm_iris_mat <- jf.norm(imp_iris_mat, method = "uv")

fs_norm_iris_mat <- jf.norm(imp_iris_mat, method = "fs")

all(round(z_norm_iris_mat, 5) == round(scale(imp_iris_mat), 5))

if (requireNamespace("preprocessCore")) {
  ppc_norm <- preprocessCore::normalize.quantiles(imp_iris_mat)
  jf_norm <- jf.norm(imp_iris_mat, method = "q")
  
  # somehow there are differences with an average of ~ 0.01
  # mean(abs(jf_norm - ppc_norm))
  
  
  all(jf_norm == ppc_norm)
}
