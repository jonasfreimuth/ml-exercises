data("iris")

iris_mat <- as.matrix(iris[,1:4])

indx_mat <- matrix(data = as.logical(rbinom(length(iris_mat), 1, 0.05)),
                   nrow = nrow(iris_mat), ncol = ncol(iris_mat))


# Task 1 ------------------------------------------------------------------


jf.na.intro <- function(x, prob = 0.05) {
  
  is.df <- is.data.frame(x)
  
  if (is.df) {
    
    x <- as.matrix(x)
    
  }
  
  indx_mat <- matrix(data = as.logical(rbinom(length(x), 1, prob)),
                     nrow = nrow(x), ncol = ncol(x))
  
  x[indx_mat] <- NA
  
  if (is.df) {
    
    x <- as.data.frame(x)
    
  }
  
  return(x)
  
}

na_iris_mat <- jf.na.intro(iris_mat)

# Task 2 ------------------------------------------------------------------


jf.impute <- function(x, method = "mean") {
  
  if (method == "mean") {
    
    for (i in 1:ncol(x)) {
      
      x[is.na(x[,i]), i] <- mean(x[, i], na.rm = TRUE)
      
    }
    
  } else if (method == "median") {
    
    for (i in 1:ncol(x)) {
      
      x[is.na(x[,i]), i] <- median(x[, i], na.rm = TRUE)
      
    }
    
  }
  
  return(x)
  
}

imp_iris_mat <- jf.impute(na_iris_mat)


# Task 3 ------------------------------------------------------------------


jf.norm <- function(x, method = "z", na.rm = FALSE) {
  
  if (method == "z") {
    
    x <- apply(x, 2, function(x) {
      
      x <- (x - mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm)
      
      return(x)
      
    }
    )
    
  } else if (method == "uv") {
    
    x <- apply(x, 2, function(x) {
      
      x <- (x)/sd(x, na.rm = na.rm)
      
      return(x)
      
    }
    )
    
  } else if (method == "fs") {
    
    x <- apply(x, 2, function(x) {
      
      x <- (x - min(x, na.rm = na.rm))/
        (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
      
      return(x)
      
    }
    )
    
  }
  
  return(x)
  
}

z_norm_iris_mat <- jf.norm(imp_iris_mat)

uv_norm_iris_mat <- jf.norm(imp_iris_mat, method = "uv")

fs_norm_iris_mat <- jf.norm(imp_iris_mat, method = "fs")

all(round(z_norm_iris_mat, 5) == round(scale(imp_iris_mat), 5))
