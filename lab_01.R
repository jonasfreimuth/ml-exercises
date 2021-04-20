
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
    # ensure rpart is available
    if (!require("rpart", quietly = TRUE)) {
      stop(paste("Package rpart required for these functions, install via",
                 "'install.packages(\"rpart\")'."))
    }
    
    for (i in 1:ncol(x)) {
      
      mod_i <- rpart::rpart(formula(paste(colnames(x)[i], "~ .")),
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
    
    if (any(is.na(x))) {
      if (! na.rm) {
        stop(paste("Method 'q' not possible with missing values",
                   "and na.rm is disabled."))
      }
    }
    
    # this does not work completely, i have no idea why
    x_ranks <- list(apply(x, 2, rank, ties.method = "average"), 
                    apply(x, 2, rank, ties.method = "first"))
    
    x_sort <- apply(x, 2, sort, na.last = TRUE)
    x_rowavg <- sort(apply(x_sort, 1, mean, na.rm = na.rm))
    
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


# Tests -------------------------------------------------------------------

jf.test <- function() {
  
  works <- list()
  
  n_col <- round(runif(1, 2, 100))
  n_row <- round(runif(1, 2, 100))
  n_cell <- n_col * n_row
  
  col_names <- list()
  
  for (i in 1:n_col){
    col_names[[i]] <- paste(sample(c(LETTERS, letters),
                                   round(runif(1, 1, 12)),
                                   replace = TRUE), collapse = "")
  }
  
  mat <- matrix(runif(n_cell), n_row, n_col)
  
  colnames(mat) <- col_names
  
  # Task 1 ------------------------------------------------------------------
  
  na_mat <- jf.na.intro(mat)
  
  works["jf.na.intro"] <- (!(any(is.na(mat))) && any(is.na(na_mat)) ) 
  
  # Task 2 ------------------------------------------------------------------
  
  imp_mat_list <- list()
  
  for (m in eval(formals(jf.impute)$method)) {
    imp_mat_list[[m]] <- jf.impute(na_mat, method = m)
  }
  
  works["jf.impute"] <- (any(is.na(na_mat)) &&
                  !any(sapply(imp_mat_list, is.na)))
  
  # Task 3 ------------------------------------------------------------------
  
  norm_mat_list <- list()
  
  for (m in eval(formals(jf.norm)$method)) {
    norm_mat_list[[m]] <- jf.norm(imp_mat_list[[1]], method = m)
  }
  
  works["jf.norm.z"] <- (all(
    round(norm_mat_list[["z"]], 5) == round(scale(imp_mat_list[[1]]), 5)))
  
  if (requireNamespace("preprocessCore")) {
    ppc_norm <- preprocessCore::normalize.quantiles(imp_mat_list[[1]])
    jf_norm <- jf.norm(imp_mat_list[[1]], method = "q")
    
    # somehow there are differences with an average of ~ 0.01
    # mean(abs(jf_norm - ppc_norm))
    
    works["jf.norm.q"] <- all(jf_norm == ppc_norm)
  }

  # Result ------------------------------------------------------------------

  works <- unlist(works)
  
  if (all(works)) {
    print("All tests passed.")
  } else {
    passed <- names(works[works])
    failed <- names(works[!works])
    
    print(paste("Tests", paste(passed, collapse = ", "), "passed,",
                "tests", paste(failed, collapse = ", "), "failed."))
  }
}

