
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

jf.norm <- function(x, method = c("z", "uv", "fs", "q")) {
  
  method <- match.arg(method)
  
  is.df <- is.data.frame(x)
  
  x_names <- dimnames(x)
  
  if (is.df) {
    x <- as.matrix(x)
  }
  
  if (!(method %in% c("q"))) {
    
    x <- apply(x, 2, function(x) {
      if (method == "z") {
        x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
        
      } else if (method == "uv") {
        x <- (x)/sd(x, na.rm = TRUE)
        
      } else if (method == "fs") {
        x <- (x - min(x, na.rm = TRUE))/
          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      }
      
      return(x)
    }
    ) 
    
  } else if (method == "q") {

    # sort each column in descending order
    # ranks are also in order afterwards
    x_sort <- apply(x, 2, sort, na.last = TRUE)
    
    # calculate the average for each row in the column sorted
    # matrix
    # i.e. calculate the average for each rank
    x_rankavg <- sort(apply(x_sort, 1, mean, na.rm = TRUE))
    
    # match rank averages back to individual columns
    # in cases where multiple cells have the same ranks
    # use the mean rank average for all tied cells
    x <- apply(x, 2, function(x_col) {
      
      x_col_new <- x_col
      
      # go through every unique rank (group tied cells together)
      for (rank in unique(rank(x_col, na.last = TRUE))) {
        # generate selection vector where x_col has the current rank
        rank_select <- rank(x_col, na.last = TRUE) == rank
        
        # find the indices of the rank average corresponding to the rows
        # in x_col sharing the same rank
        rank_avg_idcs <- rank(x_col,
                             na.last = TRUE,
                             ties.method = "first"
                             )[rank_select]
        
        # match the mean rank average to its corresponding rows
        x_col_new[rank_select] <- mean(x_rankavg[rank_avg_idcs])
      }
        return(x_col_new)
    })
  }
  
  if (is.df) {
    x <- as.data.frame(x)
  }
  
  dimnames(x) <- x_names
  
  return(x)
}


# Tests -------------------------------------------------------------------

# Very poor mans unit tests:
jf.test <- function(data = NULL) {
  
  works <- list()
  
  if (is.null(data)) {
    
    n_col <- round(runif(1, 2, 100))
    n_row <- round(runif(1, 2, 100))
    n_cell <- n_col * n_row
    
    # ensure colnames are unique
    # there is probably a better way to do this but i'm annoyed i'm unable to 
    # deliver a solution that works properly so i don't want to spend more time
    # on this
    repeat {
      col_names <- list()
      
      for (i in 1:n_col){
        col_names[[i]] <- paste(sample(letters,
                                       round(runif(1, 1, 12)),
                                       replace = TRUE), collapse = "")
      }
      
      if (length(col_names) == length(unique(col_names))) {
        break
      }
      
    }
    mat <- matrix(rnorm(n_cell), n_row, n_col)
    
    colnames(mat) <- col_names
    
    mat <- as.data.frame(mat)
  }
  
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
    norm_mat_list[[m]] <- jf.norm(na_mat, method = m)
  }
  
  works["jf.norm.z"] <- (all(
    round(norm_mat_list[["z"]], 5) == round(scale(na_mat), 5), na.rm = TRUE))
  
  if (require("preprocessCore", quietly = TRUE)) {
    # for mean differences results need to be in matrix form
    
    # ppC normalize.quantiles expects a matrix, therefore coercion takes place
    # immediately
    ppc_norm <- normalize.quantiles(as.matrix(na_mat))
    
    # jf.norm should deal with dataframes so matrix coercion takes place after
    # the function call
    jf_norm <- as.matrix(jf.norm(na_mat, method = "q"))
    
    # somehow there are differences with an average of ~ 0.01
    print(paste("Mean difference between jf.norm.q and",
                "preprocessCore::normalize.quantiles:"))
    print(mean(abs(jf_norm - ppc_norm), na.rm = TRUE))
    
    works["jf.norm.q"] <- all(jf_norm == ppc_norm, na.rm = TRUE)
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

