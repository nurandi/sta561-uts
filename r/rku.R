## --------------------------------------------------------------------------------------------------------
## BAGIAN 1. ANALISIS KOMPONEN UTAMA
## ---------------------------------------------------------------------------------------------------------

AKU <- function(X){
  
  # transform X into matrix
  X <- as.matrix(X)
  
  # transform all var into z-score
  X <- scale(X, center = TRUE, scale = TRUE)
  x.bar <- attr(X, "scaled:center")
  x.stdev <- attr(X, "scaled:scale")
  
  # compute singular-value decomposition
  SVD <- La.svd(X)
  D  <- SVD$d
  
  # compute principal component
  pc.score <- SVD$u %*% diag(D, nrow = ncol(X))
  
  # get eigenvector from SVD
  eigenvector <- SVD$vt
  
  # compute eigenvalues and PC contributions
  pc.score.stdev <- D / sqrt(max(1, nrow(X) - 1))
  eigenvalue <- pc.score.stdev^2
  eigenvalue.proportion <- eigenvalue/sum(eigenvalue)
  eigenvalue.cum.proportion <- cumsum(eigenvalue.proportion)
  eigenanalysis <-  rbind(eigenvalue, eigenvalue.proportion, eigenvalue.cum.proportion)
  
  # assign names
  pc.name <- paste0('PC', seq(ncol(X)))
  colnames(pc.score) <- rownames(eigenvector) <- colnames(eigenanalysis) <- names(pc.score.stdev) <- pc.name
  rownames(pc.score) <- rownames(X)
  colnames(eigenvector) <- colnames(X)
  rownames(eigenanalysis) <- c("Eigenvalue", "Proportion", "Cumulative")
  
  # wrap output into list
  out <- list(
    n.col = ncol(X),
    n.row = nrow(X),
    x.bar = x.bar, x.stdev = x.stdev,
    pc.score = pc.score, 
    pc.score.stdev = pc.score.stdev, 
    eigenvalue = eigenanalysis, eigenvector = t(eigenvector)
  )
  
  # asign a class
  class(out) <- "AKU"
  return(out)
  
}


## --------------------------------------------------------------------------------------------------------
print.AKU <- function(obj) {
  cat("PRINCIPAL COMPONENT ANALYSIS\n")
  cat("Input: ", obj$n.row, "rows X", obj$n.col, "columns\n")
  cat("\nEigenanalysis of Correlation Matrix:\n")
  print(obj$eigenvalue)
  cat("\nEigenvectors:\n")
  print(obj$eigenvector)
  cat("\nScore of Principal Component (Preview):\n")
  print(rbind(head(obj$pc.score)))
  if(nrow(obj$pc.score) > 6) cat("...")
}




## --------------------------------------------------------------------------------------------------------
## BAGIAN 2. REGRESI KOMPONEN UTAMA
## --------------------------------------------------------------------------------------------------------

RKU <- function(y, X, k){
  
  if(is.null(colnames(X))) colnames(X) <- paste0("V", seq(ncol(X)))
  
  aku <- AKU(X)
  
  if(is.null(k)){
    k <- ncol(X)
  } else if(k < 1){
    k <- sum(aku$eigenvalue[3,] < k) + 1
  }
    
  pc.score <-  aku$pc.score[,1:k]
  pcr.data <- as.data.frame(cbind(y, pc.score))
  pc.name <- colnames(aku$pc.score)[1:k]
  colnames(pcr.data) <- c("y", pc.name)
  
  pcr.lm <- lm(y ~ ., data = pcr.data)
  pcr.summary <- summary.lm(pcr.lm)[c("coefficients", "sigma", 
                                      "r.squared", "adj.r.squared",
                                      "fstatistic", "df")]
  
  beta.pcr <- as.matrix(pcr.lm$coefficients) 
  
  beta.z.score1 <- (aku$eigenvector)[,1:k] %*% as.matrix(beta.pcr[-1])
  beta.z.score <- rbind(beta.pcr[1], beta.z.score1)
  beta.x <- rbind(beta.pcr[1] - (aku$x.bar/aku$x.stdev) %*% beta.z.score1,
                  beta.z.score1/aku$x.stdev)
  
  # asign name
  colnames(beta.pcr) <- colnames(beta.z.score) <- colnames(beta.x) <- "Estimates"
  rownames(beta.z.score) <- c("(Intercept)", paste0("Z_", colnames(X)))
  rownames(beta.x) <- c("(Intercept)", colnames(X))
  
  out <- list(pca = aku,
              k = k,
              pcr.lm = pcr.lm,
              pcr.summary = pcr.summary,
              pcr.coefficient = summary.lm(pcr.lm)$coefficients,
              beta.z.score = beta.z.score,
              beta.x = beta.x)
  out <- c(aku, out)
  class(out) <- "RKU"
  return(out)
} 


## --------------------------------------------------------------------------------------------------------
print.RKU <- function(obj, mode = "simple") {
  
  if(mode == "ext"){
    print(obj$pca)
    cat("\n\n")
  }
  
  cat("PRINCIPAL COMPONENT REGRESSION\n")
  cat("With", obj$k, "Principal Component(s)\n")
  
  cat("\nFormula: y ~ Principal Component Score\n\n")
  print(obj$pcr.coefficient)
  
  cat("\nR-squared: ", obj$pcr.summary$r.squared)
  cat(",  Adjusted R-squared: ", obj$pcr.summary$adj.r.squared,
      "\nF Statistics:", obj$pcr.summary$fstatistic[1])
  cat(",  DF1:", obj$pcr.summary$fstatistic[2])
  cat(",  DF2:", obj$pcr.summary$fstatistic[3])
  cat(",  p-value:", format.pval(pf(obj$pcr.summary$fstatistic[1], 
                                    obj$pcr.summary$fstatistic[2],
                                    obj$pcr.summary$fstatistic[3], 
                                    lower.tail = FALSE)))
  
  if(mode == "simple"){
    cat("\n\nNote: use print(obj, 'ext') to print extended output\n")}
  
  if(mode == "ext"){
    cat("\n\nInverse Transform into Z Score\n")
    print(obj$beta.z.score)
    cat("\nInverse Transform into X\n")
    print(obj$beta.x)
  }
  
}
