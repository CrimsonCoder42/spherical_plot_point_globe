#jaxmat.R
#Utility functions
#Creates a column vector in TeX
tex.column <- function(v, as.point = FALSE){
  M <- length(v)
  vec <- paste(v,sep = "\\\\",collapse= "\\\\")
  if (as.point)
    return(paste("\\begin{pmatrix}",vec,"\\end{pmatrix}"))
  return(paste("\\begin{bmatrix}",vec,"\\end{bmatrix}"))
}
tex.column(c(0,2,3))

#Creates a row of a matrix in TeX
tex.row <- function(v){
  vec <- paste(v,sep = "&",collapse= "&")
}

#Converts a matrix to TeX
tex.matrix <- function(m){
  M <- nrow(m)
  str <- "\\begin{bmatrix}"
  str <- paste(str,tex.row(m[1,]),sep = "",collapse= "")
  if (M == 1)
    return(paste(str,"\\end{bmatrix}"))
  for (i in 2:M) {
    str <- paste(str,tex.row(m[i,]),sep = "\\\\",collapse= "\\\\")
  }
  return(paste(str,"\\end{bmatrix}"))
}
tex.matrix(rbind(c(1,2),c(3,4)))

#These functions can be called from R Shiny.
#If display is TRUE, the output is centered on a separate line.
#If display is FALSE, the output is inline.

#Display the columns of matrix m as a sequence of column vectors
jax.vecList <- function(m, sep = ",",display = TRUE){
  N <- ncol(m)
  str <- ifelse(display,"\\[","\\(")
  for (i in 1:N){
    str <- paste0(str,tex.column(m[,i]),sep="")
    if (i < N)
      str <- paste0(str,sep)
  }
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#m <- rbind(c(0,1,2,-2,-1),c(0,0,0,0,0))
#jax.vecList(m)

#Display a matrix acting on vector v1 to produce vector v2
jax.mTimesV <- function(mName, v1, v2 ,display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  str <- paste0(str,mName,sep="")
  str <- paste0(str,tex.column(v1),sep="=")
  str <- paste0(str,tex.column(v2),sep="")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#Display matrix m1 times vector m2 to produce vector v2
jax.mTimesM <- function(mName, m1, m2 ,display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  str <- paste0(str,mName,sep="=")
  str <- paste0(str,tex.matrix(m1),sep="")
  str <- paste0(str,tex.matrix(m2),sep="=")
  str <- paste0(str,tex.matrix(m1%*%m2),sep="")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#jax.mTimesV("A", c(2,3), c(4,4))

#Display a matrix, preceded by an optional name
jax.matrix <- function(m, name =character(0), display = TRUE){
  str <- ifelse(display,"\\[","\\(")
  if (length(name) > 0){
    str <- paste0(str,name,"=")
  }
  str <- paste0(str,tex.matrix(m),collapse = "")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#Display a vector, preceded by an optional name
jax.vector <- function(v, name =character(0), display = TRUE, as.point = FALSE){
  str <- ifelse(display,"\\[","\\(")
  if (length(name) > 0){
    str <- paste0(str,ifelse(as.point,"\\mathbf{","\\vec{\\mathbf{"),name)
    str <- paste0(str,ifelse(as.point,"}","}}"),"=")
  }
  str <- paste0(str,tex.column(v, as.point),collapse = "")
  withMathJax(paste0(str,ifelse(display,"\\]","\\)"),collapse=""))
}

#Display a letter in bold with an arrow over it
jax.vecname <- function(name){
  withMathJax(paste0("\\(\\vec{\\mathbf{",name,"}}\\)"),collapse="")
}

#Inline -- use ` in place of \\
jaxI <- function(s){
  s <- gsub("`","\\\\",s)
  withMathJax(paste0("\\(",s,"\\)"))
}

#Display -- use ` in place of \\
jaxD <- function(s){
  s <- gsub("`","\\\\",s)
  withMathJax(paste0("\\[",s,"\\]"))
}

gsub("`","\\\\","`l")


