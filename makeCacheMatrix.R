makeCacheMatrix<- function(A = numeric()){
	if (class(A)!="matrix"){print("Mais A n'est pas une matrice") ; stop("A n'est pas une matrice") }
	if (dim(A)[1]!= dim(A)[2]){print("Mais la matrice n'est pas carrÃ©"); stop("A n'est pas une matrice carrÃ©")}
	if(abs(det(A))<1E-5){print("Mais la matrice n'est pas inversible"); stop("A n'est pas inversible")}
	m<- NULL
	get <- function() {A}
	setinv<-function(INV){ m <<- INV }
	getinv <- function() {m}
	list(get=get,setinv=setinv, getinv=getinv)
}
