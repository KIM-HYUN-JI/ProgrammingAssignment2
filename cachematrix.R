#R은 메모리를 사용하기 때문에 큰 데이터에는 종종 캐시를 활용해야함
# <<-연산자는 변수를 global variable로 만들어 준다.   
## inverse matrix를 cache 하는 함수 만들기
## cache : 데이터나 값을 미리 복사해놓는 임시 저장소

#makeCacheMatrix라는 이름의 함수 만들기
makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL                             
  set <- function(y) {                    
    x <<- y                             
    inv <<- NULL                    #<<-연산자로 x와 inv값을 scope 외부에서도 사용할 수 있도록 함
  }
  get <- function() x                    
  setinverse <- function(inverse) inv <<- inverse    
  getinverse <- function() inv                     
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)  
}



#위에서 만든 makeCacheMatrix로 만들어진 matrix를 캐시에서 읽어서 역행렬을 만드는 함수
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  #역행렬을 구하는 함수 solve()
  x$setinverse(inv)
  inv
}
