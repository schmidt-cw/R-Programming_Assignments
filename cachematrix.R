######################################
#
#   R Programming Assignment #2
#
#   Author: Chris Schmidt
#   Date: 1/18/2016
#
#   Purpose: The assignment is to write a pair of functions that cache the
#   inverse of a matrix.
#
#   Functions:
#
#   [makeCacheMatrix]: Function creates a special "matrix" object that performs
#   various calls to cached data or sets cache for inverse matrix.
#
#   [cacheResolve]: This function looks for cached inverse matrix, if not found
#   computes the inverse of input matrix from data retrieved from
#   makeCacheMatrix.If the inverse matrix has already been
#   calculated (and the matrix has not changed), then cacheSolve should retrieve
#   the inverse from the cache.
#
######################################

makeCacheMatrix <- function(z = matrix()) {

      # initializes/clears cache
      m_cache <- NULL

      # function to set matrix for local environment
      set <- function(y=matrix()) {
            z <<- y
            m_cache <<- NULL
      }

      # function to retrieve input matrix
      get <- function() z

      # function to load inverse soln to cache (m_cache)
      set_matrix <- function(inverse) m_cache <<- inverse

      # function to retrieve data in cache (m_cache)
      get_invmatrix <- function() m_cache

      # creates list for function calls (NOTE: 'set' is not explicitly called)
      list(set = set, get = get, set_matrix = set_matrix, get_invmatrix = get_invmatrix)
}

cacheSolve <- function(x, ...) {

      # looks in cache and calls to retrieve data
      cache <- x$get_invmatrix()

      # evaluates cache retrieval (m_cache) to see if cached, if so returns soln
      if(!is.null(cache)) {
            message("retrieved cached data...")
            return(cache)
      }

      # if soln is not cached (m_cache), then retrieves input matrix
      data_matrix <- x$get()

      # perform calculation to find inverse matrix using R function solve()
      soln <- solve(data_matrix)

      # places inverse matrix soln into cache (m_cache) for subsequent retrieval
      x$set_matrix(soln)

      # returns the inverse matrix soln
      return(soln)
}
