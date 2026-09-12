\name{Ids}
\alias{Ids}

\alias{colIds}
\alias{rowIds}
\alias{colIds<-}
\alias{rowIds<-}


\title{Set and retrieve column/row names}

\description{
    
  Sets and retrieves column and row names. The functions
  are for compatibility with SPlus. 
  
}

\usage{
colIds(x, \dots)
rowIds(x, \dots)

colIds(x) <- value
rowIds(x) <- value
}

\arguments{
  \item{x}{
    a numeric matrix.
  }
  \item{\dots}{
    passed on to \code{colnames} or \code{rownames}.
  }
  \item{value}{
    a character vector or \code{NULL}.
  }
}

\details{

  \code{colIds} and \code{rowIds} call \code{as.matrix(x)} and return,
  respectively the column names and row names of the converted object.
    
  The replacement versions set the column names and row names,
  respectively.

  Usually in R the functions \code{colnames} and \code{rownames} are 
  used to retrieve and set the names of matrices. The functions 
  \code{rowIds} and \code{colIds}, are S-Plus like synonyms.

}

\examples{
## Create Pascal Matrix:
P <- pascal(3)
P
 
rownames(P) <- letters[1:3]
P   
   
colIds(P) <- as.character(1:3)
P                            
}

\keyword{math}
