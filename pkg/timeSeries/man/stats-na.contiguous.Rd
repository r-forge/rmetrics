\name{na.contiguous}

\alias{na.contiguous,timeSeries-method}

\alias{is.na}
\alias{is.na,timeSeries-method}


\title{Find longest contiguous stretch of non-NAs or check for NAs}

\description{

  Find the longest consecutive stretch of non-missing values in a
  \code{"timeSeries"} object. In the event of a tie, the first such
  stretch. Also, \code{"timeSeries"} method for \code{is.na}.
      
}

\usage{
\S4method{na.contiguous}{timeSeries}(object, ...)
}

\arguments{
  \item{object}{
    a \code{"timeSeries"} object.
  }
  \item{\dots}{
    further arguments passed to other methods.
  }  
}

\value{

  for the \code{na.contiguous} method, a \code{"timeSeries"} object
  without missing values,

  for the \code{is.na} method, a \code{"timeSeries"} object whose data
  part is a logical matrix of the same dimension as in \code{x} indicating 
  if the corresponding values are \code{NA} or not.
  
}

\examples{
## Dummy 'timeSeries' with NAs entries
   data <- matrix(sample(c(1:20, rep(NA,4))), ncol = 2)
   s <- timeSeries(data, timeCalendar())
   is.na(s)  
## Find the longest consecutive non-missing values
   na.contiguous(s)
}


## stats:::na.contiguous.default
fu <- 
function (object, ...) 
{
    tm <- time(object)
    xfreq <- frequency(object)
    if (is.matrix(object)) 
        good <- apply(!is.na(object), 1L, all)
    else good <- !is.na(object)
    if (!sum(good)) 
        stop("all times contain an NA")
    tt <- cumsum(!good)
    ln <- sapply(0:max(tt), function(i) sum(tt == i))
    seg <- (seq_along(ln)[ln == max(ln)])[1L] - 1
    keep <- (tt == seg)
    st <- min(which(keep))
browser()
    if (!good[st]) 
        st <- st + 1
    en <- max(which(keep))
    omit <- integer()
    n <- NROW(object)
    if (st > 1) 
        omit <- c(omit, 1L:(st - 1))
    if (en < n) 
        omit <- c(omit, (en + 1):n)
    cl <- class(object)
    if (length(omit)) {
        object <- if (is.matrix(object)) 
            object[st:en, ]
        else object[st:en]
        attr(omit, "class") <- "omit"
        attr(object, "na.action") <- omit
        tsp(object) <- c(tm[st], tm[en], xfreq)
        if (!is.null(cl)) 
            class(object) <- cl
    }
    object
}
