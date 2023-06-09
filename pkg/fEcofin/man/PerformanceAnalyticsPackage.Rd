\name{PerformanceAnalyticsData}


\alias{PerformanceAnalyticsData}


\alias{edhec.tS}
\alias{managers.tS}


\title{Performance Analytics Data Sets} 


\description{

    A collection and description of data sets 
    used in the examples of the contributed
    R package "PerformanceAnalytics".
    
    The data sets are:
    
    \tabular{ll}{
    \code{edhec.tS} \tab composite hedge fund style index returns, \cr
    \code{managers.tS} \tab fixed income benchmarks. }
   
}


\details{


    \bold{EDHEC composite hedge fund style index returns:}
    
    \dQuote{The EDHEC Risk and Asset Management Research Centre plays a 
    noted role in furthering applied financial research and systematically 
    highlighting its practical uses. As part of its philosophy, the centre 
    maintains a dialogue with professionals which benefits the industry 
    as a whole. At the same time, its proprietary R&D provides sponsors 
    with an edge over competition and joint ventures allow selected 
    partners to develop new business opportunities.

    To further assist financial institutions and investors implement the 
    latest research advances in order to meet the challenges of the 
    changing asset management landscape, the centre has spawned two 
    consultancies and an executive education arm. Clients of these 
    derivative activities include many of the leading organisations 
    throughout Europe}. [Source: EDHEC website]

    see \url{https://risk.edhec.edu/who-we-are}

    Data used in PerformanceAnalytics and related publications with the 
    kind permission of the EDHEC Risk and Asset Management Research Center.
    \cr

    \bold{Hypothetical Alternative Asset Manager Data and Fixed Income Benchmarks:}

    A data frame that contains columns of monthly returns for six hypothetical
    asset managers (HAM1 through HAM6), the EDHEC Long-Short Equity hedge
    fund index, the S\&P 500 total returns, and total return series for
    the US Treasury 10-year bond and 3-month bill. Monthly returns for
    all series end in December 2006 and begin at different periods starting
    from January 1996.
    
}


\format{
    
    All files are in CSV Excel spreadsheet format. The
    delimiter is a semicolon. The time stamps are ISO-8601 formatted.
    
}


\references{

Berndt E.R. (1991);
    \emph{The Practice of Econometrics: Classic and Contemporary},
    Addison-Wesley Publishing Co. 
  
EDHEC (2003);
    \emph{About EDHEC Alternative Indexes},
    EDHEC-Risk,\cr
    \url{https://risk.edhec.edu/about-edhec-risk-alternative-indexes}.

Vaissie Mathieu (2003);
    \emph{A Detailed Analysis of the Construction Methods and Management 
        Principles of Hedge Fund Indices},\cr
    \url{https://risk.edhec.edu/publications/detailed-analysis-construction-methods-and}.

Carl P., Peterson B.G. (2007);
   \emph{PerformanceAnalytics: Econometric Tools for Performance and 
    Risk Analysis}, \cr
    \url{https://CRAN.R-project.org/package=PerformanceAnalytics}.
}


\examples{
## Load Example Data Set:
   data(edhec.tS)
   edhec.tS
}
 
 
\keyword{datasets}

