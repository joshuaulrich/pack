\name{rawToNum}
\alias{rawToNum}
\title{ Raw to Numeric vector }
\description{
  Convert raw values to numeric.
}
\usage{
  rawToNum(x, nBytes = 1)
}
\arguments{
  \item{x}{ A raw vector to be converted }
  \item{nBytes}{ The number of bytes to use }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A numeric vlaue containing the bytes in \code{x}.
}
%\references{
%  \url{http://perldoc.perl.org/functions/pack.html}\cr
%}
\author{ Josh Ulrich }
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{numToRaw}} }
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
\keyword{ utilities }
