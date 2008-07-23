\name{numToRaw}
\alias{numToRaw}
\title{ Numeric to Raw vector }
\description{
  Convert numeric values to a raw vector.
}
\usage{
  numToRaw(x, nBytes = 1)
}
\arguments{
  \item{x}{ A number to be converted }
  \item{nBytes}{ The number of bytes to use }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A raw vector containing the bytes representing \code{x}.
}
%\references{
% \url{http://perldoc.perl.org/functions/pack.html}\cr
%}
\author{ Josh Ulrich }
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{rawToNum}} }
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}
\keyword{ utilities }
