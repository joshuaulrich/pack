\name{pack}
\alias{pack}
\title{ Pack raw vectors }
\description{
  Combine values into a raw vector according to the values in \code{template}.
}
\usage{
  pack(template, \dots)
}
\arguments{
  \item{template}{ A string, see \'Details\' }
  \item{\dots}{ Values to be packed into a raw vector }
}
\details{
  \code{template} should follow the convention below:
  'a' - A null padded string\cr
  'A' - A space padded ASCII string\cr
  'C' - An unsigned char (octet) value\cr
  'v' - An unsigned short (16-bit) in "VAX" (little-endian) order\cr
  'V' - An unsigned long (32-bit) in "VAX" (little-endian) order\cr\cr
  Both 'a' and 'A' may be followed by a repeat value.  A repeat value of '*'
  will cause the remainder of the bytes in \code{values} to be placed in
  the last element.\cr
  '/' allows packing and unpacking of a sequence of values where the packed
  structure contains a packed item count followed by the packed items themselves.
}
\value{
  A raw vector following the elements in \code{template}
}
\references{
  \url{http://perldoc.perl.org/functions/pack.html}
}
\author{ Josh Ulrich }
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{unpack}} }
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%}
\keyword{ utilities }
