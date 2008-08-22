\name{unpack}
\alias{unpack}
\title{ Unpack raw vectors }
\description{
  Break a raw vector into chunks according to the values in \code{template}.
}
\usage{
  unpack(template, ...)
}
\arguments{
  \item{template}{ A string, see 'Details' }
  \item{\dots}{ A raw vector to be unpacked }
}
\details{
  Currently supported \code{template} values are:\cr
  'a' - A null padded string\cr
  'A' - A space padded string\cr
  'C' - An unsigned char (octet) value\cr
  'v' - An unsigned short (16-bit) in "VAX" (little-endian) order\cr
  'V' - An unsigned long (32-bit) in "VAX" (little-endian) order\cr\cr
  Both 'a' and 'A' may be followed by a repeat value.  A repeat value of '*'
  will cause the remainder of the bytes in \code{values} to be placed in
  the last element.\cr\cr
  '/' allows packing and unpacking of a sequence of values where the packed
  structure contains a packed item count followed by the packed items themselves.\cr\cr
  If there are more \code{template} values or if the repeat count of a field
  or a group is larger than what the remainder \dots allows, \code{unpack}
  returns \code{NULL}.  If \dots is longer than what is described by
  \code{template}, the rest is ignored.
}
\value{
  A list with an element for each value in \code{template}.
}
\references{
  \url{http://perldoc.perl.org/functions/unpack.html}
}
\author{ Josh Ulrich }
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{pack}} }
\examples{
  (x <- pack('A4 C v A8 V', 'pack', 2, 8, 'sequence', 68098))
  (u1 <- unpack('A4 C a*', x))
  (u2 <- unpack('v/A V', u1[[3]]))
}
\keyword{ utilities }
