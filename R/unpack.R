#-------------------------------------------------------------------------#
# pack R package, copyright (C) Joshua M. Ulrich, 2007-2008               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

'unpack' <-
function(template, ...) {

  # http://perldoc.perl.org/functions/unpack.html

  template <- unlist(strsplit(template,"\\s"))
  values <- unlist(list(...))

  types <- gsub('[0-9]|\\*','',template)
  bytes <- gsub('[[:alpha:]]|/','',template)
  bytes <- gsub('\\*','-1',bytes)
  suppressWarnings(bytes <- as.numeric(bytes))
  result <- list()
  
  # Loop over template / value pairs
  for( i in 1:length(template) ) {
    
    type <- types[i]
    byte <- bytes[i]

    if( is.na(byte) ) # Single letter without count
      byte <- 1

    # A null byte
    if( type == 'x' ) {
      values <- values[-1]
      next
    }

    # Check template and values length
    if( length(values) == 0 ) {
      val <- NULL
      #stop('template values too long for binary data')
    } else
    # (decimal 240 would be hex F0.)
    if( type == 'H' ) {
      if( byte == -1 )  # 'H*'
        byte <- length(values)
      if( byte > length(values) )
        stop('template too long for values')
      val <- values[1:byte]
      values <- values[-(1:byte)]
    } else
    # A null padded string
    if( type == 'a' ) {
      if( byte == -1) # 'a*'
        byte <- length(values)
      if( byte > length(values) )
        stop('template too long for values')

      val <- values[1:byte]
      # strings can no longer have embedded nuls as of R-2.8.0
      val <- rawToChar( val[as.logical(val)] )
      values <- values[-(1:byte)]
    } else
    # A space padded ASCII string
    if( type == 'A' ) {
      if( byte == -1) # 'A*'
        byte <- length(values)
      if( byte > length(values) )
        stop('template too long for values')
      val <- values[1:byte]
      # strings can no longer have embedded nuls as of R-2.8.0
      val <- rawToChar( val[as.logical(val)] )
      val <- sub(" +$", "", val)
      values <- values[-(1:byte)]
    } else
    # Bit string, low-to-high order
    if( type == 'b' ) {
      val <- rawToBits( values[1] )
      values <- values[-1]
    } else
    # Bit string, high-to-low order
    if( type == 'B' ) {
      val <- rev( rawToBits( values[1] ) )
      values <- values[-1]
    } else
    # Hex string - high nibble first
    # An unsigned char (octet) value.
    if( type == 'C' ) {
      val <- as.integer( values[1] )
      values <- values[-1]
    } else
    # An unsigned short (16-bit) in "VAX" (little-endian) order.
    if( type == 'v' ) {
      val <- rawToNum( values[1:2], 2 )
      values <- values[-(1:2)]
    } else
    # An unsigned long (32-bit) in "VAX" (little-endian) order.
    if( type == 'V' ) {
      val <- rawToNum( values[1:4], 4 )
      values <- values[-(1:4)]
    } else
    # A double-precision float in the native format.
    if( type == 'd' ) {
      val <- readBin(values[1:8], "numeric", 1L, 8L)
      values <- values[-(1:8)]
    } else
    # A single-precision float in the native format.
    if( type == 'f' ) {
      val <- readBin(values[1:4], "numeric", 1L, 4L)
      values <- values[-(1:4)]
    } else
    # Packed item count followed by packed items
    if( regexpr('/',type)>0 ) {
      seq <- unlist(strsplit(type,'/'))
      num <- unpack(paste(seq[1],'H*'), values)
      val <- unpack(paste(seq[2],num[[1]],' H*',sep=''),num[[2]])
      values <- val[[2]]
      val <- unlist(val[[1]])
    } else
    if( type != 'x' ) {
      stop('\'',type,'\' is an unsupported template value')
    }

    # Combine result
    result <- c(result,list(val))
  }
  return(result)
}
