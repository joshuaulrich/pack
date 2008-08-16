#-------------------------------------------------------------------------#
# pack R package, copyright (C) Joshua M. Ulrich, 2007-2008               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

'unpack' <-
function(template, values) {

  # http://perldoc.perl.org/functions/unpack.html
  
  template <- unlist(strsplit(template,"\\s"))

  types <- gsub('[0-9]|\\*','',template)
  bytes <- gsub('[a-Z]|/','',template)
  bytes <- gsub('\\*','-1',bytes)
  bytes <- as.numeric(bytes)
  result <- NULL
  
  # Loop over template / value pairs
  for( i in 1:length(template) ) {
    type <- types[i]
    byte <- bytes[i]

    # A null byte
    if( type == 'x' ) {
      values <- values[-1]
      next
    }

    # A null padded string
    if( type == 'a' ) {
      # In the case of 'a*'
      if( byte == -1 ) {
        val <- values
      } else {
        if( byte > length(values) )
          stop('template too long for values')
        val <- values[1:byte]
        values <- values[-(1:byte)]
      }
    } else
    # A space padded ASCII string
    if( type == 'A' ) {
      if( byte > length(values) )
        stop('template too long for values')
      val <- values[1:byte]
      val[val==0] <- charToRaw(' ')
      val <- rawToChar( val )
      values <- values[-(1:byte)]
    } else
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
      bits <- as.integer(rawToBits(values[1:8]))
      val <- (-1)^bits[64] * 2^(sum( 2^(10:0) * bits[63:53] )-1023) * (1 + sum( 2^-(1:52) * bits[52:1] ))
      values <- values[-(1:8)]
    } else
    # A single-precision float in the native format.
    if( type == 'f' ) {
      bits <- as.integer(rawToBits(values[1:4]))
      val <- (-1)^bits[32] * 2^(sum( 2^( 7:0) * bits[31:24] )- 127) * (1 + sum( 2^-(1:23) * bits[1:23] ))
      values <- values[-(1:4)]
    } else
    # Packed item count followed by packed items
    if( regexpr('/',type) ) {
      seq <- unlist(strsplit(type,'/'))
      num <- unpack(paste(seq[1],'a*'), values)
      val <- unpack(paste(seq[2],num[[1]],' a*',sep=''),num[[2]])
      values <- val[[2]]
      val <- unlist(val[[1]])
    }
    
    # Combine result
    result <- c(result,list(val))
  }
  return(result)
}
