#-------------------------------------------------------------------------#
# pack R package, copyright (C) Joshua M. Ulrich, 2007-2008               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

'pack' <-
function(template, ...) {

  # http://perldoc.perl.org/functions/pack.html
  
  template <- unlist(strsplit(template,"\\s"))
  values <- list(...)

  types <- gsub('[0-9]|\\*','',template)
  bytes <- gsub('[a-Z]|/','',template)
  bytes <- gsub('\\*','-1',bytes)
  bytes <- as.numeric(bytes)
  result <- NULL

  # Loop over template / value pairs
  shift <- 0
  for( i in 1:length(template) ) {
    type <- types[i]
    byte <- bytes[i]

    # A null byte
    if( type == 'x' ) {
      val <- as.raw(0)
      nul <- raw(0)
      shift <- shift + 1
    } else {
      value <- values[[i-shift]]
    }

    # A null padded string
    if( type == 'a' ) {
      # In the case of 'a*'
      if( byte == -1 ) {
        val <- charToRaw( value )
        nul <- raw(0)
      } else {
        if( nchar(value) > byte )
          stop(paste('list value (',value,') too large for template value',sep=''))
        val <- charToRaw( value )
        nul <- rep( as.raw(0), byte-nchar(value) )
      }
    } else
    # A space padded ASCII string
    if( type == 'A' ) {
      if( nchar(value) > byte )
        stop(paste('list value (',value,') too large for template value',sep=''))
      val <- charToRaw( value )
      nul <- rep( charToRaw(' '), byte-nchar(value) )
    } else
    # An unsigned char (octet) value.
    if( type == 'C' ) {
      val <- numToRaw( value, 1 )
      nul <- raw(0)
    } else
    # An unsigned short (16-bit) in "VAX" (little-endian) order.
    if( type == 'v' ) {
      val <- numToRaw( value, 2 )
      nul <- raw(0)
    } else
    # An unsigned long (32-bit) in "VAX" (little-endian) order.
    if( type == 'V' ) {
      val <- numToRaw( value, 4 )
      nul <- raw(0)
    } else
    # Packed item count followed by packed items
    if( regexpr('/',type) ) {
      seq <- unlist(strsplit(type,'/'))
      len <- nchar(value)
      num <- pack(seq[1], len)
      val <- pack(paste(seq[2],len,sep=''),value)
      val <- c(num,val)
      nul <- raw(0)
    }

    # Combine result
    result <- c(result,val,nul)
  }
  return(result)
}
