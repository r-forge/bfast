converttodailyzooreg <- function(y, dates)
{
	yday365 <- function(x) {
		x <- as.POSIXlt(x)
		mdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
		cumsum(c(0L, mdays))[1L + x$mon] + x$mday
	}
	
	yz <- zoo(y,
							1900 + as.POSIXlt(dates)$year + (yday365(dates) - 1)/365,
							freq = 365)
  return(yz)
}
