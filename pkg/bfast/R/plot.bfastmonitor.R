plot.bfastmonitor <- function(x, main = TRUE, ylab = "Data", ...)
{
  if(isTRUE(main)) main <- if(is.na(x$breakpoint)) {
    "No break detected"
  } else {
    sprintf("Break detected at: %i(%i)", floor(x$breakpoint),
      round((x$breakpoint - floor(x$breakpoint)) * frequency(x$data)) + 1)
  }

  y <- if(is.null(dim(x$data))) x$data else x$data[,1L]
  plot(y, type = "n", main = main, ylab = ylab, ...)
  lines(window(y, end = x$history[2]), col = "black")
  lines(window(y, start = x$history[1], end = x$history[2]),
    col = "green", type = "p", pch = 19, cex = 1)
  lines(window(y, start = x$monitor[1]), col = "red")

  test_pred <- predict(x$model, newdata = x$tspp)
  test_pred <- as.ts(zoo(test_pred, x$tspp$time))
  lines(test_pred, col = "blue", lwd = 1.5)

  abline(v = x$monitor[1], lty = 2, col = "black", lwd = 1)
  abline(v = x$breakpoint, lty = 2, col = "red", lwd = 2)
    
  legend("bottomleft", bty = "n",
    c("History", "Stable history", "Monitoring", "Fit based on stable history", "Time of detected break"),
    lty = c(1, NA, 1, 1, 2),
    col = c("black", "green", "red", "blue", "red"),
    pch = c(NA, 19, NA, NA, NA)
  )
  invisible(x)
}
