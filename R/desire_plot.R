desire_plot <- function(x, desire_func, desire_args, ...){

  # check function selected correctly (one must be specified)
  if (!desire_func %in% c("desire_low","desire_high","desire_extremes",
    "desire_middle")) {
    stop("\ndesire_func must be one of 'desire_low', 'desire_high',
      'desire_extremes', or 'desire_middle'\n")
  }

  # make values for plotting
  vals <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=300)

  # add lines to existing graph
  graphics::par(new=TRUE)

  if (desire_func=="desire_low") {
    graphics:: plot(do.call(desire_low, c(list(x=vals), des.args) ) ~ vals,
      ylim=c(0,1), type="l",  yaxt="n", bty="n", xaxt="n",
      ylab="", xlab="", ...)
  }


  if (desire_func=="desire_high") {
    graphics::plot(do.call(desire_high, c(list(x=vals), des.args) ) ~ vals,
      ylim=c(0,1), type="l",  yaxt="n", bty="n", xaxt="n",
      ylab="", xlab="", ...)
  }


  if (desire_func=="desire_extremes") {
    graphics::plot(do.call(desire_extremes, c(list(x=vals), des.args) ) ~ vals,
      ylim=c(0,1), type="l",  yaxt="n", bty="n", xaxt="n",
      ylab="", xlab="", ...)
  }


  if (desire_func=="desire_middle") {
    graphics::plot(do.call(desire_middle, c(list(x=vals), des.args) ) ~ vals,
      ylim=c(0,1), type="l",  yaxt="n", bty="n", xaxt="n",
      ylab="", xlab="", ...)
  }

  }
