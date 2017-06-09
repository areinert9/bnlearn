# check a colour identifier (not necessarily a string/integer).
check.colour = function(col) {

  if (length(col) > 1)
    stop(sprintf("%s must be a single colour.", deparse(substitute(col))))
  if (identical(tryCatch(col2rgb(col), error = function(x) { FALSE }), FALSE))
    stop(sprintf("%s is not a valid colour identifier.",
           deparse(substitute(col))))

}#CHECK.COLOUR

# check the line type identifier.
check.lty = function(lty) {

  lty.strings = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash",
                  "twodash")

  if (length(lty) > 1)
    stop(sprintf("%s must be a single line type identifier.",
      deparse(substitute(lty))))
  if ((lty %!in% 0:6) && (lty %!in% lty.strings))
    stop(sprintf("%s is not a valid line type identifier.",
           deparse(substitute(lty))))

}#CHECK.LTY
