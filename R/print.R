# print methods for the energyRt classes ##############################


#' Print methods for the energyRt classes
#'
#' @inheritParams print
#'
#' @include class-model.R class-modInp.R
#' @export
#'
setGeneric("print", function(x, ...) UseMethod("print"))

# Print modInp ####
#' @export
setMethod("print", "modInp", function(x, ...) {
  if (length(x@parameters) == 0) {
    cat("There is no data\n")
  } else {
    for (i in 1:length(x@parameters)) {
      print(x@parameters[[i]])
    }
  }
})

# Print commodity ####
#' @exportS3Method print commodity
print.commodity <- function(x, ...) {
  # print commodity
  if_print_data_frame <- function(x, sl) {
    df <- slot(x, sl)
    if (nrow(df) != 0) {
      cat("\n", sl, "\n")
      # all-NA columns are noise: the property table is deliberately sparse
      # (uncertainty is optional), so drop what carries nothing.
      keep <- vapply(df, \(z) !all(is.na(z)), logical(1))
      print(df[, keep, drop = FALSE])
      cat("\n")
    }
  }
  cat("Name: ", x@name, "\n")
  # if (x@type != '') cat('type: ', x@type, '\n')
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  # if (x@origin != '') cat('Region of origin: ',x@origin, '\n')
  if (length(x@unit) != 0 && nzchar(x@unit[1])) cat("unit: ", x@unit, "\n")
  if (!is.null(x@misc$color)) cat("color: ", x@misc$color, "\n")
  img <- object_image(x)
  if (!identical(img$status, "none")) {
    cat("image: ", img$path, " (", img$status, ")\n", sep = "")
  }

  g <- getClass("commodity")
  yy <- names(g@slots)[sapply(names(g@slots), function(y) {
    g@slots[[y]] ==
      "data.frame"
  })]
  for (i in yy) if_print_data_frame(x, i)
  invisible(x)
}

print.demand <- function(x) {
  # print demand
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  cat("Name: ", x@name, "\n")
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  cat("Commodity: ", x@commodity, "\n")
  g <- getClass("demand")
  yy <- names(g@slots)[sapply(names(g@slots), function(y) {
    g@slots[[y]] ==
      "data.frame"
  })]
  for (i in yy) if_print_data_frame(x, i)
}


#------------------------------------------------------------------------------
#' @exportS3Method print constraint
print.constraint <- function(x) {
  # print constraint
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  cat("Name: ", x@name, ", eq: ", as.character(x@eq), ", defVal: ", x@defVal, "\n", sep = "")
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  g <- getClass("constraint")
  yy <- names(g@slots)[sapply(names(g@slots), function(y) {
    g@slots[[y]] ==
      "data.frame"
  })]
  for (i in yy) if_print_data_frame(x, i)
  for (i in seq_along(x@lhs)) {
    cat("Term ", i, ":\n", sep = "")
    print(x@lhs[[i]])
  }
}

#------------------------------------------------------------------------------
#' @exportS3Method print summand
print.summand <- function(x) {
  # print summand
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  cat("variable: ", x@variable, ", defVal: ", x@defVal, "\n", sep = "")
  g <- getClass("summand")
  yy <- names(g@slots)[sapply(names(g@slots), function(y) {
    g@slots[[y]] ==
      "data.frame"
  })]
  for (i in yy) if_print_data_frame(x, i)
  if (length(x@for.sum) != 0) {
    cat("for.sum set:\n")
    print(x@for.sum)
  }
}

#------------------------------------------------------------------------------
print.model <- function(x, ...) {
  # print model
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  cat("Name: ", x@name, "\n")
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  # print(x@config)
  # if (length(x@data) != 0) {
  #   for (i in 1:length(x@data)) {
  #     cat("Repository ", i, "(", class(x@data[[i]]), "):\n", sep = "")
  #     print(x@data[[i]])
  #   }
  # }
}

#' @export
setMethod("print", "model", function(x, ...) {
  print.model(x, ...)
})

#' @export
setMethod("show", "model", function(object) {
  print(object)
})

#------------------------------------------------------------------------------
print.region <- function(x) {
  cat("Name: ", x@name, "\n")
  # if (x@type != '') cat('type: ', x@type, '\n')
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  if (!is.null(x@misc$color)) cat("color: ", x@misc$color, "\n")
}

print.scenario <- function(x) {
  cat("Name: ", x@name, "\n")
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  if (!is.null(x@model)) {
    cat("Model:\n")
    print(x@model)
  }
}

#------------------------------------------------------------------------------
print.supply <- function(x) {
  # print supply
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  cat("Name: ", x@name, "\n")
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  cat("Commodity: ", x@commodity, "\n")
  # cat('Reserve: ', x@reserve, '\n')
  if (!is.null(x@region)) cat('region: "', paste(x@region, collapse = '", "'), '"\n', sep = "")
  g <- getClass("supply")
  yy <- names(g@slots)[sapply(names(g@slots), function(y) {
    g@slots[[y]] ==
      "data.frame"
  })]
  for (i in yy) if_print_data_frame(x, i)
}

#------------------------------------------------------------------------------
print.technology <- function(x, ...) {
  # print technology
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  cat("Name: ", x@name, "\n")
  # if (x@type!='') cat('type: ', x@type, '\n')
  if (length(x@desc) != 0 && x@desc != "") cat("desc: ", x@desc, "\n")
  if (x@cap2act != "") cat("cap2act: ", x@cap2act, "\n")
  if (!is.null(x@region)) cat('region: "', paste(x@region, collapse = '", "'), '"\n', sep = "")
  # if(!is.null(x@reporting_years)) cat('reporting period: ', x@reporting_years, '\n')
  for (i in .technology_data_frame()) if_print_data_frame(x, i)
}

#------------------------------------------------------------------------------
print.config <- function(x) {
  # print model
  if_print_data_frame <- function(x, sl) {
    if (nrow(slot(x, sl)) != 0) {
      cat("\n", sl, "\n")
      print(slot(x, sl))
      cat("\n")
    }
  }
  if_print_data_frame(x, "debug")
  if_print_data_frame(x, "discount")
  # if_print_data_frame(x, 'tax')
  if_print_data_frame(x, "defVal")
  if_print_data_frame(x, "interpolation")
}
