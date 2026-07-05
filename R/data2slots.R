#' An internal function to add data to slots of a new S4-class object or update a given one.
#'
#' @description
#' This function creates and adds data/parameters to `energyRt` classes. It is recommended to to use wrapper functions such as `newTechnology`, `newCommodity`, etc. with guided names of each class slots.
#'
#' @param class_name character, name of the class to create.
#' @param x character or object. Character string with a name to add to the `@name` slot of the object to create, or the object of `class_name` to update.
#' @param ... slot names with the data to add to the object's slots.
#' @param ignore_args character vector of arguments to drop from `...` if exist.
#' @param ignore_classes character vector of classes to drop from `...`.
#' @param update logical, if an object of class `class_name` is supplied (in `x`) then `update` must be `TRUE`, and the object will be updated with the given data in `...`. Otherwise, if the `x` is character, the `update` should be `FALSE`, and the new object will be created.
#'
#' @return created or updated object with the added/updated data in the object slots.
#' @noRd
.data2slots <- function(
    class_name = NULL,
    x,
    ...,
    ignore_args = NULL,
    ignore_classes = NULL,
    update = !is.character(x),
    warn_nodata = TRUE) {
  # alternative names: data2class, add2slots, fit2slots, ...
  # browser()
  if (update) {
    if (!grepl("energyRt", attr(class(x), "package"))) {
      stop("Unknown type of the object")
    }
    if (!is.null(class_name) && (class(x) != class_name)) {
      stop(
        "In the case of update = TRUE, 'x' should be an object of class ",
        class_name
      )
    }
    obj <- x
    class_name = class(obj)
    if ("name" %in% slotNames(class_name)) {
      x <- obj@name
    } else {
      x <- ""
    }
  } else {
    stopifnot(is.character(class_name))
    obj <- new(class_name)
  }
  slots <- getSlots(class_name)
  arg <- list(...)
  if (!is.null(ignore_args)) arg <- arg[!(names(arg) %in% ignore_args)]
  if (!is.null(ignore_classes)) arg <- arg[!(sapply(arg, class) %in%
                                               ignore_classes)]
  if ("name" %in% slotNames(class_name)) {obj@name <- x}
  if (length(arg) != 0) {
    # if (any(names(arg) == "name")) stop('Duplicated parameter "name"')
    if (is.null(names(arg)) || any(names(arg) == "")) stop("Unnamed parameters")
    if (anyDuplicated(names(arg)) != 0) {
      stop("Duplicated parameters ", names(arg)[anyDuplicated(names(arg))])
    }
    # if (any(sapply(arg, is.null))) stop('There is NULL argument')
    arg <- arg[!sapply(arg, is.null)]
    if (any(!(names(arg) %in% names(slots)))) {
      # check consistency of the data in `...` with the object slots
      if (warn_nodata) {
        warning('Unidentified slots: "',
                paste(names(arg)[!(names(arg) %in% names(slots))],
                      collapse = '", "'),
                '", in class = ', class_name, ", object x = ", x)
      }
      return(obj)
    }
    # Add data from `...`, s - slot, dat - data in the argument with s-name
    for (s in names(arg)) {
      dat <- arg[[s]]
      if (slots[s] == "list") {
        # slots in "list" format
        slot(obj, s) <- dat
      } else if (slots[s] == "data.frame") {
        # slots in data.frame format
        if (is.data.frame(dat)) {
          # data in the same (data.frame) format
          if (any(!(colnames(dat) %in% colnames(slot(obj, s))))) {
            # !!! ToDo: take columns from "new()" or from the class
            # Check column names
            stop(paste(
              'Unknown column "',
              paste(colnames(dat)[!(colnames(dat) %in% colnames(slot(obj, s)))],
              '"in the slot: "',
              s,
              collapse = '", "'),
              '"\n',
              sep = ""
            ))
          }
          slot(obj, s) <- slot(obj, s)[0, , drop = FALSE] # initiate data.frame
          if (nrow(dat) != 0) {
            nn <- 1:nrow(dat)
            slot(obj, s)[nn, ] <- NA
            for (i in names(dat)) {
              # fill-in the data by columns !!! Check ANNUAL/HOUR
              # browser()
              if (is.factor(slot(obj, s)[, i, drop = FALSE]) ||
                  is.factor(dat[[i]])) {
                # coerce factors to characters
                slot(obj, s)[[i]][nn] <- as.character(dat[[i]])
              } else {
                # add check of type of columns
                slot_column_class <- class(slot(obj, s)[[i]][nn])
                data_column_class <- class(dat[[i]])
                if (!any(data_column_class %in% slot_column_class)) {
                  # allow bounds-class (inheritance) format
                  if (is.numeric(slot(obj, s)[[i]][nn]) & is.numeric(dat[[i]])) {
                    # coerce-able between numeric classes, not an error
                    dat[[i]] <- as(dat[[i]], slot_column_class)
                    data_column_class <- class(dat[[i]])
                  } else {
                    # error
                    stop(
                      "Unexpected data format (", data_column_class,
                      ") in ", x, "@", s, ", column ", i,
                      ", expecting ", slot_column_class
                    )
                  }
                }
                if (!all(data_column_class %in% slot_column_class)) {
                  # not exact match, issue a warning and try to coerce
                  warning(
                    "Object '", x, "', slot '", s, "', column '", i,
                    "':\n Not exact match of the given data (",
                    data_column_class, ") and the expected format (",
                    slot_column_class, ", coercing."
                  )
                  dat[[i]] <- as(dat[[i]], slot_column_class) # try to coerce
                }
                slot(obj, s)[[i]][nn] <- dat[[i]]
              }
            }
          }
        } else if (is.list(dat)) {
          # data in list format for a data.frame slot
          # in this case length of vectors should be equal
          hh <- sapply(dat, length)
          slot(obj, s) <- slot(obj, s)[0, , drop = FALSE] # initiate/reset
          # Check: Equal length
          if (any(hh != hh[1])) {
            stop("Different length of vectors in the list ", s, ", object: ", x)
          }
          # Check: All vectors have expected, unique names
          if (is.null(names(dat)) || any(names(dat) == "")) {
            stop("Unnamed elements in the list object ", x, "@", s)
          }
          if (anyDuplicated(names(dat)) != 0) {
            stop("Duplicated names/parameters in the list ", x, "@", s)
          }
          # Check for unknown columns
          ii <- names(dat) %in% colnames(slot(obj, s))
          if (any(!ii)) {
            stop(
              "Unrecognized parameter(s) ",
              paste(names(dat[!ii]), collapse = ", "),
              " in the list ", x, "@", s
            )
          }
          if (hh[1] != 0) {
            nn <- 1:hh[1]
            slot(obj, s)[nn, ] <- NA
            for (i in names(dat)) {
              slot(obj, s)[[i]][nn] <- dat[[i]]
            }
          }
        } else if (any(colnames(slot(obj, s)) == s) && length(dat) == 1) {
          # scalar-data to add to a data.frame
          # for cases like "start = 2000" instead of "start = list(start = 2000)"
          slot(obj, s)[1, ] <- NA
          if (all(class(dat) %in% class(slot(obj, s)[1, s]))) {
            slot(obj, s)[1, s] <- dat
          } else {
            stop(
              "Unmatched data class (", class(dat), ") in ", x, "@", s,
              ". Expecting class ", class(slot(obj, s)[1, s])
            )
          }
        } else {
          stop("Unidentified data/argument ", s, ", object: ", x)
        }
        # Other formats
      } else if (slots[s] == "factor") {
        slot(obj, s) <- slot(obj, s)[0] # reset
        # check consistency of the factor levels
        slot_levels <- levels(slot(obj, s))
        data_levels <- as.character(dat)
        ii <- data_levels %in% slot_levels
        if (!all(ii)) {
          stop(
            "Unexpected values: ", paste(data_levels, collapse = ", "),
            " in ", x, "@", s, ". Allowed levels: ",
            paste(slot_levels, collapse = ", ")
          )
        }
        if (length(dat) != 0) slot(obj, s)[seq(along = dat)] <- dat
      } else if (slots[s] == "integer") {
        if (!is.integer(dat)) {
          if (is.numeric(dat)) {
            if (all(dat - trunc(dat) == 0)) dat <- as.integer(dat)
          } else {
            stop(
              "Unexpected data format (", class(dat), ") in 'integer' slot ",
              x, "@", s
            )
          }
        }
        slot(obj, s) <- dat
      } else if (slots[s] == "numeric") {
        if (!is.numeric(dat)) {
          stop(
            "Unexpected data format (", class(dat), ") in 'numeric' slot ",
            x, "@", s
          )
        }
        slot(obj, s) <- dat
      } else if (slots[s] == "character") {
        if (!is.character(dat)) {
          stop(
            "Unexpected data format (", class(dat), ") in 'character' slot ",
            x, "@", s
          )
        }
        slot(obj, s) <- as.character(dat)
      } else {
        # all other formats
        if (all(class(dat) %in% class(slot(obj, s)))) {
          slot(obj, s) <- dat
        } else {
          stop(
            "Unmatched data class (", class(dat), ") in ", x, "@", s,
            ". Expecting class ", slots[s]
          )
        }
      }
    }
  }
  obj
}
