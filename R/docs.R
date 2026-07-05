## Functions to generate roxygen2 documentation for large classes/methods ######
# generate roxygen2 script from a data.frame
roxygen_slots <- function(df, roxy_par = "slot") {

  roxygen_script <- c()

  # Get unique slot names to avoid duplicating them
  unique_slots <- unique(df$slotname)

  # Loop through each unique slot and create roxygen comments
  for (slot in unique_slots) {
    # Extract slot information
    slot_data <- df |> filter(slotname == slot)
    slot_name <- slot_data$slotname[1]
    slot_type <- slot_data$type[1]
    slot_description <- slot_data$description[1]

    # Start creating the roxygen script for the slot
    slot_entry <- paste0("#' @", roxy_par," ", slot_name, " ", slot_type,
                         ", ", slot_description)
    roxygen_script <- c(roxygen_script, slot_entry)

    # Check if the slot is a data.frame and contains column information
    if (slot_type == "data.frame") {
      describe_start <- "#' \\describe{"
      roxygen_script <- c(roxygen_script, describe_start)

      # Loop through each row where slotname matches and generate column descriptions
      slot_columns <- slot_data |> filter(!is.na(col.name))

      for (j in 1:nrow(slot_columns)) {
        col_name <- slot_columns$col.name[j]
        col_type <- slot_columns$col.type[j]
        col_description <- slot_columns$col.description[j]

        # Properly format the description, ensuring multiline content is handled
        wrapped_description <- paste(strwrap(col_description, width = 80), collapse = "\n#'   ")

        item_entry <- paste0("#'   \\item{", col_name, "}{", col_type, ", ", wrapped_description, "}")
        roxygen_script <- c(roxygen_script, item_entry)
      }

      # Add the closing brace for describe block with the correct roxygen2 formatting
      describe_end <- "#' }"
      roxygen_script <- c(roxygen_script, describe_end)
    }
  }

  # Join the roxygen script lines into a single string with no trailing or empty lines
  return(paste0(roxygen_script, collapse = "\n"))
}

#' Retrieve slot details in rd-format
#'
#' @param class_name character, name of class.
#' @param slot_name character, name of slot to retrieve.
#' @param col_names logical, if columns information should be
#' returned for data.frame slots.
#'
#' @return character, roxygen2 formatted string with slot details.
#' @export
#'
#' @examples
#' slotNames("technology")
#' get_slot_doc("technology", "input") |> cat()
#' get_slot_doc("technology", "capacity") |> cat()
#' get_slot_doc("demand", "dem") |> cat()
#' get_slot_doc("commodity", "agg") |> cat()
get_slot_doc <- function(class_name = "technology",
                          slot_name = "ceff",
                          col_names = TRUE
                          ) {
  # Get the slot documentation for a specific slot in a class
  # returns a roxygen2 formatted string
  # slot_doc <- getSlots(class_name) |> filter(slotname == slot_name)
  df <- .classes
  # browser()
  slot_data <- df |> filter(slotname == slot_name, 
                            class == class_name)
  if (nrow(slot_data) == 0) {
    stop("No slot found for slot: ", slot_name)
  }

  slot_type <- unique(slot_data$type)
  if (length(slot_type) > 1) {
    stop("Multiple slot types found for slot: ", slot_name)
  } else if (length(slot_type) == 0) {
    stop("No slot type found for slot: ", slot_name)
  }

  slot_description <- slot_data$description |> unique()
  if (length(slot_description) > 1) {
    stop("Multiple slot descriptions found for slot: ", slot_name)
  } else if (length(slot_description) == 0) {
    stop("No slot description found for slot: ", slot_name)
  }

  slot_desc <- paste0(slot_type, ". ", slot_description)
  if (nrow(slot_data) == 1 || !col_names) {
    # return one-line description
    return(slot_desc)
  }
  # browser()
  # Check if the slot is a data.frame and contains column information
  if (slot_type == "data.frame") {
    slot_desc <- paste0(slot_desc, "\n  \\describe{\n")
    slot_columns <- slot_data |> filter(!is.na(col.name))
    if (nrow(slot_columns) == 0) {
      stop("No columns found for slot: ", slot_name)
    }

    for (j in 1:nrow(slot_columns)) {
      col_name <- slot_columns$col.name[j]
      col_type <- slot_columns$col.type[j]
      col_description <- slot_columns$col.description[j]

      # Properly format the description, ensuring multiline content is handled
      # wrapped_description <- paste(strwrap(col_description, width = 80),
      #                              collapse = "\n   ")
      item_entry <- paste0("    \\item{", col_name, "}{", col_type, ". ",
                           col_description, "}\n")
      slot_desc <- c(slot_desc, item_entry)
    }

    # Add the closing brace for describe block with the correct roxygen2 formatting
    describe_end <- " }"
    slot_desc <- c(slot_desc, describe_end)
  }
  paste(slot_desc, collapse = "")
}

