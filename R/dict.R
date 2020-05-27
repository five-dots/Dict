#' @title Dictionary Class
#'
#' @description
#' A key-value dictionary data structure based on R6 class.
#'
#' @usage
#' Dict$new(..., .class = "any", .overwrite = TRUE)
#'
#' @examples
#' ## Instantiation (dict() is a wrapper for Dict$new())
#' ages <- dict(
#'   Charlie = 40L,
#'   Alice = 30L,
#'   Bob = 25L,
#'   .class = "integer",
#'   .overwrite = TRUE
#' )
#'
#' ## Get a value
#' ages["Bob"] # or ages$get("Bob")
#' ages[3] # also by integer index
#'
#' ## Return the default value if the key does not exists (default = NULL)
#' ages["Michael", default = 30]
#'
#' ## Add a new item
#' ages["John"] <- 18L # or ages$add(John = 18L)
#'
#' ## Can be overridden if .overwrite = TRUE (default)
#' ages["Bob"] <- 26L
#' ages$get("Bob")
#'
#' ## Check if items contains a key
#' ages$has("Bob")
#'
#' ## Remove item
#' ages$remove("Bob")
#' ages$has("Bob")
#'
#' ## Sort by keys
#' ages$sort()
#' ages
#'
#' ## Some additonal fields
#' ages$keys   # a character vector of keys
#' ages$values # a list of values
#' ages$items  # a tbl_df of items
#' ages$length # items length
#'
#' ## Clear
#' ages$clear()
#' ages
#'
#' @export
Dict <- R6::R6Class(
  classname = "Dict",
  class = TRUE,
  public = rlang::list2(
    #' @description
    #' Construct a new Dict object.
    #'
    #' @param ... Any length of key and value pairs. If you would like to use
    #' a not valid R name as a key, you must wrap it by backquotes or convert it
    #' using \code{\link{make.names}}.
    #' @param .class A character scalar of value object's class. It must be an
    #' output from \code{\link{class}}. If \code{"any"} (default), value can
    #' contain any type of object.
    #' @param .overwrite A logical scalar whether to overwrite the value if the
    #' key is overlapped.
    #'
    #' @return A \code{Dict} class object.
    initialize = function(..., .class = "any", .overwrite = TRUE) {
      if (!rlang::is_string(.class))
        stop(".class must be a character scalar.", call. = FALSE)
      if (!rlang::is_bool(.overwrite))
        stop(".overwrite must be a logical scalar.", call. = FALSE)
      private$.class <- .class
      private$.overwrite <- .overwrite
      self$add(...)
    },

    #' @description
    #' Print Dict \code{items} which is a \code{\link{tbl_df}} object by tibble
    #' package.
    #'
    #' @param ... Additional arguments passed to \code{print.tbl}.
    #'
    #' @return \code{Dict} object by \code{invisible(self)}.
    print = function(...) {
      print(private$.items, ...)
      invisible(self)
    },

    #' @description
    #' Add key-value objects to the dictionary.
    #'
    #' @param ... Any length of key and value pairs. If you would like to use
    #' a not valid R name as a key, you must wrap it by backquotes or convert it
    #' using \code{\link{make.names}}.
    #'
    #' @return \code{Dict} object by \code{invisible(self)}.
    add = function(...) {
      new_items <- private$.as_dict(...)
      all_items <- dplyr::bind_rows(private$.items, new_items)
      if (any(new_items$key %in% self$keys)) {
        ## Key duplicate case
        ## Slice index 1: keep old item (.overwrite=FALSE)
        ## Slice index 2: Replace old item with new one (.overwrite=TRUE)
        index <- dplyr::if_else(self$overwrite, 2, 1)
        items <- all_items %>%
          dplyr::group_by(key) %>%
          dplyr::group_modify(function(rows, ...) {
            ## Skip if no duplicate rows
            if (nrow(rows) == 1) return(rows)
            dplyr::slice(rows, index)
          }) %>%
          dplyr::ungroup()
      } else {
        ## Not duplicate case
        items <- all_items
      }
      private$.items <- items
      invisible(self)
    },

    #' @description
    #' Check if the object contains the key.
    #'
    #' @param key A character scalar of the dictionary key.
    #'
    #' @return A logical scalar.
    has = function(key = NULL) {
      if (!rlang::is_string(key))
        stop("key must be a character scalar.", call. = FALSE)
      key %in% self$keys
    },

    #' @description
    #' Retrieves object with a key from the dictionary.
    #'
    #' @param key A character scalar, integer scalar of items index or NULL.
    #' If key is NULL and items is not empty, the first value is returned.
    #' @param default A default value returned, if the key is not found. Default
    #' is \code{NULL}.
    #'
    #' @return A object with the key.
    get = function(key = NULL, default = NULL) {
      ## Normalize integer key to character one. NULL returned if no key found.
      key <- private$.normalize_key(key)
      if (is.null(key)) return(default)
      private$.items %>%
        dplyr::filter(key == !!key) %>%
        dplyr::pull(value) %>%
        purrr::pluck(1)
    },

    #' @description
    #' Removes a key-value from the dictionary by a key. If the key is a not
    #' valid key, this function throw an error. Use \code{self$has()} to check
    #' key availability.
    #'
    #' @param key A character scalar of the dictionary key.
    #'
    #' @return \code{Dict} object by \code{invisible(self)}.
    remove = function(key = NULL) {
      if (self$has(key)) {
        private$.items <- dplyr::filter(private$.items, key != !!key)
      } else {
        stop("key \"", key, "\" is not found.", call. = FALSE)
      }
      invisible(self)
    },

    #' @description
    #' Sort dictionary by keys.
    #'
    #' @param desc A logical scalar whether to sort in descending order. Default
    #' is \code{FALSE}.
    #'
    #' @return \code{Dict} object by \code{invisible(self)}.
    sort = function(desc = FALSE) {
      if (!rlang::is_bool(desc))
        stop("desc must be a logical scalar.", call. = FALSE)
      if (!desc) {
        private$.items <- dplyr::arrange(private$.items, key)
      } else {
        private$.items <- dplyr::arrange(private$.items, dplyr::desc(key))
      }
      invisible(self)
    },

    #' @description
    #' Clear dictionary.
    #'
    #' @return \code{Dict} object by \code{invisible(self)}.
    clear = function() {
      private$.items <- tibble::tibble(key = character(), value = list())
      invisible(self)
    },
  ),
  active = rlang::list2(
    #' @field items A \code{tbl_df} of the dictionary items.
    items = function() private$.items,
    #' @field keys A character vector of the dictionary keys.
    keys = function() private$.items$key,
    #' @field values A list of of the dictionary values.
    values = function() private$.items$value,
    #' @field length A integer scalar of the items length.
    length = function() nrow(private$.items),
    #' @field class A character scalar of value class.
    class = function() private$.class,
    #' @field overwrite A logical scalar whether to overwrite value if key is
    #' overlapped.
    overwrite = function() private$.overwrite,
  ),
  private = rlang::list2(
    .assert_dots = function(...) {
      dots <- list(...)
      if (!rlang::is_dictionaryish(dots))
        stop("all items must have unique keys.", call. = FALSE)
      ## Check all values class, if .class is not "any"
      if (private$.class != "any") {
        purrr::iwalk(dots, function(value, key) {
          if (!inherits(value, private$.class))
            stop("A value for key = \"", key, "\" must be a ", private$.class,
                 " object.", call. = FALSE)
        })
      }
      dots
    },

    .as_dict = function(...) {
      dots <- private$.assert_dots(...)
      dots <- list(...)
      tibble::enframe(dots, name = "key", value = "value")
    },

    .normalize_key = function(key = NULL) {
      ## Empty items: -> NULL
      valid_keys <- self$keys
      if (length(valid_keys) == 0) return(NULL)
      ## key = NULL: -> the first key
      if (is.null(key)) return(valid_keys[1])
      ## key = character: -> return as-is if exists
      if (rlang::is_string(key) && key %in% valid_keys) return(key)
      ## key = integer: -> convert to character key
      if (rlang::is_scalar_integerish(key) && key %in% seq_along(valid_keys))
        return(valid_keys[key])
      ## No key found: -> NULL
      NULL
    },

    .class = character(),
    .overwrite = logical(),
    .items = tibble::tibble(key = character(), value = list()),
  )
)


#' Dict Class Constructor
#'
#' @rdname Dict
#'
#' @param ... Any length of key and value pairs. If you would like to use
#' a not valid R name as a key, you must wrap it by backquotes or convert it
#' using \code{\link{make.names}}.
#' @param .class A character scalar of value object's class. It must be an
#' output from \code{\link{class}}. If \code{"any"} (default), value can contain
#' any type of object.
#' @param .overwrite A logical scalar whether to overwrite the value if the
#' key is overlapped.
#'
#' @return A \code{Dict} class object.
#'
#' @export
dict <- function(..., .class = "any", .overwrite = TRUE) {
  Dict$new(..., .class = .class, .overwrite = .overwrite)
}


#' @export
`[.Dict` <- function(x, key = NULL, default = NULL) {
  x$get(key, default)
}


#' @export
`[<-.Dict` <- function(x, key, value) {
  if (!rlang::is_scalar_character(key))
    stop("key must be a character scalar.", call. = FALSE)
  arg <- rlang::set_names(list(value), key)
  rlang::exec(x$add, !!!arg)
}
