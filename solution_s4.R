setClass("animal",
  slots = list(name = "character", weight = "numeric", female = "logical"),
  validity = function(object) {
    invalids <- character(0)
    no_name <- nchar(object@name) == 0
    if (no_name) invalids <- "No <name> provided."
    if (length(invalids)) invalids else TRUE
  }
)

setClass("prey",
  slots = list(hide = "numeric"),
  contains = "animal",
  validity = function(object) {
    invalids <- character(0)
    check_hide <- (object@hide >= 0 && object@hide <= 1)
    if (!check_hide) invalids <- "Hide is not between 0 and 1."
    if (!length(invalids)) invalids else TRUE
  }
)

setClass("mouse",
         contains = "prey",
         prototype = list(name = "mouse"),
         validity = function(object) {
           invalids <- character(0)
           check_weight <- (object@weight >= 0.5 && object@weight <= 1)
           check_hide <- (object@hide >= 0.6 && object@hide <= 1)
           if (!check_hide) invalids <- "Hide is not between 0.6 and 1."
           if (!check_weight) invalids <- "This is not the weight of a mouse!
           A mouse weights between 0.5 and 1."
           if (length(invalids)) invalids else TRUE
         }
)

test <- new("mouse",name = "a", weight = 0.7, female = T, hide = 0.7)
test
test@hide
