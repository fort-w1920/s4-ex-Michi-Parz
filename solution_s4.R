# Unterfuntionen---------------------------------------------------------------
validity_subclass <- function(object, weight_area, hunt_area, hide = TRUE) {
  invalids <- character(0)
  check_weight <- (
    object@weight >= weight_area[1] && object@weight <= weight_area[2]
  )
  check_hunt <- ifelse(hide,
    (object@hide >= hunt_area[1] && object@hide <= hunt_area[2]),
    (object@seek >= hunt_area[1] && object@seek <= hunt_area[2])
  )
  # Wenn beides falsch ist noch beides als Fehler ausgeben lassen!
  if (!check_hunt) invalids <- "Fehlermeldung muss noch angepasst werden."
  if (!check_weight) invalids <- "Unrealistic weight!"
  if (length(invalids)) invalids else TRUE
}

make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE
    )
  paste(name, collapse = "")
}


# Klassen-----------------------------------------------------------------------
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

setClass("predator",
  slots = list(seek = "numeric"),
  contains = "animal",
  validity = function(object) {
    invalids <- character(0)
    check_hide <- (object@seek >= 0 && object@seek <= 1)
    if (!check_hide) invalids <- "Seek is not between 0 and 1."
    if (!length(invalids)) invalids else TRUE
  }
)



# Unterklassen von prey---------------------------------------------------------
setClass("mouse",
  contains = "prey",
  prototype = list(name = "mouse"),
  validity = function(object) {
    validity_subclass(object, weight_area = c(0.5, 1), hunt_area = c(0.6, 1))
  }
)

setClass("rabbit",
  contains = "prey",
  prototype = list(name = "rabbit"),
  validity = function(object) {
    validity_subclass(object, weight_area = c(1, 5), hunt_area = c(0.3, 0.8))
  }
)

setClass("deer",
  contains = "prey",
  prototype = list(name = "deer"),
  validity = function(object) {
    validity_subclass(object, weight_area = c(15, 30), hunt_area = c(0.2, 0.7))
  }
)

# Unterklassen von predator-----------------------------------------------------

setClass("hawk",
  contains = "predator",
  prototype = list(name = "hawk"),
  validity = function(object) {
    validity_subclass(object,
      weight_area = c(3, 8),
      hunt_area = c(0.6, 1),
      hide = FALSE
    )
  }
)

setClass("lynx",
  contains = "predator",
  prototype = list(name = "hawk"),
  validity = function(object) {
    validity_subclass(object,
      weight_area = c(20, 60),
      hunt_area = c(0.5, 0.9),
      hide = FALSE
    )
  }
)


# Methode-----------------------------------------

setMethod("show", "prey", function(object) {
  gender <- ifelse(object@female, "(f)", "(m)")

  cat(is(object)[[1]], " '", object@name, "' ", gender, "\n",
    "  Weight: ", object@weight, "\n",
    "  Hide:  ", object@hide, "\n",
    sep = ""
  )
})

setMethod("show", "predator", function(object) {
  gender <- ifelse(object@female, "(f)", "(m)")
  
  cat(is(object)[[1]], " '", object@name, "' ", gender, "\n",
      "  Weight: ", object@weight, "\n",
      "  Hide:  ", object@seek, "\n",
      sep = ""
  )
})

# Konstruktorfunktionen prey-------------------------------------------------

mouse <- function(
                  name = make_name(),
                  weight = runif(1, 0.5, 1),
                  hide = runif(1, 0.6, 1),
                  female = sample(c(TRUE, FALSE), 1)) {
  new("mouse", name = name, weight = weight, hide = hide, female = female)
}

rabbit <- function(
  name = make_name(),
  weight = runif(1, 1, 5),
  hide = runif(1, 0.3, 0.8),
  female = sample(c(TRUE, FALSE), 1)) {
  new("rabbit", name = name, weight = weight, hide = hide, female = female)
}

deer <- function(
  name = make_name(),
  weight = runif(1, 15, 30),
  hide = runif(1, 0.2, 0.7),
  female = sample(c(TRUE, FALSE), 1)) {
  new("deer", name = name, weight = weight, hide = hide, female = female)
}

# Konstruktorfunktionen predator-------------------------------------------------

hawk <- function(
  name = make_name(),
  weight = runif(1, 3, 8),
  seek = runif(1, 0.6, 1),
  female = sample(c(TRUE, FALSE), 1)) {
  new("hawk", name = name, weight = weight, seek = seek, female = female)
}

lynx <- function(
  name = make_name(),
  weight = runif(1, 20, 60),
  seek = runif(1, 0.5, 0.9),
  female = sample(c(TRUE, FALSE), 1)) {
  new("lynx", name = name, weight = weight, seek = seek, female = female)
}



# Test-------------------------------------------------------------------------
deer()
hawk()
str(mouse(female = TRUE))
str(hawk(weight = 4))
str(lynx(name = "", weight = NA + 1))
str(mouse(weight = 100))

