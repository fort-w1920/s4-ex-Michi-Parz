# ToDo:
# Fehlermeldungen anpassen: Beide Fehlermeldungen wenn zwei falsch, die Meldungen aus der Vorlage ect.
# DRY überprüfen


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
    sample(vowels, size = floor(length / 2), replace = TRUE)
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



# Unterklassen von prey und predator--------------------------------------------

set_classes <- function(object, ...) UseMethod("set_classes")
set_classes.default <- function(name,
                                contains = c("prey", "predator"),
                                weight_area,
                                hunt_area,
                                prototype = NULL) {
  checkmate::assert_character(name)
  checkmate::assert_numeric(weight_area, min.len = 2, max.len = 2)
  checkmate::assert_numeric(weight_area, min.len = 2, max.len = 2)
  checkmate::assert_list(prototype, null.ok = TRUE)

  contains <- match.arg(contains)

  hide <- (contains == "prey")

  setClass(name,
    contains = contains,
    prototype = prototype,
    validity = function(object) {
      validity_subclass(object,
        weight_area = weight_area,
        hunt_area = hunt_area,
        hide = hide
      )
    }
  )
}

set_classes.list <- function(list) {
  set_classes.default(
    name = list$name,
    contains = list$contains,
    weight_area = list$weight_area,
    hunt_area = list$hunt_area
  )
}

create_animals_list <- list(
  # Preys
  list(
    "name" = "mouse", "contains" = "prey", "weight_area" = c(0.5, 1),
    "hunt_area" = c(0.6, 1)
  ),
  list(
    "name" = "rabbit", "contains" = "prey", "weight_area" = c(1, 5),
    "hunt_area" = c(0.3, 0.8)
  ),
  list(
    "name" = "deer", "contains" = "prey", "weight_area" = c(15, 30),
    "hunt_area" = c(0.2, 0.7)
  ),
  # Predators
  list(
    "name" = "hawk", "contains" = "predator", "weight_area" = c(3, 8),
    "hunt_area" = c(0.6, 1)
  ),
  list(
    "name" = "lynx", "contains" = "predator", "weight_area" = c(20, 60),
    "hunt_area" = c(0.5, 0.9)
  )
)



lapply(create_animals_list, set_classes)



# Methode-----------------------------------------

# Nicht DRY genug!
# Die Funktion außerhalb definieren und unterschiede übergeben!

setMethod("show", "prey", function(object) {
  gender <- ifelse(object@female, "(f)", "(m)")

  cat(is(object)[[1]], " '", object@name, "' ", gender, "\n",
    "  weight: ", object@weight, "\n",
    "  hide:  ", object@hide, "\n",
    sep = ""
  )
})

setMethod("show", "predator", function(object) {
  gender <- ifelse(object@female, "(f)", "(m)")

  cat(is(object)[[1]], " '", object@name, "' ", gender, "\n",
    "  weight: ", object@weight, "\n",
    "  seek:  ", object@seek, "\n",
    sep = ""
  )
})

# Konstruktorfunktionen prey-------------------------------------------------

# geht bestimmt schöner (DRY)


mouse <- function(name = make_name(),
                  weight = runif(1, 0.5, 1),
                  hide = runif(1, 0.6, 1),
                  female = sample(c(TRUE, FALSE), 1)) {
  new("mouse", name = name, weight = weight, hide = hide, female = female)
}

rabbit <- function(name = make_name(),
                   weight = runif(1, 1, 5),
                   hide = runif(1, 0.3, 0.8),
                   female = sample(c(TRUE, FALSE), 1)) {
  new("rabbit", name = name, weight = weight, hide = hide, female = female)
}

deer <- function(name = make_name(),
                 weight = runif(1, 15, 30),
                 hide = runif(1, 0.2, 0.7),
                 female = sample(c(TRUE, FALSE), 1)) {
  new("deer", name = name, weight = weight, hide = hide, female = female)
}

# Konstruktorfunktionen predator-------------------------------------------------

hawk <- function(name = make_name(),
                 weight = runif(1, 3, 8),
                 seek = runif(1, 0.6, 1),
                 female = sample(c(TRUE, FALSE), 1)) {
  new("hawk", name = name, weight = weight, seek = seek, female = female)
}

lynx <- function(name = make_name(),
                 weight = runif(1, 20, 60),
                 seek = runif(1, 0.5, 0.9),
                 female = sample(c(TRUE, FALSE), 1)) {
  new("lynx", name = name, weight = weight, seek = seek, female = female)
}



# Method test-------------------------------------------------------------------
deer()
hawk()
str(mouse(female = TRUE))
str(hawk(weight = 4))
str(lynx(name = "", weight = NA + 1))
str(mouse(weight = 100))




# Meet--------------------------------------------------------------------------

setGeneric(
  "meet",
  function(animal1, animal2, ...) {
    standardGeneric("meet")
  }
)


setMethod("meet",
  signature = c(animal1 = "animal", animal2 = "animal"),
  function(animal1, animal2, text = "") {
    message(
      is(animal1)[[1]], " '", animal1@name, "' & ", is(animal2)[[1]],
      " '", animal2@name, "' ", message
    )
  }
)


meet_list <- list(
  list(
    "contains1" = "prey", "contains2" = "prey",
    "events" = c(
      "ignore each other", "sniff each others' butts",
      "make sweet, sweet love"
    ),
    "prob_same_sex" = c(1 / 2, 1 / 2, 0),
    "prob_different_sex" = c(1 / 4, 1 / 4, 1 / 2)
  ),
  list(
    "contains1" = "predator", "contains2" = "predator",
    "events" = c(
      "ignore each other", "sniff each others' butts",
      "make sweet, sweet love", "fight for territory"
    ),
    "prob_same_sex" = c(1 / 3, 1 / 3, 1 / 3, 0),
    "prob_different_sex" = c(0, 0, 1 / 2, 1 / 2)
  ),
  list(
    "contains1" = "prey", "contains2" = "predator",
    "events" = c(
      "kills and eats", "escapes from",
      "ignore each other", "sniff each others' butts"
    ),
    "prob_same_sex" = NULL,
    "prob_different_sex" = NULL
  )
)

# if if if if if if, wegbekommen


create_situation <- function(animal1, animal2, contains1, contains2, 
                                    events, prob_same_sex, prob_different_sex) {
  probabilites <- prob_same_sex
  if (animal1@female != animal2@female & is(animal1)[[1]] == is(animal2)[[1]]) {
    probabilites <- prob_different_sex
  }
  
  if (contains1 != contains2) {
    
    probabilites <- c(0, 0, 1/2, 1/2)
    
    if (prey@weight >= 0.05 * predator@weight &
      prey@weight <= 0.70 * predator@weight) {
      prob_eat <- min(1, max(0,0.6 + predator@seek - prey@hide))
      
      probabilites <- c(prob_eat, 1 - prob_eat, 0, 0)
    }
    
  }
  
  sample(events, size = 1, prob = probabilites)
}





for (i in seq_len(length(meet_list))) {

}

set_method_meet <- function(object, ...) UseMethod("set_classes")
set_method_meet.default <- function(contains1, contains2, events,
                            prob_same_sex, prob_different_sex) {
  setMethod("meet",
    signature = c(animal1 = contains1, animal2 = contains2),
    function(animal1, animal2) {
      message <- create_situation(
        animal1, animal2, contains1, contains2, events,
        prob_same_sex, prob_different_sex
      )

      callNextMethod(animal1, animal2, message)
    }
  )
}

set_method_meet.list <- function(list){
  set_method_meet.default(list$contains1, list$contains2, list$events, 
                          list$prob_same_sex, list$prob_different_sex)
}


setMethod("meet",
  signature = c(animal1 = "prey", animal2 = "prey"),
  function(animal1, animal2) {
    event <- c(
      "ignore each other", "sniff each others' butts",
      "make sweet, sweet love"
    )
    
    probabilites <- c(1 / 2, 1 / 2, 0)
    if (animal1@female != animal2@female & is(animal1)[[1]] == is(animal2)[[1]]) {
      probabilites <- c(1 / 4, 1 / 4, 1 / 2)
    }

    text <- sample(event, size = 1, prob = probabilites)

    callNextMethod(animal1, animal2, text)
  }
)

# Andere Kombinationen und Geschlechter beachten und so

meet(deer(), deer())
