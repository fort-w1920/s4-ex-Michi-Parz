# ToDo:
# Fehlermeldungen anpassen: Beide Fehlermeldungen wenn zwei falsch, die Meldungen aus der Vorlage ect.
# DRY überprüfen


# Unterfuntionen---------------------------------------------------------------
validity_subclass <- function(object, weight_area = c(0, Inf), hunt_area = NULL,
                              hide = TRUE) {
  
  invalids <- character(0)
  invalids_index <- 1L
  check_weight <- checkmate::test_numeric(object@weight, lower = weight_area[1],
                                           upper = weight_area[2], any.missing = FALSE)
  # check_weight <- (
  #   object@weight >= weight_area[1] && object@weight <= weight_area[2] && 
  #     !is.na(object@weight)
  # )
  
  if (!is.null(hunt_area)) {
    check_hunt <- ifelse(hide,
                         (object@hide >= hunt_area[1] && object@hide <= hunt_area[2]),
                         (object@seek >= hunt_area[1] && object@seek <= hunt_area[2])
    )
    if (!check_hunt) {
      invalids[invalids_index] <- "hide/seek musst be in [0, 1]"
      invalids_index <- invalids_index + 1L
    } 
  }
  
  if (!check_weight) {
    invalids[invalids_index] <- paste("Weight musst be in [", weight_area[1], 
                      ", ", weight_area[2], "]", sep = "")
  } 
  if (length(invalids) > 0) invalids else TRUE
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
    if (no_name) invalids <- "animals need a 'name'."
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


show_illustration <- function(object, subclass = c("prey", "predator")){
  
  subclass <- match.arg(subclass)
  gender <- ifelse(object@female, "(f)", "(m)")
  
  hunt <- ifelse(subclass == "prey", object@hide, object@seek)
  hunt_description <- ifelse(subclass == "prey", "  hide:  ", "  seek:  ")
  
  cat(is(object)[[1]], " '", object@name, "' ", gender, "\n",
      "  weight: ", object@weight, "\n",
      hunt_description, hunt, "\n",
      sep = ""
  )
}


setMethod("show", "prey", function(object) {
    show_illustration(object, "prey")
})
setMethod("show", "predator", function(object) {
  show_illustration(object, "predator")
})
  




# Konstruktorfunktionen prey-------------------------------------------------

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




# Help functions for meet-------------------------------------------------------

check_prey_or_predator <- function(animal) {
  is_animal <- is(animal)
  if (length(is_animal) != 3) {
    stop("A subclass of prey or predator is necessary!")
  }

  is_animal[2]
}



prey_meets_predator <- function(prey, predator) {
  probabilites <- c(
    "ignore" = 1 / 2, "sniff" = 1 / 2, "love" = 0,
    "fight" = 0, "kill" = 0, "escape" = 0
  )
  if (prey@weight >= 0.05 * predator@weight &
    prey@weight <= 0.70 * predator@weight) {
    prob_kill <- min(1, max(0, 0.6 + predator@seek - prey@hide))

    probabilites <- c(
      "ignore" = 0, "sniff" = 0, "love" = 0,
      "fight" = 0, "kill" = prob_kill, "escape" = 1 - prob_kill
    )
  }

  probabilites
}


prey_meets_prey <- function(prey1, prey2) {
  same_species <- is(prey1)[1] == is(prey2)[1]
  if (prey1@female != prey2@female & same_species) {
    probabilites <- c(
      "ignore" = 1 / 4, "sniff" = 1 / 4, "love" = 1 / 2,
      "fight" = 0, "kill" = 0, "escape" = 0
    )

    return(probabilites)
  }
  probabilites <- c(
    "ignore" = 1 / 2, "sniff" = 1 / 2, "love" = 0,
    "fight" = 0, "kill" = 0, "escape" = 0
  )

  probabilites
}



predator_meets_predator <- function(predator1, predator2) {
  same_species <- is(predator1)[1] == is(predator2)[1]
  if (predator1@female != predator2@female & same_species) {
    probabilites <- c(
      "ignore" = 0, "sniff" = 0, "love" = 1 / 2,
      "fight" = 1 / 2, "kill" = 0, "escape" = 0
    )

    return(probabilites)
  }
  probabilites <- c(
    "ignore" = 1 / 2, "sniff" = 1 / 2, "love" = 0,
    "fight" = 0, "kill" = 0, "escape" = 0
  )

  probabilites
}


sitaution_probability <- function(animal1, animal2) {
  subclass_animal1 <- check_prey_or_predator(animal1)
  subclass_animal2 <- check_prey_or_predator(animal2)

  if (subclass_animal1 != subclass_animal2) {
    if (subclass_animal1 == "prey") {
      return(prey_meets_predator(prey = animal1, predator = animal2))
    }

    return(prey_meets_predator(predator = animal1, prey = animal2))
  }

  if (subclass_animal1 == "prey") {
    return(prey_meets_prey(animal1, animal2))
  }

  predator_meets_predator(animal1, animal2)
}


hunt_message <- function(event, animal1, animal2) {
  if (is(animal1)[2] == "prey") {
    return(hunt_message(event, animal1 = animal2, animal2 = animal1))
  }

  if (event == "kill") {
    return(message(
      is(animal1)[[1]], " '", animal1@name, "' kills and eats ",
      is(animal2)[[1]], " '", animal2@name, "'"
    ))
  }

  message(
    is(animal2)[[1]], " '", animal2@name, "'escapes from ",
    is(animal1)[[1]], " '", animal1@name, "'"
  )
}



situation <- function(animal1, animal2, probabilites) {
  event <- sample(names(probabilites), size = 1, prob = probabilites)


  if (event == "kill" | event == "escape") {
    return(hunt_message(event, animal1, animal2))
  }


  intro <- paste(is(animal1)[[1]], " '", animal1@name, "' & ", is(animal2)[[1]],
    " '", animal2@name, "'",
    sep = ""
  )

  description <- switch(event,
    "ignore" = "ignore each other",
    "sniff" = "sniff each others' butts",
    "love" = "make sweet, sweet love",
    "fight" = "fight for territory"
  )
  message(intro, description)
}



# Meet Method-------------------------------------------------------------------
setGeneric(
  "meet",
  function(animal1, animal2, ...) {
    standardGeneric("meet")
  }
)


setMethod("meet",
  signature = c(animal1 = "animal", animal2 = "animal"),

  function(animal1, animal2, probability) {
    situation(animal1, animal2, probability)
  }
)


for (subclass1 in c("prey", "predator")) {
  for (subclass2 in c("prey", "predator")) {
    setMethod("meet",
      signature = c(animal1 = subclass1, animal2 = subclass2),
      function(animal1, animal2) {
        probability <- sitaution_probability(animal1, animal2)

        callNextMethod(animal1, animal2, probability)
      }
    )
  }
}

# example code for animal class:
set.seed(2020)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}
