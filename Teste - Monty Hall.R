montyHall <- function(change) {
  abc <- LETTERS[1:3]

  trueDoor <- sample(abc, 1)
  yourChoice <- sample(abc, 1)

  if(change) {
    notYourChoice <- abc[abc != yourChoice]
    if(trueDoor %in% notYourChoice)
      yourChoice <- trueDoor
    else
      yourChoice <- sample(notYourChoice, 1)
  }

  yourChoice == trueDoor
}


repMH <- function(change) {
  mean(replicate(1e4, montyHall(change)))
}


lapply(c(TRUE, FALSE), repMH)