#### project ideas for first module ####


## Calculator

### What this would test:
# creating and transforming objects
# building functions
# creating loops? For generating sequences?




calculator <- function() {
  number1 <- as.numeric(readline(prompt = "give a number: "))
  number2 <- as.numeric(readline(prompt = "give a number: "))
  result <- number1 + number2
  return(result)
}
calculator()





## Data simulator
# Simulate data that fulfill certain conditions?


## Game?
# Hangman?
# Some numbers game?




guess_game <- function() {
  number <- sample(1:10, 1)
  input <- readline(prompt = "guess the number: ")
  while (input != number) {
    if (input > number) {
      print("Too large")
      input <- readline(prompt = "guess the number: ")
    
    } else if (input < number) {
      print("Too small")
      input <- readline(prompt = "guess the number: ")
    }
  }
  print("correct!")
}

guess_game()


## Hangman game:

hangman <- function() {
  words <- c("hangman", "delicious", "journal", "painting", "pathological", "raspberry")
  
  chosen <- sample(words, 1)
  
  word <- unlist(strsplit(chosen, ""))
  length <- length(word)
  display <- unlist(strsplit(strrep("_", length), ""))
  counter <- 0
  print(display)
  letter <- readline(prompt = "Guess the letter: ")
  
  while ("_" %in% display) {
    
    for (i in seq(1, length)) {
      
      if (word[i] == letter) {
        display[i] <- letter
      }
    }
    
    if (!(letter %in% word)) {
      print("Wrong")
      counter <- counter + 1
      if (counter == 5) {
        return ("You lose")
      }
      
      print(display)
      print(paste("number of errors: ", as.character(counter)))
      letter <- readline(prompt = "Guess the letter: ")
    } else {
      print(display)
      print(paste("number of errors: ", as.character(counter)))
      if (!("_" %in% display)) {
        return("Great! You win")
      }
      letter <- readline(prompt = "Guess the letter: ")
    }
    
    
    
  }
  print("Great! You win")
  
}
hangman()






