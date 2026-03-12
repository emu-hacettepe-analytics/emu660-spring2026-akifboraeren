install.packages("dslabs")
library(dslabs)


#C

data(mtcars)
print(mtcars)

mpg <- mtcars$mpg

compute_stats <- function(num_vector){
  
  statslist <- c(
    mean = mean(num_vector),
    median = median(num_vector),
    variance = var(num_vector),
    iqr = IQR(num_vector),
    minimum = min(num_vector),
    maximum = max(num_vector)
    )
  return(statslist)
}

#instead of c(), we can use list() but it is harder for apply function to make it a matrix.

compute_stats(mpg)

for (i in names(mtcars)){
  print(i)
  cat("\n")
  print(compute_stats(mtcars[[i]]))
  cat("\n")
}

results1 <- sapply(mtcars, compute_stats)

print(results1)

results2 <- apply(mtcars, 2, compute_stats)

print(results2)



#D

data(polls_us_election_2016)
head(polls_us_election_2016)
str(polls_us_election_2016)

my_first <- "Akif Bora"
my_birth_year <- 2001

k <- (nchar(my_first) + my_birth_year) %% 15 + 8

if (k%%2 ==0) {
  trimmed_data <- head(polls_us_election_2016, k)
} else{
  trimmed_data <- tail(polls_us_election_2016, k)
}

#number of na values in the entire dataset
sum(is.na(polls_us_election_2016))

#replacinfg the na values

for (i in names(trimmed_data)) {
  column <- trimmed_data[[i]]

  if (is.numeric(column)) {
    column[is.na(column)] <- my_birth_year + k
  } 
  else if (is.character(column) || is.factor(column)) {
    column <- as.character(column)

    column[is.na(column)] <- paste0(my_first, "_", k)
  }
  else if (is.factor(column) || is.factor(column)) {
    column <- as.character(column)
  }
  trimmed_data[[i]] <- column
}

