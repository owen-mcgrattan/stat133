# remove_missing()
# Descripiton: Returns input vector with missing values removed
# Inputs: 
#  x: vector
# Output: 
#  same vector with NA values removed
remove_missing <- function(x) {
  x <- x[!is.na(x)]
  return(x)
}
# get_minimum()
# Description: Takes in a numeric vector, returns minimum value
# Inputs:
#  x : A vector
#  na.rm: Option to remove NA values (defaulted to TRUE)
# Outputs:
#  minimum value in the vector
get_minimum <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  x <- sort(x)
  return(x[1])
}

# get_maximum()
# Description: Takes in a numeric vector, returns maximum value
# Inputs: 
#  x: A vector
#  na.rm: Option to remove NA values (defaulted to TRUE)
# Outputs:
#  minimum value in the vector
get_maximum <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  x <- sort(x, decreasing = TRUE)
  return(x[1])
}

# get_range()
# Description: Takes in a vector, returns the range of a numeric vector
# Inputs: 
#   x: A vector
#   na.rm: Option to remove NA values (defaulted to TRUE)
# Outputs:
# range of numeric values in the vector
get_range <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  return(get_maximum(x) - get_minimum(x))
}

# get_median()
# Description: Take a numeric vector and return the median
# Inputs: 
#  x: A vector
#  na.rm: Option to remove NA values (defaulted to TRUE)
# Outputs:
# median of the vector
get_median <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  if ((length(x) %% 2) != 0) {   # Compute for odd length vectors
    return(x[length(x) - ((length(x) - 1) / 2)])
  } else {  # Even length vectors
    return((x[length(x) / 2] + x[(length(x) / 2) + 1]) / 2)
  }
}

# get_average()
# Description: Take a numeric vector and return the average
# Inputs:
#  x: A vector
#  na.rm: Option to remove NA values (defaulted to true)
# Outputs:
#  average of the vector
get_average <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  q <- 0
  for (i in 1:length(x)) {
    q <- q + x[i]
  }
  return(q / length(x))
}

# get_stdev()
# Description: Take a numeric vector and return the standard deviation
# Inputs:
#  x: A vector
#  na.rm: Option to remove NA values (defaulted to true)
# Outputs:
#  standard deviation of the vector
get_stdev <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  mean_diff <- 0
  mean <- get_average(x)
  for (i in 1:length(x)) {
    mean_diff <- mean_diff + (x[i] - mean) ^ 2
  } 
  return(sqrt((1/(length(x) - 1)) * mean_diff))
}

# get_quartile1()
# Description: Take a numeric vector and return the first quartile
# Inputs:
#  x: A vector
#  na.rm: Option to remove NA values(defaulted to true)
# Outputs:
#  the first quartile of the vector
get_quartile1 <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  return(unname(quantile(x, probs = 0.25)))
}

# get_quartile3()
# Description: Take a numeric vector and return the third quartile
# Inputs:
#  x: A vector
#  na.rm: Option to remove NA values(defaulted to true)
# Outputs:
#  the third quartile of the vector
get_quartile3 <- function(x, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  } else {
    x <- x
  }
  return(unname(quantile(x, probs = 0.75)))
}

# count_missing()
# Description: Take a numeric vector and count the number of missing values
# Inputs:
#  x: A numeric vector
# Outputs:
# the number of NA values in the vector
count_missing <- function(x) {
  na <- 0
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      na <- na + 1
    } 
  } 
  return(na)
}

# get_quartile()
# Description: A function that takes in a vector, quartile and returns
#             the corresponding quartile value
# Input: 
#   x: A vector
#   quar: The desired quartile of the vector
#   na.rm: Option to remove NA values (defaulted to TRUE)
# Output:
#  the given quartile of the vector
get_quartile <- function(x, quar, na.rm = TRUE) {
  if(na.rm == TRUE) {
    x <- remove_missing(x)
  }
  return(unname(quantile(x, probs = quar)))
}


# summary_stats()
# Description: Take a numeric vector and return a list of summary stats
# Inputs:
# x: A numeric vector
# Outputs: 
# List of summary stats
summary_stats <- function(x) {
  lst <- list("minimum" = get_minimum(x),
              "percent10" = get_quartile(x, 0.10),
              "quartile1" = get_quartile1(x),
              "median" = get_median(x),
              "mean" = get_average(x),
              "quartile3" = get_quartile3(x),
              "percent90" = get_quartile(x, 0.90),
              "maximum" = get_maximum(x),
              "range" = get_range(x),
              "stdev" = get_stdev(x),
              "missing" = count_missing(x))
  
  return(lst)
}

# print_stats()
# Description: Take a list of summary stats and print the values in a 
#             nice format
# Inputs:
#   stats: A list of summary stats
# Outputs: 
# Well formatted list of summary statistics
print_stats <- function(stats) {
  str_pad(paste(names(stats), stats, sep = " : "), 
          width = 3, side = "both")
}


# rescale100()
# Description: Compute a rescaled vector with a potential scale from 0 to 100
# Inputs:
#  x: a numeric vector
# Outputs:
#  a rescaled vector
rescale100 <- function(x, xmin, xmax) {
  if(is.na(xmin) | is.na(xmax)){
    stop()
  }
  z <- 100 * ((x - xmin) / (xmax - xmin))
  return(z)
}

# drop_lowest()
# Description: Return a vector of length n-1 by dropping the lowest value
# Inputs:
#  x: a numeric vector
# Outputs:
#  a vector with the lowest value removed
drop_lowest <- function(x) {
   return(sort(x)[-1])
}


# score_homework()
# Description: Return the average of homework scores, dropping the lowest
#             score if necessary
# Inputs:  
#  hws:  A vector of homework scores
#  drop:  A logical argument, if TRUE, lowest score is dropped
# Outputs:
#  an average of homework scores
score_homework <- function(hws, drop = TRUE) {
  if (drop == TRUE) {
    hws <- drop_lowest(hws)
  }
  return(get_average(hws))
}

# score_quiz()
# Description: Return the average of quiz scores, dropping the lowest
#             score if necessary
# Inputs:  
#  hws:  A vector of quiz scores
#  drop:  A logical argument, if TRUE, lowest score is dropped
# Outputs:
#  an average of quiz scores
score_quiz <- function(quizzes, drop = TRUE) {
  if (drop == TRUE) {
    quizzes <- drop_lowest(quizzes)
  }
  return(get_average(quizzes))
}

# score_lab()
# Description: Return the lab score based on number of attendances
# Inputs: 
#  x: input value
# Outputs:
#  a lab score
score_lab <- function(x) {
  if (is.na(x)) {
    stop("Input a number")
  } else if (x == 11 | x == 12) {
    score <- 100
  } else if(x == 10) {
    score <- 80
  } else if(x == 9) {
    score <- 60
  } else if(x == 8) {
    score <- 40
  } else if(x == 7) {
    score <- 20
  } else if (x >= 0){
    score <- 0
  } 
  return(score)
}


