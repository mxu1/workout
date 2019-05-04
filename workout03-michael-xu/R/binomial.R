
# @title check_prob
# @description: checks that the input prob is between 0 and 1
check_prob <- function(prob) {
  if (prob >= 0 && prob <= 1)  {
    return(TRUE)
  }
  else {
    stop("invalid prob value")
  }
}

# @title check_trials
# @description: checks that trials is over 0
check_trials <- function(trials) {
  if ( trials > 0){
    return(TRUE)
  }
  else {
    stop("invalid trails")
  }
}

# @title check_success
# @description: checks that all inputs in success are valid
check_success <- function(success, trials) {
  for(i in success) {
    if (i < 0 || i > trials) {
      stop("invalid success value")
    }
  }
  return(TRUE)
}


aux_mean <- function(trial, prob) {
  return(trial*prob)
}

aux_variance <- function(trial, prob) {
  return (trial * prob * (1 - prob))
}

aux_mode <- function(trial, prob) {
  return(int(trial*prob + prob))
}

aux_skewness <- function(trial, prob) {
  return((1-2*prob)/sqrt(aux_variance(trial,prob)))
}

aux_kurtosis <- function(trial, prob) {
  return((1-6*prob*(1-prob)) / aux_variance(trial,prob))
}


#' @title bin_choose
#' @description returns n choose k and stops if the input is invalid
#' @param n, k
#' @return n choose k and stops if the input is invalid
#' @export
bin_choose <- function(n,k) {
  if (length(k) > 1) {
    for(i in k) {
      if(i > n) {
        stop("k cannot be greater than n")
      }
    }
  }
  else {
    if (k > n) {
      stop("k cannot be greater than n")
    }
  }
  top <- factorial(n)
  bottom <- factorial(k) * factorial(n-k)
  return(top/bottom)
}

#' @title bin_probability
#' @description returns the probability
#' @param success, trials, prob
#' @return the probability you get success of out trials with prob
#' @export
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(prob^success*(1-prob)^(trials-success)*bin_choose(trials, success))
}

#' @title bin_distribution
#' @description returns an object that has "success", which is seq 0 to trials and its respective probability
#' @param trials, prob
#' @return return an object "bindis" and "data.frame"
#' @export
bin_distribution <- function(trials, prob) {
  success <- seq(0,trials,by=1)
  probability <- c()
  for(i in success){
    probability <- c(probability, bin_probability(i,trials,prob))
  }

  df <- data.frame("success" = success, "probability" = probability)
  return(df)
}

#' @title plot.bindis
#' @description plots the bindis
#' @param dis1
#' @export
plot.bindis <- function(dis1) {
  plot(dis1)
}


#' @title bin_variable
#' @description
#' @param trials, prob
#' @return bin_distribution with an extra column "cumulative" that totals the previous probabilities
#' @export
bin_cumulative <- function(trials, prob) {
  df <- bin_distribution(trials, prob)
  cumulative <- c()
  for(i in seq(1,length(df[["probability"]]),by=1)) {
    print(df[["probability"]][[i]])
    if (i != 1) {
      cumulative <- c(cumulative, df[["probability"]][i] + cumulative[i-1])
    } else {
      cumulative <- c(cumulative, df[["probability"]][i])
    }
  }
  df$cumulative <- cumulative
  return(df)
}

#' @title plot.bincum
#' @description plots the bindis
#' @param dis2
#' @export
plot.bincum <- function(dis2) {
  plot(dis2)
}

#' @title bin_variable
#' @description
#' @param
#' @return
#' @export
#' @examples
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  object <- list(
    trials = trials,
    prob = prob
  )

  names(object) = c("trials", "prob")
  class(object) <- "binvar"

  return(object)
}

#' @title print.binvar
#' @description prints the binvar
#' @param object
#' @export
print.binvar <- function(object) {
  print("Parameters")
  print(paste0("- number of trials: ", object["trials"])
  print(paste0("- prob of success: ", object["prob"]))
}


#' @title summary.binvar
#' @description object that contains all the data about the binvar object
#' @param object
#' @export
summary.binvar <- function(object) {
  trials = object["trials"]
  prob = object["prob"]
  object <- list(
    trials = object["trials"],
    prob = object["prob"],
    mean = aux_mean(trials, prob),
    variance = aux_variance(trials, prob),
    mode = aux_mode(trials, prob),
    skewness = aux_skewness(trials, prob),
    kurtosis = aux_kurtosis(trials, prob)
  )
  names(object) = c("trials", "prob", "mean", "variance", "mode", "skewness", "kurtosis")
  class(object) <- "summary.binvar"

  return(object)
}

#' @title print.summary.binvar
#' @description prints out the object summary.binvar
#' @param object
#' @export
print.summary.binvar <- function(object) {
  print("Parameters")
  print(paste0("- number of trials: ", object["trials"])
  print(paste0("- prob of success: ", object["prob"]))
  print("")
  print("Measures")
  print(paste0("- mean    :", object["mean"])
  print(paste0("- variance: ", object["variance"]))
  print(paste0("- mode    : ", object["mode"])
  print(paste0("- skewness: ", object["skewness"]))
  print(paste0("- kurtosis: ", object["kurtosis"])
}

#' @title bin_mean
#' @description returns mean of t and p
#' @param t, p
#' @export
bin_mean <- function(t, p) {
  return(aux_mean(t,p))
}

#' @title bin_variance
#' @description returns variance of t and p
#' @param t, p
#' @export
bin_variance <- function(t, p) {
  return(aux_variance(t,p))
}

#' @title bin_mode
#' @description returns mode of t and p
#' @param t, p
#' @export
bin_mode <- function(t, p) {
  return(aux_mode(t,p))
}

#' @title bin_skewness
#' @description returns skewness of t and p
#' @param t, p
#' @export
bin_skewness <- function(t, p) {
  return(aux_skewness(t,p))
}

#' @title bin_kurtosis
#' @description returns kurtosis of t and p
#' @param t, p
#' @export
bin_kurtosis <- function(t, p) {
  return(aux_kurtosis(t,p))
}

