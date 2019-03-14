# title: make-shots-data-script.R
# description: convert raw data to to a global table
# inputs(s): 5 data tables
# output(s): 1 global table
iguodala  <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green  <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant  <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson  <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry  <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)

iguodala$name = "Andre Iguodala"
green$name = "Draymon Green"
durant$name = "Kevin Durant"
thompson$name = "Klay Thompson"
curry$name = "Stephen Curry"

iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"

green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"

durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"

thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"

curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"

iguodala$minute <- 12 * (iguodala$period) - iguodala$minutes_remaining
green$minute <- 12 * (green$period) - green$minutes_remaining
durant$minute <- 12 * (durant$period) - durant$minutes_remaining
thompson$minute <- 12 * (thompson$period) - thompson$minutes_remaining
curry$minute <- 12 * (curry$period) - curry$minutes_remaining


f <- write(summary(iguodala), "../output/andre-iguodala-summary.txt")
sink(file = f)
f <- write(summary(green), "../output/draymond-green-summary.txt")
sink(file = f)
f <- write(summary(durant), "../output/kevin-durant-summary.txt")
sink(file = f)
f <- write(summary(thompson), "../output/klay-thompson-summary.txt")
sink(file = f)
f <- write(summary(curry), "../output/stephen-curry-summary.txt")
sink(file = f)

gsw <- rbind(iguodala, green, durant, thompson, curry)
f <- write.csv(gsw, "../data/shots-data.csv")

f <- write(summary(gsw), "../output/shots-data-summary.txt")
sink(file = f)

# gsw <- data.frame("Andre Iguodala" = iguodala,  "Draymon Green" = green, "Kevin Durant" = durant, "Klay Thompson" = thompson, "Stephen Curry" = curry)

