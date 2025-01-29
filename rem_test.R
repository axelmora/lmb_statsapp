data_ru <- event_without_fielder_id

data_ru$half_inning <- with(data_ru, paste(game_id, inning, half_inning))

#RUNS.SCORED = runs_on_event

RUNS.SCORED.INNING <- aggregate(data_ru$runs_on_event, list(HALF.INNING = data_ru$half_inning), sum)

RUNS.SCORED.START <- aggregate(data_ru$runs, list(HALF.INNING = data_ru$half_inning), "[", 1)

MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data_ru <- merge(data_ru, MAX)
N <- ncol(data_ru)
names(data_ru)[N] <- "MAX.RUNS"

data_ru$RUNS_ROI <- with(data_ru, MAX.RUNS - runs)

RUNNER1 <- ifelse(as.character(data_ru[ , "pre_runner_1b_id"]) == "", 0, 1)
RUNNER2 <- ifelse(as.character(data_ru[ , "pre_runner_2b_id"]) == "", 0, 1)
RUNNER3 <- ifelse(as.character(data_ru[ , "pre_runner_3b_id"]) == "", 0, 1)

RUNNER1[is.na(RUNNER1)] <- 0
RUNNER2[is.na(RUNNER2)] <- 0
RUNNER3[is.na(RUNNER3)] <- 0

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}

data_ru$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data_ru$pre_outs)

NRUNNER1 <- ifelse(as.character(data_ru[ , "post_runner_1b_id"]) == "", 0, 1)
NRUNNER2 <- ifelse(as.character(data_ru[ , "post_runner_2b_id"]) == "", 0, 1)
NRUNNER3 <- ifelse(as.character(data_ru[ , "post_runner_3b_id"]) == "", 0, 1)
NOUTS <- with(data_ru, pre_outs + post_outs)

NRUNNER1[is.na(NRUNNER1)] <- 0
NRUNNER2[is.na(NRUNNER2)] <- 0
NRUNNER3[is.na(NRUNNER3)] <- 0

data_ru$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

data_ru <- subset(data_ru, (STATE != NEW.STATE) | (runs_on_event > 0))

library(plyr)

data.outs <- ddply(data_ru, .(HALF.INNING), summarize,
                   Outs.Inning=sum(post_outs))
data_ru <- merge(data_ru, data.outs)
data_ruC <- subset(data_ru, Outs.Inning == 3)

RUNS <- with(data_ruC, aggregate(RUNS.ROI, list(STATE), mean))

RUNS$Outs <- substr(RUNS$Group, 5, 5)
RUNS <- RUNS[order(RUNS$Outs), ]

RUNS.Out <- matrix(round(RUNS$x, 2), 8, 3)
dimnames(RUNS.out)[[2]] <- c("0 Outs", "1 Out", "2 Outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")

