#Packages
list.of.packages <- c("data.table", "magrittr", "stringr", "dplyr", "ggplot2", "gridExtra", "grid", "doParallel", "microbenchmark", "combinat")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(data.table)
require(magrittr)
require(stringr)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(grid)
library(doParallel)
library(microbenchmark)
library(combinat)


#####################################################################
################         MODIFY ACCORDINGLY          ################
#####################################################################

#Use your own path the the git repo
path <- 'C:/Users/Frank/OneDrive/Documents/git/MAST679_P3'

#Load file
markov_data <- readRDS(paste(path, 'Data/Markov_Chain_Data.rds', sep = '/'))

#Load functions
source(paste(path, 'R/Functions.r', sep = '/'))


#####################################################################
################        EVALUTATION TOOL             ################
#####################################################################


#Picks a match and evaluate the team's batting order
eval_batting_order_by_E_points <- function(year, id){

	match_data <- markov_data[[as.character(year)]][ID == id]

	to <- as.Date(match_data$Date[1]) - 1
	from <- to - 2*365 

	innings <- sort(unique(match_data$Inn.))
	n <- length(innings)

	E_home <- rep(NA, n)
	E_away <- rep(NA, n)

	players_home_inn <- list()
	players_away_inn <- list()

	for(k in 1:n){

		players_home <- match_data[Inn. == k & Team_Bat == Team_Home & Move_n_player == 1]$Player
		players_away <- match_data[Inn. == k & Team_Bat == Team_Away & Move_n_player == 1]$Player

		if(length(players_home) == 0 | length(players_away) == 0){

			setTxtProgressBar(pb, k)
			next

		}

		m <- min(length(players_home), length(players_away))

		players_home_inn[[k]] <- players_home[1:m]
		players_away_inn[[k]] <- players_away[1:m]

	}

	message("Computing matrices and survival functions...")

	all_players_home <- unique(unlist(players_home_inn))
	all_players_away <- unique(unlist(players_away_inn))

	survival_f_home <- sapply(all_players_home, function(x){compute_w_weights(x, from, to)})
	survival_f_away <- sapply(all_players_away, function(x){compute_w_weights(x, from, to)})

	if(class(survival_f_home)[1] == "matrix"){

		temp <- list()
		for(j in 1:ncol(survival_f_home)){

			temp[[j]] <- survival_f_home[, j]

		}

		names(temp) <- all_players_home
		survival_f_home <- temp

	}

	if(class(survival_f_away)[1] == "matrix"){

		temp <- list()
		for(j in 1:ncol(survival_f_away)){

			temp[[j]] <- survival_f_away[, j]

		}

		names(temp) <- all_players_away
		survival_f_away <- temp

	}

	T_mat_home <- lapply(as.list(all_players_home), function(x){compute_transition_mat(x, from, to)})
	T_mat_away <- lapply(as.list(all_players_away), function(x){compute_transition_mat(x, from, to)})

	names(T_mat_home) <- all_players_home
	names(T_mat_away) <- all_players_away

	if(any(which(is.na(survival_f_home)) | any(is.na(T_mat_home)) | any(is.na(survival_f_away)) | any(is.na(T_mat_away)))){

		warning("Missing player information. Cannot compute scores.")
		return(NA)

	}

	message("Computing scores...")
	for(k in 1:n){

		if(length(players_home_inn) < k | length(players_away_inn) < k){

			break

		}

		players_home <- players_home_inn[[k]]
		players_away <- players_away_inn[[k]]

		if(length(players_home) == 0 | length(players_away) == 0){

			setTxtProgressBar(pb, k)
			next

		}

		m <- min(length(players_home), length(players_away))

		players_home <- players_home[1:m]
		players_away <- players_away[1:m]

		E_home[k] <- E_points(players_home, survival_f_home[players_home], T_mat_home[players_home], points_matrix)
		E_away[k] <- E_points(players_away, survival_f_away[players_away], T_mat_away[players_away], points_matrix)


	}

	E_home <- E_home[which(!is.na(E_home))]
	E_away <- E_away[which(!is.na(E_away))]

	out <- match_data[nrow(match_data), c("Score_Home", "Score_Away"), with = FALSE] %>%
						.[, E_Home := mean(E_home)] %>%
						.[, E_Away := mean(E_away)]

	print(out)

	return(out)

}