#####################################################################
################ MARKOV CHAIN DATA FROM PLAY BY PLAY ################
#####################################################################

#Add state transition data
process_data <- function(df){

	#Action number
	df[, Action_n := lapply(.SD, function(x){c(1:length(x))}), by = c("ID"), .SDcol = "Base"]

	#Transition number
	df[, Move_n := lapply(.SD, function(x){c(1:length(x))}), by = c("ID", "Inn.", "Team_Bat"), .SDcol = "Base"]

	#Transition number, players
	df[, Move_n_player := lapply(.SD, function(x){c(1:length(x))}), by = c("ID", "Inn.", "Team_Bat", "Player"), .SDcol = "Base"]

	#Batting transition number
	df[, Move_n_batting := lapply(.SD, function(x){

		n <- length(x)
		sapply(c(1:n), function(i){length(unique(x[1:i]))})

		}), by = c("ID", "Inn.", "Team_Bat"), .SDcol = "Player"]


	#Fix an error in the data where the batting and pitching team as missendified
	df[, Team_playing := lapply(.SD, function(x){

		switch_at <- which(diff(x) < 0)

		if(length(switch_at) > 0){

			switch_at <- switch_at[1]
			out <- rep(0, length(x))
			out[-c(1:switch_at)] <- 1
			out

		} else {

			rep(0, length(x))

		}

		}), by = c("ID", "Inn."), .SDcol = "Outs"]


	df[Team_playing == 0, Team_Bat := Team_Away] %>%
		.[Team_playing == 1, Team_Bat := Team_Home] %>%
		.[Team_playing == 0, Team_Pitch := Team_Home] %>%
		.[Team_playing == 1, Team_Pitch := Team_Away] 


	#Score for pitching and batting team
	df[Team_Home == Team_Pitch, Score_Pitch := Score_Home]
	df[Team_Away == Team_Pitch, Score_Pitch := Score_Away]

	df[Team_Home == Team_Bat, Score_Bat := Score_Home]
	df[Team_Away == Team_Bat, Score_Bat := Score_Away]	

	#Points scored
	df[, Points_scored_bat := lapply(.SD, function(x){c(0, diff(x))}), by = c("ID", "Inn.", "Team_Bat"), .SDcol = "Score_Bat"]

	#Transitions tied to batting
	df[, Is_batting := (Move_n_player == 1)]

	#Base stealing
	df[, Is_stealing := grepl("STEAL|STOLEN", toupper(Play))]

	#Final move
	df[, Is_final := lapply(.SD, function(x){

			n <- length(x)
			c(rep(FALSE, n-1), TRUE)

		}), by = c("ID", "Inn.", "Team_Bat"), .SDcol = "Base"]

	#Next base state
	df[, Next_base := c(Base[2:length(Base)], "End")] %>%
		.[Is_final == TRUE, Next_base := "End"]

	#Next n_out state
	df[, Next_outs := c(Outs[2:length(Outs)], 3)] %>%
		.[Is_final == TRUE, Next_outs := 3]


	#Player who performed the steal
	df[Is_stealing == TRUE, Player_stealing := sapply(Play,function(x){

		x <- str_split(x, "\\.")[[1]]
		x <- x[grepl("steal|stolen", x)][1]

		splt <- str_split(x, " ")[[1]]
		is_capital <- sapply(splt, function(x){substr(x, 1, 1) == toupper(substr(x, 1, 1)) & !grepl("\\d", x)})
		splt <- splt[which(is_capital)]

		out <- substr(splt[1], 1, 1)
		for(i in 2:length(splt)){

			out <- paste(out, splt[i])

		}

		str_replace(out, " Jr$", " Jr\\.")

		})]


	#Players who exited the play
	df[Outs < Next_outs, Player_out := sapply(Play,function(x){

		x <- str_split(x, "\\.")[[1]]
		x <- x[grepl("scored|out|caught|sacrificed|homered|Error|interference|picked off|balked|score", x)]

		if(length(x) == 0){

			NA

		} else {

			x <- sapply(x, function(s){

				splt <- str_split(str_trim(s), " ")[[1]]
				is_capital <- sapply(splt, function(x){substr(x, 1, 1) == toupper(substr(x, 1, 1)) & !grepl("\\d", x)})
				splt <- splt[which(is_capital)]

				out <- substr(splt[1], 1, 1)
				for(i in 2:length(splt)){

					out <- paste(out, splt[i])

				}

				out <- str_trim(str_replace(out, "\\(\\D+\\)", ""))
				return(as.character(str_replace(out, " Jr$", " Jr\\.")))

				})

			names(x) <- NULL
			c(x)

		}

		})]


	#Delete matches with nonsensical transitions
	#There are very few of them and tbh I don't know where the error stems from
	to_delete <- unique(df[Next_outs < Outs]$ID)
	df <- df[!(ID %in% to_delete)]


}


#####################################################################
################    TRANSITION MATRIX VALIDATION     ################
#####################################################################

#Remove impossible states from a transition matrix
remove_imp_states <- function(M, is_M = TRUE, verif_matrix, verif_matrix_steal){

	if(is_M){

		M <- M * verif_matrix

	} else {

		M <- M * verif_matrix_steal

	}

	for(i in 1:nrow(M)){

		s <- sum(M[i, ])
		if(s > 0){

			M[i, ] <- M[i, ] / s

		} else {

			M[i, i] <- 1

		}

	}

	M[25, ] <- 0 
	M[25, 25] <- 1

	return(M)

}


#####################################################################
################    SURVIVAL FUNCTION OF PLAYERS     ################
#####################################################################

#Compute P(Player still in play after n transitions have occured)
compute_w_weights <- function(player, from, to, markov_data){

	min_date <- as.Date(from)
	max_date <- as.Date(to)

	from <- year(min_date)
	to <- year(max_date)

	values <- list()
	for(y in from:to){

		markov_data[[as.character(y)]][, Eliminated := sapply(Player_out, function(x){player %in% x})]

		involved <- markov_data[[as.character(y)]][(Player == player | Eliminated == TRUE) & Date %between% c(min_date, max_date)]
		if(nrow(involved) == 0){next}

		ID_frame <- unique(involved[, c("ID", "Inn."), with = FALSE])
		setkeyv(markov_data[[as.character(y)]], c("ID", "Inn."))
		setkeyv(ID_frame, c("ID", "Inn."))

		involved <- markov_data[[as.character(y)]][ID_frame] %>%
										.[, N_transitions := max(Move_n_batting), by = c("ID", "Inn.")] %>%
										.[Player == player | Eliminated == TRUE]

		markov_data[[as.character(y)]][, Eliminated := NULL]

		#1st case: batted without being eliminated. Chain length is number of transitions - when player entered
		#2nd case: eliminated at bat. Chain length is 0
		#3rd case: eliminated on bases. Chain length is finish - start
		involved[, Started_at := min(Move_n_batting), by = c("ID", "Inn.")] %>%
				 .[Eliminated == FALSE & Move_n_player == 1, N_transitions_by_player := N_transitions - Move_n_batting] %>%
				 .[Eliminated == TRUE & Move_n_player == 1, N_transitions_by_player := 0] %>%
				 .[Eliminated == TRUE & Move_n_player > 1, N_transitions_by_player := Move_n_batting - Started_at] 

		n_transitions <- involved[!is.na(N_transitions_by_player)]$N_transitions_by_player
		if(length(n_transitions) == 0){next}

		out <- c(0:max(n_transitions))
		for(i in 1:length(out)){

			out[i] <- length(which(n_transitions == out[i]))

		}

		values[[length(values) + 1]] <- out
		setorderv(markov_data[[as.character(y)]], c("Date", "ID", "Action_n"))

	}

	if(length(values) == 0){

		return(NA)

	}

	out <- rep(0, max(unlist(lapply(values, length))))
	for(v in values){

		n <- length(v)
		out[1:n] <- out[1:n] + v

	}

	if(length(out) < 9){

		n <- length(out)
		out <- c(out, rep(0, 9 - n))

	}

	return(1 - cumsum(out / sum(out)))

}


#####################################################################
################   TRANSITION MATRICES OF PLAYERS    ################
#####################################################################

compute_transition_mat  <- function(player, from, to, markov_data, verif_matrix, verif_matrix_steal){

	min_date <- as.Date(from)
	max_date <- as.Date(to)

	from <- year(min_date)
	to <- year(max_date)

	M <- matrix(0, 25, 25)
	colnames(M) <- c(1:25)
	rownames(M) <- c(1:25)

	W <- M

	n_succes_at_bat <- 0

	for(y in from:to){

		M_transitions <- markov_data[[as.character(y)]][Player == player & Move_n_player == 1 & Date %between% c(min_date, max_date)] %>%
														.[, Eliminated := sapply(Player_out, function(x){player %in% x})]

		W_transitions <- markov_data[[as.character(y)]][Player_stealing == player & Move_n_player > 1 & Date %between% c(min_date, max_date)]

		if(nrow(M_transitions) > 0){

			for(i in 1:nrow(M_transitions)){

				M[M_transitions$i[i], M_transitions$j[i]] <- M[M_transitions$i[i], M_transitions$j[i]] + 1

				n_succes_at_bat <- n_succes_at_bat + nrow(M_transitions) - sum(M_transitions$Eliminated)

			}


		}

		if(nrow(W_transitions) > 0){

			for(i in 1:nrow(W_transitions)){

				W[W_transitions$i[i], W_transitions$j[i]] <- W[W_transitions$i[i], W_transitions$j[i]] + 1

			}


		}

	}

	if(sum(M) == 0){

		return(NA)

	}

	#How often does the player attempt to steal bases?
	p_steal_attempt <- sum(W) / n_succes_at_bat
	
	for(i in 1:24){

		s <- sum(M[i, ])
		if(s != 0){

			M[i, ] <- M[i, ] / sum(M[i, ])

		} else {

			M[i, i] <- 1

		}

		s <- sum(W[i, ])
		if(s != 0){

			W[i, ] <- W[i, ] / sum(W[i, ])

		} else {

			W[i, i] <- 1

		}

	}

	M[25, 25] <- 1
	W[25, 25] <- 1

	M <- remove_imp_states(M, TRUE, verif_matrix, verif_matrix_steal)
	W <- remove_imp_states(W, TRUE, verif_matrix, verif_matrix_steal)

	return(list(M = M, W = W, p_steal_attempt = p_steal_attempt))

}



#####################################################################
################       EXPECTED CHAIN LENGTH         ################
#####################################################################

E_runtime <- function(players, survival_f, T_mat){

	E_X = 0

	survival_f <- survival_f[players]
	T_mat <- T_mat[players]	

	T <- diag(rep(1, 25))
	I <- T
	for(i in 1:length(T_mat)){

		#M matrix
		T <- T %*% T_mat[[i]]$M 
		#W matrix
		weights <- 0
		p_steal_attempt <- 0 

		#Build the W matrix		
		W <- matrix(0, 25, 25)
		for(j in 1:i){

			#P(player j is still in play)
			p_survival <- survival_f[[j]][i - j + 1]

			weights <- weights + p_survival
			#Total P(base steal), sum of product of P(player j still in play) and P(player j attempts steal)
			p_steal_attempt <- p_steal_attempt + p_survival * T_mat[[j]]$p_steal_attempt
			W <- W + p_survival * T_mat[[j]]$W

		}

		W <- W / weights
		W <- p_steal_attempt * W + (1-p_steal_attempt) * I

		T <- T %*% W

		E_X <- E_X + (1 - T[1,25])

	}

	return(E_X)

}



E_points <- function(players, survival_f, T_mat, points_matrix){

	E_X = 0

	survival_f <- survival_f[players]
	T_mat <- T_mat[players]	

	I <- diag(rep(1, 25))

	v <- rep(0, 25)
	v[1] <- 1

	#First round
	E_X <- E_X + sum(v %*% (T_mat[[1]]$M * points_matrix))
	v <- v %*% T_mat[[1]]$M

	for(i in 1:(length(T_mat) - 1)){

		#W matrix
		weights <- 0
		p_steal_attempt <- 0 

		#Build the W matrix		
		W <- matrix(0, 25, 25)
		for(j in 1:i){

			#P(player j is still in play)
			p_survival <- survival_f[[j]][i - j + 1]

			weights <- weights + p_survival
			#Total P(base steal), sum of product of P(player j still in play) and P(player j attempts steal)
			p_steal_attempt <- p_steal_attempt + p_survival * T_mat[[j]]$p_steal_attempt
			W <- W + p_survival * T_mat[[j]]$W

		}

		W <- W / weights
		W <- p_steal_attempt * W + (1-p_steal_attempt) * I

		E_X <- E_X + sum(v %*% ((W %*% T_mat[[i + 1]]$M) * points_matrix))
		v <- v %*% W %*% T_mat[[i + 1]]$M 

	}

	return(E_X)

}




#####################################################################
################              OPTIMIZER              ################
#####################################################################


bruteforce_optim_runtime <- function(survival_f, T_mat, fun, ncores = NULL, plot_title = NULL){

	if(is.null(ncores)){

		ncores <- detectCores() - 4

	} else {

		ncores <- min(ncores, detectCores() - 1)

	}

	players <- names(survival_f)
	orderings <- combinat::permn(players)

	message(paste("Brute-forcing with", ncores, "cores..."))

	cl <- makeCluster(ncores, type = "PSOCK") 
	registerDoParallel(cl)

		E_X <- foreach(i = 1:length(orderings)) %dopar% fun(orderings[[i]], survival_f, T_mat)

	stopCluster(cl)

	#Isolate best runtime
	E_X <- unlist(E_X)

	best <- which.max(E_X)
	chain_runtime <- E_X[best]
	players <- orderings[[best]]

	#Descriptive stats
	E_X_mean <- mean(E_X)
	E_X_sd <- sd(E_X)
	n_sd_above_mean <- (chain_runtime - E_X_mean) / E_X_sd

	#Plot density
	dframe <- as.data.table(E_X)
	names(dframe) <- "Runtime"
	density <- ggplot(dframe, aes(x = Runtime)) + 
				geom_density(alpha = 0.5, fill = "blue") +
				ggtitle(plot_title) +
				ylab("Density") +
				xlab("Markov chain runtime")

	message("Done. See results below.")
	message(paste("Best runtime:", round(chain_runtime, 2)))
	message(paste("Number of sdevs above mean:", round(n_sd_above_mean, 2)))

	return(list(best_ordering = players,
				best_runtime = chain_runtime,
				runtime_mean = E_X_mean,
				runtime_sd = E_X_sd,
				n_sd_above_mean = n_sd_above_mean,
				density_plot = density,
				values = dframe))

}


bruteforce_optim_points <- function(survival_f, T_mat, fun, points_matrix, ncores = NULL, plot_title = NULL){

	if(is.null(ncores)){

		ncores <- detectCores() - 4

	} else {

		ncores <- min(ncores, detectCores() - 1)

	}

	players <- names(survival_f)
	orderings <- combinat::permn(players)

	message(paste("Brute-forcing with", ncores, "cores..."))

	cl <- makeCluster(ncores, type = "PSOCK") 
	registerDoParallel(cl)

		E_X <- foreach(i = 1:length(orderings)) %dopar% fun(orderings[[i]], survival_f, T_mat, points_matrix)

	stopCluster(cl)

	#Isolate best runtime
	E_X <- unlist(E_X)

	best <- which.max(E_X)
	chain_runtime <- E_X[best]
	players <- orderings[[best]]

	#Descriptive stats
	E_X_mean <- mean(E_X)
	E_X_sd <- sd(E_X)
	n_sd_above_mean <- (chain_runtime - E_X_mean) / E_X_sd

	#Plot density
	dframe <- as.data.table(E_X)
	names(dframe) <- "Runtime"
	density <- ggplot(dframe, aes(x = Runtime)) + 
				geom_density(alpha = 0.5, fill = "blue") +
				ggtitle(plot_title) +
				ylab("Density") +
				xlab("E[Points]")

	message("Done. See results below.")
	message(paste("Best E[Points]:", round(chain_runtime, 2)))
	message(paste("Number of sdevs above mean:", round(n_sd_above_mean, 2)))

	return(list(best_ordering = players,
				best_points = chain_runtime,
				points_mean = E_X_mean,
				points_sd = E_X_sd,
				n_sd_above_mean = n_sd_above_mean,
				density_plot = density,
				values = dframe))

}


#####################################################################
################       EXTRACT TOP 9 BATTERS         ################
#####################################################################

#Extract the top 9 batters for a given team and year
extract_top_9_batters_data <- function(team, year, n_year_T = 2, markov_data, verif_matrix, verif_matrix_steal){

	#Top 9 batters
	n_appearances <- markov_data[[as.character(year)]][Team_Bat == team, lapply(.SD, length), by = "Player", .SDcol = "Player"] 
	names(n_appearances)[2] <- "n"
	top_9 <- n_appearances[order(-n)] %>%
							.[c(1:9)]

	players <- top_9$Player

	from <- as.Date(paste(year - n_year_T + 1, "-01-01", sep = ""))
	to <- as.Date(paste(year, "-12-31", sep = ""))


	survival_f <- sapply(players, function(x){compute_w_weights(x, from, to, markov_data)})
	T_mat <- lapply(as.list(players), function(x){compute_transition_mat(x, from, to, markov_data, verif_matrix, verif_matrix_steal)})
	names(T_mat) <- players

	if(class(survival_f)[1] == "matrix"){

		temp <- list()
		for(j in 1:ncol(survival_f)){

			temp[[j]] <- survival_f[, j]

		}

		names(temp) <- players
		survival_f <- temp

	}

	missing_s <- is.na(survival_f)
	missing_T <- is.na(T_mat)

	if(any(missing_s | missing_T)){

		message("Error: Missing data", quote = FALSE)
		message(cat("Survival:", names(missing_s)[which(missing_s)]), quote = FALSE)
		message(cat("T-Mat:", names(missing_T)[which(missing_T)]), quote = FALSE)

		return(NA)

	} else {

		return(list(survival_f = survival_f,
					T_mat = T_mat))

	}

}


#####################################################################
################        EVALUTATION TOOL             ################
#####################################################################


#Picks a match and computes E[points per innings] of the announced lineup
eval_batting_order_by_E_points <- function(id, lineup_data, markov_data, verif_matrix, verif_matrix_steal, points_matrix){

	match_data <- lineup_data[ID == id]

	all_players_home <- match_data[Team == Team_Home]$Name
	all_players_away <- match_data[Team == Team_Away]$Name

	year <- year(match_data$Date[1])

	player_home <- markov_data[[as.character(year)]][Batter_Name %in% all_players_home & Move_n_player == 1, lapply(.SD, length), by = c("Batter_Name", "Player"), .SDcol = "Base"] %>%
					.[, Is_Max := lapply(.SD, function(x){

						out <- rep(0, length(x))
						out[which.max(x)] <- 1 
						out

						}), by = "Batter_Name", .SDcol = "Base"] %>%
					.[Is_Max == 1] %>%
					.[match(all_players_home, Batter_Name)]

	all_players_home <- player_home$Player

	player_away <- markov_data[[as.character(year)]][Batter_Name %in% all_players_away & Move_n_player == 1, lapply(.SD, length), by = c("Batter_Name", "Player"), .SDcol = "Base"] %>%
					.[, Is_Max := lapply(.SD, function(x){

						out <- rep(0, length(x))
						out[which.max(x)] <- 1 
						out

						}), by = "Batter_Name", .SDcol = "Base"] %>%
					.[Is_Max == 1] %>%
					.[match(all_players_away, Batter_Name)]

	all_players_away <- player_away$Player

	if(length(all_players_home) != 9 | length(all_players_away) != 9){

		warning("Missing players")
		return(NA)

	}

	message("Computing matrices and survival functions...")

	from <- match_data$Date[1] - 2*365
	to <- match_data$Date[1] - 1

	survival_f_home <- sapply(all_players_home, function(x){compute_w_weights(x, from, to, markov_data)})
	survival_f_away <- sapply(all_players_away, function(x){compute_w_weights(x, from, to, markov_data)})

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

	T_mat_home <- lapply(as.list(all_players_home), function(x){compute_transition_mat(x, from, to, markov_data, verif_matrix, verif_matrix_steal)})
	T_mat_away <- lapply(as.list(all_players_away), function(x){compute_transition_mat(x, from, to, markov_data, verif_matrix, verif_matrix_steal)})

	names(T_mat_home) <- all_players_home
	names(T_mat_away) <- all_players_away

	if(any(which(is.na(survival_f_home)) | any(is.na(T_mat_home)) | any(is.na(survival_f_away)) | any(is.na(T_mat_away)))){

		warning("Missing player information. Cannot compute scores.")
		return(NA)

	}

	message("Computing scores...")

	E_home <- E_points(all_players_home, survival_f_home, T_mat_home, points_matrix)
	E_away <- E_points(all_players_away, survival_f_away, T_mat_away, points_matrix)	

	out <- match_data[nrow(match_data), c("Score_Home", "Score_Away"), with = FALSE] %>%
						.[, E_Home := E_home] %>%
						.[, E_Away := E_away]

	print(out)

	return(out)

}