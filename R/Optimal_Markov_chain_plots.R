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
################    TRANSITION MATRIX VALIDATION     ################
#####################################################################

#Checks for impossible transitions
#Might have some errors due to incorrect data
states <- fread(paste(path, 'Data/Markov_Chain_States_Dict.csv', sep = '/'))
states[, Players := sapply(Base, function(x){length(str_extract_all(x, "\\d{1}")[[1]])})]

#0 = impossible, 1 = possible
verif_matrix <- matrix(1, 25, 25)
for(i in 1:25){

	for(j in 1:25){

		#Can add at most 1 player on base
		#Cannot reduce the number of eliminated players
		if(states$Player[j] > states$Player[i] + 1 | states$Outs[j] < states$Outs[i]){

			verif_matrix[i, j] <- 0

		}

		#Maximum number of players elimnated = players on base + 1
		if(states$Outs[j] - states$Outs[i] > states$Player[i] - states$Player[j] + 1){

			verif_matrix[i, j] <- 0

		}		

	}

}


verif_matrix_steal <- matrix(1, 25, 25)
for(i in 1:25){

	for(j in 1:25){

		#Cannot add a player on base
		#Cannot reduce the number of eliminated players
		if(states$Player[j] > states$Player[i] | states$Outs[j] < states$Outs[i]){

			verif_matrix_steal[i, j] <- 0

		}

		#Maximum number of players elimnated = players on base 
		if(states$Outs[j] - states$Outs[i] > states$Player[i] - states$Player[j]){

			verif_matrix_steal[i, j] <- 0

		}	


	}

}


saveRDS(verif_matrix, paste(path, 'Data/verif_matrix.rds', sep = '/'))
saveRDS(verif_matrix_steal, paste(path, 'Data/verif_matrix_steal.rds', sep = '/'))

#####################################################################
################            POINT MATRIX             ################
#####################################################################

#How many points does a transition award?
points_matrix <- matrix(0, 25, 25)
for(i in 1:25){

	for(j in 1:25){

		#Number of player who exited the play - number of players eliminated
		points_matrix[i, j] <- max((states$Players[i] - states$Players[j]) - (states$Out[j] - states$Out[i]), 0)

	}

}

points_matrix <- points_matrix * verif_matrix
saveRDS(points_matrix, paste(path, 'Data/points_matrix.rds', sep = '/'))


#####################################################################
################         PLOTS AND TABLE             ################
#####################################################################

#Points
SLN <- extract_top_9_batters_data("SLN", 2015, 2, markov_data, verif_matrix, verif_matrix_steal)
CLE <- extract_top_9_batters_data("CLE", 2015, 2, markov_data, verif_matrix, verif_matrix_steal)
PHI <- extract_top_9_batters_data("PHI", 2015, 2, markov_data, verif_matrix, verif_matrix_steal)


SLN_optim <- bruteforce_optim_points(SLN$survival_f, SLN$T_mat, fun = E_points, points_matrix,
										plot_title = "SLN, 2014-2015 seasons")
CLE_optim <- bruteforce_optim_points(CLE$survival_f, CLE$T_mat, fun = E_points, points_matrix,
										plot_title = "CLE, 2014-2015 seasons")
PHI_optim <- bruteforce_optim_points(PHI$survival_f, PHI$T_mat, fun = E_points, points_matrix,
										plot_title = "PHI, 2014-2015 seasons")

all_values <- cbind(SLN_optim$values, CLE_optim$values, PHI_optim$values)
names(all_values) <- c("SLN", "CLE", "PHI")

density_plot <- ggplot(data = melt(all_values), aes(x = value, fill = variable)) +
					geom_density(alpha = 0.5) +
					xlab("E[Points]") +
					ylab("Density") +
					scale_fill_discrete(name = "Team")


info_table <- as.data.table(matrix(NA, 3, 5))
names(info_table) <- c("Team", "Ranking", "E[Points]", "Max(Points)", "Z-Score")

info_table[, Team := c("SLN", "CLE", "PHI")] %>%
			.[, Ranking := c(1, 15, 30)] %>%
			.[, "E[Points]" := sapply(c(SLN_optim$points_mean, CLE_optim$points_mean, PHI_optim$points_mean), function(x){round(x, 2)})] %>%
			.[, "Max(Points)" := sapply(c(SLN_optim$best_points, CLE_optim$best_points, PHI_optim$best_points), function(x){round(x, 2)})] %>%
			.[, 'Z-Score' := sapply(c(SLN_optim$n_sd_above_mean, CLE_optim$n_sd_above_mean, PHI_optim$n_sd_above_mean), function(x){round(x, 2)})] 


grid.table(info_table, rows = NULL)