#Packages
list.of.packages <- c("data.table", "magrittr", "stringr", "dplyr", "ggplot2", "gridExtra", "grid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(data.table)
require(magrittr)
require(stringr)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(grid)


#####################################################################
################         MODIFY ACCORDINGLY          ################
#####################################################################

#Use your own path to the git repo
path <- 'C:/Users/Frank/OneDrive/Documents/git/MAST679_P3'

#Load file
play_by_play <- readRDS(paste(path, 'Data/Play_by_play_Database.rds', sep = '/'))

#Load functions
source(paste(path, 'R/Functions.r', sep = '/'))


#####################################################################
################ MARKOV CHAIN DATA FROM PLAY BY PLAY ################
#####################################################################

#Might take a while (10mins on a i7-12700k)
play_by_play <- lapply(play_by_play, process_data)

#Label states from 1 to 25
bases <- c("___", "1__", "_2_", "__3", "12_", "1_3", "_23", "123")
states <- as.data.table(rbind(expand.grid(x = bases, y = c(0, 1, 2)), c(NA, 3)))
names(states) <- c("Base", "Outs")
states[25, Base := "End"]

states[, n := c(1:nrow(states))]


#Add state number to data
for(i in 1:length(play_by_play)){

	names(states)[1:2] <- c("Base", "Outs")
	data.table::setkeyv(play_by_play[[i]], c("Base", "Outs"))
	play_by_play[[i]][states, i := i.n]

	names(states)[1:2] <- c("Next_base", "Next_outs")
	data.table::setkeyv(play_by_play[[i]], c("Next_base", "Next_outs"))
	play_by_play[[i]][states, j := i.n]

	setkey(play_by_play[[i]], NULL)
	setorderv(play_by_play[[i]], c("ID", "Action_n"))

}


#####################################################################
################   SAVE THE MARKOV AUGMENTED DATA    ################
#####################################################################

saveRDS(play_by_play, paste(path, 'Data/Markov_Chain_Data.rds', sep = '/'))
names(states)[1:2] <- c("Base", "Outs")
write.csv(states, paste(path, 'Data/Markov_Chain_States_Dict.csv', sep = '/'), row.names = FALSE)

