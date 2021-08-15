# This is the data management file for the supplemental instruction
# project. Fortunately, only minor changes were required because the data were 
# used in a previous project. 


path <- ".../"

# load data
load(paste0(path, "si_omit_clean.Rdata"))

# rename variables, change factors to numeric variables, and select 
# variables of interest
mod.data$class <- paste(mod.data$Subject, mod.data$Number, sep = "-")
mod.data$term <- as.numeric(mod.data$TERM_LDESC)
mod.data$gender <- as.numeric(mod.data$Gender_Desc) - 1
mod.data$si <- as.numeric(mod.data$SI_STUDENT) - 1
mod.data$dfw <- as.numeric(mod.data$dfw) - 1
mod.data$college <- as.numeric(mod.data$college)
mod.data$soc <- ifelse(mod.data$SOC == "SOC", 1, 0)
mod.data$class <- as.numeric(as.factor(mod.data$class))
mod <- mod.data %>% select(-Subject, -Number, -FirstGeneration, -PELL_Eligible, -TERM_LDESC, -Gender_Desc, -SI_STUDENT, -SOC)

# save cleaned data
save(mod, file=paste0(path, "si_bayes_clean.Rdata"))