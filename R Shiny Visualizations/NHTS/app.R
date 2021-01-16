#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(rstudioapi)
# current_path = getActiveDocumentContext()$path
# setwd(dirname(current_path))

library(readxl)
# hh <- read.csv("data/hhpub.csv", stringsAsFactors=FALSE)
# per <- read.csv("data/perpub.csv", stringsAsFactors=FALSE)

hh <- read.csv(unz("data.zip", "hhpub.csv"), stringsAsFactors = F)
per <- read.csv(unz("data.zip", "perpub.csv"), stringsAsFactors = F)

hh$CNTTDHH_weighted = hh$CNTTDHH * hh$WTHHFIN
hh$HHSIZE_weighted = hh$HHSIZE * hh$WTHHFIN
per$CNTTDTR_weighted = per$CNTTDTR * per$WTPERFIN

###########
# HH Size #
###########

# Households Unweighted
hh.by.size_unweighted.freq <- table(hh$HH_CBSA, hh$HHSIZE)
hh.by.size_unweighted.freq[,4] <- rowSums(hh.by.size_unweighted.freq[,4:13])
hh.by.size_unweighted.freq <- hh.by.size_unweighted.freq[,-c(5:13)]
colnames(hh.by.size_unweighted.freq)[4] <- "4+"
hh.by.size_unweighted.prop <- hh.by.size_unweighted.freq/rowSums(hh.by.size_unweighted.freq) * 100
hh.by.size_unweighted.freq <- as.data.frame.matrix(hh.by.size_unweighted.freq)
hh.by.size_unweighted.prop <- as.data.frame.matrix(hh.by.size_unweighted.prop)

# Trips Unweighted
trips.by.size_unweighted.freq <- tapply(X=hh$CNTTDHH, INDEX=list(hh$HH_CBSA, hh$HHSIZE), FUN=sum)
trips.by.size_unweighted.freq[is.na(trips.by.size_unweighted.freq)] <- 0
trips.by.size_unweighted.freq[,4] <- rowSums(trips.by.size_unweighted.freq[,4:13])
trips.by.size_unweighted.freq <- trips.by.size_unweighted.freq[,1:4]
colnames(trips.by.size_unweighted.freq)[4] <- "4+"
trips.by.size_unweighted.prop <- trips.by.size_unweighted.freq/rowSums(trips.by.size_unweighted.freq) * 100
trips.by.size_unweighted.freq <- as.data.frame.matrix(trips.by.size_unweighted.freq)
trips.by.size_unweighted.prop <- as.data.frame.matrix(trips.by.size_unweighted.prop)

# Household Trip Rate Unweighted
tripRate.hh.by.size_unweighted <- trips.by.size_unweighted.freq/ hh.by.size_unweighted.freq

# Persons All Ages Unweighted
per_all.by.size_unweighted.freq <- tapply(X=hh$HHSIZE, INDEX=list(hh$HH_CBSA, hh$HHSIZE), FUN=sum)
per_all.by.size_unweighted.freq[is.na(per_all.by.size_unweighted.freq)] <- 0
per_all.by.size_unweighted.freq[,4] <- rowSums(per_all.by.size_unweighted.freq[,4:13])
per_all.by.size_unweighted.freq <- per_all.by.size_unweighted.freq[,1:4]
colnames(per_all.by.size_unweighted.freq)[4] <- "4+"
per_all.by.size_unweighted.prop <- per_all.by.size_unweighted.freq/rowSums(per_all.by.size_unweighted.freq) * 100
per_all.by.size_unweighted.freq <- as.data.frame.matrix(per_all.by.size_unweighted.freq)
per_all.by.size_unweighted.prop <- as.data.frame.matrix(per_all.by.size_unweighted.prop)

# Person Trip Rate (All Ages) Unweighted
tripRate.per_all.by.size_unweighted <- trips.by.size_unweighted.freq/ per_all.by.size_unweighted.freq

# Person Ages 5+ Unweighted
per_5.by.size_unweighted.freq <- tapply(X=per$HH_CBSA, INDEX=list(per$HH_CBSA, per$HHSIZE), FUN=length)
per_5.by.size_unweighted.freq[is.na(per_5.by.size_unweighted.freq)] <- 0
per_5.by.size_unweighted.freq[,4] <- rowSums(per_5.by.size_unweighted.freq[,4:13])
per_5.by.size_unweighted.freq <- per_5.by.size_unweighted.freq[,1:4]
colnames(per_5.by.size_unweighted.freq)[4] <- "4+"
per_5.by.size_unweighted.prop <- per_5.by.size_unweighted.freq/rowSums(per_5.by.size_unweighted.freq) * 100
per_5.by.size_unweighted.freq <- as.data.frame.matrix(per_5.by.size_unweighted.freq)
per_5.by.size_unweighted.prop <- as.data.frame.matrix(per_5.by.size_unweighted.prop)

# Person Trip Rate (5+) Unweighted
tripRate.per_5.by.size_unweighted <- trips.by.size_unweighted.freq/ per_5.by.size_unweighted.freq

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Households Weighted
hh.by.size_weighted.freq <- t(tapply(hh$WTHHFIN, list(hh$HHSIZE, hh$HH_CBSA), sum))
hh.by.size_weighted.freq[is.na(hh.by.size_weighted.freq)] <- 0
hh.by.size_weighted.freq[,4] <- rowSums(hh.by.size_weighted.freq[,4:13])
hh.by.size_weighted.freq <- hh.by.size_weighted.freq[,1:4]
colnames(hh.by.size_weighted.freq)[4] <- "4+"
hh.by.size_weighted.prop <- hh.by.size_weighted.freq/rowSums(hh.by.size_weighted.freq) * 100
hh.by.size_weighted.freq <- as.data.frame.matrix(hh.by.size_weighted.freq)
hh.by.size_weighted.prop <- as.data.frame.matrix(hh.by.size_weighted.prop)

# Trips Weighted
trips.by.size_weighted.freq <- t(tapply(hh$CNTTDHH_weighted, list(hh$HHSIZE, hh$HH_CBSA), sum))
trips.by.size_weighted.freq[is.na(trips.by.size_weighted.freq)] <- 0
trips.by.size_weighted.freq[,4] <- rowSums(trips.by.size_weighted.freq[,4:13])
trips.by.size_weighted.freq <- trips.by.size_weighted.freq[,1:4]
colnames(trips.by.size_weighted.freq)[4] <- "4+"
trips.by.size_weighted.prop <- trips.by.size_weighted.freq/rowSums(trips.by.size_weighted.freq) * 100
trips.by.size_weighted.freq <- as.data.frame.matrix(trips.by.size_weighted.freq)
trips.by.size_weighted.prop <- as.data.frame.matrix(trips.by.size_weighted.prop)

# Household Trip Rate Weighted
tripRate.hh.by.size_weighted <- trips.by.size_weighted.freq/ hh.by.size_weighted.freq

# Persons All Ages Weighted
per_all.by.size_weighted.freq <- t(tapply(hh$HHSIZE_weighted, list(hh$HHSIZE, hh$HH_CBSA), sum))
per_all.by.size_weighted.freq[is.na(per_all.by.size_weighted.freq)] <- 0
per_all.by.size_weighted.freq[,4] <- rowSums(per_all.by.size_weighted.freq[,4:13])
per_all.by.size_weighted.freq <- per_all.by.size_weighted.freq[,1:4]
colnames(per_all.by.size_weighted.freq)[4] <- "4+"
per_all.by.size_weighted.prop <- per_all.by.size_weighted.freq/rowSums(per_all.by.size_weighted.freq) * 100
per_all.by.size_weighted.freq <- as.data.frame.matrix(per_all.by.size_weighted.freq)
per_all.by.size_weighted.prop <- as.data.frame.matrix(per_all.by.size_weighted.prop)

# Person Trip Rate (All Ages) Weighted
tripRate.per_all.by.size_weighted <- trips.by.size_weighted.freq/ per_all.by.size_weighted.freq

# Person Ages 5+ Weighted
per_5.by.size_weighted.freq <- t(tapply(per$WTPERFIN, list(per$HHSIZE, per$HH_CBSA), sum))
per_5.by.size_weighted.freq[is.na(per_5.by.size_weighted.freq)] <- 0
per_5.by.size_weighted.freq[,4] <- rowSums(per_5.by.size_weighted.freq[,4:13])
per_5.by.size_weighted.freq <- per_5.by.size_weighted.freq[,1:4]
colnames(per_5.by.size_weighted.freq)[4] <- "4+"
per_5.by.size_weighted.prop <- per_5.by.size_weighted.freq/rowSums(per_5.by.size_weighted.freq) * 100
per_5.by.size_weighted.freq <- as.data.frame.matrix(per_5.by.size_weighted.freq)
per_5.by.size_weighted.prop <- as.data.frame.matrix(per_5.by.size_weighted.prop)

# Person Trip Rate (5+) Weighted
tripRate.per_5.by.size_weighted <- trips.by.size_weighted.freq/ per_5.by.size_weighted.freq

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

#################
# Vehicle Count #
#################

# Households Unweighted
hh.by.veh_unweighted.freq <- table(hh$HH_CBSA, hh$HHVEHCNT)
hh.by.veh_unweighted.freq[,4] <- rowSums(hh.by.veh_unweighted.freq[,4:13])
hh.by.veh_unweighted.freq <- hh.by.veh_unweighted.freq[,-c(5:13)]
colnames(hh.by.veh_unweighted.freq)[4] <- "3+"
hh.by.veh_unweighted.prop <- hh.by.veh_unweighted.freq/rowSums(hh.by.veh_unweighted.freq) * 100
hh.by.veh_unweighted.freq <- as.data.frame.matrix(hh.by.veh_unweighted.freq)
hh.by.veh_unweighted.prop <- as.data.frame.matrix(hh.by.veh_unweighted.prop)

# Trips Unweighted
trips.by.veh_unweighted.freq <- tapply(X=hh$CNTTDHH, INDEX=list(hh$HH_CBSA, hh$HHVEHCNT), FUN=sum)
trips.by.veh_unweighted.freq[is.na(trips.by.veh_unweighted.freq)] <- 0
trips.by.veh_unweighted.freq[,4] <- rowSums(trips.by.veh_unweighted.freq[,4:13])
trips.by.veh_unweighted.freq <- trips.by.veh_unweighted.freq[,1:4]
colnames(trips.by.veh_unweighted.freq)[4] <- "3+"
trips.by.veh_unweighted.prop <- trips.by.veh_unweighted.freq/rowSums(trips.by.veh_unweighted.freq) * 100
trips.by.veh_unweighted.freq <- as.data.frame.matrix(trips.by.veh_unweighted.freq)
trips.by.veh_unweighted.prop <- as.data.frame.matrix(trips.by.veh_unweighted.prop)

# Household Trip Rate Unweighted
tripRate.hh.by.veh_unweighted <- trips.by.veh_unweighted.freq/ hh.by.veh_unweighted.freq

# Persons All Ages Unweighted
per_all.by.veh_unweighted.freq <- tapply(X=hh$HHSIZE, INDEX=list(hh$HH_CBSA, hh$HHVEHCNT), FUN=sum)
per_all.by.veh_unweighted.freq[is.na(per_all.by.veh_unweighted.freq)] <- 0
per_all.by.veh_unweighted.freq[,4] <- rowSums(per_all.by.veh_unweighted.freq[,4:13])
per_all.by.veh_unweighted.freq <- per_all.by.veh_unweighted.freq[,1:4]
colnames(per_all.by.veh_unweighted.freq)[4] <- "3+"
per_all.by.veh_unweighted.prop <- per_all.by.veh_unweighted.freq/rowSums(per_all.by.veh_unweighted.freq) * 100
per_all.by.veh_unweighted.freq <- as.data.frame.matrix(per_all.by.veh_unweighted.freq)
per_all.by.veh_unweighted.prop <- as.data.frame.matrix(per_all.by.veh_unweighted.prop)

# Person Trip Rate (All Ages) Unweighted
tripRate.per_all.by.veh_unweighted <- trips.by.veh_unweighted.freq/ per_all.by.veh_unweighted.freq

# Person Ages 5+ Unweighted
per_5.by.veh_unweighted.freq <- tapply(X=per$HH_CBSA, INDEX=list(per$HH_CBSA, per$HHVEHCNT), FUN=length)
per_5.by.veh_unweighted.freq[is.na(per_5.by.veh_unweighted.freq)] <- 0
per_5.by.veh_unweighted.freq[,4] <- rowSums(per_5.by.veh_unweighted.freq[,4:13])
per_5.by.veh_unweighted.freq <- per_5.by.veh_unweighted.freq[,1:4]
colnames(per_5.by.veh_unweighted.freq)[4] <- "3+"
per_5.by.veh_unweighted.prop <- per_5.by.veh_unweighted.freq/rowSums(per_5.by.veh_unweighted.freq) * 100
per_5.by.veh_unweighted.freq <- as.data.frame.matrix(per_5.by.veh_unweighted.freq)
per_5.by.veh_unweighted.prop <- as.data.frame.matrix(per_5.by.veh_unweighted.prop)

# Person Trip Rate (5+) Unweighted
tripRate.per_5.by.veh_unweighted <- trips.by.veh_unweighted.freq/ per_5.by.veh_unweighted.freq

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Households Weighted
hh.by.veh_weighted.freq <- t(tapply(hh$WTHHFIN, list(hh$HHVEHCNT, hh$HH_CBSA), sum))
hh.by.veh_weighted.freq[is.na(hh.by.veh_weighted.freq)] <- 0
hh.by.veh_weighted.freq[,4] <- rowSums(hh.by.veh_weighted.freq[,4:13])
hh.by.veh_weighted.freq <- hh.by.veh_weighted.freq[,1:4]
colnames(hh.by.veh_weighted.freq)[4] <- "3+"
hh.by.veh_weighted.prop <- hh.by.veh_weighted.freq/rowSums(hh.by.veh_weighted.freq) * 100
hh.by.veh_weighted.freq <- as.data.frame.matrix(hh.by.veh_weighted.freq)
hh.by.veh_weighted.prop <- as.data.frame.matrix(hh.by.veh_weighted.prop)

# Trips Weighted
trips.by.veh_weighted.freq <- t(tapply(hh$CNTTDHH_weighted, list(hh$HHVEHCNT, hh$HH_CBSA), sum))
trips.by.veh_weighted.freq[is.na(trips.by.veh_weighted.freq)] <- 0
trips.by.veh_weighted.freq[,4] <- rowSums(trips.by.veh_weighted.freq[,4:13])
trips.by.veh_weighted.freq <- trips.by.veh_weighted.freq[,1:4]
colnames(trips.by.veh_weighted.freq)[4] <- "3+"
trips.by.veh_weighted.prop <- trips.by.veh_weighted.freq/rowSums(trips.by.veh_weighted.freq) * 100
trips.by.veh_weighted.freq <- as.data.frame.matrix(trips.by.veh_weighted.freq)
trips.by.veh_weighted.prop <- as.data.frame.matrix(trips.by.veh_weighted.prop)

# Household Trip Rate Weighted
tripRate.hh.by.veh_weighted <- trips.by.veh_weighted.freq/ hh.by.veh_weighted.freq

# Persons All Ages Weighted
per_all.by.veh_weighted.freq <- t(tapply(hh$HHSIZE_weighted, list(hh$HHVEHCNT, hh$HH_CBSA), sum))
per_all.by.veh_weighted.freq[is.na(per_all.by.veh_weighted.freq)] <- 0
per_all.by.veh_weighted.freq[,4] <- rowSums(per_all.by.veh_weighted.freq[,4:13])
per_all.by.veh_weighted.freq <- per_all.by.veh_weighted.freq[,1:4]
colnames(per_all.by.veh_weighted.freq)[4] <- "3+"
per_all.by.veh_weighted.prop <- per_all.by.veh_weighted.freq/rowSums(per_all.by.veh_weighted.freq) * 100
per_all.by.veh_weighted.freq <- as.data.frame.matrix(per_all.by.veh_weighted.freq)
per_all.by.veh_weighted.prop <- as.data.frame.matrix(per_all.by.veh_weighted.prop)

# Person Trip Rate (All Ages) Weighted
tripRate.per_all.by.veh_weighted <- trips.by.veh_weighted.freq/ per_all.by.veh_weighted.freq

# Person Ages 5+ Weighted
per_5.by.veh_weighted.freq <- t(tapply(per$WTPERFIN, list(per$HHVEHCNT, per$HH_CBSA), sum))
per_5.by.veh_weighted.freq[is.na(per_5.by.veh_weighted.freq)] <- 0
per_5.by.veh_weighted.freq[,4] <- rowSums(per_5.by.veh_weighted.freq[,4:13])
per_5.by.veh_weighted.freq <- per_5.by.veh_weighted.freq[,1:4]
colnames(per_5.by.veh_weighted.freq)[4] <- "3+"
per_5.by.veh_weighted.prop <- per_5.by.veh_weighted.freq/rowSums(per_5.by.veh_weighted.freq) * 100
per_5.by.veh_weighted.freq <- as.data.frame.matrix(per_5.by.veh_weighted.freq)
per_5.by.veh_weighted.prop <- as.data.frame.matrix(per_5.by.veh_weighted.prop)

# Person Trip Rate (5+) Weighted
tripRate.per_5.by.veh_weighted <- trips.by.veh_weighted.freq/ per_5.by.veh_weighted.freq

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

#################
# Family Income #
#################

# Households Unweighted
hh.by.inc_unweighted.freq <- table(hh$HH_CBSA, hh$HHFAMINC)
hh.by.inc_unweighted.freq[is.na(hh.by.inc_unweighted.freq)] <- 0
hh.by.inc_unweighted.freq <- hh.by.inc_unweighted.freq[,-c(1:3)]
hh.by.inc_unweighted.freq[,1] <- rowSums(hh.by.inc_unweighted.freq[,1:4])
hh.by.inc_unweighted.freq[,2] <- rowSums(hh.by.inc_unweighted.freq[,5:6])
hh.by.inc_unweighted.freq[,3] <- rowSums(hh.by.inc_unweighted.freq[,7:11])
hh.by.inc_unweighted.freq <- hh.by.inc_unweighted.freq[,-c(4:11)]
colnames(hh.by.inc_unweighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
hh.by.inc_unweighted.prop <- hh.by.inc_unweighted.freq/rowSums(hh.by.inc_unweighted.freq) * 100
hh.by.inc_unweighted.freq <- as.data.frame.matrix(hh.by.inc_unweighted.freq)
hh.by.inc_unweighted.prop <- as.data.frame.matrix(hh.by.inc_unweighted.prop)

# Trips Unweighted
trips.by.inc_unweighted.freq <- tapply(X=hh$CNTTDHH, INDEX=list(hh$HH_CBSA, hh$HHFAMINC), FUN=sum)
trips.by.inc_unweighted.freq[is.na(trips.by.inc_unweighted.freq)] <- 0
trips.by.inc_unweighted.freq <- trips.by.inc_unweighted.freq[,-c(1:3)]
trips.by.inc_unweighted.freq[,1] <- rowSums(trips.by.inc_unweighted.freq[,1:4])
trips.by.inc_unweighted.freq[,2] <- rowSums(trips.by.inc_unweighted.freq[,5:6])
trips.by.inc_unweighted.freq[,3] <- rowSums(trips.by.inc_unweighted.freq[,7:11])
trips.by.inc_unweighted.freq <- trips.by.inc_unweighted.freq[,-c(4:11)]
colnames(trips.by.inc_unweighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
trips.by.inc_unweighted.prop <- trips.by.inc_unweighted.freq/rowSums(trips.by.inc_unweighted.freq) * 100
trips.by.inc_unweighted.freq <- as.data.frame.matrix(trips.by.inc_unweighted.freq)
trips.by.inc_unweighted.prop <- as.data.frame.matrix(trips.by.inc_unweighted.prop)

# Household Trip Rate Unweighted
tripRate.hh.by.inc_unweighted <- trips.by.inc_unweighted.freq/ hh.by.inc_unweighted.freq

# Persons All Ages Unweighted
per_all.by.inc_unweighted.freq <- tapply(X=hh$HHSIZE, INDEX=list(hh$HH_CBSA, hh$HHFAMINC), FUN=sum)
per_all.by.inc_unweighted.freq[is.na(per_all.by.inc_unweighted.freq)] <- 0
per_all.by.inc_unweighted.freq <- per_all.by.inc_unweighted.freq[,-c(1:3)]
per_all.by.inc_unweighted.freq[,1] <- rowSums(per_all.by.inc_unweighted.freq[,1:4])
per_all.by.inc_unweighted.freq[,2] <- rowSums(per_all.by.inc_unweighted.freq[,5:6])
per_all.by.inc_unweighted.freq[,3] <- rowSums(per_all.by.inc_unweighted.freq[,7:11])
per_all.by.inc_unweighted.freq <- per_all.by.inc_unweighted.freq[,-c(4:11)]
colnames(per_all.by.inc_unweighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
per_all.by.inc_unweighted.prop <- per_all.by.inc_unweighted.freq/rowSums(per_all.by.inc_unweighted.freq) * 100
per_all.by.inc_unweighted.freq <- as.data.frame.matrix(per_all.by.inc_unweighted.freq)
per_all.by.inc_unweighted.prop <- as.data.frame.matrix(per_all.by.inc_unweighted.prop)

# Person Trip Rate (All Ages) Unweighted
tripRate.per_all.by.inc_unweighted <- trips.by.inc_unweighted.freq/ per_all.by.inc_unweighted.freq

# Person Ages 5+ Unweighted
per_5.by.inc_unweighted.freq <- tapply(X=per$HH_CBSA, INDEX=list(per$HH_CBSA, per$HHFAMINC), FUN=length)
per_5.by.inc_unweighted.freq[is.na(per_5.by.inc_unweighted.freq)] <- 0
per_5.by.inc_unweighted.freq <- per_5.by.inc_unweighted.freq[,-c(1:3)]
per_5.by.inc_unweighted.freq[,1] <- rowSums(per_5.by.inc_unweighted.freq[,1:4])
per_5.by.inc_unweighted.freq[,2] <- rowSums(per_5.by.inc_unweighted.freq[,5:6])
per_5.by.inc_unweighted.freq[,3] <- rowSums(per_5.by.inc_unweighted.freq[,7:11])
per_5.by.inc_unweighted.freq <- per_5.by.inc_unweighted.freq[,-c(4:11)]
colnames(per_5.by.inc_unweighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
per_5.by.inc_unweighted.prop <- per_5.by.inc_unweighted.freq/rowSums(per_5.by.inc_unweighted.freq) * 100
per_5.by.inc_unweighted.freq <- as.data.frame.matrix(per_5.by.inc_unweighted.freq)
per_5.by.inc_unweighted.prop <- as.data.frame.matrix(per_5.by.inc_unweighted.prop)

# Person Trip Rate (5+) Unweighted
tripRate.per_5.by.inc_unweighted <- trips.by.inc_unweighted.freq/ per_5.by.inc_unweighted.freq

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Households Weighted
hh.by.inc_weighted.freq <- t(tapply(hh$WTHHFIN, list(hh$HHFAMINC, hh$HH_CBSA), sum))
hh.by.inc_weighted.freq[is.na(hh.by.inc_weighted.freq)] <- 0
hh.by.inc_weighted.freq <- hh.by.inc_weighted.freq[,-c(1:3)]
hh.by.inc_weighted.freq[,1] <- rowSums(hh.by.inc_weighted.freq[,1:4])
hh.by.inc_weighted.freq[,2] <- rowSums(hh.by.inc_weighted.freq[,5:6])
hh.by.inc_weighted.freq[,3] <- rowSums(hh.by.inc_weighted.freq[,7:11])
hh.by.inc_weighted.freq <- hh.by.inc_weighted.freq[,-c(4:11)]
colnames(hh.by.inc_weighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
hh.by.inc_weighted.prop <- hh.by.inc_weighted.freq/rowSums(hh.by.inc_weighted.freq) * 100
hh.by.inc_weighted.freq <- as.data.frame.matrix(hh.by.inc_weighted.freq)
hh.by.inc_weighted.prop <- as.data.frame.matrix(hh.by.inc_weighted.prop)

# Trips Weighted
trips.by.inc_weighted.freq <- t(tapply(hh$CNTTDHH_weighted, list(hh$HHFAMINC, hh$HH_CBSA), sum))
trips.by.inc_weighted.freq[is.na(trips.by.inc_weighted.freq)] <- 0
trips.by.inc_weighted.freq <- trips.by.inc_weighted.freq[,-c(1:3)]
trips.by.inc_weighted.freq[,1] <- rowSums(trips.by.inc_weighted.freq[,1:4])
trips.by.inc_weighted.freq[,2] <- rowSums(trips.by.inc_weighted.freq[,5:6])
trips.by.inc_weighted.freq[,3] <- rowSums(trips.by.inc_weighted.freq[,7:11])
trips.by.inc_weighted.freq <- trips.by.inc_weighted.freq[,-c(4:11)]
colnames(trips.by.inc_weighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
trips.by.inc_weighted.prop <- trips.by.inc_weighted.freq/rowSums(trips.by.inc_weighted.freq) * 100
trips.by.inc_weighted.freq <- as.data.frame.matrix(trips.by.inc_weighted.freq)
trips.by.inc_weighted.prop <- as.data.frame.matrix(trips.by.inc_weighted.prop)

# Household Trip Rate Weighted
tripRate.hh.by.inc_weighted <- trips.by.inc_weighted.freq/ hh.by.inc_weighted.freq

# Persons All Ages Weighted
per_all.by.inc_weighted.freq <- t(tapply(hh$HHSIZE_weighted, list(hh$HHFAMINC, hh$HH_CBSA), sum))
per_all.by.inc_weighted.freq[is.na(per_all.by.inc_weighted.freq)] <- 0
per_all.by.inc_weighted.freq <- per_all.by.inc_weighted.freq[,-c(1:3)]
per_all.by.inc_weighted.freq[,1] <- rowSums(per_all.by.inc_weighted.freq[,1:4])
per_all.by.inc_weighted.freq[,2] <- rowSums(per_all.by.inc_weighted.freq[,5:6])
per_all.by.inc_weighted.freq[,3] <- rowSums(per_all.by.inc_weighted.freq[,7:11])
per_all.by.inc_weighted.freq <- per_all.by.inc_weighted.freq[,-c(4:11)]
colnames(per_all.by.inc_weighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
per_all.by.inc_weighted.prop <- per_all.by.inc_weighted.freq/rowSums(per_all.by.inc_weighted.freq) * 100
per_all.by.inc_weighted.freq <- as.data.frame.matrix(per_all.by.inc_weighted.freq)
per_all.by.inc_weighted.prop <- as.data.frame.matrix(per_all.by.inc_weighted.prop)

# Person Trip Rate (All Ages) Weighted
tripRate.per_all.by.inc_weighted <- trips.by.inc_weighted.freq/ per_all.by.inc_weighted.freq

# Person Ages 5+ Weighted
per_5.by.inc_weighted.freq <- t(tapply(per$WTPERFIN, list(per$HHFAMINC, per$HH_CBSA), sum))
per_5.by.inc_weighted.freq[is.na(per_5.by.inc_weighted.freq)] <- 0
per_5.by.inc_weighted.freq <- per_5.by.inc_weighted.freq[,-c(1:3)]
per_5.by.inc_weighted.freq[,1] <- rowSums(per_5.by.inc_weighted.freq[,1:4])
per_5.by.inc_weighted.freq[,2] <- rowSums(per_5.by.inc_weighted.freq[,5:6])
per_5.by.inc_weighted.freq[,3] <- rowSums(per_5.by.inc_weighted.freq[,7:11])
per_5.by.inc_weighted.freq <- per_5.by.inc_weighted.freq[,-c(4:11)]
colnames(per_5.by.inc_weighted.freq) <- c("<$35k","$35k-<$75k","$75k+")
per_5.by.inc_weighted.prop <- per_5.by.inc_weighted.freq/rowSums(per_5.by.inc_weighted.freq) * 100
per_5.by.inc_weighted.freq <- as.data.frame.matrix(per_5.by.inc_weighted.freq)
per_5.by.inc_weighted.prop <- as.data.frame.matrix(per_5.by.inc_weighted.prop)

# Person Trip Rate (5+) Weighted
tripRate.per_5.by.inc_weighted <- trips.by.inc_weighted.freq/ per_5.by.inc_weighted.freq


################################
# VISUALIZING DEMOGRAPHIC INFO #
################################

library(data.table)
library(magrittr)
library(plotly)
library(Hmisc)
library(tibble)

# legend formatting
l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2)

CBSA_name <- c("Atlanta-Sandy Springs-Roswell, GA", "Austin-Round Rock, TX", "Baltimore-Columbia-Towson, MD", "Birmingham-Hoover, AL",
               "Boston-Cambridge-Newton, MA-NH", "Buffalo-Cheektowaga-Niagara Falls, NY", "Charlotte-Concord-Gastonia, NC-SC", "Chicago-Naperville-Elgin, IL-IN-WI",
               "Cincinnati, OH-KY-IN", "Cleveland-Elyria, OH", "Columbus, OH", "Dallas-Fort Worth-Arlington, TX", "Denver-Aurora-Lakewood, CO", "Detroit-Warren-Dearborn, MI",
               "Grand Rapids-Wyoming, MI", "Hartford-West Hartford-East Hartford, CT", "Houston-The Woodlands-Sugar Land, TX", "Indianapolis-Carmel-Anderson, IN",
               "Jacksonville, FL", "Kansas City, MO-KS", "Las Vegas-Henderson-Paradise, NV", "Los Angeles-Long Beach-Anaheim, CA", "Louisville/Jefferson County, KY-IN",
               "Memphis, TN-MS-AR", "Miami-Fort Lauderdale-West Palm Beach, FL", "Milwaukee-Waukesha-West Allis, WI", "Minneapolis-St. Paul-Bloomington, MN-WI", "Nashville-Davidson--Murfreesboro--Franklin, TN",
               "New Orleans-Metairie, LA", "New York-Newark-Jersey City, NY-NJ-PA", "Oklahoma City, OK", "Orlando-Kissimmee-Sanford, FL", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
               "Phoenix-Mesa-Scottsdale, AZ", "Pittsburgh, PA", "Portland-Vancouver-Hillsboro, OR-WA", "Providence-Warwick, RI-MA", "Raleigh, NC", "Richmond, VA",
               "Riverside-San Bernardino-Ontario, CA", "Rochester, NY", "Sacramento--Roseville--Arden-Arcade, CA", "St. Louis, MO-IL", "Salt Lake City, UT", "San Antonio-New Braunfels, TX",
               "San Diego-Carlsbad, CA", "San Francisco-Oakland-Hayward, CA", "San Jose-Sunnyvale-Santa Clara, CA", "Seattle-Tacoma-Bellevue, WA", "Tampa-St. Petersburg-Clearwater, FL",
               "Virginia Beach-Norfolk-Newport News, VA-NC", "Washington-Arlington-Alexandria, DC-VA-MD-WV", "Suppressed, in an MSA of less than 1 million")


a <- list(
  title = "",
  showticklabels = TRUE,
  autotick = FALSE,
  ticklen = 7,
  tickwidth = 3,
  tickcolor = toRGB("black"),
  tickfont = list(size = 12)
)

# Preparing the geographical data
library(rgdal)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(utils)

states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

#CBSA <- readOGR("data/tl_2017_us_cbsa.shp", layer = "tl_2017_us_cbsa")
unzip("data.zip", exdir="data")
CBSA <- readOGR(dsn = "data", layer = "tl_2017_us_cbsa")

cols.num <- c("ALAND","AWATER")
CBSA@data[cols.num] <- sapply(CBSA@data[cols.num],as.character)
CBSA@data[cols.num] <- sapply(CBSA@data[cols.num],as.numeric)

CBSA_main <- subset(CBSA, CBSA$NAME %in% CBSA_name)

#####################################################################################################

library(tidyr)
library(dplyr)
library(qwraps2)

FrincUn <- setDT(hh.by.inc_unweighted.freq, keep.rownames = TRUE)[]
FrincUn <- add_column(FrincUn, CBSA_name, .after = 1)
names(FrincUn)[1:2] <- c("CBSA", "Name")
FrincUn_long <- gather(FrincUn[-53,], `HH Income Level`, `Number of Households`, `<$35k`:`$75k+`, factor_key=TRUE)

PincUn <- setDT(hh.by.inc_unweighted.prop, keep.rownames = TRUE)[]
PincUn[,2:4] <- round(PincUn[,2:4]/100, 3)
PincUn <- add_column(PincUn, CBSA_name, .after = 1)
names(PincUn)[1:2] <- c("CBSA", "Name")
PincUn_long <- gather(PincUn[-53,], `HH Income Level`, `Proportion of Households`, `<$35k`:`$75k+`, factor_key=TRUE)

FrincUn2 <- setDT(per_all.by.inc_unweighted.freq, keep.rownames = TRUE)[]
FrincUn2 <- add_column(FrincUn2, CBSA_name, .after = 1)
names(FrincUn2)[1:2] <- c("CBSA", "Name")
FrincUn2_long <- gather(FrincUn2[-53,], `HH Income Level`, `Number of Persons`, `<$35k`:`$75k+`, factor_key=TRUE)

PincUn2 <- setDT(per_all.by.inc_unweighted.prop, keep.rownames = TRUE)[]
PincUn2[,2:4] <- round(PincUn2[,2:4]/100, 3)
PincUn2 <- add_column(PincUn2, CBSA_name, .after = 1)
names(PincUn2)[1:2] <- c("CBSA", "Name")
PincUn2_long <- gather(PincUn2[-53,], `HH Income Level`, `Proportion of Persons`, `<$35k`:`$75k+`, factor_key=TRUE)

FrincUn3 <- setDT(trips.by.inc_unweighted.freq, keep.rownames = TRUE)[]
FrincUn3 <- add_column(FrincUn3, CBSA_name, .after = 1)
names(FrincUn3)[1:2] <- c("CBSA", "Name")
FrincUn3_long <- gather(FrincUn3[-53,], `HH Income Level`, `Number of Trips`, `<$35k`:`$75k+`, factor_key=TRUE)

PincUn3 <- setDT(trips.by.inc_unweighted.prop, keep.rownames = TRUE)[]
PincUn3[,2:4] <- round(PincUn3[,2:4]/100, 3)
PincUn3 <- add_column(PincUn3, CBSA_name, .after = 1)
names(PincUn3)[1:2] <- c("CBSA", "Name")
PincUn3_long <- gather(PincUn3[-53,], `HH Income Level`, `Proportion of Trips`, `<$35k`:`$75k+`, factor_key=TRUE)

htrincUn <- setDT(tripRate.hh.by.inc_unweighted, keep.rownames = TRUE)[]
htrincUn[,2:4] <- round(htrincUn[,2:4], 2)
htrincUn <- add_column(htrincUn, CBSA_name, .after = 1)
names(htrincUn)[1:2] <- c("CBSA", "Name")
htrincUn_long <- gather(htrincUn[-53,], `HH Income Level`, `Household Trip Rate`, `<$35k`:`$75k+`, factor_key=TRUE)

ptrincUn <- setDT(tripRate.per_all.by.inc_unweighted, keep.rownames = TRUE)[]
ptrincUn[,2:4] <- round(ptrincUn[,2:4], 2)
ptrincUn <- add_column(ptrincUn, CBSA_name, .after = 1)
names(ptrincUn)[1:2] <- c("CBSA", "Name")
ptrincUn_long <- gather(ptrincUn[-53,], `HH Income Level`, `Person Trip Rate`, `<$35k`:`$75k+`, factor_key=TRUE)

all.dataUn_income <- Reduce(function(x, y) merge(x, y, by = c("CBSA","Name","HH Income Level"), sort = FALSE), list(FrincUn_long, PincUn_long, FrincUn2_long, PincUn2_long, FrincUn3_long, PincUn3_long, htrincUn_long, ptrincUn_long))


FrincWT <- setDT(hh.by.inc_weighted.freq, keep.rownames = TRUE)[]
FrincWT[,2:4] <- round(FrincWT[,2:4], 0)
FrincWT <- add_column(FrincWT, CBSA_name, .after = 1)
names(FrincWT)[1:2] <- c("CBSA", "Name")
FrincWT_long <- gather(FrincWT[-53,], `HH Income Level`, `Number of Households`, `<$35k`:`$75k+`, factor_key=TRUE)

PincWT <- setDT(hh.by.inc_weighted.prop, keep.rownames = TRUE)[]
PincWT[,2:4] <- round(PincWT[,2:4]/100, 3)
PincWT <- add_column(PincWT, CBSA_name, .after = 1)
names(PincWT)[1:2] <- c("CBSA", "Name")
PincWT_long <- gather(PincWT[-53,], `HH Income Level`, `Proportion of Households`, `<$35k`:`$75k+`, factor_key=TRUE)

FrincWT2 <- setDT(per_all.by.inc_weighted.freq, keep.rownames = TRUE)[]
FrincWT2[,2:4] <- round(FrincWT2[,2:4], 0)
FrincWT2 <- add_column(FrincWT2, CBSA_name, .after = 1)
names(FrincWT2)[1:2] <- c("CBSA", "Name")
FrincWT2_long <- gather(FrincWT2[-53,], `HH Income Level`, `Number of Persons`, `<$35k`:`$75k+`, factor_key=TRUE)

PincWT2 <- setDT(per_all.by.inc_weighted.prop, keep.rownames = TRUE)[]
PincWT2[,2:4] <- round(PincWT2[,2:4]/100, 3)
PincWT2 <- add_column(PincWT2, CBSA_name, .after = 1)
names(PincWT2)[1:2] <- c("CBSA", "Name")
PincWT2_long <- gather(PincWT2[-53,], `HH Income Level`, `Proportion of Persons`, `<$35k`:`$75k+`, factor_key=TRUE)

FrincWT3 <- setDT(trips.by.inc_weighted.freq, keep.rownames = TRUE)[]
FrincWT3[,2:4] <- round(FrincWT3[,2:4], 0)
FrincWT3 <- add_column(FrincWT3, CBSA_name, .after = 1)
names(FrincWT3)[1:2] <- c("CBSA", "Name")
FrincWT3_long <- gather(FrincWT3[-53,], `HH Income Level`, `Number of Trips`, `<$35k`:`$75k+`, factor_key=TRUE)

PincWT3 <- setDT(trips.by.inc_weighted.prop, keep.rownames = TRUE)[]
PincWT3[,2:4] <- round(PincWT3[,2:4]/100, 3)
PincWT3 <- add_column(PincWT3, CBSA_name, .after = 1)
names(PincWT3)[1:2] <- c("CBSA", "Name")
PincWT3_long <- gather(PincWT3[-53,], `HH Income Level`, `Proportion of Trips`, `<$35k`:`$75k+`, factor_key=TRUE)

htrincWT <- setDT(tripRate.hh.by.inc_weighted, keep.rownames = TRUE)[]
htrincWT[,2:4] <- round(htrincWT[,2:4], 2)
htrincWT <- add_column(htrincWT, CBSA_name, .after = 1)
names(htrincWT)[1:2] <- c("CBSA", "Name")
htrincWT_long <- gather(htrincWT[-53,], `HH Income Level`, `Household Trip Rate`, `<$35k`:`$75k+`, factor_key=TRUE)

ptrincWT <- setDT(tripRate.per_all.by.inc_weighted, keep.rownames = TRUE)[]
ptrincWT[,2:4] <- round(ptrincWT[,2:4], 2)
ptrincWT <- add_column(ptrincWT, CBSA_name, .after = 1)
names(ptrincWT)[1:2] <- c("CBSA", "Name")
ptrincWT_long <- gather(ptrincWT[-53,], `HH Income Level`, `Person Trip Rate`, `<$35k`:`$75k+`, factor_key=TRUE)

all.dataWT_income <- Reduce(function(x, y) merge(x, y, by = c("CBSA","Name","HH Income Level"), sort = FALSE), list(FrincWT_long, PincWT_long, FrincWT2_long, PincWT2_long, FrincWT3_long, PincWT3_long, htrincWT_long, ptrincWT_long))

#####################################################################################################

FrsizeUn <- setDT(hh.by.size_unweighted.freq, keep.rownames = TRUE)[]
FrsizeUn <- add_column(FrsizeUn, CBSA_name, .after = 1)
names(FrsizeUn)[1:2] <- c("CBSA", "Name")
FrsizeUn_long <- gather(FrsizeUn[-53,], `Household Size`, `Number of Households`, `1`:`4+`, factor_key=TRUE)

PsizeUn <- setDT(hh.by.size_unweighted.prop, keep.rownames = TRUE)[]
PsizeUn[,2:5] <- round(PsizeUn[,2:5]/100, 3)
PsizeUn <- add_column(PsizeUn, CBSA_name, .after = 1)
names(PsizeUn)[1:2] <- c("CBSA", "Name")
PsizeUn_long <- gather(PsizeUn[-53,], `Household Size`, `Proportion of Households`, `1`:`4+`, factor_key=TRUE)

FrsizeUn2 <- setDT(per_all.by.size_unweighted.freq, keep.rownames = TRUE)[]
FrsizeUn2 <- add_column(FrsizeUn2, CBSA_name, .after = 1)
names(FrsizeUn2)[1:2] <- c("CBSA", "Name")
FrsizeUn2_long <- gather(FrsizeUn2[-53,], `Household Size`, `Number of Persons`, `1`:`4+`, factor_key=TRUE)

PsizeUn2 <- setDT(per_all.by.size_unweighted.prop, keep.rownames = TRUE)[]
PsizeUn2[,2:5] <- round(PsizeUn2[,2:5]/100, 3)
PsizeUn2 <- add_column(PsizeUn2, CBSA_name, .after = 1)
names(PsizeUn2)[1:2] <- c("CBSA", "Name")
PsizeUn2_long <- gather(PsizeUn2[-53,], `Household Size`, `Proportion of Persons`, `1`:`4+`, factor_key=TRUE)

FrsizeUn3 <- setDT(trips.by.size_unweighted.freq, keep.rownames = TRUE)[]
FrsizeUn3 <- add_column(FrsizeUn3, CBSA_name, .after = 1)
names(FrsizeUn3)[1:2] <- c("CBSA", "Name")
FrsizeUn3_long <- gather(FrsizeUn3[-53,], `Household Size`, `Number of Trips`, `1`:`4+`, factor_key=TRUE)

PsizeUn3 <- setDT(trips.by.size_unweighted.prop, keep.rownames = TRUE)[]
PsizeUn3[,2:5] <- round(PsizeUn3[,2:5]/100, 3)
PsizeUn3 <- add_column(PsizeUn3, CBSA_name, .after = 1)
names(PsizeUn3)[1:2] <- c("CBSA", "Name")
PsizeUn3_long <- gather(PsizeUn3[-53,], `Household Size`, `Proportion of Trips`, `1`:`4+`, factor_key=TRUE)

htrsizeUn <- setDT(tripRate.hh.by.size_unweighted, keep.rownames = TRUE)[]
htrsizeUn[,2:5] <- round(htrsizeUn[,2:5], 2)
htrsizeUn <- add_column(htrsizeUn, CBSA_name, .after = 1)
names(htrsizeUn)[1:2] <- c("CBSA", "Name")
htrsizeUn_long <- gather(htrsizeUn[-53,], `Household Size`, `Household Trip Rate`, `1`:`4+`, factor_key=TRUE)

ptrsizeUn <- setDT(tripRate.per_all.by.size_unweighted, keep.rownames = TRUE)[]
ptrsizeUn[,2:5] <- round(ptrsizeUn[,2:5], 2)
ptrsizeUn <- add_column(ptrsizeUn, CBSA_name, .after = 1)
names(ptrsizeUn)[1:2] <- c("CBSA", "Name")
ptrsizeUn_long <- gather(ptrsizeUn[-53,], `Household Size`, `Person Trip Rate`, `1`:`4+`, factor_key=TRUE)

all.dataUn_hhsize <- Reduce(function(x, y) merge(x, y, by = c("CBSA","Name","Household Size"), sort = FALSE), list(FrsizeUn_long, PsizeUn_long, FrsizeUn2_long, PsizeUn2_long, FrsizeUn3_long, PsizeUn3_long, htrsizeUn_long, ptrsizeUn_long))


FrsizeWT <- setDT(hh.by.size_weighted.freq, keep.rownames = TRUE)[]
FrsizeWT[,2:5] <- round(FrsizeWT[,2:5], 0)
FrsizeWT <- add_column(FrsizeWT, CBSA_name, .after = 1)
names(FrsizeWT)[1:2] <- c("CBSA", "Name")
FrsizeWT_long <- gather(FrsizeWT[-53,], `Household Size`, `Number of Households`, `1`:`4+`, factor_key=TRUE)

PsizeWT <- setDT(hh.by.size_weighted.prop, keep.rownames = TRUE)[]
PsizeWT[,2:5] <- round(PsizeWT[,2:5]/100, 3)
PsizeWT <- add_column(PsizeWT, CBSA_name, .after = 1)
names(PsizeWT)[1:2] <- c("CBSA", "Name")
PsizeWT_long <- gather(PsizeWT[-53,], `Household Size`, `Proportion of Households`, `1`:`4+`, factor_key=TRUE)

FrsizeWT2 <- setDT(per_all.by.size_weighted.freq, keep.rownames = TRUE)[]
FrsizeWT2[,2:5] <- round(FrsizeWT2[,2:5], 0)
FrsizeWT2 <- add_column(FrsizeWT2, CBSA_name, .after = 1)
names(FrsizeWT2)[1:2] <- c("CBSA", "Name")
FrsizeWT2_long <- gather(FrsizeWT2[-53,], `Household Size`, `Number of Persons`, `1`:`4+`, factor_key=TRUE)

PsizeWT2 <- setDT(per_all.by.size_weighted.prop, keep.rownames = TRUE)[]
PsizeWT2[,2:5] <- round(PsizeWT2[,2:5]/100, 3)
PsizeWT2 <- add_column(PsizeWT2, CBSA_name, .after = 1)
names(PsizeWT2)[1:2] <- c("CBSA", "Name")
PsizeWT2_long <- gather(PsizeWT2[-53,], `Household Size`, `Proportion of Persons`, `1`:`4+`, factor_key=TRUE)

FrsizeWT3 <- setDT(trips.by.size_weighted.freq, keep.rownames = TRUE)[]
FrsizeWT3[,2:5] <- round(FrsizeWT3[,2:5], 0)
FrsizeWT3 <- add_column(FrsizeWT3, CBSA_name, .after = 1)
names(FrsizeWT3)[1:2] <- c("CBSA", "Name")
FrsizeWT3_long <- gather(FrsizeWT3[-53,], `Household Size`, `Number of Trips`, `1`:`4+`, factor_key=TRUE)

PsizeWT3 <- setDT(trips.by.size_weighted.prop, keep.rownames = TRUE)[]
PsizeWT3[,2:5] <- round(PsizeWT3[,2:5]/100, 3)
PsizeWT3 <- add_column(PsizeWT3, CBSA_name, .after = 1)
names(PsizeWT3)[1:2] <- c("CBSA", "Name")
PsizeWT3_long <- gather(PsizeWT3[-53,], `Household Size`, `Proportion of Trips`, `1`:`4+`, factor_key=TRUE)

htrsizeWT <- setDT(tripRate.hh.by.size_weighted, keep.rownames = TRUE)[]
htrsizeWT[,2:5] <- round(htrsizeWT[,2:5], 2)
htrsizeWT <- add_column(htrsizeWT, CBSA_name, .after = 1)
names(htrsizeWT)[1:2] <- c("CBSA", "Name")
htrsizeWT_long <- gather(htrsizeWT[-53,], `Household Size`, `Household Trip Rate`, `1`:`4+`, factor_key=TRUE)

ptrsizeWT <- setDT(tripRate.per_all.by.size_weighted, keep.rownames = TRUE)[]
ptrsizeWT[,2:5] <- round(ptrsizeWT[,2:5], 2)
ptrsizeWT <- add_column(ptrsizeWT, CBSA_name, .after = 1)
names(ptrsizeWT)[1:2] <- c("CBSA", "Name")
ptrsizeWT_long <- gather(ptrsizeWT[-53,], `Household Size`, `Person Trip Rate`, `1`:`4+`, factor_key=TRUE)

all.dataWT_hhsize <- Reduce(function(x, y) merge(x, y, by = c("CBSA","Name","Household Size"), sort = FALSE), list(FrsizeWT_long, PsizeWT_long, FrsizeWT2_long, PsizeWT2_long, FrsizeWT3_long, PsizeWT3_long, htrsizeWT_long, ptrsizeWT_long))

#####################################################################################################

FrvehUn <- setDT(hh.by.veh_unweighted.freq, keep.rownames = TRUE)[]
FrvehUn <- add_column(FrvehUn, CBSA_name, .after = 1)
names(FrvehUn)[1:2] <- c("CBSA", "Name")
FrvehUn_long <- gather(FrvehUn[-53,], `No. of Vehicles Owned`, `Number of Households`, `0`:`3+`, factor_key=TRUE)

PvehUn <- setDT(hh.by.veh_unweighted.prop, keep.rownames = TRUE)[]
PvehUn[,2:5] <- round(PvehUn[,2:5]/100, 3)
PvehUn <- add_column(PvehUn, CBSA_name, .after = 1)
names(PvehUn)[1:2] <- c("CBSA", "Name")
PvehUn_long <- gather(PvehUn[-53,], `No. of Vehicles Owned`, `Proportion of Households`, `0`:`3+`, factor_key=TRUE)

FrvehUn2 <- setDT(per_all.by.veh_unweighted.freq, keep.rownames = TRUE)[]
FrvehUn2 <- add_column(FrvehUn2, CBSA_name, .after = 1)
names(FrvehUn2)[1:2] <- c("CBSA", "Name")
FrvehUn2_long <- gather(FrvehUn2[-53,], `No. of Vehicles Owned`, `Number of Persons`, `0`:`3+`, factor_key=TRUE)

PvehUn2 <- setDT(per_all.by.veh_unweighted.prop, keep.rownames = TRUE)[]
PvehUn2[,2:5] <- round(PvehUn2[,2:5]/100, 3)
PvehUn2 <- add_column(PvehUn2, CBSA_name, .after = 1)
names(PvehUn2)[1:2] <- c("CBSA", "Name")
PvehUn2_long <- gather(PvehUn2[-53,], `No. of Vehicles Owned`, `Proportion of Persons`, `0`:`3+`, factor_key=TRUE)

FrvehUn3 <- setDT(trips.by.veh_unweighted.freq, keep.rownames = TRUE)[]
FrvehUn3 <- add_column(FrvehUn3, CBSA_name, .after = 1)
names(FrvehUn3)[1:2] <- c("CBSA", "Name")
FrvehUn3_long <- gather(FrvehUn3[-53,], `No. of Vehicles Owned`, `Number of Trips`, `0`:`3+`, factor_key=TRUE)

PvehUn3 <- setDT(trips.by.veh_unweighted.prop, keep.rownames = TRUE)[]
PvehUn3[,2:5] <- round(PvehUn3[,2:5]/100, 3)
PvehUn3 <- add_column(PvehUn3, CBSA_name, .after = 1)
names(PvehUn3)[1:2] <- c("CBSA", "Name")
PvehUn3_long <- gather(PvehUn3[-53,], `No. of Vehicles Owned`, `Proportion of Trips`, `0`:`3+`, factor_key=TRUE)

htrvehUn <- setDT(tripRate.hh.by.veh_unweighted, keep.rownames = TRUE)[]
htrvehUn[,2:5] <- round(htrvehUn[,2:5], 2)
htrvehUn <- add_column(htrvehUn, CBSA_name, .after = 1)
names(htrvehUn)[1:2] <- c("CBSA", "Name")
htrvehUn_long <- gather(htrvehUn[-53,], `No. of Vehicles Owned`, `Household Trip Rate`, `0`:`3+`, factor_key=TRUE)

ptrvehUn <- setDT(tripRate.per_all.by.veh_unweighted, keep.rownames = TRUE)[]
ptrvehUn[,2:5] <- round(ptrvehUn[,2:5], 2)
ptrvehUn <- add_column(ptrvehUn, CBSA_name, .after = 1)
names(ptrvehUn)[1:2] <- c("CBSA", "Name")
ptrvehUn_long <- gather(ptrvehUn[-53,], `No. of Vehicles Owned`, `Person Trip Rate`, `0`:`3+`, factor_key=TRUE)

all.dataUn_hhveh <- Reduce(function(x, y) merge(x, y, by = c("CBSA","Name","No. of Vehicles Owned"), sort = FALSE), list(FrvehUn_long, PvehUn_long, FrvehUn2_long, PvehUn2_long, FrvehUn3_long, PvehUn3_long, htrvehUn_long, ptrvehUn_long))


FrvehWT <- setDT(hh.by.veh_weighted.freq, keep.rownames = TRUE)[]
FrvehWT[,2:5] <- round(FrvehWT[,2:5], 0)
FrvehWT <- add_column(FrvehWT, CBSA_name, .after = 1)
names(FrvehWT)[1:2] <- c("CBSA", "Name")
FrvehWT_long <- gather(FrvehWT[-53,], `No. of Vehicles Owned`, `Number of Households`, `0`:`3+`, factor_key=TRUE)

PvehWT <- setDT(hh.by.veh_weighted.prop, keep.rownames = TRUE)[]
PvehWT[,2:5] <- round(PvehWT[,2:5]/100, 3)
PvehWT <- add_column(PvehWT, CBSA_name, .after = 1)
names(PvehWT)[1:2] <- c("CBSA", "Name")
PvehWT_long <- gather(PvehWT[-53,], `No. of Vehicles Owned`, `Proportion of Households`, `0`:`3+`, factor_key=TRUE)

FrvehWT2 <- setDT(per_all.by.veh_weighted.freq, keep.rownames = TRUE)[]
FrvehWT2[,2:5] <- round(FrvehWT2[,2:5], 0)
FrvehWT2 <- add_column(FrvehWT2, CBSA_name, .after = 1)
names(FrvehWT2)[1:2] <- c("CBSA", "Name")
FrvehWT2_long <- gather(FrvehWT2[-53,], `No. of Vehicles Owned`, `Number of Persons`, `0`:`3+`, factor_key=TRUE)

PvehWT2 <- setDT(per_all.by.veh_weighted.prop, keep.rownames = TRUE)[]
PvehWT2[,2:5] <- round(PvehWT2[,2:5]/100, 3)
PvehWT2 <- add_column(PvehWT2, CBSA_name, .after = 1)
names(PvehWT2)[1:2] <- c("CBSA", "Name")
PvehWT2_long <- gather(PvehWT2[-53,], `No. of Vehicles Owned`, `Proportion of Persons`, `0`:`3+`, factor_key=TRUE)

FrvehWT3 <- setDT(trips.by.veh_weighted.freq, keep.rownames = TRUE)[]
FrvehWT3[,2:5] <- round(FrvehWT3[,2:5], 0)
FrvehWT3 <- add_column(FrvehWT3, CBSA_name, .after = 1)
names(FrvehWT3)[1:2] <- c("CBSA", "Name")
FrvehWT3_long <- gather(FrvehWT3[-53,], `No. of Vehicles Owned`, `Number of Trips`, `0`:`3+`, factor_key=TRUE)

PvehWT3 <- setDT(trips.by.veh_weighted.prop, keep.rownames = TRUE)[]
PvehWT3[,2:5] <- round(PvehWT3[,2:5]/100, 3)
PvehWT3 <- add_column(PvehWT3, CBSA_name, .after = 1)
names(PvehWT3)[1:2] <- c("CBSA", "Name")
PvehWT3_long <- gather(PvehWT3[-53,], `No. of Vehicles Owned`, `Proportion of Trips`, `0`:`3+`, factor_key=TRUE)

htrvehWT <- setDT(tripRate.hh.by.veh_weighted, keep.rownames = TRUE)[]
htrvehWT[,2:5] <- round(htrvehWT[,2:5], 2)
htrvehWT <- add_column(htrvehWT, CBSA_name, .after = 1)
names(htrvehWT)[1:2] <- c("CBSA", "Name")
htrvehWT_long <- gather(htrvehWT[-53,], `No. of Vehicles Owned`, `Household Trip Rate`, `0`:`3+`, factor_key=TRUE)

ptrvehWT <- setDT(tripRate.per_all.by.veh_weighted, keep.rownames = TRUE)[]
ptrvehWT[,2:5] <- round(ptrvehWT[,2:5], 2)
ptrvehWT <- add_column(ptrvehWT, CBSA_name, .after = 1)
names(ptrvehWT)[1:2] <- c("CBSA", "Name")
ptrvehWT_long <- gather(ptrvehWT[-53,], `No. of Vehicles Owned`, `Person Trip Rate`, `0`:`3+`, factor_key=TRUE)

all.dataWT_hhveh <- Reduce(function(x, y) merge(x, y, by = c("CBSA","Name","No. of Vehicles Owned"), sort = FALSE), list(FrvehWT_long, PvehWT_long, FrvehWT2_long, PvehWT2_long, FrvehWT3_long, PvehWT3_long, htrvehWT_long, ptrvehWT_long))


our_summary1 <-
  list(" " =
         list("Min" = ~ min(.data$`Proportion of Households`),
              "Median" = ~ median(.data$`Proportion of Households`),
              "Max" = ~ max(.data$`Proportion of Households`),
              "Mean (SD)" = ~ mean_sd(.data$`Proportion of Households`, denote_sd = "paren"))
  )

our_summary2 <-
  list(" " =
         list("Min" = ~ min(.data$`Proportion of Persons`),
              "Median" = ~ median(.data$`Proportion of Persons`),
              "Max" = ~ max(.data$`Proportion of Persons`),
              "Mean (SD)" = ~ mean_sd(.data$`Proportion of Persons`, denote_sd = "paren"))
  )

our_summary3 <-
  list(" " =
         list("Min" = ~ min(.data$`Proportion of Trips`),
              "Median" = ~ median(.data$`Proportion of Trips`),
              "Max" = ~ max(.data$`Proportion of Trips`),
              "Mean (SD)" = ~ mean_sd(.data$`Proportion of Trips`, denote_sd = "paren"))
  )

our_summary4 <-
  list(" " =
         list("Min" = ~ min(.data$`Household Trip Rate`),
              "Median" = ~ median(.data$`Household Trip Rate`),
              "Max" = ~ max(.data$`Household Trip Rate`),
              "Mean (SD)" = ~ mean_sd(.data$`Household Trip Rate`, denote_sd = "paren"))
  )

our_summary5 <-
  list(" " =
         list("Min" = ~ min(.data$`Person Trip Rate`),
              "Median" = ~ median(.data$`Person Trip Rate`),
              "Max" = ~ max(.data$`Person Trip Rate`),
              "Mean (SD)" = ~ mean_sd(.data$`Person Trip Rate`, denote_sd = "paren"))
  )


# income unweighted

#PincUn_summary <- ggplot(PincUn_long, aes(`Proportion of Households`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PincUn_summary <- ggplot(PincUn_long, aes(`Proportion of Households`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PincUn_stats <- summary_table(dplyr::group_by(PincUn_long, `HH Income Level`), our_summary1)

PincUn.plot <- plot_ly(PincUn[-53,], x = ~`<$35k`, y = ~Name, type = 'bar', orientation = 'h', name = '<$35k', height = 900, width = 1200,
                       hoverinfo = 'text', text = ~paste(Name,': ', `<$35k`), marker = list(color = 'rgba(0, 191, 255, 0.6)',
                                                                                            line = list(color = 'rgba(0, 191, 255, 1.0)',
                                                                                                        width = 3))) %>%
  add_trace(x = ~`$35k-<$75k`, name = '$35k - <$75k', hoverinfo = 'text', text = ~paste(Name,': ', `$35k-<$75k`),
            marker = list(color = 'rgba(178, 34, 34, 0.6)',
                          line = list(color = 'rgba(178, 34, 34, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`$75k+`, name = '$75k+', hoverinfo = 'text', text = ~paste(Name,': ', `$75k+`),
            marker = list(color = 'rgba(154, 205, 50, 0.6)',
                          line = list(color = 'rgba(154, 205, 50, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Income Level", xref="paper", yref="paper",
                          x=1.1, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PincUn2_summary <- ggplot(PincUn2_long, aes(`Proportion of Persons`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PincUn2_summary <- ggplot(PincUn2_long, aes(`Proportion of Persons`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PincUn2_stats <- summary_table(dplyr::group_by(PincUn2_long, `HH Income Level`), our_summary2)

PincUn2.plot <- plot_ly(PincUn2[-53,], x = ~`<$35k`, y = ~Name, type = 'bar', orientation = 'h', name = '<$35k', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `<$35k`), marker = list(color = 'rgba(0, 191, 255, 0.6)',
                                                                                             line = list(color = 'rgba(0, 191, 255, 1.0)',
                                                                                                         width = 3))) %>%
  add_trace(x = ~`$35k-<$75k`, name = '$35k - <$75k', hoverinfo = 'text', text = ~paste(Name,': ', `$35k-<$75k`),
            marker = list(color = 'rgba(178, 34, 34, 0.6)',
                          line = list(color = 'rgba(178, 34, 34, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`$75k+`, name = '$75k+', hoverinfo = 'text', text = ~paste(Name,': ', `$75k+`),
            marker = list(color = 'rgba(154, 205, 50, 0.6)',
                          line = list(color = 'rgba(154, 205, 50, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Income Level", xref="paper", yref="paper",
                          x=1.1, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PincUn3_summary <- ggplot(PincUn3_long, aes(`Proportion of Trips`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PincUn3_summary <- ggplot(PincUn3_long, aes(`Proportion of Trips`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PincUn3_stats <- summary_table(dplyr::group_by(PincUn3_long, `HH Income Level`), our_summary3)

PincUn3.plot <- plot_ly(PincUn3[-53,], x = ~`<$35k`, y = ~Name, type = 'bar', orientation = 'h', name = '<$35k', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `<$35k`), marker = list(color = 'rgba(0, 191, 255, 0.6)',
                                                                                             line = list(color = 'rgba(0, 191, 255, 1.0)',
                                                                                                         width = 3))) %>%
  add_trace(x = ~`$35k-<$75k`, name = '$35k - <$75k', hoverinfo = 'text', text = ~paste(Name,': ', `$35k-<$75k`),
            marker = list(color = 'rgba(178, 34, 34, 0.6)',
                          line = list(color = 'rgba(178, 34, 34, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`$75k+`, name = '$75k+', hoverinfo = 'text', text = ~paste(Name,': ', `$75k+`),
            marker = list(color = 'rgba(154, 205, 50, 0.6)',
                          line = list(color = 'rgba(154, 205, 50, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Income Level", xref="paper", yref="paper",
                          x=1.1, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#htrincUn_summary <- ggplot(htrincUn_long, aes(`Household Trip Rate`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
htrincUn_summary <- ggplot(htrincUn_long, aes(`Household Trip Rate`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 1.5) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

htrincUn_stats <- summary_table(dplyr::group_by(htrincUn_long, `HH Income Level`), our_summary4)

CBSA_main_htrincUn <- merge(CBSA_main, htrincUn,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_htrincUn <- colorNumeric(palette = "YlOrRd",
                        domain = c(CBSA_main_htrincUn$`<$35k`,
                                   CBSA_main_htrincUn$`$35k-<$75k`,
                                   CBSA_main_htrincUn$`$75k+`))

labels_htrincUn <- list()

labels_htrincUn[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrincUn$NAME, CBSA_main_htrincUn$`<$35k`
) %>% lapply(htmltools::HTML)

labels_htrincUn[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrincUn$NAME, CBSA_main_htrincUn$`$35k-<$75k`
) %>% lapply(htmltools::HTML)

labels_htrincUn[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrincUn$NAME, CBSA_main_htrincUn$`$75k+`
) %>% lapply(htmltools::HTML)

htrincUn.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_htrincUn,
              group = "<$35k",
              fillColor = ~pal_htrincUn(`<$35k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrincUn[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrincUn,
              group = "$35k-<$75k",
              fillColor = ~pal_htrincUn(`$35k-<$75k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrincUn[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrincUn,
              group = "$75k+",
              fillColor = ~pal_htrincUn(`$75k+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrincUn[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_htrincUn, values = c(CBSA_main_htrincUn$`<$35k`,
                                      CBSA_main_htrincUn$`$35k-<$75k`,
                                      CBSA_main_htrincUn$`$75k+`),
            opacity = 0.7, title = "Household Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("<$35k", "$35k-<$75k", "$75k+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("$35k-<$75k", "$75k+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Income Level</label></strong>');
        }
    ")

#ptrincUn_summary <- ggplot(ptrincUn_long, aes(`Person Trip Rate`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
ptrincUn_summary <- ggplot(ptrincUn_long, aes(`Person Trip Rate`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.25) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

ptrincUn_stats <- summary_table(dplyr::group_by(ptrincUn_long, `HH Income Level`), our_summary5)

CBSA_main_ptrincUn <- merge(CBSA_main, ptrincUn,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_ptrincUn <- colorNumeric(palette = "YlOrRd",
                             domain = c(CBSA_main_ptrincUn$`<$35k`,
                                        CBSA_main_ptrincUn$`$35k-<$75k`,
                                        CBSA_main_ptrincUn$`$75k+`))

labels_ptrincUn <- list()

labels_ptrincUn[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrincUn$NAME, CBSA_main_ptrincUn$`<$35k`
) %>% lapply(htmltools::HTML)

labels_ptrincUn[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrincUn$NAME, CBSA_main_ptrincUn$`$35k-<$75k`
) %>% lapply(htmltools::HTML)

labels_ptrincUn[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrincUn$NAME, CBSA_main_ptrincUn$`$75k+`
) %>% lapply(htmltools::HTML)

ptrincUn.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_ptrincUn,
              group = "<$35k",
              fillColor = ~pal_ptrincUn(`<$35k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrincUn[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrincUn,
              group = "$35k-<$75k",
              fillColor = ~pal_ptrincUn(`$35k-<$75k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrincUn[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrincUn,
              group = "$75k+",
              fillColor = ~pal_ptrincUn(`$75k+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrincUn[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_ptrincUn, values = c(CBSA_main_ptrincUn$`<$35k`,
                                      CBSA_main_ptrincUn$`$35k-<$75k`,
                                      CBSA_main_ptrincUn$`$75k+`),
            opacity = 0.7, title = "Person Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("<$35k", "$35k-<$75k", "$75k+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("$35k-<$75k", "$75k+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Income Level</label></strong>');
        }
    ")


# income weighted

#PincWT_summary <- ggplot(PincWT_long, aes(`Proportion of Households`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PincWT_summary <- ggplot(PincWT_long, aes(`Proportion of Households`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PincWT_stats <- summary_table(dplyr::group_by(PincWT_long, `HH Income Level`), our_summary1)

PincWT.plot <- plot_ly(PincWT[-53,], x = ~`<$35k`, y = ~Name, type = 'bar', orientation = 'h', name = '<$35k', height = 900, width = 1200,
                       hoverinfo = 'text', text = ~paste(Name,': ', `<$35k`), marker = list(color = 'rgba(0, 191, 255, 0.6)',
                                                                                            line = list(color = 'rgba(0, 191, 255, 1.0)',
                                                                                                        width = 3))) %>%
  add_trace(x = ~`$35k-<$75k`, name = '$35k - <$75k', hoverinfo = 'text', text = ~paste(Name,': ', `$35k-<$75k`),
            marker = list(color = 'rgba(178, 34, 34, 0.6)',
                          line = list(color = 'rgba(178, 34, 34, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`$75k+`, name = '$75k+', hoverinfo = 'text', text = ~paste(Name,': ', `$75k+`),
            marker = list(color = 'rgba(154, 205, 50, 0.6)',
                          line = list(color = 'rgba(154, 205, 50, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Income Level", xref="paper", yref="paper",
                          x=1.1, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)



#PincWT2_summary <- ggplot(PincWT2_long, aes(`Proportion of Persons`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PincWT2_summary <- ggplot(PincWT2_long, aes(`Proportion of Persons`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PincWT2_stats <- summary_table(dplyr::group_by(PincWT2_long, `HH Income Level`), our_summary2)

PincWT2.plot <- plot_ly(PincWT2[-53,], x = ~`<$35k`, y = ~Name, type = 'bar', orientation = 'h', name = '<$35k', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `<$35k`), marker = list(color = 'rgba(0, 191, 255, 0.6)',
                                                                                             line = list(color = 'rgba(0, 191, 255, 1.0)',
                                                                                                         width = 3))) %>%
  add_trace(x = ~`$35k-<$75k`, name = '$35k - <$75k', hoverinfo = 'text', text = ~paste(Name,': ', `$35k-<$75k`),
            marker = list(color = 'rgba(178, 34, 34, 0.6)',
                          line = list(color = 'rgba(178, 34, 34, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`$75k+`, name = '$75k+', hoverinfo = 'text', text = ~paste(Name,': ', `$75k+`),
            marker = list(color = 'rgba(154, 205, 50, 0.6)',
                          line = list(color = 'rgba(154, 205, 50, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Income Level", xref="paper", yref="paper",
                          x=1.1, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PincWT3_summary <- ggplot(PincWT3_long, aes(`Proportion of Trips`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PincWT3_summary <- ggplot(PincWT3_long, aes(`Proportion of Trips`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PincWT3_stats <- summary_table(dplyr::group_by(PincWT3_long, `HH Income Level`), our_summary3)

PincWT3.plot <- plot_ly(PincWT3[-53,], x = ~`<$35k`, y = ~Name, type = 'bar', orientation = 'h', name = '<$35k', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `<$35k`), marker = list(color = 'rgba(0, 191, 255, 0.6)',
                                                                                             line = list(color = 'rgba(0, 191, 255, 1.0)',
                                                                                                         width = 3))) %>%
  add_trace(x = ~`$35k-<$75k`, name = '$35k - <$75k', hoverinfo = 'text', text = ~paste(Name,': ', `$35k-<$75k`),
            marker = list(color = 'rgba(178, 34, 34, 0.6)',
                          line = list(color = 'rgba(178, 34, 34, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`$75k+`, name = '$75k+', hoverinfo = 'text', text = ~paste(Name,': ', `$75k+`),
            marker = list(color = 'rgba(154, 205, 50, 0.6)',
                          line = list(color = 'rgba(154, 205, 50, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Income Level", xref="paper", yref="paper",
                          x=1.1, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#htrincWT_summary <- ggplot(htrincWT_long, aes(`Household Trip Rate`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
htrincWT_summary <- ggplot(htrincWT_long, aes(`Household Trip Rate`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 1.5) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

htrincWT_stats <- summary_table(dplyr::group_by(htrincWT_long, `HH Income Level`), our_summary4)

CBSA_main_htrincWT <- merge(CBSA_main, htrincWT,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_htrincWT <- colorNumeric(palette = "YlOrRd",
                             domain = c(CBSA_main_htrincWT$`<$35k`,
                                        CBSA_main_htrincWT$`$35k-<$75k`,
                                        CBSA_main_htrincWT$`$75k+`))

labels_htrincWT <- list()

labels_htrincWT[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrincWT$NAME, CBSA_main_htrincWT$`<$35k`
) %>% lapply(htmltools::HTML)

labels_htrincWT[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrincWT$NAME, CBSA_main_htrincWT$`$35k-<$75k`
) %>% lapply(htmltools::HTML)

labels_htrincWT[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrincWT$NAME, CBSA_main_htrincWT$`$75k+`
) %>% lapply(htmltools::HTML)

htrincWT.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_htrincWT,
              group = "<$35k",
              fillColor = ~pal_htrincWT(`<$35k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrincWT[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrincWT,
              group = "$35k-<$75k",
              fillColor = ~pal_htrincWT(`$35k-<$75k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrincWT[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrincWT,
              group = "$75k+",
              fillColor = ~pal_htrincWT(`$75k+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrincWT[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_htrincWT, values = c(CBSA_main_htrincWT$`<$35k`,
                                           CBSA_main_htrincWT$`$35k-<$75k`,
                                           CBSA_main_htrincWT$`$75k+`),
            opacity = 0.7, title = "Household Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("<$35k", "$35k-<$75k", "$75k+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("$35k-<$75k", "$75k+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Income Level</label></strong>');
        }
    ")

#ptrincWT_summary <- ggplot(ptrincWT_long, aes(`Person Trip Rate`, colour = `HH Income Level`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
ptrincWT_summary <- ggplot(ptrincWT_long, aes(`Person Trip Rate`, fill = `HH Income Level`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.25) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

ptrincWT_stats <- summary_table(dplyr::group_by(ptrincWT_long, `HH Income Level`), our_summary5)

CBSA_main_ptrincWT <- merge(CBSA_main, ptrincWT,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_ptrincWT <- colorNumeric(palette = "YlOrRd",
                             domain = c(CBSA_main_ptrincWT$`<$35k`,
                                        CBSA_main_ptrincWT$`$35k-<$75k`,
                                        CBSA_main_ptrincWT$`$75k+`))

labels_ptrincWT <- list()

labels_ptrincWT[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrincWT$NAME, CBSA_main_ptrincWT$`<$35k`
) %>% lapply(htmltools::HTML)

labels_ptrincWT[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrincWT$NAME, CBSA_main_ptrincWT$`$35k-<$75k`
) %>% lapply(htmltools::HTML)

labels_ptrincWT[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrincWT$NAME, CBSA_main_ptrincWT$`$75k+`
) %>% lapply(htmltools::HTML)

ptrincWT.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_ptrincWT,
              group = "<$35k",
              fillColor = ~pal_ptrincWT(`<$35k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrincWT[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrincWT,
              group = "$35k-<$75k",
              fillColor = ~pal_ptrincWT(`$35k-<$75k`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrincWT[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrincWT,
              group = "$75k+",
              fillColor = ~pal_ptrincWT(`$75k+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrincWT[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_ptrincWT, values = c(CBSA_main_ptrincWT$`<$35k`,
                                           CBSA_main_ptrincWT$`$35k-<$75k`,
                                           CBSA_main_ptrincWT$`$75k+`),
            opacity = 0.7, title = "Person Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("<$35k", "$35k-<$75k", "$75k+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("$35k-<$75k", "$75k+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Income Level</label></strong>');
        }
    ")


# size unweighted

#PsizeUn_summary <- ggplot(PsizeUn_long, aes(`Proportion of Households`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PsizeUn_summary <- ggplot(PsizeUn_long, aes(`Proportion of Households`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PsizeUn_stats <- summary_table(dplyr::group_by(PsizeUn_long, `Household Size`), our_summary1)

PsizeUn.plot <- plot_ly(PsizeUn[-53,], x = ~`1`, y = ~Name, type = 'bar', orientation = 'h', name = '1', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `1`), marker = list(color = 'rgba(255, 215, 0, 0.6)',
                                                                                         line = list(color = 'rgba(255, 215, 0, 1.0)',
                                                                                                     width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(255, 20, 147, 0.6)',
                          line = list(color = 'rgba(255, 20, 147, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3`, name = '3', hoverinfo = 'text', text = ~paste(Name,': ', `3`),
            marker = list(color = 'rgba(24, 13, 122, 0.6)',
                          line = list(color = 'rgba(24, 13, 122, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`4+`, name = "4+", hoverinfo = 'text', text = ~paste(Name,': ', `4+`),
            marker = list(color = 'rgba(170, 220, 125, 0.6)',
                          line = list(color = 'rgba(170, 220, 125, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Size", xref="paper", yref="paper",
                          x=1.06, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PsizeUn2_summary <- ggplot(PsizeUn2_long, aes(`Proportion of Persons`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PsizeUn2_summary <- ggplot(PsizeUn2_long, aes(`Proportion of Persons`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PsizeUn2_stats <- summary_table(dplyr::group_by(PsizeUn2_long, `Household Size`), our_summary2)

PsizeUn2.plot <- plot_ly(PsizeUn2[-53,], x = ~`1`, y = ~Name, type = 'bar', orientation = 'h', name = '1', height = 900, width = 1200,
                         hoverinfo = 'text', text = ~paste(Name,': ', `1`), marker = list(color = 'rgba(255, 215, 0, 0.6)',
                                                                                          line = list(color = 'rgba(255, 215, 0, 1.0)',
                                                                                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(255, 20, 147, 0.6)',
                          line = list(color = 'rgba(255, 20, 147, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3`, name = '3', hoverinfo = 'text', text = ~paste(Name,': ', `3`),
            marker = list(color = 'rgba(24, 13, 122, 0.6)',
                          line = list(color = 'rgba(24, 13, 122, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`4+`, name = "4+", hoverinfo = 'text', text = ~paste(Name,': ', `4+`),
            marker = list(color = 'rgba(170, 220, 125, 0.6)',
                          line = list(color = 'rgba(170, 220, 125, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Size", xref="paper", yref="paper",
                          x=1.06, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PsizeUn3_summary <- ggplot(PsizeUn3_long, aes(`Proportion of Trips`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PsizeUn3_summary <- ggplot(PsizeUn3_long, aes(`Proportion of Trips`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PsizeUn3_stats <- summary_table(dplyr::group_by(PsizeUn3_long, `Household Size`), our_summary3)

PsizeUn3.plot <- plot_ly(PsizeUn3[-53,], x = ~`1`, y = ~Name, type = 'bar', orientation = 'h', name = '1', height = 900, width = 1200,
                         hoverinfo = 'text', text = ~paste(Name,': ', `1`), marker = list(color = 'rgba(255, 215, 0, 0.6)',
                                                                                          line = list(color = 'rgba(255, 215, 0, 1.0)',
                                                                                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(255, 20, 147, 0.6)',
                          line = list(color = 'rgba(255, 20, 147, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3`, name = '3', hoverinfo = 'text', text = ~paste(Name,': ', `3`),
            marker = list(color = 'rgba(24, 13, 122, 0.6)',
                          line = list(color = 'rgba(24, 13, 122, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`4+`, name = "4+", hoverinfo = 'text', text = ~paste(Name,': ', `4+`),
            marker = list(color = 'rgba(170, 220, 125, 0.6)',
                          line = list(color = 'rgba(170, 220, 125, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Size", xref="paper", yref="paper",
                          x=1.06, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#htrsizeUn_summary <- ggplot(htrsizeUn_long, aes(`Household Trip Rate`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
htrsizeUn_summary <- ggplot(htrsizeUn_long, aes(`Household Trip Rate`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 1.5) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

htrsizeUn_stats <- summary_table(dplyr::group_by(htrsizeUn_long, `Household Size`), our_summary4)

CBSA_main_htrsizeUn <- merge(CBSA_main, htrsizeUn,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_htrsizeUn <- colorNumeric(palette = "YlGnBu",
                             domain = c(CBSA_main_htrsizeUn$`1`,
                                        CBSA_main_htrsizeUn$`2`,
                                        CBSA_main_htrsizeUn$`3`,
                                        CBSA_main_htrsizeUn$`4+`))

labels_htrsizeUn <- list()

labels_htrsizeUn[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeUn$NAME, CBSA_main_htrsizeUn$`1`
) %>% lapply(htmltools::HTML)

labels_htrsizeUn[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeUn$NAME, CBSA_main_htrsizeUn$`2`
) %>% lapply(htmltools::HTML)

labels_htrsizeUn[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeUn$NAME, CBSA_main_htrsizeUn$`3`
) %>% lapply(htmltools::HTML)

labels_htrsizeUn[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeUn$NAME, CBSA_main_htrsizeUn$`4+`
) %>% lapply(htmltools::HTML)

htrsizeUn.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_htrsizeUn,
              group = "1",
              fillColor = ~pal_htrsizeUn(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeUn[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrsizeUn,
              group = "2",
              fillColor = ~pal_htrsizeUn(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeUn[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrsizeUn,
              group = "3",
              fillColor = ~pal_htrsizeUn(`3`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeUn[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrsizeUn,
              group = "4+",
              fillColor = ~pal_htrsizeUn(`4+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeUn[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_htrsizeUn, values = c(CBSA_main_htrsizeUn$`1`,
                                           CBSA_main_htrsizeUn$`2`,
                                           CBSA_main_htrsizeUn$`3`,
                                           CBSA_main_htrsizeUn$`4+`),
            opacity = 0.7, title = "Household Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("1", "2", "3", "4+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("2", "3", "4+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Size</label></strong>');
        }
    ")

#ptrsizeUn_summary <- ggplot(ptrsizeUn_long, aes(`Person Trip Rate`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
ptrsizeUn_summary <- ggplot(ptrsizeUn_long, aes(`Person Trip Rate`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.25) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

ptrsizeUn_stats <- summary_table(dplyr::group_by(ptrsizeUn_long, `Household Size`), our_summary5)

CBSA_main_ptrsizeUn <- merge(CBSA_main, ptrsizeUn,
                             by.x = c("CBSAFP", "NAME"),
                             by.y = c("CBSA", "Name"))

pal_ptrsizeUn <- colorNumeric(palette = "YlGnBu",
                              domain = c(CBSA_main_ptrsizeUn$`1`,
                                         CBSA_main_ptrsizeUn$`2`,
                                         CBSA_main_ptrsizeUn$`3`,
                                         CBSA_main_ptrsizeUn$`4+`))

labels_ptrsizeUn <- list()

labels_ptrsizeUn[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeUn$NAME, CBSA_main_ptrsizeUn$`1`
) %>% lapply(htmltools::HTML)

labels_ptrsizeUn[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeUn$NAME, CBSA_main_ptrsizeUn$`2`
) %>% lapply(htmltools::HTML)

labels_ptrsizeUn[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeUn$NAME, CBSA_main_ptrsizeUn$`3`
) %>% lapply(htmltools::HTML)

labels_ptrsizeUn[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeUn$NAME, CBSA_main_ptrsizeUn$`4+`
) %>% lapply(htmltools::HTML)

ptrsizeUn.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_ptrsizeUn,
              group = "1",
              fillColor = ~pal_ptrsizeUn(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeUn[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrsizeUn,
              group = "2",
              fillColor = ~pal_ptrsizeUn(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeUn[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrsizeUn,
              group = "3",
              fillColor = ~pal_ptrsizeUn(`3`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeUn[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrsizeUn,
              group = "4+",
              fillColor = ~pal_ptrsizeUn(`4+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeUn[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_ptrsizeUn, values = c(CBSA_main_ptrsizeUn$`1`,
                                            CBSA_main_ptrsizeUn$`2`,
                                            CBSA_main_ptrsizeUn$`3`,
                                            CBSA_main_ptrsizeUn$`4+`),
            opacity = 0.7, title = "Person Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("1", "2", "3", "4+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("2", "3", "4+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Size</label></strong>');
        }
    ")


# size weighted

#PsizeWT_summary <- ggplot(PsizeWT_long, aes(`Proportion of Households`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PsizeWT_summary <- ggplot(PsizeWT_long, aes(`Proportion of Households`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PsizeWT_stats <- summary_table(dplyr::group_by(PsizeWT_long, `Household Size`), our_summary1)

PsizeWT.plot <- plot_ly(PsizeWT[-53,], x = ~`1`, y = ~Name, type = 'bar', orientation = 'h', name = '1', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `1`), marker = list(color = 'rgba(255, 215, 0, 0.6)',
                                                                                         line = list(color = 'rgba(255, 215, 0, 1.0)',
                                                                                                     width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(255, 20, 147, 0.6)',
                          line = list(color = 'rgba(255, 20, 147, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3`, name = '3', hoverinfo = 'text', text = ~paste(Name,': ', `3`),
            marker = list(color = 'rgba(24, 13, 122, 0.6)',
                          line = list(color = 'rgba(24, 13, 122, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`4+`, name = "4+", hoverinfo = 'text', text = ~paste(Name,': ', `4+`),
            marker = list(color = 'rgba(170, 220, 125, 0.6)',
                          line = list(color = 'rgba(170, 220, 125, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Size", xref="paper", yref="paper",
                          x=1.06, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PsizeWT2_summary <- ggplot(PsizeWT2_long, aes(`Proportion of Persons`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PsizeWT2_summary <- ggplot(PsizeWT2_long, aes(`Proportion of Persons`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PsizeWT2_stats <- summary_table(dplyr::group_by(PsizeWT2_long, `Household Size`), our_summary2)

PsizeWT2.plot <- plot_ly(PsizeWT2[-53,], x = ~`1`, y = ~Name, type = 'bar', orientation = 'h', name = '1', height = 900, width = 1200,
                         hoverinfo = 'text', text = ~paste(Name,': ', `1`), marker = list(color = 'rgba(255, 215, 0, 0.6)',
                                                                                          line = list(color = 'rgba(255, 215, 0, 1.0)',
                                                                                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(255, 20, 147, 0.6)',
                          line = list(color = 'rgba(255, 20, 147, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3`, name = '3', hoverinfo = 'text', text = ~paste(Name,': ', `3`),
            marker = list(color = 'rgba(24, 13, 122, 0.6)',
                          line = list(color = 'rgba(24, 13, 122, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`4+`, name = "4+", hoverinfo = 'text', text = ~paste(Name,': ', `4+`),
            marker = list(color = 'rgba(170, 220, 125, 0.6)',
                          line = list(color = 'rgba(170, 220, 125, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Size", xref="paper", yref="paper",
                          x=1.06, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PsizeWT3_summary <- ggplot(PsizeWT3_long, aes(`Proportion of Trips`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PsizeWT3_summary <- ggplot(PsizeWT3_long, aes(`Proportion of Trips`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PsizeWT3_stats <- summary_table(dplyr::group_by(PsizeWT3_long, `Household Size`), our_summary3)

PsizeWT3.plot <- plot_ly(PsizeWT3[-53,], x = ~`1`, y = ~Name, type = 'bar', orientation = 'h', name = '1', height = 900, width = 1200,
                         hoverinfo = 'text', text = ~paste(Name,': ', `1`), marker = list(color = 'rgba(255, 215, 0, 0.6)',
                                                                                          line = list(color = 'rgba(255, 215, 0, 1.0)',
                                                                                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(255, 20, 147, 0.6)',
                          line = list(color = 'rgba(255, 20, 147, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3`, name = '3', hoverinfo = 'text', text = ~paste(Name,': ', `3`),
            marker = list(color = 'rgba(24, 13, 122, 0.6)',
                          line = list(color = 'rgba(24, 13, 122, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`4+`, name = "4+", hoverinfo = 'text', text = ~paste(Name,': ', `4+`),
            marker = list(color = 'rgba(170, 220, 125, 0.6)',
                          line = list(color = 'rgba(170, 220, 125, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="HH Size", xref="paper", yref="paper",
                          x=1.06, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#htrsizeWT_summary <- ggplot(htrsizeWT_long, aes(`Household Trip Rate`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
htrsizeWT_summary <- ggplot(htrsizeWT_long, aes(`Household Trip Rate`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 1.5) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

htrsizeWT_stats <- summary_table(dplyr::group_by(htrsizeWT_long, `Household Size`), our_summary4)

CBSA_main_htrsizeWT <- merge(CBSA_main, htrsizeWT,
                             by.x = c("CBSAFP", "NAME"),
                             by.y = c("CBSA", "Name"))

pal_htrsizeWT <- colorNumeric(palette = "YlGnBu",
                              domain = c(CBSA_main_htrsizeWT$`1`,
                                         CBSA_main_htrsizeWT$`2`,
                                         CBSA_main_htrsizeWT$`3`,
                                         CBSA_main_htrsizeWT$`4+`))

labels_htrsizeWT <- list()

labels_htrsizeWT[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeWT$NAME, CBSA_main_htrsizeWT$`1`
) %>% lapply(htmltools::HTML)

labels_htrsizeWT[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeWT$NAME, CBSA_main_htrsizeWT$`2`
) %>% lapply(htmltools::HTML)

labels_htrsizeWT[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeWT$NAME, CBSA_main_htrsizeWT$`3`
) %>% lapply(htmltools::HTML)

labels_htrsizeWT[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrsizeWT$NAME, CBSA_main_htrsizeWT$`4+`
) %>% lapply(htmltools::HTML)

htrsizeWT.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_htrsizeWT,
              group = "1",
              fillColor = ~pal_htrsizeWT(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeWT[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrsizeWT,
              group = "2",
              fillColor = ~pal_htrsizeWT(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeWT[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrsizeWT,
              group = "3",
              fillColor = ~pal_htrsizeWT(`3`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeWT[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrsizeWT,
              group = "4+",
              fillColor = ~pal_htrsizeWT(`4+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrsizeWT[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_htrsizeWT, values = c(CBSA_main_htrsizeWT$`1`,
                                            CBSA_main_htrsizeWT$`2`,
                                            CBSA_main_htrsizeWT$`3`,
                                            CBSA_main_htrsizeWT$`4+`),
            opacity = 0.7, title = "Household Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("1", "2", "3", "4+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("2", "3", "4+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Size</label></strong>');
        }
    ")

#ptrsizeWT_summary <- ggplot(ptrsizeWT_long, aes(`Person Trip Rate`, colour = `Household Size`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
ptrsizeWT_summary <- ggplot(ptrsizeWT_long, aes(`Person Trip Rate`, fill = `Household Size`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.25) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

ptrsizeWT_stats <- summary_table(dplyr::group_by(ptrsizeWT_long, `Household Size`), our_summary5)

CBSA_main_ptrsizeWT <- merge(CBSA_main, ptrsizeWT,
                             by.x = c("CBSAFP", "NAME"),
                             by.y = c("CBSA", "Name"))

pal_ptrsizeWT <- colorNumeric(palette = "YlGnBu",
                              domain = c(CBSA_main_ptrsizeWT$`1`,
                                         CBSA_main_ptrsizeWT$`2`,
                                         CBSA_main_ptrsizeWT$`3`,
                                         CBSA_main_ptrsizeWT$`4+`))

labels_ptrsizeWT <- list()

labels_ptrsizeWT[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeWT$NAME, CBSA_main_ptrsizeWT$`1`
) %>% lapply(htmltools::HTML)

labels_ptrsizeWT[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeWT$NAME, CBSA_main_ptrsizeWT$`2`
) %>% lapply(htmltools::HTML)

labels_ptrsizeWT[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeWT$NAME, CBSA_main_ptrsizeWT$`3`
) %>% lapply(htmltools::HTML)

labels_ptrsizeWT[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrsizeWT$NAME, CBSA_main_ptrsizeWT$`4+`
) %>% lapply(htmltools::HTML)

ptrsizeWT.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_ptrsizeWT,
              group = "1",
              fillColor = ~pal_ptrsizeWT(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeWT[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrsizeWT,
              group = "2",
              fillColor = ~pal_ptrsizeWT(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeWT[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrsizeWT,
              group = "3",
              fillColor = ~pal_ptrsizeWT(`3`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeWT[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrsizeWT,
              group = "4+",
              fillColor = ~pal_ptrsizeWT(`4+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrsizeWT[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_ptrsizeWT, values = c(CBSA_main_ptrsizeWT$`1`,
                                            CBSA_main_ptrsizeWT$`2`,
                                            CBSA_main_ptrsizeWT$`3`,
                                            CBSA_main_ptrsizeWT$`4+`),
            opacity = 0.7, title = "Person Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("1", "2", "3", "4+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("2", "3", "4+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">HH Size</label></strong>');
        }
    ")


# vehicle unweighted

#PvehUn_summary <- ggplot(PvehUn_long, aes(`Proportion of Households`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + labs(colour="No. of\nVehicles") + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PvehUn_summary <- ggplot(PvehUn_long, aes(`Proportion of Households`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PvehUn_stats <- summary_table(dplyr::group_by(PvehUn_long, `No. of Vehicles Owned`), our_summary1)

PvehUn.plot <- plot_ly(PvehUn[-53,], x = ~`0`, y = ~Name, type = 'bar', orientation = 'h', name = '0', height = 900, width = 1200,
                       hoverinfo = 'text', text = ~paste(Name,': ', `0`), marker = list(color = 'rgba(100, 149, 237, 0.6)',
                                                                                        line = list(color = 'rgba(100, 149, 237, 1.0)',
                                                                                                    width = 3))) %>%
  add_trace(x = ~`1`, name = '1', hoverinfo = 'text', text = ~paste(Name,': ', `1`),
            marker = list(color = 'rgba(205, 133, 63, 0.6)',
                          line = list(color = 'rgba(205, 133, 63, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(143, 188, 143, 0.6)',
                          line = list(color = 'rgba(143, 188, 143, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3+`, name = "3+", hoverinfo = 'text', text = ~paste(Name,': ', `3+`),
            marker = list(color = 'rgba(148, 0, 211, 0.6)',
                          line = list(color = 'rgba(148, 0, 211, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="No. of Vehicles", xref="paper", yref="paper",
                          x=1.05, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PvehUn2_summary <- ggplot(PvehUn2_long, aes(`Proportion of Persons`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + labs(colour="No. of\nVehicles") + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PvehUn2_summary <- ggplot(PvehUn2_long, aes(`Proportion of Persons`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PvehUn2_stats <- summary_table(dplyr::group_by(PvehUn2_long, `No. of Vehicles Owned`), our_summary2)

PvehUn2.plot <- plot_ly(PvehUn2[-53,], x = ~`0`, y = ~Name, type = 'bar', orientation = 'h', name = '0', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `0`), marker = list(color = 'rgba(100, 149, 237, 0.6)',
                                                                                         line = list(color = 'rgba(100, 149, 237, 1.0)',
                                                                                                     width = 3))) %>%
  add_trace(x = ~`1`, name = '1', hoverinfo = 'text', text = ~paste(Name,': ', `1`),
            marker = list(color = 'rgba(205, 133, 63, 0.6)',
                          line = list(color = 'rgba(205, 133, 63, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(143, 188, 143, 0.6)',
                          line = list(color = 'rgba(143, 188, 143, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3+`, name = "3+", hoverinfo = 'text', text = ~paste(Name,': ', `3+`),
            marker = list(color = 'rgba(148, 0, 211, 0.6)',
                          line = list(color = 'rgba(148, 0, 211, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="No. of Vehicles", xref="paper", yref="paper",
                          x=1.05, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PvehUn3_summary <- ggplot(PvehUn3_long, aes(`Proportion of Trips`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + labs(colour="No. of\nVehicles") + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PvehUn3_summary <- ggplot(PvehUn3_long, aes(`Proportion of Trips`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PvehUn3_stats <- summary_table(dplyr::group_by(PvehUn3_long, `No. of Vehicles Owned`), our_summary3)

PvehUn3.plot <- plot_ly(PvehUn3[-53,], x = ~`0`, y = ~Name, type = 'bar', orientation = 'h', name = '0', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `0`), marker = list(color = 'rgba(100, 149, 237, 0.6)',
                                                                                         line = list(color = 'rgba(100, 149, 237, 1.0)',
                                                                                                     width = 3))) %>%
  add_trace(x = ~`1`, name = '1', hoverinfo = 'text', text = ~paste(Name,': ', `1`),
            marker = list(color = 'rgba(205, 133, 63, 0.6)',
                          line = list(color = 'rgba(205, 133, 63, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(143, 188, 143, 0.6)',
                          line = list(color = 'rgba(143, 188, 143, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3+`, name = "3+", hoverinfo = 'text', text = ~paste(Name,': ', `3+`),
            marker = list(color = 'rgba(148, 0, 211, 0.6)',
                          line = list(color = 'rgba(148, 0, 211, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="No. of Vehicles", xref="paper", yref="paper",
                          x=1.05, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#htrvehUn_summary <- ggplot(htrvehUn_long, aes(`Household Trip Rate`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
htrvehUn_summary <- ggplot(htrvehUn_long, aes(`Household Trip Rate`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 1.5) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

htrvehUn_stats <- summary_table(dplyr::group_by(htrvehUn_long, `No. of Vehicles Owned`), our_summary4)

CBSA_main_htrvehUn <- merge(CBSA_main, htrvehUn,
                             by.x = c("CBSAFP", "NAME"),
                             by.y = c("CBSA", "Name"))

pal_htrvehUn <- colorNumeric(palette = "inferno",
                              domain = c(CBSA_main_htrvehUn$`0`,
                                         CBSA_main_htrvehUn$`1`,
                                         CBSA_main_htrvehUn$`2`,
                                         CBSA_main_htrvehUn$`3+`))

labels_htrvehUn <- list()

labels_htrvehUn[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehUn$NAME, CBSA_main_htrvehUn$`0`
) %>% lapply(htmltools::HTML)

labels_htrvehUn[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehUn$NAME, CBSA_main_htrvehUn$`1`
) %>% lapply(htmltools::HTML)

labels_htrvehUn[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehUn$NAME, CBSA_main_htrvehUn$`2`
) %>% lapply(htmltools::HTML)

labels_htrvehUn[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehUn$NAME, CBSA_main_htrvehUn$`3+`
) %>% lapply(htmltools::HTML)

htrvehUn.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_htrvehUn,
              group = "0",
              fillColor = ~pal_htrvehUn(`0`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehUn[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrvehUn,
              group = "1",
              fillColor = ~pal_htrvehUn(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehUn[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrvehUn,
              group = "2",
              fillColor = ~pal_htrvehUn(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehUn[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrvehUn,
              group = "3+",
              fillColor = ~pal_htrvehUn(`3+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehUn[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_htrvehUn, values = c(CBSA_main_htrvehUn$`0`,
                                           CBSA_main_htrvehUn$`1`,
                                           CBSA_main_htrvehUn$`2`,
                                           CBSA_main_htrvehUn$`3+`),
            opacity = 0.7, title = "Household Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("0", "1", "2", "3+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("1", "2", "3+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">No. of Vehicles</label></strong>');
        }
    ")

#ptrvehUn_summary <- ggplot(ptrvehUn_long, aes(`Person Trip Rate`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
ptrvehUn_summary <- ggplot(ptrvehUn_long, aes(`Person Trip Rate`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.25) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

ptrvehUn_stats <- summary_table(dplyr::group_by(ptrvehUn_long, `No. of Vehicles Owned`), our_summary5)

CBSA_main_ptrvehUn <- merge(CBSA_main, ptrvehUn,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_ptrvehUn <- colorNumeric(palette = "inferno",
                             domain = c(CBSA_main_ptrvehUn$`0`,
                                        CBSA_main_ptrvehUn$`1`,
                                        CBSA_main_ptrvehUn$`2`,
                                        CBSA_main_ptrvehUn$`3+`))

labels_ptrvehUn <- list()

labels_ptrvehUn[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehUn$NAME, CBSA_main_ptrvehUn$`0`
) %>% lapply(htmltools::HTML)

labels_ptrvehUn[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehUn$NAME, CBSA_main_ptrvehUn$`1`
) %>% lapply(htmltools::HTML)

labels_ptrvehUn[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehUn$NAME, CBSA_main_ptrvehUn$`2`
) %>% lapply(htmltools::HTML)

labels_ptrvehUn[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehUn$NAME, CBSA_main_ptrvehUn$`3+`
) %>% lapply(htmltools::HTML)

ptrvehUn.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_ptrvehUn,
              group = "0",
              fillColor = ~pal_ptrvehUn(`0`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehUn[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrvehUn,
              group = "1",
              fillColor = ~pal_ptrvehUn(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehUn[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrvehUn,
              group = "2",
              fillColor = ~pal_ptrvehUn(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehUn[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrvehUn,
              group = "3+",
              fillColor = ~pal_ptrvehUn(`3+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehUn[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_ptrvehUn, values = c(CBSA_main_ptrvehUn$`0`,
                                           CBSA_main_ptrvehUn$`1`,
                                           CBSA_main_ptrvehUn$`2`,
                                           CBSA_main_ptrvehUn$`3+`),
            opacity = 0.7, title = "Person Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("0", "1", "2", "3+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("1", "2", "3+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">No. of Vehicles</label></strong>');
        }
    ")


# vehicle weighted

#PvehWT_summary <- ggplot(PvehWT_long, aes(`Proportion of Households`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + labs(colour="No. of\nVehicles") + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PvehWT_summary <- ggplot(PvehWT_long, aes(`Proportion of Households`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PvehWT_stats <- summary_table(dplyr::group_by(PvehWT_long, `No. of Vehicles Owned`), our_summary1)

PvehWT.plot <- plot_ly(PvehWT[-53,], x = ~`0`, y = ~Name, type = 'bar', orientation = 'h', name = '0', height = 900, width = 1200,
                       hoverinfo = 'text', text = ~paste(Name,': ', `0`), marker = list(color = 'rgba(100, 149, 237, 0.6)',
                                                                                        line = list(color = 'rgba(100, 149, 237, 1.0)',
                                                                                                    width = 3))) %>%
  add_trace(x = ~`1`, name = '1', hoverinfo = 'text', text = ~paste(Name,': ', `1`),
            marker = list(color = 'rgba(205, 133, 63, 0.6)',
                          line = list(color = 'rgba(205, 133, 63, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(143, 188, 143, 0.6)',
                          line = list(color = 'rgba(143, 188, 143, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3+`, name = "3+", hoverinfo = 'text', text = ~paste(Name,': ', `3+`),
            marker = list(color = 'rgba(148, 0, 211, 0.6)',
                          line = list(color = 'rgba(148, 0, 211, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="No. of Vehicles", xref="paper", yref="paper",
                          x=1.05, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PvehWT2_summary <- ggplot(PvehWT2_long, aes(`Proportion of Persons`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + labs(colour="No. of\nVehicles") + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PvehWT2_summary <- ggplot(PvehWT2_long, aes(`Proportion of Persons`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PvehWT2_stats <- summary_table(dplyr::group_by(PvehWT2_long, `No. of Vehicles Owned`), our_summary2)

PvehWT2.plot <- plot_ly(PvehWT2[-53,], x = ~`0`, y = ~Name, type = 'bar', orientation = 'h', name = '0', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `0`), marker = list(color = 'rgba(100, 149, 237, 0.6)',
                                                                                         line = list(color = 'rgba(100, 149, 237, 1.0)',
                                                                                                     width = 3))) %>%
  add_trace(x = ~`1`, name = '1', hoverinfo = 'text', text = ~paste(Name,': ', `1`),
            marker = list(color = 'rgba(205, 133, 63, 0.6)',
                          line = list(color = 'rgba(205, 133, 63, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(143, 188, 143, 0.6)',
                          line = list(color = 'rgba(143, 188, 143, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3+`, name = "3+", hoverinfo = 'text', text = ~paste(Name,': ', `3+`),
            marker = list(color = 'rgba(148, 0, 211, 0.6)',
                          line = list(color = 'rgba(148, 0, 211, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="No. of Vehicles", xref="paper", yref="paper",
                          x=1.05, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#PvehWT3_summary <- ggplot(PvehWT3_long, aes(`Proportion of Trips`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + labs(colour="No. of\nVehicles") + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
PvehWT3_summary <- ggplot(PvehWT3_long, aes(`Proportion of Trips`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.05) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

PvehWT3_stats <- summary_table(dplyr::group_by(PvehWT3_long, `No. of Vehicles Owned`), our_summary3)

PvehWT3.plot <- plot_ly(PvehWT3[-53,], x = ~`0`, y = ~Name, type = 'bar', orientation = 'h', name = '0', height = 900, width = 1200,
                        hoverinfo = 'text', text = ~paste(Name,': ', `0`), marker = list(color = 'rgba(100, 149, 237, 0.6)',
                                                                                         line = list(color = 'rgba(100, 149, 237, 1.0)',
                                                                                                     width = 3))) %>%
  add_trace(x = ~`1`, name = '1', hoverinfo = 'text', text = ~paste(Name,': ', `1`),
            marker = list(color = 'rgba(205, 133, 63, 0.6)',
                          line = list(color = 'rgba(205, 133, 63, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`2`, name = '2', hoverinfo = 'text', text = ~paste(Name,': ', `2`),
            marker = list(color = 'rgba(143, 188, 143, 0.6)',
                          line = list(color = 'rgba(143, 188, 143, 1.0)',
                                      width = 3))) %>%
  add_trace(x = ~`3+`, name = "3+", hoverinfo = 'text', text = ~paste(Name,': ', `3+`),
            marker = list(color = 'rgba(148, 0, 211, 0.6)',
                          line = list(color = 'rgba(148, 0, 211, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack', legend = l,
         annotations=list(text="No. of Vehicles", xref="paper", yref="paper",
                          x=1.05, xanchor="center",
                          y=1, yanchor="bottom",    # Same y as legend below
                          legendtitle=TRUE, showarrow=FALSE),
         xaxis = list(title = "", gridcolor = "black", gridwidth = 3, tickfont = list(size = 16)),
         yaxis = a)

#htrvehWT_summary <- ggplot(htrvehWT_long, aes(`Household Trip Rate`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
htrvehWT_summary <- ggplot(htrvehWT_long, aes(`Household Trip Rate`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 1.5) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

htrvehWT_stats <- summary_table(dplyr::group_by(htrvehWT_long, `No. of Vehicles Owned`), our_summary4)

CBSA_main_htrvehWT <- merge(CBSA_main, htrvehWT,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_htrvehWT <- colorNumeric(palette = "inferno",
                             domain = c(CBSA_main_htrvehWT$`0`,
                                        CBSA_main_htrvehWT$`1`,
                                        CBSA_main_htrvehWT$`2`,
                                        CBSA_main_htrvehWT$`3+`))

labels_htrvehWT <- list()

labels_htrvehWT[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehWT$NAME, CBSA_main_htrvehWT$`0`
) %>% lapply(htmltools::HTML)

labels_htrvehWT[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehWT$NAME, CBSA_main_htrvehWT$`1`
) %>% lapply(htmltools::HTML)

labels_htrvehWT[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehWT$NAME, CBSA_main_htrvehWT$`2`
) %>% lapply(htmltools::HTML)

labels_htrvehWT[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / household",
  CBSA_main_htrvehWT$NAME, CBSA_main_htrvehWT$`3+`
) %>% lapply(htmltools::HTML)

htrvehWT.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_htrvehWT,
              group = "0",
              fillColor = ~pal_htrvehWT(`0`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehWT[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrvehWT,
              group = "1",
              fillColor = ~pal_htrvehWT(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehWT[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrvehWT,
              group = "2",
              fillColor = ~pal_htrvehWT(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehWT[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_htrvehWT,
              group = "3+",
              fillColor = ~pal_htrvehWT(`3+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_htrvehWT[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_htrvehWT, values = c(CBSA_main_htrvehWT$`0`,
                                           CBSA_main_htrvehWT$`1`,
                                           CBSA_main_htrvehWT$`2`,
                                           CBSA_main_htrvehWT$`3+`),
            opacity = 0.7, title = "Household Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("0", "1", "2", "3+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("1", "2", "3+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">No. of Vehicles</label></strong>');
        }
    ")

#ptrvehWT_summary <- ggplot(ptrvehWT_long, aes(`Person Trip Rate`, colour = `No. of Vehicles Owned`)) + geom_density(size=1.5) + theme(text = element_text(size=14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))
ptrvehWT_summary <- ggplot(ptrvehWT_long, aes(`Person Trip Rate`, fill = `No. of Vehicles Owned`)) + geom_histogram(position = "identity", alpha = 0.4, binwidth = 0.25) + ylab("CBSA Count") + theme(text = element_text(size = 14), axis.text = element_text(size = 16), legend.text = element_text(size = 12.5))

ptrvehWT_stats <- summary_table(dplyr::group_by(ptrvehWT_long, `No. of Vehicles Owned`), our_summary5)

CBSA_main_ptrvehWT <- merge(CBSA_main, ptrvehWT,
                            by.x = c("CBSAFP", "NAME"),
                            by.y = c("CBSA", "Name"))

pal_ptrvehWT <- colorNumeric(palette = "inferno",
                             domain = c(CBSA_main_ptrvehWT$`0`,
                                        CBSA_main_ptrvehWT$`1`,
                                        CBSA_main_ptrvehWT$`2`,
                                        CBSA_main_ptrvehWT$`3+`))

labels_ptrvehWT <- list()

labels_ptrvehWT[[1]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehWT$NAME, CBSA_main_ptrvehWT$`0`
) %>% lapply(htmltools::HTML)

labels_ptrvehWT[[2]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehWT$NAME, CBSA_main_ptrvehWT$`1`
) %>% lapply(htmltools::HTML)

labels_ptrvehWT[[3]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehWT$NAME, CBSA_main_ptrvehWT$`2`
) %>% lapply(htmltools::HTML)

labels_ptrvehWT[[4]] <- sprintf(
  "<strong>%s</strong><br/>%g trips / person",
  CBSA_main_ptrvehWT$NAME, CBSA_main_ptrvehWT$`3+`
) %>% lapply(htmltools::HTML)

ptrvehWT.map <- leaflet(states) %>%
  setView(-96, 35.8, 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CBSA_main_ptrvehWT,
              group = "0",
              fillColor = ~pal_ptrvehWT(`0`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehWT[[1]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrvehWT,
              group = "1",
              fillColor = ~pal_ptrvehWT(`1`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehWT[[2]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrvehWT,
              group = "2",
              fillColor = ~pal_ptrvehWT(`2`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehWT[[3]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addPolygons(data = CBSA_main_ptrvehWT,
              group = "3+",
              fillColor = ~pal_ptrvehWT(`3+`),
              color = "black",
              fillOpacity = 0.7,
              weight = 2,
              dashArray = "3",
              highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
              label = labels_ptrvehWT[[4]],
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal_ptrvehWT, values = c(CBSA_main_ptrvehWT$`0`,
                                           CBSA_main_ptrvehWT$`1`,
                                           CBSA_main_ptrvehWT$`2`,
                                           CBSA_main_ptrvehWT$`3+`),
            opacity = 0.7, title = "Person Trip Rates", position = "bottomright") %>%
  addLayersControl(baseGroups = c("0", "1", "2", "3+"),
                   options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("1", "2", "3+")) %>%
  htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-list').prepend('<strong><label style=\"text-align:center\">No. of Vehicles</label></strong>');
        }
    ")



Household = list(
  
  `Household Size` = list(Unweighted = list(all.dataUn_hhsize, PsizeUn.plot, PsizeUn_summary, PsizeUn_stats), Weighted = list(all.dataWT_hhsize, PsizeWT.plot, PsizeWT_summary, PsizeWT_stats)),
  
  `Income Level` = list(Unweighted = list(all.dataUn_income, PincUn.plot, PincUn_summary, PincUn_stats), Weighted = list(all.dataWT_income, PincWT.plot, PincWT_summary, PincWT_stats)),
  
  `Number of Vehicles` = list(Unweighted = list(all.dataUn_hhveh, PvehUn.plot, PvehUn_summary, PvehUn_stats), Weighted = list(all.dataWT_hhveh, PvehWT.plot, PvehWT_summary, PvehWT_stats))
  
)

Person = list(
  
  `Household Size` = list(Unweighted = list(all.dataUn_hhsize, PsizeUn2.plot, PsizeUn2_summary, PsizeUn2_stats), Weighted = list(all.dataWT_hhsize, PsizeWT2.plot, PsizeWT2_summary, PsizeWT2_stats)),
  
  `Income Level` = list(Unweighted = list(all.dataUn_income, PincUn2.plot, PincUn2_summary, PincUn2_stats), Weighted = list(all.dataWT_income, PincWT2.plot, PincWT2_summary, PincWT2_stats)),
  
  `Number of Vehicles` = list(Unweighted = list(all.dataUn_hhveh, PvehUn2.plot, PvehUn2_summary, PvehUn2_stats), Weighted = list(all.dataWT_hhveh, PvehWT2.plot, PvehWT2_summary, PvehWT2_stats))
  
)

Trip = list(
  
  `Household Size` = list(Unweighted = list(all.dataUn_hhsize, PsizeUn3.plot, PsizeUn3_summary, PsizeUn3_stats), Weighted = list(all.dataWT_hhsize, PsizeWT3.plot, PsizeWT3_summary, PsizeWT3_stats)),
  
  `Income Level` = list(Unweighted = list(all.dataUn_income, PincUn3.plot, PincUn3_summary, PincUn3_stats), Weighted = list(all.dataWT_income, PincWT3.plot, PincWT3_summary, PincWT3_stats)),
  
  `Number of Vehicles` = list(Unweighted = list(all.dataUn_hhveh, PvehUn3.plot, PvehUn3_summary, PvehUn3_stats), Weighted = list(all.dataWT_hhveh, PvehWT3.plot, PvehWT3_summary, PvehWT3_stats))
  
)

HTR = list(
  
  `Household Size` = list(Unweighted = list(all.dataUn_hhsize, htrsizeUn.map, htrsizeUn_summary, htrsizeUn_stats), Weighted = list(all.dataWT_hhsize, htrsizeWT.map, htrsizeWT_summary, htrsizeWT_stats)),
  
  `Income Level` = list(Unweighted = list(all.dataUn_income, htrincUn.map, htrincUn_summary, htrincUn_stats), Weighted = list(all.dataWT_income, htrincWT.map, htrincWT_summary, htrincWT_stats)),
  
  `Number of Vehicles` = list(Unweighted = list(all.dataUn_hhveh, htrvehUn.map, htrvehUn_summary, htrvehUn_stats), Weighted = list(all.dataWT_hhveh, htrvehWT.map, htrvehWT_summary, htrvehWT_stats))
  
)

PTR = list(
  
  `Household Size` = list(Unweighted = list(all.dataUn_hhsize, ptrsizeUn.map, ptrsizeUn_summary, ptrsizeUn_stats), Weighted = list(all.dataWT_hhsize, ptrsizeWT.map, ptrsizeWT_summary, ptrsizeWT_stats)),
  
  `Income Level` = list(Unweighted = list(all.dataUn_income, ptrincUn.map, ptrincUn_summary, ptrincUn_stats), Weighted = list(all.dataWT_income, ptrincWT.map, ptrincWT_summary, ptrincWT_stats)),
  
  `Number of Vehicles` = list(Unweighted = list(all.dataUn_hhveh, ptrvehUn.map, ptrvehUn_summary, ptrvehUn_stats), Weighted = list(all.dataWT_hhveh, ptrvehWT.map, ptrvehWT_summary, ptrvehWT_stats))
  
)

choice_data <- names(Household)


library(DT)
library(shiny)
library(shinythemes)

ui <- fluidPage( theme = shinytheme("cosmo"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 helpText("Interactive app for comparing 2017 NHTS demographic information and trip rates among core-based statistical areas (CBSA)"),
                 br(),
                 selectInput("dataset", "Compare by:", width = "80%",
                             choices = choice_data,
                             selected = "Age"),
                 br(),
                 selectInput("choice", "Visualize:", width = "80%",
                             choices = c("Proportion of Households", "Proportion of Persons", "Proportion of Trips", "Household Trip Rates", "Person Trip Rates"),
                             selected = "Proportion of Households"),
                 radioButtons(inputId = "ptype", label = "", choices = c("Unweighted", "Weighted")),
                 br(), br(), br(), br(), br(),
                 downloadButton("downloadData", "Download Full Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 br(),
                 fluidRow(plotOutput("summary", width = "80%")),
                 br(),
                 br(),
                 fluidRow(DT::dataTableOutput("stats", width = "80%"))
        ),
        tabPanel("Demographics Plot",
                 br(),
                 plotlyOutput("plot", width = "80%")
        ),
        tabPanel("Trip Rates Map",
                 br(),
                 tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                 leafletOutput("map")
        ),
        tabPanel("Full Data",
                 br(),
                 DT::dataTableOutput("table")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$summary <- renderPlot({
    if (input$choice == "Proportion of Households") {
      Household[[input$dataset]][[input$ptype]][[3]]
    } else if (input$choice == "Proportion of Persons") {
      Person[[input$dataset]][[input$ptype]][[3]]
    } else if (input$choice == "Proportion of Trips") {
      Trip[[input$dataset]][[input$ptype]][[3]]
    } else if (input$choice == "Household Trip Rates") {
      HTR[[input$dataset]][[input$ptype]][[3]]
    } else if (input$choice == "Person Trip Rates") {
      PTR[[input$dataset]][[input$ptype]][[3]]
    }
  })
  output$stats <- DT::renderDataTable({
    if (input$choice == "Proportion of Households") {
      DT::datatable(Household[[input$dataset]][[input$ptype]][[4]])
    } else if (input$choice == "Proportion of Persons") {
      DT::datatable(Person[[input$dataset]][[input$ptype]][[4]])
    } else if (input$choice == "Proportion of Trips") {
      DT::datatable(Trip[[input$dataset]][[input$ptype]][[4]])
    } else if (input$choice == "Household Trip Rates") {
      DT::datatable(HTR[[input$dataset]][[input$ptype]][[4]])
    } else if (input$choice == "Person Trip Rates") {
      DT::datatable(PTR[[input$dataset]][[input$ptype]][[4]])
    }
  })
  output$plot <- renderPlotly({
    if (input$choice == "Proportion of Households") {
      Household[[input$dataset]][[input$ptype]][[2]]
    } else if (input$choice == "Proportion of Persons") {
      Person[[input$dataset]][[input$ptype]][[2]]
    } else if (input$choice == "Proportion of Trips") {
      Trip[[input$dataset]][[input$ptype]][[2]]
    }
  })
  output$map <- renderLeaflet({
    if (input$choice == "Household Trip Rates") {
      HTR[[input$dataset]][[input$ptype]][[2]]
    } else if (input$choice == "Person Trip Rates") {
      PTR[[input$dataset]][[input$ptype]][[2]]
    }
  })
  output$table <- DT::renderDataTable({
    dataset <- as.data.frame(Household[[input$dataset]][[input$ptype]][[1]])
    DT::datatable(dataset)
  })
  
  datasetInputUn <- reactive({
    switch(input$dataset,
           "Household Size" = all.dataUn_hhsize,
           "Income Level" = all.dataUn_income,
           "Number of Vehicles" = all.dataUn_hhveh)
  })
  
  datasetInputWT <- reactive({
    switch(input$dataset,
           "Household Size" = all.dataWT_hhsize,
           "Income Level" = all.dataWT_income,
           "Number of Vehicles" = all.dataWT_hhveh)
  })
  
  output$downloadData <- downloadHandler(    
      filename = function(){
        if(input$ptype == 'Unweighted') {
          paste(input$dataset, " Unweighted Data", '.csv', sep = "")}
        else if (input$ptype == "Weighted") {
          paste(input$dataset, " Weighted Data", '.csv', sep = "")}
      },
      
      content = function(file) {
        if (input$ptype == "Unweighted") {
          write.csv(datasetInputUn(), file, row.names = FALSE)}
        else if (input$ptype == "Weighted") {
          write.csv(datasetInputWT(), file, row.names = FALSE)}
      }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

