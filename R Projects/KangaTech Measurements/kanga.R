# Load packages
library(dplyr)
library(readxl)
library(openxlsx)
library(DBI)
library(zoo)
library(tidyr)

# Set working directory 
setwd("C:/Users/NPendyal.ANGELS/Desktop/data/mlb_kangatech")

# Save raw data in that directory as an excel file
# Format should be "export_#-#to#-#" with numbers to replace hash symbols
# Ex. "export_3-15to5-2.xlsx"

# Save dates spanning range of data as variables 
# **ENTER DATES HERE**
before_date <- "3-1"
after_date <- "8-19"

# Club ids
ids <- c("1269", "1390", "1297", "1245", "1076", "1334", "1033")

# Read in the raw data
filenm <- paste0("export_", before_date, "to", after_date, ".xlsx")
raw_data <- read_excel(filenm)

# Read in the body weight data
db <- dbConnect(odbc::odbc(),
                Driver   = "SQL Server",
                Server   = "abb-sql2",
                Database = "bbops")

# club_id has to be changed
BW <- dbGetQuery(db, paste0("select W1.full_name AS Name, throws, club_id, W1.TestDate, W2.Value AS Weight
                            FROM
                            
                            (select full_name, throws, club_id 
                            from angels.player
                            where club_id = ", ids[7]," AND org_id = 1269 AND active = 1 )W
                            LEFT OUTER JOIN
                            (SELECT first_name + ' ' + last_name AS full_name, exercise_name, MAX(completed_date) AS TestDate
                            from team_buildr.raw_data
                            where exercise_name = 'Body Weight'
                            group by first_name + ' ' + last_name, exercise_name) W1
                            
                            ON W1.full_name = W.full_name
                            
                            LEFT OUTER JOIN
                            (SELECT first_name + ' ' + last_name AS full_name, exercise_name, completed_date AS TestDate, TRY_CONVERT(float, [highest_max]) AS Value
                            from team_buildr.raw_data
                            where exercise_name = 'Body Weight') W2
                            
                            ON W2.full_name = W1.full_name
                            AND W2.exercise_name = W1.exercise_name
                            AND W2.TestDate = W1.TestDate
                            AND W2.full_name = W.full_name
                            
                            WHERE W1.full_name IS NOT NULL
                            
                            ")
)

dbDisconnect(db)

if(unique(BW$club_id) == 1269) {
  club <- "MLB"
} else if (unique(BW$club_id) == 1390) {
  club <- "SLC"
} else if (unique(BW$club_id) == 1297) {
  club <- "MOB"
} else if (unique(BW$club_id) == 1245) {
  club <- "IE"
} else if (unique(BW$club_id) == 1076) {
  club <- "BURL"
} else if (unique(BW$club_id) == 1334) {
  club <- "ORE"
} else if (unique(BW$club_id) == 1033) {
  club <- "AZL"
}

# Only keep MLB players
raw_data <- raw_data[ raw_data$Name %in% BW$Name , ]

if (club == "MLB") {
  raw_data <- raw_data[raw_data$Name != "Tyler Skaggs" , ]
}

# Change variable types
raw_data$Date <- as.character(raw_data$Date)
raw_data$Left <- as.numeric(raw_data$Left)
raw_data$Right <- as.numeric(raw_data$Right)

# Drop Pos variable
raw_data <- raw_data[ , !(names(raw_data) %in% "Pos")]

# Replace "None" entries with NAs
raw_data[raw_data == "None"] <- NA

# Identify athletes with multiple same-day tests
tests.more.than.1 <- raw_data %>% filter(Type == "Shoulder IR 90 Supine" | Type == "Shoulder ER 90 Supine" | Type == "Hip Adduction (Neutral)" | Type == "Hip Abduction" | Type == "Knee Flexion") %>%
  group_by(Date, Name, Type) %>% filter(n()>1) 

# Take athletes' best values from those days
best.values <- tests.more.than.1 %>% 
  filter(Purpose == "monitoring") %>%
  group_by(Date, No, Name, Type, Status) %>% 
  summarise_at(vars(`Left 1`:`Right Torque per kg`), max, na.rm = TRUE)

# Create cleaned dataset with one test for each day & type
kanga.new <- anti_join(raw_data %>% filter(Purpose == "monitoring") %>% filter(Type == "Shoulder IR 90 Supine" | Type == "Shoulder ER 90 Supine" | Type == "Hip Adduction (Neutral)" | Type == "Hip Abduction" | Type == "Knee Flexion"), tests.more.than.1, by = names(raw_data))
kanga.new <- kanga.new[,-c(22:23)]
kanga.new <- rbind(kanga.new, as.data.frame(best.values))
kanga.new <- do.call(data.frame,lapply(kanga.new, function(x) replace(x, is.infinite(x),NA)))
kanga.new <- kanga.new[order(kanga.new$Date),]
row.names(kanga.new) <- 1:nrow(kanga.new)

# More cleaning to account for athletes with measurements taken over multiple days
Diffdays_3orless <- kanga.new %>% group_by(Name, Type) %>%
  mutate(diff_days = difftime(as.Date(Date), lag(as.Date(Date)), units = 'days')) %>% filter(diff_days <= 3)

closeDays <- kanga.new[ as.Date(kanga.new$Date) %in% 
                          c(unique(as.Date(Diffdays_3orless$Date)), unique(as.Date(Diffdays_3orless$Date) - Diffdays_3orless$diff_days)) &
                          kanga.new$Name %in% unique(Diffdays_3orless$Name) & kanga.new$Type %in% unique(Diffdays_3orless$Type) , ]

kanga.new <- anti_join(kanga.new, closeDays, by = names(closeDays))

closeDays <- closeDays %>%
  group_by(Name, Type) %>%
  mutate_at(vars(Left.1:Right.Torque.per.kg), funs(na.locf(., na.rm = FALSE)))

closeDays <- na.omit(closeDays)

kanga.new <- rbind(kanga.new, as.data.frame(closeDays))
kanga.new <- kanga.new[order(kanga.new$Date),]
row.names(kanga.new) <- 1:nrow(kanga.new)

# Pounds Conversion
kanga.new$`Left (lbs)` <- kanga.new$Left * 2.20462
kanga.new$`Right (lbs)` <- kanga.new$Right * 2.20462

# Raw and % Differences
kanga.new$`Raw Diff (lbs)` = kanga.new$`Left (lbs)` - kanga.new$`Right (lbs)`
kanga.new$`Percent Diff` = 100 * kanga.new$`Raw Diff (lbs)` / kanga.new$`Right (lbs)`

# Current Data
most.recent <- kanga.new %>%
  group_by(Name, Type) %>% 
  filter(as.Date(Date) == max(as.Date(Date)))

# Asymmetry Counts
asymmetry.counts <- kanga.new %>%
  filter(`Percent Diff` < 200) %>%
  group_by(Name, Type) %>%
  summarise(`Total Measurements` = n(),
            `Red Flags` = sum(`Percent Diff` >= 10 | `Percent Diff` <= -10, na.rm = TRUE))

current.redFlag <- most.recent %>%
  filter(`Percent Diff` < 200) %>%
  group_by(Name, Type) %>%
  summarise(`Current Red Flag` = ifelse(`Percent Diff` >= 10 | `Percent Diff` <= -10, "Yes", "No"))

current.asymmetry <- merge(current.redFlag, most.recent[c("Date", "Name", "Type")], all = TRUE, by = c("Name", "Type"))
names(current.asymmetry)[4] <- "Current Date"

asymmetry.counts <- merge(asymmetry.counts, current.asymmetry, by = names(asymmetry.counts)[1:2], all = TRUE)

asymmetry.counts$`Weak Side (Current)` <- NA
for (i in 1:nrow(asymmetry.counts)) {
  temp <- most.recent %>% filter(Name == asymmetry.counts$Name[i] & Type == asymmetry.counts$Type[i] & `Percent Diff` < 200)
  
  if (nrow(temp) == 0) {
    asymmetry.counts$`Weak Side (Current)`[i] <- NA
  } else if (temp$`Percent Diff` <= -10 & temp$`Percent Diff` > -20) {
    asymmetry.counts$`Weak Side (Current)`[i] <- "left"
  } else if (temp$`Percent Diff` <= -20) {
    asymmetry.counts$`Weak Side (Current)`[i] <- "left (large)"
  } else if (temp$`Percent Diff` >= 10 & temp$`Percent Diff` < 20) {
    asymmetry.counts$`Weak Side (Current)`[i] <- "right"
  } else if (temp$`Percent Diff` >= 20) {
    asymmetry.counts$`Weak Side (Current)`[i] <- "right (large)"
  } else if (temp$`Percent Diff` > -10 & temp$`Percent Diff` < 10) {
    asymmetry.counts$`Weak Side (Current)`[i] <- "No weakness"
  }
}

# Calculate shoulder ratios
shoulder_LR <- kanga.new %>%
  filter(Type == "Shoulder IR 90 Supine" | Type == "Shoulder ER 90 Supine") %>%
  group_by(Date, Name, Type) %>%
  summarise(Left = max(`Left`, na.rm = TRUE), Right = max(`Right`, na.rm = TRUE)) 

for (i in 2:(nrow(shoulder_LR)-1)) {
  if (shoulder_LR$Name[i+1] == shoulder_LR$Name[i] & 
      shoulder_LR$Name[i] != shoulder_LR$Name[i-1] &
      shoulder_LR$Date[i] != shoulder_LR$Date[i-1] &
      shoulder_LR$Type[i+1] != shoulder_LR$Type[i] & 
      (as.Date(shoulder_LR$Date[i+1]) - as.Date(shoulder_LR$Date[i])) <= 3) {
    
    shoulder_LR$Date[i] <- shoulder_LR$Date[i+1]
    
  }
}

shoulder_LR <- shoulder_LR %>%
  group_by(Date, Name) %>%
  filter(any(Type == "Shoulder ER 90 Supine") & any(Type == "Shoulder IR 90 Supine"))
shoulder_LR <- do.call(data.frame,lapply(shoulder_LR, function(x) replace(x, is.infinite(x),NA)))

shoulder_ratio <- shoulder_LR %>%
  group_by(Date, Name) %>%
  summarise(`Left Shoulder ER/IR` = Left[Type=="Shoulder ER 90 Supine"] / Left[Type=="Shoulder IR 90 Supine"], 
            `Right Shoulder ER/IR` = Right[Type=="Shoulder ER 90 Supine"] / Right[Type=="Shoulder IR 90 Supine"])

# Calculate hip ratios
hip_LR <- kanga.new %>%
  filter(Type == "Hip Adduction (Neutral)" | Type == "Hip Abduction") %>%
  group_by(Date, Name, Type) %>%
  summarise(Left = max(`Left`, na.rm = TRUE), Right = max(`Right`, na.rm = TRUE)) 

for (i in 2:(nrow(hip_LR)-1)) {
  if (hip_LR$Name[i+1] == hip_LR$Name[i] & 
      hip_LR$Name[i] != hip_LR$Name[i-1] &
      hip_LR$Date[i] != hip_LR$Date[i-1] &
      hip_LR$Type[i+1] != hip_LR$Type[i] & 
      (as.Date(hip_LR$Date[i+1]) - as.Date(hip_LR$Date[i])) <= 3) {
    
    hip_LR$Date[i] <- hip_LR$Date[i+1]
    
  }
}

hip_LR <- hip_LR %>% 
  group_by(Date, Name) %>%
  filter(any(Type == "Hip Adduction (Neutral)") & any(Type == "Hip Abduction"))
hip_LR <- do.call(data.frame,lapply(hip_LR, function(x) replace(x, is.infinite(x),NA)))

hip_ratio <- hip_LR %>%
  group_by(Date, Name) %>%
  summarise(`Left Hip Adduction/Abduction` = Left[Type=="Hip Adduction (Neutral)"] / Left[Type=="Hip Abduction"], 
            `Right Hip Adduction/Abduction` = Right[Type=="Hip Adduction (Neutral)"] / Right[Type=="Hip Abduction"])

# Merge shoulder and hip ratio data
ratios <- merge(shoulder_ratio, hip_ratio, by = c("Date", "Name"), all = TRUE)
ratios2write <- rapply(object = ratios, f = round, classes = "numeric", how = "replace", digits = 2)

# Duplicate data & remove illogical values
ratios2 <- ratios
ratios2[ratios2 > 2] <- NA

# Ratio Counts
shoulder_left <- ratios2 %>%
  group_by(Name) %>%
  summarise(`Red Flags` = sum(`Left Shoulder ER/IR` > 1 | `Left Shoulder ER/IR` < 0.85, na.rm = TRUE),
            `Total Measurements` = sum(!is.na(`Left Shoulder ER/IR`)))

shoulder_right <- ratios2 %>%
  group_by(Name) %>%
  summarise(`Red Flags` = sum(`Right Shoulder ER/IR` > 1 | `Right Shoulder ER/IR` < 0.85, na.rm = TRUE),
            `Total Measurements` = sum(!is.na(`Right Shoulder ER/IR`)))

shoulder.counts <- merge(shoulder_left, shoulder_right, by = "Name")
names(shoulder.counts)[2:5] <- c("Left Red Flags", "Left Total", "Right Red Flags", "Right Total")

# Current ratios
# Calculate shoulder ratios
shoulder_LR <- most.recent %>%
  filter(Type == "Shoulder IR 90 Supine" | Type == "Shoulder ER 90 Supine") %>%
  group_by(Date, Name, Type) %>%
  summarise(Left = max(`Left`, na.rm = TRUE), Right = max(`Right`, na.rm = TRUE)) 

for (i in 2:(nrow(shoulder_LR)-1)) {
  if (shoulder_LR$Name[i+1] == shoulder_LR$Name[i] & 
      shoulder_LR$Name[i] != shoulder_LR$Name[i-1] &
      shoulder_LR$Date[i] != shoulder_LR$Date[i-1] &
      shoulder_LR$Type[i+1] != shoulder_LR$Type[i] & 
      (as.Date(shoulder_LR$Date[i+1]) - as.Date(shoulder_LR$Date[i])) <= 3) {
    
    shoulder_LR$Date[i] <- shoulder_LR$Date[i+1]
    
  }
}

shoulder_LR <- shoulder_LR %>%
  group_by(Date, Name) %>%
  filter(any(Type == "Shoulder ER 90 Supine") & any(Type == "Shoulder IR 90 Supine"))
shoulder_LR <- do.call(data.frame,lapply(shoulder_LR, function(x) replace(x, is.infinite(x),NA)))

shoulder_ratio <- shoulder_LR %>%
  group_by(Date, Name) %>%
  summarise(`Left Shoulder ER/IR` = Left[Type=="Shoulder ER 90 Supine"] / Left[Type=="Shoulder IR 90 Supine"], 
            `Right Shoulder ER/IR` = Right[Type=="Shoulder ER 90 Supine"] / Right[Type=="Shoulder IR 90 Supine"])

# Calculate hip ratios
hip_LR <- most.recent %>%
  filter(Type == "Hip Adduction (Neutral)" | Type == "Hip Abduction") %>%
  group_by(Date, Name, Type) %>%
  summarise(Left = max(`Left`, na.rm = TRUE), Right = max(`Right`, na.rm = TRUE)) 

for (i in 2:(nrow(hip_LR)-1)) {
  if (hip_LR$Name[i+1] == hip_LR$Name[i] & 
      hip_LR$Name[i] != hip_LR$Name[i-1] &
      hip_LR$Date[i] != hip_LR$Date[i-1] &
      hip_LR$Type[i+1] != hip_LR$Type[i] & 
      (as.Date(hip_LR$Date[i+1]) - as.Date(hip_LR$Date[i])) <= 3) {
    
    hip_LR$Date[i] <- hip_LR$Date[i+1]
    
  }
}

hip_LR <- hip_LR %>% 
  group_by(Date, Name) %>%
  filter(any(Type == "Hip Adduction (Neutral)") & any(Type == "Hip Abduction"))
hip_LR <- do.call(data.frame,lapply(hip_LR, function(x) replace(x, is.infinite(x),NA)))

hip_ratio <- hip_LR %>%
  group_by(Date, Name) %>%
  summarise(`Left Hip Adduction/Abduction` = Left[Type=="Hip Adduction (Neutral)"] / Left[Type=="Hip Abduction"], 
            `Right Hip Adduction/Abduction` = Right[Type=="Hip Adduction (Neutral)"] / Right[Type=="Hip Abduction"])

current.ratios <- merge(shoulder_ratio, hip_ratio, by = c("Date", "Name"), all = TRUE)
names(current.ratios)[3:6] <- c("Current Left Shoulder Ratio", "Current Right Shoulder Ratio", "Current Left Hip Ratio", "Current Right Hip Ratio")

# Merge ratio counts & current measures
shoulder.counts <- merge(shoulder.counts, 
                         rapply(object = current.ratios, f = round, classes = "numeric", how = "replace", digits = 2),
                         by = "Name", all = TRUE)
names(shoulder.counts)[6] <- "Current Date"

# Create the excel workbook
wb <- createWorkbook()

# Define styles
headerStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                           fgFill = "#4F81BD", border="TopBottom")
labelStyle <- createStyle(fontSize = 12, fontColour = "#FFFFFF", halign = "center",
                          fgFill = "black", border="TopBottom", textDecoration = "bold")
redStyle <- createStyle(fgFill = "red")
yellowStyle <- createStyle(fgFill = "yellow")
orangeStyle <- createStyle(fgFill = "orange")
thickBorder <- createStyle(border = "Left", borderColour = "black", borderStyle = "thick")
headerStyle2 <- createStyle(fontSize = 12, fontColour = "white", halign = "center", 
                            fgFill = "#3CB371", border = "Top Bottom", textDecoration = "bold")
greenStyle <- createStyle(fgFill = "#92D050")
fontSize14 <- createStyle(fontSize = 14)
fontSize16 <- createStyle(fontSize = 16)

# Calculate averages, stdevs, and coef of variation for each athlete 
kanga_percDiff <- kanga.new %>%
  filter(`Percent Diff` < 200) %>%
  group_by(Name, Type) %>%
  filter(n()>1) %>%
  summarise(`Avg % Diff` = mean(abs(`Percent Diff`), na.rm = TRUE), `SD % Diff` = sd(abs(`Percent Diff`), na.rm = TRUE))
kanga_percDiff <- na.omit(kanga_percDiff)
kanga_percDiff$`CV % Diff` <- abs(kanga_percDiff$`SD % Diff` / kanga_percDiff$`Avg % Diff`)

# Calculate MBIs for each test type
test.types <- as.character(unique(kanga_percDiff$Type))
test.set <- c("Hip Abduction", "Hip Adduction (Neutral)", "Knee Flexion", "Shoulder ER 90 Supine", "Shoulder IR 90 Supine")
for (i in test.set) {
  if (i %in% test.types) {
    filter.kanga.by.test <- kanga_percDiff %>% filter(Type == i)
    assign(paste0(gsub("[[:blank:]]", "", i), "_SWC"), 0.2*sd(filter.kanga.by.test$`Avg % Diff`))
    assign(paste0(gsub("[[:blank:]]", "", i), "_CV"), mean(filter.kanga.by.test$`Avg % Diff`) * mean(filter.kanga.by.test$`CV % Diff`))
    assign(paste0(gsub("[[:blank:]]", "", i), "_2CV"), 2 * mean(filter.kanga.by.test$`Avg % Diff`) * mean(filter.kanga.by.test$`CV % Diff`))
  } else if (! i %in% test.types) {
    assign(paste0(gsub("[[:blank:]]", "", i), "_SWC"), NA)
    assign(paste0(gsub("[[:blank:]]", "", i), "_CV"), NA)
    assign(paste0(gsub("[[:blank:]]", "", i), "_2CV"), NA)
  }
}

# Group MBIs by test type
MBI <- as.data.frame(matrix(NA, ncol = 3, nrow = length(test.types)))
names(MBI) <- c("SWC", "CV", "2CV")
rownames(MBI) <- test.types
for (i in 1:length(test.types)) {
  if (rownames(MBI)[i] == "Eccentric Hamstring") {
    MBI[i,] <- c(EccentricHamstring_SWC, EccentricHamstring_CV, EccentricHamstring_2CV)
  } else if (rownames(MBI)[i] == "Hip Abduction") {
    MBI[i,] <- c(HipAbduction_SWC, HipAbduction_CV, HipAbduction_2CV)
  } else if (rownames(MBI)[i] == "Hip Adduction (Neutral)") {
    MBI[i,] <- c(`HipAdduction(Neutral)_SWC`, `HipAdduction(Neutral)_CV`, `HipAdduction(Neutral)_2CV`)
  } else if (rownames(MBI)[i] == "Shoulder ER 90 Supine") {
    MBI[i,] <- c(ShoulderER90Supine_SWC, ShoulderER90Supine_CV, ShoulderER90Supine_2CV)
  } else if (rownames(MBI)[i] == "Shoulder IR 90 Supine") {
    MBI[i,] <- c(ShoulderIR90Supine_SWC, ShoulderIR90Supine_CV, ShoulderIR90Supine_2CV)
  } else if (rownames(MBI)[i] == "Knee Flexion") {
    MBI[i,] <- c(KneeFlexion_SWC, KneeFlexion_CV, KneeFlexion_2CV)
  }
}

# Merge weight data
BW <- BW %>% arrange(desc(TestDate)) %>% group_by(Name) %>% slice(1)
kanga.new_plusWT <- merge(kanga.new, BW[,c(1,5)], by = "Name")
kanga.new_plusWT$`Normalized Left` <- kanga.new_plusWT$`Left (lbs)` / kanga.new_plusWT$Weight
kanga.new_plusWT$`Normalized Right` <- kanga.new_plusWT$`Right (lbs)` / kanga.new_plusWT$Weight

# Keep only columns of interest
kanga2write <- kanga.new_plusWT[,c(2,1,4,14,18,22:23,27:28,24:25)]
names(kanga2write)[4:5] <- c("Left (kg)", "Right (kg)")
kanga2write[,c(4:7,10:11)] <- round(kanga2write[,c(4:7,10:11)], 1)
kanga2write[,8:9] <- round(kanga2write[,8:9], 2)
kanga2write <- kanga2write[order(kanga2write$Date, kanga2write$Name) , ]
rownames(kanga2write) <- 1:nrow(kanga2write)

# Change in strength
recent_asymmetry <- kanga2write %>%
  group_by(Name, Type) %>% filter(n()>1) %>%
  slice(c(n() - 1, n()))

recent_asymmetry <- recent_asymmetry[,1:7]
ind <- seq(1, nrow(recent_asymmetry), 2)
first_measure <- recent_asymmetry[ind, ]
second_measure <- recent_asymmetry[-ind, ]
recent_asymmetry <- merge(first_measure, second_measure, by = names(first_measure)[2:3])

recent_asymmetry$`Left Diff (kg)` <- recent_asymmetry$`Left (kg).y` - recent_asymmetry$`Left (kg).x`
recent_asymmetry$`Right Diff (kg)` <- recent_asymmetry$`Right (kg).y` - recent_asymmetry$`Right (kg).x`
recent_asymmetry$`Left Diff (lbs)` <- recent_asymmetry$`Left (lbs).y` - recent_asymmetry$`Left (lbs).x`
recent_asymmetry$`Right Diff (lbs)` <- recent_asymmetry$`Right (lbs).y` - recent_asymmetry$`Right (lbs).x`

names(recent_asymmetry)[3:7] <- sub(".x", "", names(recent_asymmetry)[3:7])
names(recent_asymmetry)[8:12] <- sub(".y", "", names(recent_asymmetry)[8:12])


## RED FLAG SUMMARIES

# ---
section1 <- subset(asymmetry.counts, asymmetry.counts$`Weak Side (Current)` != "No weakness")[,c(1:2,7)]
section1_wide <- spread(section1, Type, `Weak Side (Current)`)

# ---
section2 <- subset(shoulder.counts, shoulder.counts$`Current Left Shoulder Ratio` < 0.85 | shoulder.counts$`Current Left Shoulder Ratio` > 1 |
                     shoulder.counts$`Current Right Shoulder Ratio` < 0.85 | shoulder.counts$`Current Right Shoulder Ratio` > 1)[,c(1,7:8)]
names(section2)[2:3] <- c("Left Ratio", "Right Ratio")

# ---
section3 <- subset(recent_asymmetry, recent_asymmetry$`Left Diff (kg)` <= -4 | recent_asymmetry$`Right Diff (kg)` <= -4)[,c(1:2,13:14)]
section3[is.na(section3)] <- 0
section3$`Reduced Strength Side` <- ifelse(section3$`Left Diff (kg)` <= -4 & section3$`Right Diff (kg)` <= -4, "both",
                                           ifelse(section3$`Right Diff (kg)` <= -4, "right", 
                                                  ifelse(section3$`Left Diff (kg)` <= -4, "left", NA)))
section3 <- section3[,-c(3:4)]
section3_wide <- spread(section3, Type, `Reduced Strength Side`)

# ---
redFlag.Summary <- merge(section1_wide, section2, all = TRUE, sort = FALSE, by = "Name")
redFlag.Summary <- merge(redFlag.Summary, section3_wide, all = TRUE, sort = FALSE, by = "Name")
redFlag.Summary <- merge(BW[,1:2], redFlag.Summary, all.y = TRUE, by = "Name")

# ---
prefix <- c("Abd", "Add", "Flex", "ER", "IR")
which(grepl(prefix[1], names(redFlag.Summary)))

# ---
for (i in 1:length(prefix)) {
  
  index <- which(grepl(prefix[i], names(redFlag.Summary)))
  
  if (length(index) == 0) {
    next
    
  } else if (length(index) == 1) {
    j <- index
    assign(paste0(prefix[i], ".asymmetryFlag"), which(redFlag.Summary[,j] == "left (large)" | redFlag.Summary[,j] == "right (large)"))
    
  } else if (length(index) == 2) {
    j <- index[1]
    k <- index[2]
    assign(paste0(prefix[i], ".asymmetryFlag"), which(redFlag.Summary[,j] == "left (large)" | redFlag.Summary[,j] == "right (large)"))
    assign(paste0(prefix[i], ".strengthFlag"), which(
      
      ((redFlag.Summary[,k] == "left" | redFlag.Summary[,k] == "both") & redFlag.Summary["throws"] == "L") |
        ((redFlag.Summary[,k] == "right" | redFlag.Summary[,k] == "both") & redFlag.Summary["throws"] == "R")  
      
    ))
  }
}

# ---
if ('Left Ratio' %in% names(redFlag.Summary)) {
  leftRatio.Flag <- which((redFlag.Summary$`Left Ratio` < 0.85 | redFlag.Summary$`Left Ratio` > 1.0) & redFlag.Summary$throws == "L")
}
if ('Right Ratio' %in% names(redFlag.Summary)) {
  rightRatio.Flag <- which((redFlag.Summary$`Right Ratio` < 0.85 | redFlag.Summary$`Right Ratio` > 1.0) & redFlag.Summary$throws == "R")
}

# ---
section1_ind <- c()
section2_ind <- c()
section3_ind <- c()
col <- names(redFlag.Summary)

i <- 3
while (i <= ncol(redFlag.Summary)) {
  if (grepl(paste(prefix, collapse = "|"), col[i]) & i < which(col == "Left Ratio")) {
    section1_ind <- c(section1_ind, i)
  } else if (col[i] == 'Left Ratio' | col[i] == 'Right Ratio') {
    section2_ind <- c(section2_ind, i)
  } else if (grepl(paste(prefix, collapse = "|"), col[i]) & i > which(col == "Right Ratio")) {
    section3_ind <- c(section3_ind, i)
  }
  
  i <- i + 1
}

# ---
yellow.counts <- table(unname(do.call("c",mget(grep(paste(c("asymmetryFlag","Ratio.Flag","strengthFlag"), collapse = "|"),
                                     names(.GlobalEnv),value=TRUE)))))
yellow.counts <- cbind(Name = redFlag.Summary$Name[as.numeric(names(yellow.counts))], unname(yellow.counts))
redFlag.Summary <- merge(redFlag.Summary, yellow.counts, by = "Name", all = TRUE)
names(redFlag.Summary)[ncol(redFlag.Summary)] <- "YELLOW COUNT"
redFlag.Summary$`YELLOW COUNT` <- as.numeric(as.character(redFlag.Summary$`YELLOW COUNT`))

# ---
names(redFlag.Summary)[section1_ind] <- sub(".x", "", names(redFlag.Summary)[section1_ind], fixed = TRUE)
names(redFlag.Summary)[section3_ind] <- sub(".y", "", names(redFlag.Summary)[section3_ind], fixed = TRUE)

# ---
addWorksheet(wb, "Red Flag Summary", gridLines = TRUE, zoom = 60)
writeData(wb, "Red Flag Summary", redFlag.Summary, startRow = 7, startCol = 1, rowNames = FALSE)
setColWidths(wb, "Red Flag Summary", cols = 1:2, widths = "auto")
setColWidths(wb, "Red Flag Summary", cols = 3:20, widths = 30)
addStyle(wb, "Red Flag Summary", fontSize14, rows = 1:5, cols = min(section2_ind), stack = TRUE)
if (length(section1_ind) > 0) {
  mergeCells(wb, "Red Flag Summary", cols = section1_ind, rows = 6)}
if (length(section2_ind) > 0) {
  mergeCells(wb, "Red Flag Summary", cols = section2_ind, rows = 6)}
if (length(section3_ind) > 0) {
  mergeCells(wb, "Red Flag Summary", cols = section3_ind, rows = 6)}
writeData(wb, "Red Flag Summary", "Performance Therapist", startRow = 7, startCol = ncol(redFlag.Summary)+1)
if (length(section1_ind) > 0) {
  writeData(wb, "Red Flag Summary", "SECTION 1: Asymmetries showing weaker side (large asymmetry greater 20%)", startRow = 6, startCol = min(section1_ind))}
if (length(section2_ind) > 0) {
  writeData(wb, "Red Flag Summary", "SECTION 2: Shoulder Ratio outside optimal range (0.85-1.0)", startRow = 6, startCol = min(section2_ind))}
if (length(section3_ind) > 0) {
  writeData(wb, "Red Flag Summary", "SECTION 3: Strength Loss greater than 4kg from last test", startRow = 6, startCol = min(section3_ind))}
addStyle(wb, "Red Flag Summary", headerStyle, rows = 7, cols = 1:(ncol(redFlag.Summary)-1), stack = TRUE)
addStyle(wb, "Red Flag Summary", headerStyle2, rows = 7, cols = c(ncol(redFlag.Summary), ncol(redFlag.Summary)+1), stack = TRUE)
if (length(section1_ind) > 0) {
  addStyle(wb, "Red Flag Summary", labelStyle, rows = 6, cols = min(section1_ind), stack = TRUE)}
if (length(section2_ind) > 0) {
  addStyle(wb, "Red Flag Summary", labelStyle, rows = 6, cols = min(section2_ind), stack = TRUE)}
if (length(section3_ind) > 0) {
  addStyle(wb, "Red Flag Summary", labelStyle, rows = 6, cols = min(section3_ind), stack = TRUE)}
if (length(section1_ind) > 0) {
  addStyle(wb, "Red Flag Summary", thickBorder,rows = 8:1000, cols = min(section1_ind), gridExpand = TRUE, stack = TRUE)}
if (length(section2_ind) > 0) {
  addStyle(wb, "Red Flag Summary", thickBorder,rows = 8:1000, cols = min(section2_ind), gridExpand = TRUE, stack = TRUE)}
if (length(section3_ind) > 0) {
  addStyle(wb, "Red Flag Summary", thickBorder,rows = 8:1000, cols = min(section3_ind), gridExpand = TRUE, stack = TRUE)}
addStyle(wb, "Red Flag Summary", thickBorder,rows = 8:1000, cols = c(ncol(redFlag.Summary), ncol(redFlag.Summary)+2), gridExpand = TRUE, stack = TRUE)

for (j in section1_ind) {
  for (i in 1:length(prefix)) {
    if (grepl(prefix[i], col[j])) {
      addStyle(wb, "Red Flag Summary", yellowStyle, rows = get(paste0(prefix[i],".asymmetryFlag")) + 7, cols = j, stack = TRUE)
    }
  }
}

for (j in section2_ind) {
  if (col[j] == 'Left Ratio') {
    addStyle(wb, "Red Flag Summary", yellowStyle, rows = leftRatio.Flag + 7, cols = j, stack = TRUE)
  } else if (col[j] == 'Right Ratio') {
    addStyle(wb, "Red Flag Summary", yellowStyle, rows = rightRatio.Flag + 7, cols = j, stack = TRUE)
  }
}

for (j in section3_ind) {
  for (i in 1:length(prefix)) {
    if (grepl(prefix[i], col[j])) {
      addStyle(wb, "Red Flag Summary", yellowStyle, rows = get(paste0(prefix[i],".strengthFlag")) + 7, cols = j, stack = TRUE)
    }
  }
}

colNum <- ncol(redFlag.Summary)
rowNum <- nrow(redFlag.Summary)
addStyle(wb, "Red Flag Summary", fontSize16, rows = 7:(rowNum+7), cols = 1:(colNum+2), gridExpand = TRUE, stack = TRUE)

writeData(wb, "Red Flag Summary", "SECTION 1 - asymmetries in strength between left and right sides; weaker side shown", startCol = min(section2_ind), startRow = 1)
writeData(wb, "Red Flag Summary", "SECTION 2 - shoulder ER/IR ratios falling outside range of 0.85 to 1.0", startCol = min(section2_ind), startRow = 2)
writeData(wb, "Red Flag Summary", "SECTION 3 - reductions in strength from last test greater than or equal to 4 kg", startCol = min(section2_ind), startRow = 3)
writeData(wb, "Red Flag Summary", "**Highlighted cells in SECTION 2 & SECTION 3 represent deficiencies corresponding to side of throwing arm", startCol = min(section2_ind), startRow = 4)
writeData(wb, "Red Flag Summary", "*Optimal Shoulder Function (20/20/20, 0.85-1.0)  Aim is to have at least 20 Kg of IR and ER strength, greater than 20% body weight, less than 20% asymmetry, rotator cuff ratio between 0.85-1.0", 
          startCol = min(section2_ind), startRow = 5)
addStyle(wb, "Red Flag Summary", yellowStyle, rows = 4, cols = min(section2_ind):(min(section2_ind)+3), stack = TRUE)
addStyle(wb, "Red Flag Summary", greenStyle, rows = 5, cols = min(section2_ind):(min(section2_ind)+6), stack = TRUE)

# Write asymmetry info to workbook
addWorksheet(wb, "Asymmetry Counts", gridLines = TRUE, zoom = 75)
writeData(wb, "Asymmetry Counts", asymmetry.counts, rowNames = FALSE)
addStyle(wb, "Asymmetry Counts", headerStyle, rows = 1, cols = 1:ncol(asymmetry.counts))
setColWidths(wb, "Asymmetry Counts", cols = 1:ncol(asymmetry.counts), widths = "auto")

# Color code current ratios
red.currentLeft <- which(shoulder.counts$`Current Left Shoulder Ratio` < 0.85 | shoulder.counts$`Current Left Shoulder Ratio` > 1)
red.currentRight <- which(shoulder.counts$`Current Right Shoulder Ratio` < 0.85 | shoulder.counts$`Current Right Shoulder Ratio` > 1)

red.currentLeft <- red.currentLeft + 1
red.currentRight <- red.currentRight + 1

# Write ratio info to workbook
addWorksheet(wb, "Ratio Counts", gridLines = TRUE, zoom = 75)
writeData(wb, "Ratio Counts", shoulder.counts, rowNames = FALSE)
addStyle(wb, "Ratio Counts", headerStyle, rows = 1, cols = 1:ncol(shoulder.counts))
addStyle(wb, "Ratio Counts", redStyle, rows = red.currentLeft, cols = 7)
addStyle(wb, "Ratio Counts", redStyle, rows = red.currentRight, cols = 8)
setColWidths(wb, "Ratio Counts", cols = 1:ncol(shoulder.counts), widths = "auto")

# Color code strength changes
red.strengthLeft <- which(recent_asymmetry$`Left Diff (kg)` <= -4)
red.strengthRight <- which(recent_asymmetry$`Right Diff (kg)` <= -4)

red.strengthLeft <- red.strengthLeft + 2
red.strengthRight <- red.strengthRight + 2

# Write strength data to sheet
addWorksheet(wb, "Strength Changes", gridLines = TRUE, zoom = 75)
writeData(wb, "Strength Changes", recent_asymmetry, startRow = 2, rowNames = FALSE)
mergeCells(wb, "Strength Changes", cols = 3:7, rows = 1)
mergeCells(wb, "Strength Changes", cols = 8:12, rows = 1)
mergeCells(wb, "Strength Changes", cols = 13:16, rows = 1)
setColWidths(wb, "Strength Changes", cols = 1:ncol(recent_asymmetry), widths = "auto")
addStyle(wb, "Strength Changes", thickBorder,rows = 1:1000, cols = c(3,8,13,17), gridExpand = TRUE)
addStyle(wb, "Strength Changes", headerStyle, rows = 2, cols = 1:ncol(recent_asymmetry))
addStyle(wb, "Strength Changes", redStyle, rows = red.strengthLeft, cols = 13)
addStyle(wb, "Strength Changes", redStyle, rows = red.strengthRight, cols = 14)
writeData(wb, "Strength Changes", "BEFORE", startRow = 1, startCol = 3)
writeData(wb, "Strength Changes", "AFTER", startRow = 1, startCol = 8)
writeData(wb, "Strength Changes", "CHANGE", startRow = 1, startCol = 13)
addStyle(wb, "Strength Changes", labelStyle, rows = 1, cols = 3)
addStyle(wb, "Strength Changes", labelStyle, rows = 1, cols = 8)
addStyle(wb, "Strength Changes", labelStyle, rows = 1, cols = 13)

# Write player's data to sheet
Names <- sort(as.character(unique(kanga2write$Name)))
for (i in 1:length(Names)) {
  kanga.temp <- kanga2write %>% filter(Name == Names[i]) %>% arrange(desc(Date), Type)
  ratios.temp <- ratios2write %>% filter(Name == Names[i]) %>% arrange(desc(Date))
  
  red1 <- which(kanga.temp$`Percent Diff` <= -10 | kanga.temp$`Percent Diff` >= 10)
  yellow1 <- which(kanga.temp$`Percent Diff` <= -200 | kanga.temp$`Percent Diff` >= 200)
  red2 <- which(ratios.temp$`Left Shoulder ER/IR` < 0.85 | ratios.temp$`Left Shoulder ER/IR` > 1)
  yellow2 <- which(ratios.temp$`Left Shoulder ER/IR` < 0.1 | ratios.temp$`Left Shoulder ER/IR` > 2)
  red3 <- which(ratios.temp$`Right Shoulder ER/IR` < 0.85 | ratios.temp$`Right Shoulder ER/IR` > 1)
  yellow3 <- which(ratios.temp$`Right Shoulder ER/IR` < 0.1 | ratios.temp$`Right Shoulder ER/IR` > 2)
  
  red1 <- red1 + 2
  yellow1 <- yellow1 + 2
  red2 <- red2 + 2
  yellow2 <- yellow2 + 2
  red3 <- red3 + 2
  yellow3 <- yellow3 + 2
  
  addWorksheet(wb, Names[i], gridLines = TRUE, zoom = 75)
  writeData(wb, Names[i], kanga.temp,  rowNames = FALSE, startRow = 2)
  writeData(wb, Names[i], ratios.temp, rowNames = FALSE, startCol = 13, startRow = 2)
  
  addStyle(wb, Names[i], headerStyle, rows = 2, cols = c(1:ncol(kanga2write), (ncol(kanga2write) +2):(ncol(kanga2write) + ncol(ratios2write) + 1)))
  
  addStyle(wb, Names[i], redStyle, rows = red1, cols = 11, gridExpand = T)
  addStyle(wb, Names[i], redStyle, rows = red2, cols = 15, gridExpand = T)
  addStyle(wb, Names[i], redStyle, rows = red3, cols = 16, gridExpand = T)
  addStyle(wb, Names[i], yellowStyle, rows = yellow1, cols = 1:11, gridExpand = T)
  addStyle(wb, Names[i], yellowStyle, rows = yellow2, cols = 13:18, gridExpand = T)
  addStyle(wb, Names[i], yellowStyle, rows = yellow3, cols = 13:18, gridExpand = T)
}

# Write MBI data to sheets
for (i in 1:length(Names)) {
  kanga.temp <- kanga2write %>% filter(Name == Names[i])
  kanga.temp_wide <- reshape(kanga.temp[,c(1:3,11)], idvar = c("Name","Type"), timevar = "Date", direction = "wide")
  
  writeData(wb, Names[i], kanga.temp_wide, startRow = 16, startCol = 13)
  addStyle(wb, Names[i], headerStyle, rows = 16, cols = 13:(ncol(kanga.temp_wide)-1+13))
  
  stop.point = ncol(as.data.frame(kanga.temp_wide[-c(1:3)]))
  
  KneeFl_indexSWC <- ShIR90_indexSWC <- ShER90_indexSWC <- EccHam_indexSWC <- HipAbd_indexSWC <- HipAdd_indexSWC <- rep(NA, stop.point)
  KneeFl_index2CV <- KneeFl_indexCV <- KneeFl_indexSWC
  ShIR90_index2CV <- ShIR90_indexCV <- ShIR90_indexSWC
  ShER90_index2CV <- ShER90_indexCV <- ShER90_indexSWC
  EccHam_index2CV <- EccHam_indexCV <- EccHam_indexSWC
  HipAbd_index2CV <- HipAbd_indexCV <- HipAbd_indexSWC
  HipAdd_index2CV <- HipAdd_indexCV <- HipAdd_indexSWC
  
  
  if (stop.point > 0) {
    for (n in 1:nrow(kanga.temp_wide)) {
      if (as.character(kanga.temp_wide$Type[n]) == "Hip Adduction (Neutral)") {
        for (j in 1:stop.point) {
          if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < `HipAdduction(Neutral)_CV` & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= `HipAdduction(Neutral)_SWC` ) {
            
            HipAdd_indexSWC[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < `HipAdduction(Neutral)_2CV` & 
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= `HipAdduction(Neutral)_CV` ) {
            
            HipAdd_indexCV[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= `HipAdduction(Neutral)_2CV` ) {
            
            HipAdd_index2CV[j] <- j+3+12
            
          }
        }  
      }
      
      if (as.character(kanga.temp_wide$Type[n]) == "Hip Abduction") {
        for (j in 1:stop.point) {
          if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < HipAbduction_CV & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= HipAbduction_SWC ) {
            
            HipAbd_indexSWC[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < HipAbduction_2CV & 
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= HipAbduction_CV ) {
            
            HipAbd_indexCV[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= HipAbduction_2CV ) {
            
            HipAbd_index2CV[j] <- j+3+12
            
          }
        }  
      }
      
      if (as.character(kanga.temp_wide$Type[n]) == "Eccentric Hamstring") {
        for (j in 1:stop.point) {
          if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < EccentricHamstring_CV & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= EccentricHamstring_SWC ) {
            
            EccHam_indexSWC[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < EccentricHamstring_2CV & 
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= EccentricHamstring_CV ) {
            
            EccHam_indexCV[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= EccentricHamstring_2CV ) {
            
            EccHam_index2CV[j] <- j+3+12
            
          }
        }  
      }
      
      if (as.character(kanga.temp_wide$Type[n]) == "Shoulder ER 90 Supine") {
        for (j in 1:stop.point) {
          if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < ShoulderER90Supine_CV & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= ShoulderER90Supine_SWC ) {
            
            ShER90_indexSWC[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < ShoulderER90Supine_2CV & 
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= ShoulderER90Supine_CV ) {
            
            ShER90_indexCV[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= ShoulderER90Supine_2CV ) {
            
            ShER90_index2CV[j] <- j+3+12
            
          }
        }  
      }
      
      if (as.character(kanga.temp_wide$Type[n]) == "Shoulder IR 90 Supine") {
        for (j in 1:stop.point) {
          if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < ShoulderIR90Supine_CV & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= ShoulderIR90Supine_SWC ) {
            
            ShIR90_indexSWC[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < ShoulderIR90Supine_2CV & 
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= ShoulderIR90Supine_CV ) {
            
            ShIR90_indexCV[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= ShoulderIR90Supine_2CV ) {
            
            ShIR90_index2CV[j] <- j+3+12
            
          }
        }  
      }
      
      if (as.character(kanga.temp_wide$Type[n]) == "Knee Flexion") {
        for (j in 1:stop.point) {
          if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < KneeFlexion_CV & 
               abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= KneeFlexion_SWC ) {
            
            KneeFl_indexSWC[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) < KneeFlexion_2CV & 
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= KneeFlexion_CV ) {
            
            KneeFl_indexCV[j] <- j+3+12
            
          } else if ( (!is.na(kanga.temp_wide[n,j+3])) & (!is.na(kanga.temp_wide[n, j+2])) &
                      abs(kanga.temp_wide[n,j+3] - kanga.temp_wide[n, j+2]) >= KneeFlexion_2CV ) {
            
            KneeFl_index2CV[j] <- j+3+12
            
          }
        }  
      }
    }
  }
  
  HipAdd_row <- which(kanga.temp_wide$Type == "Hip Adduction (Neutral)") + 16
  HipAbd_row <- which(kanga.temp_wide$Type == "Hip Abduction") + 16
  EccHam_row <- which(kanga.temp_wide$Type == "Eccentric Hamstring") + 16
  KneeFl_row <- which(kanga.temp_wide$Type == "Knee Flexion") + 16
  ShER90_row <- which(kanga.temp_wide$Type == "Shoulder ER 90 Supine") + 16
  ShIR90_row <- which(kanga.temp_wide$Type == "Shoulder IR 90 Supine") + 16
  
  addStyle(wb, Names[i], yellowStyle, rows = HipAdd_row, cols = HipAdd_indexSWC[!is.na(HipAdd_indexSWC)], gridExpand = TRUE)
  addStyle(wb, Names[i], yellowStyle, rows = HipAbd_row, cols = HipAbd_indexSWC[!is.na(HipAbd_indexSWC)], gridExpand = TRUE)
  addStyle(wb, Names[i], yellowStyle, rows = EccHam_row, cols = EccHam_indexSWC[!is.na(EccHam_indexSWC)], gridExpand = TRUE)
  addStyle(wb, Names[i], yellowStyle, rows = KneeFl_row, cols = KneeFl_indexSWC[!is.na(KneeFl_indexSWC)], gridExpand = TRUE)
  addStyle(wb, Names[i], yellowStyle, rows = ShER90_row, cols = ShER90_indexSWC[!is.na(ShER90_indexSWC)], gridExpand = TRUE)
  addStyle(wb, Names[i], yellowStyle, rows = ShIR90_row, cols = ShIR90_indexSWC[!is.na(ShIR90_indexSWC)], gridExpand = TRUE)
  
  addStyle(wb, Names[i], orangeStyle, rows = HipAdd_row, cols = HipAdd_indexCV[!is.na(HipAdd_indexCV)], gridExpand = TRUE)
  addStyle(wb, Names[i], orangeStyle, rows = HipAbd_row, cols = HipAbd_indexCV[!is.na(HipAbd_indexCV)], gridExpand = TRUE)
  addStyle(wb, Names[i], orangeStyle, rows = EccHam_row, cols = EccHam_indexCV[!is.na(EccHam_indexCV)], gridExpand = TRUE)
  addStyle(wb, Names[i], orangeStyle, rows = KneeFl_row, cols = KneeFl_indexCV[!is.na(KneeFl_indexCV)], gridExpand = TRUE)
  addStyle(wb, Names[i], orangeStyle, rows = ShER90_row, cols = ShER90_indexCV[!is.na(ShER90_indexCV)], gridExpand = TRUE)
  addStyle(wb, Names[i], orangeStyle, rows = ShIR90_row, cols = ShIR90_indexCV[!is.na(ShIR90_indexCV)], gridExpand = TRUE)
  
  addStyle(wb, Names[i], redStyle, rows = HipAdd_row, cols = HipAdd_index2CV[!is.na(HipAdd_index2CV)], gridExpand = TRUE)
  addStyle(wb, Names[i], redStyle, rows = HipAbd_row, cols = HipAbd_index2CV[!is.na(HipAbd_index2CV)], gridExpand = TRUE)
  addStyle(wb, Names[i], redStyle, rows = EccHam_row, cols = EccHam_index2CV[!is.na(EccHam_index2CV)], gridExpand = TRUE)
  addStyle(wb, Names[i], redStyle, rows = KneeFl_row, cols = KneeFl_index2CV[!is.na(KneeFl_index2CV)], gridExpand = TRUE)
  addStyle(wb, Names[i], redStyle, rows = ShER90_row, cols = ShER90_index2CV[!is.na(ShER90_index2CV)], gridExpand = TRUE)
  addStyle(wb, Names[i], redStyle, rows = ShIR90_row, cols = ShIR90_index2CV[!is.na(ShIR90_index2CV)], gridExpand = TRUE)
  
}

# Write MBIs
addWorksheet(wb, "MBI Reference", gridLines = TRUE)
writeData(wb, "MBI Reference", round(MBI, 2), rowNames = TRUE)
addStyle(wb, "MBI Reference", headerStyle, rows = 1, cols = 2:4)
setColWidths(wb, "MBI Reference", cols = 1, widths = "auto")

# Write labels
for (i in 1:length(Names)) {
  writeData(wb, Names[i], "ASYMMETRY", startRow = 1, startCol = 1)
  addStyle(wb, Names[i], labelStyle, rows = 1, cols = 1)
  
  writeData(wb, Names[i], "RATIO", startRow = 1, startCol = 13)
  addStyle(wb, Names[i], labelStyle, rows = 1, cols = 13)
  
  writeData(wb, Names[i], "MBI", startRow = 15, startCol = 13)
  addStyle(wb, Names[i], labelStyle, rows = 15, cols = 13)
  
  setColWidths(wb, Names[i], cols = 1:50, widths = "auto")
}

qb <- createWorkbook()
addWorksheet(qb, "data_cleaned", gridLines = TRUE, zoom = 75)
writeData(qb, "data_cleaned", kanga2write, rowNames = FALSE)


saveWorkbook(qb, paste0(club,"_Cleaned Data ", before_date, " to ", after_date, ".xlsx"), overwrite = TRUE)
saveWorkbook(wb, paste0(club,"_KangaTech Monitoring ", before_date, " to ", after_date, ".xlsx"), overwrite = TRUE)

print(club)
rm(list = ls())

