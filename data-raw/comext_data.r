library(tidyverse)

#------
# CANADIAN EXPORTS (FOB)

canexport <- read.csv(file = "csv/canexport.csv", header = TRUE, colClasses = c(rep("character",3),"numeric",rep("character",2),"numeric"), sep = ";")

canexport <- as.tibble(canexport)
canexport$DECLARANT <- "CAN"

# rename FLOW
canexport <- canexport %>% select(-FLOW)
canexport$FLOW <- "EXPORTS"


# calculate average over three calendar years
canexport_avg <- canexport %>% group_by(DECLARANT, PARTNER, PRODUCT, FLOW, INDICATORS) %>% summarize(avg=mean(INDICATOR_VALUE, na.rm=FALSE))


# rename header for better merging with import data
colnames(canexport_avg) <- c("reporter","partner","hs6","flow","indicator","avg")




# -----
# CANADIAN IMPORTS (CIF)

canimport <- read.csv(file = "csv/canimport.csv", header = TRUE, colClasses = c(rep("character",3),"numeric",rep("character",2),"numeric"), sep = ";")

canimport <- as.tibble(canimport)
canimport$DECLARANT <- "CAN"

# rename FLOW
canimport <- canimport %>% select(-FLOW)
canimport$FLOW <- "IMPORTS" 

# rename to ROW and to EU_28
canimport[canimport$PARTNER=="otherthanEUAgg",]$PARTNER <- "ROW"
canimport[canimport$PARTNER=="myEU",]$PARTNER <- "EU_28"



# calculate average over three calendar years
canimport_avg <- canimport %>% group_by(DECLARANT, PARTNER, PRODUCT, FLOW, INDICATORS) %>% summarize(avg=mean(INDICATOR_VALUE, na.rm=FALSE))


# rename header for better merging with export data
colnames(canimport_avg) <- c("reporter","partner","hs6","flow","indicator","avg")



# ------
# EU-ROW trade data (both CIB and FOB)

eurow <- read.csv(file = "csv/eu_row.csv", header = TRUE, colClasses = c(rep("character",3),"numeric",rep("character",3),"numeric"), sep = ";")

eurow <- as.tibble(eurow)
eurow <- eurow %>% select(-Statistical.Procedure)

# rename indicators
eurow <- eurow %>% mutate_if(is.character, str_replace_all, pattern = "CUM_QUANTITY_TON", replacement = "QUANTITY")
eurow <- eurow %>% mutate_if(is.character, str_replace_all, pattern = "CUM_VALUE_1000ECU", replacement = "VALUE_1000EURO")

# separate import and export flows (note that the declarant is always the EU)
euimp <- eurow %>% filter(Flow == 1)
euexp <- eurow %>% filter(Flow == 2)

# rename FLOW
euimp <- euimp %>% select(-Flow)
euimp$Flow <- "IMPORTS"

euexp <- euexp %>% select(-Flow)
euexp$Flow <- "EXPORTS"


# rename ROW and EU
euimp$Partner.Country <- "ROW"
euexp$Partner.Country <- "ROW"
euimp$DECLARANT <- "EU_28"
euexp$DECLARANT <- "EU_28"





# calculate average
euimp_avg <- euimp %>% group_by(DECLARANT, Partner.Country, PRODUCT_NC, Flow, INDICATORS) %>% summarize(avg=mean(INDICATOR_VALUE, na.rm=FALSE))
euexp_avg <- euexp %>% group_by(DECLARANT, Partner.Country, PRODUCT_NC, Flow, INDICATORS) %>% summarize(avg=mean(INDICATOR_VALUE, na.rm=FALSE))


# rename for easier merge
colnames(euimp_avg) <- c("reporter", "partner", "hs6", "flow", "indicator", "avg")
colnames(euexp_avg) <- c("reporter", "partner", "hs6", "flow", "indicator", "avg")




#-----
# EU imports and exports to CAN 

euflows <- read.csv(file = "csv/eu_cif_fob.csv", header = TRUE, colClasses = c(rep("character",3),"numeric",rep("character",3),"numeric"), sep = ";")


euflows <- as.tibble(euflows)
euflows <- euflows %>% select(-Statistical.Procedure)

# rename indicators
euflows <- euflows %>% mutate_if(is.character, str_replace_all, pattern = "CUM_QUANTITY_TON", replacement = "QUANTITY")
euflows <- euflows %>% mutate_if(is.character, str_replace_all, pattern = "CUM_VALUE_1000ECU", replacement = "VALUE_1000EURO")

# separate import and export flows 
# Note that the declarant is always the EU
# and the partner is CANADA
euflows <- euflows %>% filter(Partner.Country == "0404")


euflows$Partner.Country <- "CAN"
euflows$DECLARANT <- "EU_28"


euflows[euflows$Flow == "1",]$Flow <-  "IMPORTS"
euflows[euflows$Flow == "2",]$Flow <-  "EXPORTS"


euflows_avg <- euflows %>% group_by(DECLARANT, Partner.Country, PRODUCT_NC, Flow, INDICATORS) %>% summarize(avg=mean(INDICATOR_VALUE, na.rm=FALSE))

colnames(euflows_avg) <- c("reporter", "partner", "hs6", "flow", "indicator", "avg")



#---- 
# do the merge and save to disk
trade_data <- canexport_avg %>% full_join(canimport_avg) %>% full_join(euimp_avg) %>% full_join(euexp_avg) %>% full_join(euflows_avg)

save(trade_data, file = "data/trade_data.RData")
