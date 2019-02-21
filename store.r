# store data for the GAMS scripts in GtapInGams

rm(list = ls())

source(file = "R/gdx_util.r")

load(file = "data/trade_data.RData")




# check missing quantity obesrvations
x <- ungroup(trade_data) %>% spread(indicator, avg)
x <- x %>% mutate(isMissing = QUANTITY * VALUE_1000EURO)
x <- x %>% filter(isMissing == 0)


# putting importer before exporter
trade_data <- ungroup(trade_data) %>% select(reporter, partner, hs6, flow, indicator, avg)

# call avg as value
colnames(trade_data) <- c("reporter", "partner", "hs6", "flow", "variable", "value")


# store trade matrxi in .gdx

trade_data$reporter <- factor(trade_data$reporter)
trade_data$partner <- factor(trade_data$partner)
trade_data$hs6 <- factor(trade_data$hs6)
trade_data$flow <- factor(trade_data$flow)
trade_data$variable <- factor(trade_data$variable)

write_param_togdx(trade_data, file = "gdx/ceta_data.gdx", symname = "p_CETA_tradeMatrix", ts = "CETA trade matrix with dims reporter, partner, hs6, flow, variable, value")


# hs6 list
uu <- list(c(unique(as.character(trade_data$hs6))))
wgdx("gdx/hs6list.gdx", list(name='hs6list', type='set', ts='list of HS lines', uels=uu))

