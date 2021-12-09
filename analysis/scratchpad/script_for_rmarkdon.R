setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
setwd("..")
biw.data <- read.csv("analysis/data/raw_data/Maizuru_dominant_sp.csv")
d.bestE.0 <- read.csv("analysis/data/raw_data/Maizuru_ccm_res.csv")
source("R/Functions_1_HelperFuncs.R")
source("R/extract_causality.R")
source("R/arrange_interactions.R")
source("R/plot_time_3d.R")
source("R/int_extract.R")
source("R/config.R")
source("R/Functions_2_SmapCoefPara.R")

library(plotly)
library(reticulate)

num.for.smapc <-extract_causality (d.bestE.0 )
d.name   <- names(biw.data)[4:18]
new.nums<- arrange_interactions(num.for.smapc)
smapc.res <- SmapCFunc(new.nums, smapc.tp = 1, stats.output = T,
                       embedding = "best_E", original.data = biw.data)
smapc.tp1 <- smapc.res$coefs
interaction_extract <- int_extract(smapc.tp1)
plot_obj <- plot_time_3d(interaction_extract )
save_image(plot_obj , file = "analysis/figures/image.png", width= 1200, height = 800,scale  =3)
