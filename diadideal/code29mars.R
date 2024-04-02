# var: EQ_Q01_MOBIL_V0_M1/_V1_M2/_V2_M3

#subset the dataset

eqed = subset(did, select = c("Study Subject ID",
  "EQ_Q01_MOBIL_V0_M1","EQ_Q02_AUTON_V0_M1","EQ_Q03_ACT_V0_M1",
  "EQ_Q04_DOUL_V0_M1","EQ_Q05_ANX_V0_M1", "EQ_Q01_MOBIL_V1_M2","EQ_Q02_AUTON_V1_M2","EQ_Q03_ACT_V1_M2",
  "EQ_Q04_DOUL_V1_M2","EQ_Q05_ANX_V1_M2", "EQ_Q01_MOBIL_V2_M3","EQ_Q02_AUTON_V2_M3","EQ_Q03_ACT_V2_M3",
  "EQ_Q04_DOUL_V2_M3","EQ_Q05_ANX_V2_M3", "EQ_Q01_MOBIL_V3_M4","EQ_Q02_AUTON_V3_M4","EQ_Q03_ACT_V3_M4",
  "EQ_Q04_DOUL_V3_M4","EQ_Q05_ANX_V3_M4", "EQ_Q01_MOBIL_V4_M5","EQ_Q02_AUTON_V4_M5","EQ_Q03_ACT_V4_M5",
  "EQ_Q04_DOUL_V4_M5","EQ_Q05_ANX_V4_M5", "EQ_Q01_MOBIL_V5_M6","EQ_Q02_AUTON_V5_M6","EQ_Q03_ACT_V5_M6",
  "EQ_Q04_DOUL_V5_M6","EQ_Q05_ANX_V5_M6", "EQ_Q01_MOBIL_V6_M7","EQ_Q02_AUTON_V6_M7","EQ_Q03_ACT_V6_M7",
  "EQ_Q04_DOUL_V6_M7","EQ_Q05_ANX_V6_M7","EQ_Q01_MOBIL_V7_M8","EQ_Q02_AUTON_V7_M8","EQ_Q03_ACT_V7_M8",
  "EQ_Q04_DOUL_V7_M8","EQ_Q05_ANX_V7_M8", "EQ_Q01_MOBIL_V8_M9","EQ_Q02_AUTON_V8_M9","EQ_Q03_ACT_V8_M9",
  "EQ_Q04_DOUL_V8_M9","EQ_Q05_ANX_V8_M9", "EQ_Q01_MOBIL_V9_M10","EQ_Q02_AUTON_V9_M10","EQ_Q03_ACT_V9_M10",
  "EQ_Q04_DOUL_V9_M10","EQ_Q05_ANX_V9_M10","EQ_Q01_MOBIL_V10_M11","EQ_Q02_AUTON_V10_M11","EQ_Q03_ACT_V10_M11",
  "EQ_Q04_DOUL_V10_M11","EQ_Q05_ANX_V10_M11","EQ_Q01_MOBIL_V11_M12","EQ_Q02_AUTON_V11_M12","EQ_Q03_ACT_V11_M12",
  "EQ_Q04_DOUL_V11_M12","EQ_Q05_ANX_V11_M12"))

View(eqed)
dim(eqed)

eqed <- as.tibble(eqed)
eqed <- eqed %>% rename(
  "ID" = "Study Subject ID"
)

# means 

eqed$mean_q1 = mean(eqed$EQ_Q01_MOBIL_V0_M1, na.rm = T)

# library to pivot 

library(tidyr)
library(dplyr)
library(readr)

eqed_t = t(eqed)
eqed_t2 = data.frame(cbind(names(eqed), t(eqed)))
View(eqed_t)
names(eqed_t)
table(eqed_t$V1)

eqed_t <- eqed_t[-1,]
eqed_t <- data.frame(eqed_t)
names(eqed_t)

colnames(eqed_t) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")

# graphic representation

Q1 = subset(eqed, select = c("ID", "EQ_Q01_MOBIL_V0_M1", "EQ_Q01_MOBIL_V1_M2", 
                             "EQ_Q01_MOBIL_V2_M3", "EQ_Q01_MOBIL_V3_M4", "EQ_Q01_MOBIL_V4_M5", 
                             "EQ_Q01_MOBIL_V5_M6", "EQ_Q01_MOBIL_V6_M7", "EQ_Q01_MOBIL_V7_M8",
                             "EQ_Q01_MOBIL_V8_M9", "EQ_Q01_MOBIL_V9_M10", "EQ_Q01_MOBIL_V11_M12"))

did %>%
  mutate(class = fct_reorder(EQ_Q01_MOBIL_V0_M1, EQ_Q01_MOBIL_V))