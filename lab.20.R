install.packages("nanoparquet")
library(nanoparquet)
picharter_all <- read_parquet(file='db_unified.parquet')
