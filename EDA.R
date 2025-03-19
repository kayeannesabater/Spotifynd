#working directory
df <- read.csv("top10s.csv", header = T)
df <- df[,-1]
rename_titles <- df[,1]
rownames(df) <- c(rename_titles)
