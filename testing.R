ramka <- read.csv2("123.csv", skip = 6, header = FALSE, stringsAsFactors = FALSE)

elo <- rbind(ramka$V14)
colnames(elo) <- rbind(ramka$V3)
elo <- data.frame(elo)
nowe <- matrix(data = elo, nrow = 8, ncol = 12)
rownames(nowe) <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(nowe) <- seq(1,12)


