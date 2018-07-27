ramka <- read.csv2("123.csv", skip = 6, header = FALSE, stringsAsFactors = FALSE)

elo <- rbind(ramka$V14)
colnames(elo) <- rbind(ramka$V3)
elo <- matrix(elo)
nowe <- matrix(data = elo, nrow = 8, ncol = 12)
rownames(nowe) <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(nowe) <- seq(1,12)

# BCD 2 , EFG 2, BCD 3, EFG 3 to 4 pożywki dla jednego szczepu

newnew <- matrix(nowe, nrow = 4, ncol = 24)

nowe <- nowe[-1,]
nowe <- nowe[-7,]


# każda kolumna to oddzielna pożywka oprócz dwóch pierwszych, bo to jakieś kontrole czy coś i dwóch ostatnich (dopytać dziewczyn co to)
nowa_ramka <- matrix(nowe,nrow = 3, ncol = 24)
mediums <- c("control", "control", "LB10", "TSB", "BHI", "M63", "LB10", "TSB", "BHI", "M63", "LB10", "TSB", "BHI", "M63", "LB10", "TSB", "BHI", "M63", "LB10", "TSB", "BHI", "M63", "control", "control")

colnames(nowa_ramka) <- mediums

colMeans(nowa_ramka)
