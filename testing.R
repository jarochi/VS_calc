library(robustbase)

ramka <- read.csv2("123.csv", skip = 6, header = FALSE, stringsAsFactors = FALSE)

elo <- rbind(ramka$V14)
colnames(elo) <- rbind(ramka$V3)
elo <- matrix(elo)
nowe <- matrix(data = elo, nrow = 8, ncol = 12)
rownames(nowe) <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(nowe) <- seq(1,12)

# BCD 2 , EFG 2, BCD 3, EFG 3 to 4 pożywki dla jednego szczepu

nowe <- nowe[-1,]
nowe <- nowe[-7,]

# usuwam kontrole

nowe <- nowe[,-1]
nowe <- nowe[,-11]


# # każda kolumna to oddzielna pożywka oprócz dwóch pierwszych, bo to jakieś kontrole czy coś i dwóch ostatnich (dopytać dziewczyn co to)
# nowa_ramka <- matrix(nowe,nrow = 3, ncol = 24)

# dodać wiersz u góry i na dole z pożywkami, nazwa kolumny to szczep 

mediums <- c("LB10", "TSB", 
             "BHI", "M63")

colnames(nowa_ramka) <- mediums

colMeans(nowa_ramka)
colMedians(nowa_ramka)

### przepisać dane z CV

absorbance <- c(0.204, 0.345, 0.251, 0.158, 0.141, 0.283, 
                0.202, 0.303, 0.522, 0.303, 0.183, 0.184, 
                0.791, 0.126, 0.186, 0.207, 0.219, 0.316, 
                0.22, 0.327, 0.46, 0.267, 0.202, 0.174, 
                0.195, 0.408, 0.611, 0.859, 0.872, 0.665, 
                0.29, 0.469, 0.679, 0.31, 0.265, 0.199, 
                0.44, 0.149, 0.172, 0.152, 0.189, 0.138, 
                0.185, 0.209, 0.206, 0.18, 0.168, 0.142, 
                0.032, 0.031, 0.031, 0.031, 0.032, 0.032, 
                0.146, 0.316, 0.243, 0.409, 0.189, 0.154)

absorbance <- matrix(absorbance, nrow = 6, ncol = 10)

# pearson correlation


