library(XML)
library(dplyr)
library(tibble)
library(tidyr)
trim <- function (x)
  gsub("^\\s+|\\s+$", "", x)
Allsvenskan <-
  data.frame(
    Game = character(),
    Market = character(),
    Odds = numeric(),
    stringsAsFactors = FALSE
  )
counter <- 0
for (p in 1:10) {
  ahurl <- "http://www.soccerstats.com/homeaway.asp?league=sweden"
  H_A <- readHTMLTable(ahurl, which = 7)
  
  H <- H_A[-c(18:34, 1),]
  
  H$V1 <- NULL
  H$V4 <- NULL
  H$V5 <- NULL
  H$V6 <- NULL
  H$V9 <- NULL
  H$V10 <- NULL
  
  colnames(H)[1] <- "Team"
  colnames(H)[2] <- "GP"
  colnames(H)[3] <- "GF"
  colnames(H)[4] <- "GA"
  
  H$GP <- as.numeric(levels(H$GP))[H$GP]
  H$GF <- as.numeric(levels(H$GF))[H$GF]
  H$GA <- as.numeric(levels(H$GA))[H$GA]
  
  H$AGF <- round(H$GF / H$GP, 2)
  H$AGA <- round(H$GA / H$GP, 2)
  
  H_meanGF <- round(mean(H$AGF), 2)
  H_meanGA <- round(mean(H$AGA), 2)
  
  H$atkstr <- round(H$AGF / H_meanGF, 2)
  H$defstr <- round(H$AGA / H_meanGA, 2)
  
  A <- H_A[-c(1:18),]
  
  A$V1 <- NULL
  A$V4 <- NULL
  A$V5 <- NULL
  A$V6 <- NULL
  A$V9 <- NULL
  A$V10 <- NULL
  
  colnames(A)[1] <- "Team"
  colnames(A)[2] <- "GP"
  colnames(A)[3] <- "GF"
  colnames(A)[4] <- "GA"
  
  A$GP <- as.numeric(levels(A$GP))[A$GP]
  A$GF <- as.numeric(levels(A$GF))[A$GF]
  A$GA <- as.numeric(levels(A$GA))[A$GA]
  
  A$AGF <- round(A$GF / A$GP, 2)
  A$AGA <- round(A$GA / A$GP, 2)
  
  A_meanGF <- round(mean(A$AGF), 2)
  A_meanGA <- round(mean(A$AGA), 2)
  
  A$atkstr <- round(A$AGF / A_meanGF, 2)
  A$defstr <- round(A$AGA / A_meanGA, 2)
  
  matchesurl <-
    "http://www.soccerstats.com/latest.asp?league=sweden#"
  matches <- readHTMLTable(matchesurl, which = 9)
  matches$V1 <- NULL
  matches$V3 <- NULL
  matches$V4 <- NULL
  matches <- na.omit(matches)
  matches <-
    separate(
      data = matches,
      col = V2,
      into = c("Home", "Away"),
      sep = "\\-"
    )
  matches$Home <- trim(matches$Home)
  matches$Away <- trim(matches$Away)
  
  hometeam <- matches$Home[p]
  awayteam <- matches$Away[p]
  hleague <- H$Team
  aleague <- A$Team
  
  hattackstrength <- subset(H, grepl(hometeam, hleague))
  hdefensivestrength <- subset(H, grepl(hometeam, hleague))
  aattackstrength <- subset(A, grepl(awayteam, aleague))
  adefensivestrength <- subset(A, grepl(awayteam, aleague))
  
  counter <- counter + 1
  
  hGE <-
    as.numeric(hattackstrength[7] * adefensivestrength[8] * H_meanGF)
  aGE <-
    as.numeric(aattackstrength[7] * hdefensivestrength[8] * A_meanGF)
  
  
  for (i in 0:9) {
    nam <- paste("p0_", i, sep = "")
    assign(nam, (dpois(0, hGE)) * (dpois (i, aGE)) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p1_", i, sep = "")
    assign(nam, dpois(1, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p2_", i, sep = "")
    assign(nam, dpois(2, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p3_", i, sep = "")
    assign(nam, dpois(3, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p4_", i, sep = "")
    assign(nam, dpois(4, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p5_", i, sep = "")
    assign(nam, dpois(5, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p6_", i, sep = "")
    assign(nam, dpois(6, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p7_", i, sep = "")
    assign(nam, dpois(7, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p8_", i, sep = "")
    assign(nam, dpois(8, hGE) * dpois (i, aGE) * 100)
    
  }
  for (i in 0:9) {
    nam <- paste("p9_", i, sep = "")
    assign(nam, dpois(9, hGE) * dpois (i, aGE) * 100)
    
  }
  
  h0 <- c(p0_0, p0_1, p0_2, p0_3, p0_4, p0_5, p0_6, p0_7, p0_8, p0_9)
  h1 <- c(p1_0, p1_1, p1_2, p1_3, p1_4, p1_5, p1_6, p1_7, p1_8, p1_9)
  h2 <- c(p2_0, p2_1, p2_2, p2_3, p2_4, p2_5, p2_6, p2_7, p2_8, p2_9)
  h3 <- c(p3_0, p3_1, p3_2, p3_3, p3_4, p3_5, p3_6, p3_7, p3_8, p3_9)
  h4 <- c(p4_0, p4_1, p4_2, p4_3, p4_4, p4_5, p4_6, p4_7, p4_8, p4_9)
  h5 <- c(p5_0, p5_1, p5_2, p5_3, p5_4, p5_5, p5_6, p5_7, p5_8, p5_9)
  h6 <- c(p6_0, p6_1, p6_2, p6_3, p6_4, p6_5, p6_6, p6_7, p6_8, p6_9)
  h7 <- c(p7_0, p7_1, p7_2, p7_3, p7_4, p7_5, p7_6, p7_7, p7_8, p7_9)
  h8 <- c(p8_0, p8_1, p8_2, p8_3, p8_4, p8_5, p8_6, p8_7, p8_8, p8_9)
  h9 <- c(p9_0, p9_1, p9_2, p9_3, p9_4, p9_5, p9_6, p9_7, p9_8, p9_9)
  
  pg <- data.frame(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9)
  pg <- t(pg)
  pg <- round(pg, 3)
  pg <- data.frame(pg)
  
  colnames(pg)[1] <- "a0"
  colnames(pg)[2] <- "a1"
  colnames(pg)[3] <- "a2"
  colnames(pg)[4] <- "a3"
  colnames(pg)[5] <- "a4"
  colnames(pg)[6] <- "a5"
  colnames(pg)[7] <- "a6"
  colnames(pg)[8] <- "a7"
  colnames(pg)[9] <- "a8"
  colnames(pg)[10] <- "a9"
  
  hWin <-
    sum(pg$a0[2:10],
        pg$a1[3:10],
        pg$a2[4:10],
        pg$a3[5:10],
        pg$a4[6:10],
        pg$a5[7:10],
        pg$a6[8:10],
        pg$a7[9:10],
        pg$a8[10])
  draw <-
    sum(pg$a0[1],
        pg$a1[2],
        pg$a2[3],
        pg$a3[4],
        pg$a4[5],
        pg$a5[6],
        pg$a6[7],
        pg$a7[8],
        pg$a8[9],
        pg$a9[10])
  aWin <-
    sum(pg$a1[1],
        pg$a2[1:2],
        pg$a3[1:3],
        pg$a4[1:4],
        pg$a5[1:5],
        pg$a6[1:6],
        pg$a7[1:7],
        pg$a8[1:8],
        pg$a9[1:9])
  o1.5 <-
    sum(pg$a0[3:10],
        pg$a1[2:10],
        pg$a2[1:10],
        pg$a3[1:10],
        pg$a4[1:10],
        pg$a5[1:10],
        pg$a6[1:10],
        pg$a7[1:10],
        pg$a8[1:10],
        pg$a9[1:10])
  u1.5 <- sum(pg$a0[1:2], pg$a1[1])
  o2.5 <-
    sum(pg$a0[4:10],
        pg$a1[3:10],
        pg$a2[2:10],
        pg$a3[1:10],
        pg$a4[1:10],
        pg$a5[1:10],
        pg$a6[1:10],
        pg$a7[1:10],
        pg$a8[1:10],
        pg$a9[1:10])
  u2.5 <- sum(pg$a0[1:3], pg$a1[1:2], pg$a2[1])
  o3.5 <-
    sum(pg$a0[5:10],
        pg$a1[4:10],
        pg$a2[3:10],
        pg$a3[2:10],
        pg$a4[1:10],
        pg$a5[1:10],
        pg$a6[1:10],
        pg$a7[1:10],
        pg$a8[1:10],
        pg$a9[1:10])
  u3.5 <- sum(pg$a0[1:4], pg$a1[1:3], pg$a2[1:2], pg$a3[1])
  o4.5 <-
    sum(pg$a0[6:10],
        pg$a1[5:10],
        pg$a2[4:10],
        pg$a3[3:10],
        pg$a4[2:10],
        pg$a5[1:10],
        pg$a6[1:10],
        pg$a7[1:10],
        pg$a8[1:10],
        pg$a9[1:10])
  u4.5 <- sum(pg$a0[1:5], pg$a1[1:4], pg$a2[1:3], pg$a3[1:2], pg$a4[1])
  BTTS_Y <-
    sum(pg$a1[2:10],
        pg$a2[2:10],
        pg$a3[2:10],
        pg$a4[2:10],
        pg$a5[2:10],
        pg$a6[2:10],
        pg$a7[2:10],
        pg$a8[2:10],
        pg$a9[2:10])
  BTTS_N <-
    sum(pg$a0[1:10],
        pg$a1[1],
        pg$a2[1],
        pg$a3[1],
        pg$a4[1],
        pg$a5[1],
        pg$a6[1],
        pg$a7[1],
        pg$a8[1],
        pg$a9[1])
  
  
  prediction <-
    t(data.frame(
      hWin,
      draw,
      aWin,
      o1.5,
      u1.5,
      o2.5,
      u2.5,
      o3.5,
      u3.5,
      o4.5,
      u4.5,
      BTTS_Y,
      BTTS_N
    ))
  prediction <- data.frame(prediction)
  prediction <- rownames_to_column(prediction, "Market")
  prediction["Game"] <- paste(hometeam, awayteam, sep = " v. ")
  prediction <- prediction[, c(3, 1, 2)]
  colnames(prediction)[3] <- "Odds"
  
  assign(paste("PROB", p, sep = ""),
         subset(prediction, prediction$Odds >= 0))
  
}
Allsvenskan <-
  mget(paste('PROB', 1:counter, sep = ""), envir = parent.frame())
Allsvenskan <- do.call('rbind', Allsvenskan)
rownames(Allsvenskan) <- NULL
Allsvenskan["Decimal"] <- round(100 / Allsvenskan$Odds, 2)
