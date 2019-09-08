library(legcoplus)
library(magrittr)
library(qgraph)

vote_table5 <- readRDS("vote_table5.rds")
vote_table6 <- readRDS("vote_table6.rds")

###-----5th term-----
## overall
corm5 <- cor(vote_table5)

parties5 <- sapply(colnames(corm5), function(x) 
  legco_member_affiliation[["4"]] %$% AffiliationEng[SpeakerID == x])
parties5[is.na(parties5)] <- "Independent"

name_list5 <- sapply(colnames(corm5), function(x) 
  legco_member_affiliation[["4"]] %$% NameEng[SpeakerID == x])

colours5 <- c("FF33FF", "0033FF", "FF0000", "00CC00", "33FF00", "99FFCC", "C0C0C0",
              "00FFFF", "6699FF", "FFCC00", "66FF00", "99FFFF", "FFFFCC", "CC9900",
              "33CCCC", "CC3333", "FFFF00", "99FF00") %>% paste0("#", .)
# plot
png("5th_all.png", width = 18, height = 12, units = "in", res = 600)
qgraph(corm5, layout = "spring", groups = parties5,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2,
       nodeNames = name_list5, border.width = .5, color = colours5, 
       title = "Voting Behaviour of Legislators in the Fifth LegCo",
       title.cex = 2, legend = TRUE, legend.cex = .4, legend.mode = "style2")
dev.off()

## pro-establishment
pro_est5 <- readRDS("pro_est5.rds")

corm5_pro_est <- corm5[rownames(corm5) %in% pro_est5$id,
                       colnames(corm5) %in% pro_est5$id]

parties5 <- sapply(colnames(corm5_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% AffiliationEng[SpeakerID == x])
parties5[is.na(parties5)] <- "Independent"

name_list5 <- sapply(colnames(corm5_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% NameEng[SpeakerID == x])

colours5 <- c("FF33FF", "FF0000", "C0C0C0", "FFCC00", "FFFFCC", "CC9900",
              "CC3333", "FFFF00", "99FF00") %>% paste0("#", .)
# plot
png("5th_pro_est.png", width = 18, height = 12, units = "in", res = 600)
qgraph(corm5_pro_est, layout = "spring", groups = parties5,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1,0),
       nodeNames = name_list5, border.width = .5, color = colours5, 
       title = "Voting Behaviour of Pro-Establishment Legislators in the Fifth LegCo",
       title.cex = 2, legend = TRUE, legend.cex = .5, legend.mode = "style2")
dev.off()

## non-pro-establishment
corm5_non_pro_est <- corm5[!rownames(corm5) %in% pro_est5$id,
                           !colnames(corm5) %in% pro_est5$id]

parties5 <- sapply(colnames(corm5_non_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% AffiliationEng[SpeakerID == x])
parties5[is.na(parties5)] <- "Independent"

name_list5 <- sapply(colnames(corm5_non_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% NameEng[SpeakerID == x])

colours5 <- c("0033FF", "00CC00", "33FF00", "99FFCC", "00FFFF", "6699FF",
              "66FF00", "99FFFF", "33CCCC", "99FF00") %>% paste0("#", .)
# plot
png("5th_non_pro_est.png", width = 18, height = 12, units = "in", res = 600)
qgraph(corm5_non_pro_est, layout = "spring", groups = parties5, layoutOffset = c(-.2, 0),
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, 
       nodeNames = name_list5, border.width = .5, color = colours5,
       title = "Voting Behaviour of Non-Pro-Establishment Legislators in the Fifth LegCo",
       title.cex = 2, legend = TRUE, legend.cex = .6, legend.mode = "style2")
dev.off()
  
###-----6th term-----
## overall
corm6 <- cor(vote_table6)

parties6 <- sapply(colnames(corm6), function(x) 
  legco_member_affiliation[["5"]] %$% AffiliationEng[SpeakerID == x]) %>% unlist
parties6[is.na(parties6)] <- "Independent"

name_list6 <- sapply(colnames(corm6), function(x) 
  legco_member_affiliation[["5"]] %$% NameEng[SpeakerID == x]) %>% unlist

colours6 <- c("FF33FF", "0033FF", "6699FF", "FF0000", "00CC00", "99FFFF", "99FFCC", 
              "C0C0C0", "00FFFF", "FFCC00", "66FF00", "FFFFCC", "CC9900", "33CCCC", 
              "FF99FF", "CC3333", "FFFF00", "99FF00") %>% paste0("#", .)
# plot
png("6th_all.png", width = 18, height = 12, units = "in", res = 600)
qgraph(corm6, layout = "spring", groups = parties6, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2,
       nodeNames = name_list6, border.width = .5, color = colours6,
       title = "Voting Behaviour of Legislators in the Sixth LegCo",
       title.cex = 2, legend = TRUE, legend.cex = .36, legend.mode = "style2")
dev.off()

## pro-establishment
pro_est6 <- readRDS("pro_est6.rds")

corm6_pro_est <- corm6[rownames(corm6) %in% pro_est6$id,
                       colnames(corm6) %in% pro_est6$id]

parties6 <- sapply(colnames(corm6_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% AffiliationEng[SpeakerID == x]) %>% unlist
parties6[is.na(parties6)] <- "Independent"

name_list6 <- sapply(colnames(corm6_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% NameEng[SpeakerID == x]) %>% unlist

colours6 <- c("FF33FF", "FF0000", "C0C0C0", "FFCC00", "FFFFCC", "CC9900", 
              "FF99FF", "CC3333", "FFFF00") %>% paste0("#", .)
# plot
png("6th_pro_est.png", width = 18, height = 12, units = "in", res = 600)
qgraph(corm6_pro_est, layout = "spring", groups = parties6, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1,0),
       nodeNames = name_list6, border.width = .5, color = colours6,
       title = "Voting Behaviour of Pro-Establishment Legislators in the Sixth LegCo",
       title.cex = 2, legend = TRUE, legend.cex = .5, legend.mode = "style2")
dev.off()

## non-pro-establishment
corm6_non_pro_est <- corm6[!rownames(corm6) %in% pro_est6$id,
                           !colnames(corm6) %in% pro_est6$id]

parties6 <- sapply(colnames(corm6_non_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% AffiliationEng[SpeakerID == x]) %>% unlist
parties6[is.na(parties6)] <- "Independent"

name_list6 <- sapply(colnames(corm6_non_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% NameEng[SpeakerID == x]) %>% unlist

colours6 <- c("0033FF", "6699FF", "00CC00", "99FFFF", "99FFCC", "C0C0C0",
              "00FFFF", "66FF00", "33CCCC", "99FF00") %>% paste0("#", .)
# plot
png("6th_non_pro_est.png", width = 18, height = 12, units = "in", res = 600)
qgraph(corm6_non_pro_est, layout = "spring", groups = parties6, layoutOffset = c(-.2, 0),
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2,
       nodeNames = name_list6, border.width = .5, color = colours6,
       title = "Voting Behaviour of Non-Pro-Establishment Legislators in the Sixth LegCo",
       title.cex = 2, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()
   
