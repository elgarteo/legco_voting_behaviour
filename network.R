library(legcoplus)
library(magrittr)
library(qgraph)

###-----5th term-----
vote_table5 <- readRDS("vote_table5.rds")

## overall
corm5 <- cor(vote_table5)

parties5 <- data.frame(en = sapply(colnames(corm5), function(x)
  legco_member_affiliation[["4"]] %$% AffiliationEng[SpeakerID == x]),
  zh = sapply(colnames(corm5), function(x) 
    legco_member_affiliation[["4"]] %$% AffiliationChi[SpeakerID == x]), 
  stringsAsFactors = FALSE)
parties5$en[is.na(parties5$en)] <- "Independent"
parties5$zh[is.na(parties5$zh)] <- "獨立"

name_list5 <- data.frame(en = sapply(colnames(corm5), function(x) 
  legco_member_affiliation[["4"]] %$% NameEng[SpeakerID == x]),
  zh = sapply(colnames(corm5), function(x) 
    legco_member_affiliation[["4"]] %$% NameChi[SpeakerID == x]))

# plot English
colours5 <- c("FF33FF", "0033FF", "FF0000", "00CC00", "33FF00", "99FFCC", "C0C0C0",
              "00FFFF", "6699FF", "FFCC00", "66FF00", "99FFFF", "FFFFCC", "CC9900",
              "33CCCC", "CC3333", "FFFF00", "99FF00") %>% paste0("#", .)

png("5th_all_en.png", width = 1800, height = 1200, units = "px")
qgraph(corm5, layout = "spring", groups = parties5$en,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2,
       nodeNames = name_list5$en, border.width = .5, color = colours5, 
       title = "Voting Behaviour of Legislators in the Fifth LegCo",
       title.cex = 3, legend = TRUE, legend.cex = .55, legend.mode = "style2")
dev.off()

# plot Chinese
colours5 <- c("33CCCC", "99FF00", "0033FF", "FFFF00", "00FFFF", "FFFFCC", "99FFFF",
              "CC9900", "FF0000", "33FF00", "00CC00", "CC3333", "C0C0C0", "6699FF",
              "FFCC00", "66FF00", "99FFCC", "FF33FF") %>% paste0("#", .)

png("5th_all_zh.png", width = 1800, height = 1200, units = "px")
par(family = "Heiti TC Light")
qgraph(corm5, layout = "spring", groups = parties5$zh,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2,
       nodeNames = name_list5$zh, border.width = .5, color = colours5, 
       title = "第五屆立法會會議議員投票行為",
       title.cex = 3, legend = TRUE, legend.cex = .55, legend.mode = "style2")
dev.off()

## pro-establishment
pro_est5 <- readRDS("pro_est5.rds")

corm5_pro_est <- corm5[rownames(corm5) %in% pro_est5$id,
                       colnames(corm5) %in% pro_est5$id]

parties5 <- data.frame(en = sapply(colnames(corm5_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% AffiliationEng[SpeakerID == x]),
  zh = sapply(colnames(corm5_pro_est), function(x) 
    legco_member_affiliation[["4"]] %$% AffiliationChi[SpeakerID == x]),
  stringsAsFactors = FALSE)
parties5$en[is.na(parties5$en)] <- "Independent"
parties5$zh[is.na(parties5$zh)] <- "獨立"

name_list5 <- data.frame(en = sapply(colnames(corm5_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% NameEng[SpeakerID == x]),
  zh = sapply(colnames(corm5_pro_est), function(x) 
    legco_member_affiliation[["4"]] %$% NameChi[SpeakerID == x]))

# plot English
colours5 <- c("FF33FF", "FF0000", "C0C0C0", "FFCC00", "FFFFCC", "CC9900",
              "CC3333", "FFFF00") %>% paste0("#", .)

png("5th_pro_est_en.png", width = 1800, height = 1200, units = "px")
qgraph(corm5_pro_est, layout = "spring", groups = parties5$en,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list5$en, border.width = .5, color = colours5, 
       title = "Voting Behaviour of Pro-Establishment Legislators in the Fifth LegCo",
       title.cex = 3, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

# plot Chinese
colours5 <- c("FFFF00", "FFFFCC", "CC9900", "FF0000", "CC3333", "C0C0C0",
              "FFCC00", "FF33FF") %>% paste0("#", .)

png("5th_pro_est_zh.png", width = 1800, height = 1200, units = "px")
par(family = "Heiti TC Light")
qgraph(corm5_pro_est, layout = "spring", groups = parties5$zh,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list5$zh, border.width = .5, color = colours5, 
       title = "第五屆立法會會議建制派議員投票行為",
       title.cex = 3, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

## non-pro-establishment
corm5_non_pro_est <- corm5[!rownames(corm5) %in% pro_est5$id,
                           !colnames(corm5) %in% pro_est5$id]

parties5 <- data.frame(en = sapply(colnames(corm5_non_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% AffiliationEng[SpeakerID == x]),
  zh = sapply(colnames(corm5_non_pro_est), function(x) 
    legco_member_affiliation[["4"]] %$% AffiliationChi[SpeakerID == x]),
  stringsAsFactors = FALSE)
parties5$en[is.na(parties5$en)] <- "Independent"
parties5$zh[is.na(parties5$zh)] <- "獨立"

name_list5 <- data.frame(en = sapply(colnames(corm5_non_pro_est), function(x) 
  legco_member_affiliation[["4"]] %$% NameEng[SpeakerID == x]),
  zh = sapply(colnames(corm5_non_pro_est), function(x) 
    legco_member_affiliation[["4"]] %$% NameChi[SpeakerID == x]))

# plot English
colours5 <- c("0033FF", "00CC00", "33FF00", "99FFCC", "00FFFF", "6699FF",
              "66FF00", "99FFFF", "33CCCC", "99FF00") %>% paste0("#", .)

png("5th_non_pro_est_en.png", width = 1800, height = 1200, units = "px")
qgraph(corm5_non_pro_est, layout = "spring", groups = parties5$en,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list5$en, border.width = .5, color = colours5,
       title = "Voting Behaviour of Non-Pro-Establishment Legislators in the Fifth LegCo",
       title.cex = 3, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

# plot Chinese
colours5 <- c("33CCCC", "99FF00", "0033FF", "00FFFF", "99FFFF", "33FF00",
              "00CC00", "6699FF", "66FF00", "99FFCC") %>% paste0("#", .)

png("5th_non_pro_est_zh.png", width = 1800, height = 1200, units = "px")
par(family = "Heiti TC Light")
qgraph(corm5_non_pro_est, layout = "spring", groups = parties5$zh,
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list5$zh, border.width = .5, color = colours5,
       title = "第五屆立法會會議非建制派議員投票行為",
       title.cex = 3, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

###-----6th term-----
vote_table6 <- readRDS("vote_table6.rds")

## overall
corm6 <- cor(vote_table6)

parties6 <- data.frame(en = sapply(colnames(corm6), function(x) 
  legco_member_affiliation[["5"]] %$% AffiliationEng[SpeakerID == x]),
  zh = sapply(colnames(corm6), function(x) 
    legco_member_affiliation[["5"]] %$% AffiliationChi[SpeakerID == x]),
  stringsAsFactors = FALSE)
parties6$en[is.na(parties6$en)] <- "Independent"
parties6$zh[is.na(parties6$zh)] <- "獨立"

name_list6 <- data.frame(en = sapply(colnames(corm6), function(x) 
  legco_member_affiliation[["5"]] %$% NameEng[SpeakerID == x]),
  zh = sapply(colnames(corm6), function(x) 
    legco_member_affiliation[["5"]] %$% NameChi[SpeakerID == x]))

# plot English
colours6 <- c("FF33FF", "0033FF", "6699FF", "FF0000", "00CC00", "99FFFF", "99FFCC", 
              "C0C0C0", "00FFFF", "FFCC00", "66FF00", "FFFFCC", "CC9900", "33CCCC", 
              "FF99FF", "CC3333", "FFFF00", "99FF00") %>% paste0("#", .)

png("6th_all_en.png", width = 1800, height = 1200, units = "px")
qgraph(corm6, layout = "spring", groups = parties6$en, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list6$en, border.width = .5, color = colours6,
       title = "Voting Behaviour of Legislators in the Sixth LegCo",
       title.cex = 3, legend = TRUE, legend.cex = .54, legend.mode = "style2")
dev.off()

# plot Chinese
colours6 <- c("33CCCC", "99FF00", "0033FF", "FF99FF", "00FFFF", "FFFFCC", "CC9900", 
              "FF0000", "00CC00", "CC3333", "6699FF", "C0C0C0", "FFCC00", "66FF00", 
              "FFFF00", "99FFCC", "99FFFF", "FF33FF") %>% paste0("#", .)

png("6th_all_zh.png", width = 1800, height = 1200, units = "px")
par(family = "Heiti TC Light")
qgraph(corm6, layout = "spring", groups = parties6$zh, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list6$zh, border.width = .5, color = colours6,
       title = "第六屆立法會會議議員投票行為",
       title.cex = 3, legend = TRUE, legend.cex = .54, legend.mode = "style2")
dev.off()

## pro-establishment
pro_est6 <- readRDS("pro_est6.rds")

corm6_pro_est <- corm6[rownames(corm6) %in% pro_est6$id,
                       colnames(corm6) %in% pro_est6$id]

parties6 <- data.frame(en = sapply(colnames(corm6_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% AffiliationEng[SpeakerID == x]),
  zh = sapply(colnames(corm6_pro_est), function(x) 
    legco_member_affiliation[["5"]] %$% AffiliationChi[SpeakerID == x]),
  stringsAsFactors = FALSE)
parties6$en[is.na(parties6$en)] <- "Independent"
parties6$zh[is.na(parties6$zh)] <- "獨立"

name_list6 <- data.frame(en = sapply(colnames(corm6_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% NameEng[SpeakerID == x]),
  zh = sapply(colnames(corm6_pro_est), function(x) 
    legco_member_affiliation[["5"]] %$% NameChi[SpeakerID == x]))

# plot English
colours6 <- c("FF33FF", "FF0000", "C0C0C0", "FFCC00", "FFFFCC", "CC9900", 
              "FF99FF", "CC3333", "FFFF00") %>% paste0("#", .)

png("6th_pro_est_en.png", width = 1800, height = 1200, units = "px")
qgraph(corm6_pro_est, layout = "spring", groups = parties6$en, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list6$en, border.width = .5, color = colours6,
       title = "Voting Behaviour of Pro-Establishment Legislators in the Sixth LegCo",
       title.cex = 2.8, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

# plot Chinese
colours6 <- c("FF99FF", "FFFFCC", "CC9900", "FF0000", "CC3333", "C0C0C0", 
              "FFCC00", "FFFF00", "FF33FF") %>% paste0("#", .)

png("6th_pro_est_zh.png", width = 1800, height = 1200, units = "px")
par(family = "Heiti TC Light")
qgraph(corm6_pro_est, layout = "spring", groups = parties6$zh, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list6$zh, border.width = .5, color = colours6,
       title = "第六屆立法會會議建制派議員投票行為",
       title.cex = 2.8, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

## non-pro-establishment
corm6_non_pro_est <- corm6[!rownames(corm6) %in% pro_est6$id,
                           !colnames(corm6) %in% pro_est6$id]

parties6 <- data.frame(en = sapply(colnames(corm6_non_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% AffiliationEng[SpeakerID == x]),
  zh = sapply(colnames(corm6_non_pro_est), function(x) 
    legco_member_affiliation[["5"]] %$% AffiliationChi[SpeakerID == x]),
  stringsAsFactors = FALSE)
parties6$en[is.na(parties6$en)] <- "Independent"
parties6$zh[is.na(parties6$zh)] <- "獨立"

name_list6 <- data.frame(en = sapply(colnames(corm6_non_pro_est), function(x) 
  legco_member_affiliation[["5"]] %$% NameEng[SpeakerID == x]),
  zh = sapply(colnames(corm6_non_pro_est), function(x) 
    legco_member_affiliation[["5"]] %$% NameChi[SpeakerID == x]))

# plot English
colours6 <- c("0033FF", "6699FF", "00CC00", "99FFFF", "99FFCC", "C0C0C0",
              "00FFFF", "66FF00", "33CCCC", "99FF00") %>% paste0("#", .)

png("6th_non_pro_est_en.png", width = 1800, height = 1200, units = "px")
qgraph(corm6_non_pro_est, layout = "spring", groups = parties6$en, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list6$en, border.width = .5, color = colours6,
       title = "Voting Behaviour of Non-Pro-Establishment Legislators in the Sixth LegCo",
       title.cex = 3, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()

# plot Chinese
colours6 <- c("33CCCC", "99FF00", "0033FF", "00FFFF", "00CC00", "6699FF",
              "C0C0C0", "66FF00", "99FFCC", "99FFFF") %>% paste0("#", .)

png("6th_non_pro_est_zh.png", width = 1800, height = 1200, units = "px")
par(family = "Heiti TC Light")
qgraph(corm6_non_pro_est, layout = "spring", groups = parties6$zh, 
       vsize = 2, esize = .2, labels = TRUE, label.cex = 2, layoutOffset = c(-.1, 0),
       nodeNames = name_list6$zh, border.width = .5, color = colours6,
       title = "第六屆立法會會議非建制派議員投票行為",
       title.cex = 3, legend = TRUE, legend.cex = .8, legend.mode = "style2")
dev.off()
