library(legcoplus)
library(magrittr)

## Fetch all divisions of Council meetings 
# 5th term
w <- search_division(committee_id = 2040, index = FALSE)

filibuster <- {} # remove filibuster amendment bills
filibuster %<>% c(., w %$% VoteTime[grepl("APPROPRIATION BILL 2013", MotionEn) & (MoverType == "Member")])
filibuster %<>% c(., w %$% VoteTime[grepl("APPROPRIATION BILL 2014", MotionEn) & (MoverType == "Member")])
filibuster %<>% c(., w %$% VoteTime[grepl("APPROPRIATION BILL 2015", MotionEn) & (MoverType == "Member")])
filibuster %<>% c(., w %$% VoteTime[grepl("APPROPRIATION BILL 2016", MotionEn) & (MoverType == "Member")])
w %<>% .[!.$VoteTime %in% unique(filibuster), ]

dates <- unique(w$VoteTime)
w %<>% .[order(.$VoteTime), ]
rownames(w) <- 1:nrow(w)
voting_db5 <- lapply(dates, function(x) w[w$VoteTime == x, ])

# 6th term
w <- search_division(committee_id = 2562, index = FALSE)

dates <- unique(w$VoteTime)
w <- w[order(w$VoteTime), ]
rownames(w) <- 1:nrow(w)
voting_db6 <- lapply(dates, function(x) w[w$VoteTime == x, ])

## Contruct voting record table
member_list <- search_member() # Fetch full member list to match name and id

# Function to create vote table
create_vote_table <- function(voting_db) {
  vote_table <- data.frame()
  for (i in 1:length(voting_db)) {
    for (j in 1:nrow(voting_db[[i]])) {
      id <- member_list$SpeakerID[member_list$NameChi == voting_db[[i]][j, 28]] %>%
        as.character
      vote_table[i, id] <- ifelse(voting_db[[i]][j, 31] == "Yes", 1, 
                                  ifelse(voting_db[[i]][j, 31] == "No", 2, -1))
    }
  }
  x <- apply(vote_table, 2, # Identify disqualified and by-elected members
                      function(x) any(is.na(x)))
  y <- apply(vote_table, 2, # Identify members with voting rate below 10%
                      function(x) sum(x == -1) > length(x) * .5)
  vote_table <- vote_table[, !(x | y)]
  return(vote_table)
}

vote_table5 <- create_vote_table(voting_db5)
vote_table6 <- create_vote_table(voting_db6)

saveRDS(vote_table5, file = "vote_table5.rds")
saveRDS(vote_table6, file = "vote_table6.rds")
