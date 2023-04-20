require ("jsonlite", quietly = TRUE)

if (!(require ("dplyr", quietly = TRUE)&& require ("tidyr", quietly = TRUE)) ) {
  stop("Sorry, i don't know how to transform my answers dataframe without dplyr")
}

dir <- paste(
  'https://raw.githubusercontent.com/gockelhahn/',
  'qual-o-mat-data/master/data/2021/deutschland/',
  sep = '')

# get lookup for answer ids
answer_ids <- read_json(paste0(dir, "answer.json"), simplifyVector = TRUE)

# quickly add a column for coding messages, might break in future
# TODO work out a more resilient solution to recoding answers
answer_ids$code <- c(1, -1, 0)

# get lookup data on party ids
party_ids <- read_json(paste0(dir, "party.json"), simplifyVector = TRUE)

# lookup for statement short names
statement_ids <- read_json(paste0(dir, "statement.json"), simplifyVector = TRUE)

# get party answers
party_answers <- read_json(paste0(dir, "opinion.json"), simplifyVector = TRUE)

# transform answer df with names and answers
party_answers$party <- party_ids$name[match(party_answers$party,
                                            party_ids$id)]
party_answers$answer <- answer_ids$code[match(party_answers$answer,
                                              answer_ids$id)]

party_answers$statement <- statement_ids$label[match(party_answers$statement,
                                              statement_ids$id)]

# transform df into matrix of rows corresponding to parties answers
answers <- party_answers %>% 
  select(party, statement, answer) %>% 
  pivot_wider(id_cols = party, names_from = statement, values_from = answer)

ans_mat <- as.matrix(answers[,-1])
row.names(ans_mat) <- answers$party
colnames(ans_mat) <- names(answers)[-1]

mds <- cmdscale(dist(ans_mat))

clst <- hclust(dist(ans_mat))

plot(mds, xlim = c(min(mds[,1]), max(mds[,1])) * 1.3,
     ylim = c(min(mds[,2]), max(mds[,2])) * 1.3)
text(mds, row.names(ans_mat), adj = c(0.5, 0))

plot(clst)

