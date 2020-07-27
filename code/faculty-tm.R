##### Load Packages
# In normal life, just do:
# library(tidyverse)
# library(tidytext)
# library(tm)
# library(SnowballC)
# library(Matrix)
# library(reshape2)

# For teaching purposes:
# Install a library iff it is not yet installed, then load it
library.or.install <- function(package) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

library.or.install('tidyverse')     # Modern R programming pipeline
library.or.install('tidytext')      # Tidyverse-style text analysis
library.or.install('tm')            # General-purpose text-mining in R
library.or.install('SnowballC')     # Word stemming
library.or.install('Matrix')        # Need for sparse matrix manipulation
library.or.install('reshape2')      # Better data-frame reshaping than tidyverse


##### Download And Load Dataset

# Download this file
# download.file('https://raw.githubusercontent.com/balachia/insead-text-analysis/master/code/faculty-tm.R', './faculty-tm.R')

#df <- read_csv('../data/insead-faculty.csv')
download.file('https://raw.githubusercontent.com/balachia/insead-text-analysis/master/data/insead-faculty.csv', './insead-faculty.csv')
df <- read_csv('./insead-faculty.csv')

# Split text into word tokens
# tokens <- df[, c('id', 'area', 'bio')] %>%
#    unnest_tokens(word, bio)

# For this exercise, (1) first split text into sentences...
tokens.sentence <- df[, c('id', 'area', 'bio')] %>%
    unnest_tokens(sentence, bio, token='sentences') %>%
    mutate(sentence.no=row_number())

# ... then (2) split sentences into work tokens
tokens <- tokens.sentence %>%
    unnest_tokens(word, sentence)

# Top words
tokens %>%
    count(word, sort=TRUE)


##### Basic Cleaning

# Stop-words
data(stop_words)
tokens.stop <- tokens %>%
    anti_join(stop_words)
# tokens <- tokens.stop
# Top words
tokens.stop %>%
    count(word, sort=TRUE)
# Compare
cat('Original:\n\t', tokens[tokens$sentence.no==1,]$word, '\n')
cat('Stop-words:\n\t', tokens.stop[tokens.stop$sentence.no==1,]$word, '\n')

# Stemming
tokens.stem <- tokens %>%
    mutate(word=wordStem(word, language='porter'))
# tokens <- tokens %>% mutate(word=wordStem(word, language='porter'))
# Top words
tokens.stem %>%
    count(word, sort=TRUE)
# Compare
cat('Original:\n\t', tokens[tokens$sentence.no==1,]$word, '\n')
cat('Stemmed:\n\t', tokens.stem[tokens.stem$sentence.no==1,]$word, '\n')

# Both
tokens.stop.stem <- tokens.stop %>%
    mutate(word=wordStem(word, language='porter'))
# Top words
tokens.stop.stem %>%
    count(word, sort=TRUE)
# Compare
cat('Original:\n\t', tokens[tokens$sentence.no==1,]$word, '\n')
cat('Stop-words, stemmed:\n\t', tokens.stop.stem[tokens.stop.stem$sentence.no==1,]$word, '\n')

# Lemmatization
# Look at package: textstem


##### One-Hot Encoding

# Size of original dataset
df$bio %>%
    object.size() %>%
    format(units='auto') %>%
    cat('Original corpus size:', ., '\n')

# One-Hot Encoding
ohe.dtm <- tokens %>%
    mutate(row=row_number(), n=1) %>%
    cast_dtm(row, word, n)

# Size of sparse document-term matrix
ohe.dtm %>%
    object.size() %>%
    format(units='auto') %>%
    cat('Sparse OHE size:', ., '\n')

# Size of dense document-term matrix
ohe.dtm %>%
    as.matrix() %>%
    object.size() %>%
    format(units='auto') %>%
    cat('Dense OHE size:', ., '\n')


##### Bag-of-words

# Bag-of-words: document=faculty
bow <- tokens %>%
    group_by(id) %>%
    count(word)
bow.dtm <- bow %>%
    cast_dtm(id, word, n)

# Bag-of-words size
bow.dtm %>%
    object.size() %>%
    format(units='auto') %>%
    cat('Sparse BOW size:', ., '\n')
bow.dtm %>%
    as.matrix() %>%
    object.size() %>%
    format(units='auto') %>%
    cat('Dense BOW size:', ., '\n')

# Bag-of-words: document=sentence
bow.sentence <- tokens %>%
    group_by(sentence.no) %>%
    count(word)
bow.sentence.dtm <- bow.sentence %>% cast_dtm(sentence.no, word, n)


##### Similarity Queries
# Utility functions
# Convert DTM sparse matrix format into Matrix sparse matrix format
as.sparseMatrix.DTM <- function(s) Matrix::sparseMatrix(i=s$i, j=s$j, x=s$v, dims=dim(s), dimnames=dimnames(s))
# Convert similarities matrix into a standardized tibble/data.frame
similarities2tibble <- function(x) x %>%
    as.matrix() %>%
    melt(varnames=c('id1', 'id2')) %>%
    as.tibble()

# Jaccard Similarity
# |A & B| / (|A| + |B| - |A & B|)
jsim <- function(a, b) {
    sm.a <- as.sparseMatrix.DTM(a) > 0
    sm.b <- as.sparseMatrix.DTM(b) > 0
    size.a <- matrix(rowSums(sm.a), nrow=nrow(sm.a), ncol=nrow(sm.b))
    size.b <- matrix(rowSums(sm.b), nrow=nrow(sm.a), ncol=nrow(sm.b), byrow=TRUE)
    intersection <- sm.a %*% t(sm.b)
    intersection / (size.a + size.b - intersection)
}

# Cosine Similarity
# A . B / |A| * |B|
csim <- function(a, b) {
    sm.a <- as.sparseMatrix.DTM(a)
    sm.b <- as.sparseMatrix.DTM(b)
    norm.a <- sqrt(rowSums(sm.a*sm.a))
    norm.b <- sqrt(rowSums(sm.b*sm.b))
    (sm.a %*% t(sm.b)) / (norm.a %*% t(norm.b))
}

# Match sentence to its faculty member
s2p.match <- function(sentence.dtm, person.dtm, similarity.f=csim) {
    similarity.f(sentence.dtm, person.dtm) %>%
        similarities2tibble %>%
        group_by(id1) %>%
        filter(dense_rank(-value) == 1) %>%
        inner_join(tokens.sentence[, c('sentence.no', 'id')], by=c('id1'='sentence.no')) %>%
        rename(sentence.id=id, matched.id=id2) %>%
        mutate(matched=(matched.id==sentence.id))
}

# Match faculty members to each other
p2p.match <- function(dtm, similarity.f=csim) {
    similarity.f(dtm, dtm) %>%
        similarities2tibble %>%
        filter(id1 != id2) %>%
        group_by(id1) %>%
        filter(dense_rank(-value) == 1) %>%
        arrange(-value)
}

# Attempt to match using bag-of-words
# Match sentence to person
# First, using Jaccard similarity:
bow.s2p <- s2p.match(bow.sentence.dtm, bow.dtm, similarity.f=jsim)
# What % of A's sentences were matched to them? (Recall)
bow.s2p %>%
    group_by(sentence.id) %>%
    summarize(matched=mean(matched))
bow.s2p$matched %>% mean

# Second, using cosine similarity:
bow.s2p <- s2p.match(bow.sentence.dtm, bow.dtm)
bow.s2p %>%
    group_by(sentence.id) %>%
    summarize(matched=mean(matched))
bow.s2p$matched %>% mean

# Match person to person, Jaccard
bow.p2p <- p2p.match(bow.dtm, similarity.f=jsim)
print(bow.p2p)

# Match person to person, cosine
bow.p2p <- p2p.match(bow.dtm)
print(bow.p2p)


##### TF-IDF

# Manual calculation
tokens.tfidf.manual <- tokens %>%
    count(id, word, name='n')
tokens.tfidf.manual <- tokens.tfidf.manual %>%
    group_by(word) %>%
    mutate(df=n()) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(tf=n/sum(n), idf=log(n_groups(.)/df)) %>%
    mutate(tf_idf=tf * idf) %>%
    select(-df)

# tidytext built in method
tokens.tfidf <- tokens %>%
    count(id, word) %>%
    bind_tf_idf(word, id, n)

# Have we calculated it correctly?
all(tokens.tfidf$tf_idf == tokens.tfidf.manual$tf_idf)

# Create Document-Term Matrix
tfidf.dtm <- tokens.tfidf %>%
    cast_dtm(id, word, tf_idf)

# Sentence-wise TF-IDF
tfidf.sentence <- tokens %>%
    count(sentence.no, word) %>%
    bind_tf_idf(word, sentence.no, n)
tfidf.sentence.dtm <- tfidf.sentence %>% cast_dtm(sentence.no, word, n)


# Match using TF-IDF
# Match sentence to person
tfidf.s2p <- s2p.match(tfidf.sentence.dtm, tfidf.dtm)
tfidf.s2p %>%
    group_by(sentence.id) %>%
    summarize(matched=mean(matched))
tfidf.s2p$matched %>% mean

# Match person to person
tfidf.p2p <- p2p.match(tfidf.dtm)
print(tfidf.p2p)


##### N-grams

ngrams <- tokens.sentence %>%
    unnest_tokens(ngram, sentence, token='ngrams', n=2)

# Top bigrams
ngrams %>%
    count(ngram, sort=TRUE)
# Top bigrams, stop words removed
ngrams %>%
    separate(ngram, c('word1', 'word2'), sep=' ') %>%
    filter(! word1 %in% stop_words$word) %>%
    filter(! word2 %in% stop_words$word) %>%
    unite(ngram, word1, word2, sep=' ') %>%
    count(ngram, sort=TRUE)

# Unexpected bigrams, removing stop-words
word.prob <- tokens.stop %>%
    count(word) %>%
    mutate(tf=n/sum(n)) %>%
    select(word, tf)
ngrams.stop <- ngrams %>%
    separate(ngram, c('word1', 'word2'), sep=' ') %>%
    filter(! word1 %in% stop_words$word) %>%
    filter(! word2 %in% stop_words$word) %>%
    unite(ngram, word1, word2, sep=' ')
ngram.prob <- ngrams.stop %>%
    count(ngram) %>%
    mutate(tf=n/sum(n)) %>%
    separate(ngram, c('word1', 'word2'), sep=' ') %>%
    inner_join(rename(word.prob, word1=word, tf1=tf), by='word1') %>%
    inner_join(rename(word.prob, word2=word, tf2=tf), by='word2') %>%
    unite(ngram, word1, word2, sep=' ') %>%
    mutate(expect.tf=tf1*tf2, diff.tf=tf - (tf1*tf2))
# Most-unlikely ngrams
ngram.prob %>%
    arrange(desc(tf))

# Bag-of-words: document=faculty
bow.ngram <- ngrams %>%
    group_by(id) %>%
    count(ngram)
bow.ngram.dtm <- bow.ngram %>%
    cast_dtm(id, ngram, n)

# Bag-of-words: document=sentence
bow.ngram.sentence <- ngrams %>%
    group_by(sentence.no) %>%
    count(ngram)
bow.ngram.sentence.dtm <- bow.ngram.sentence %>% cast_dtm(sentence.no, ngram, n)

# Match using N-gram BOW
# Match sentence to person
bow.ngram.s2p <- s2p.match(bow.ngram.sentence.dtm, bow.ngram.dtm)
bow.ngram.s2p %>%
    group_by(sentence.id) %>%
    summarize(matched=mean(matched))
bow.ngram.s2p$matched %>% mean

# Match person to person
bow.ngram.p2p <- p2p.match(bow.ngram.dtm)
print(bow.ngram.p2p)

# TF-IDF


##### Skip-grams

skipgrams <- tokens.sentence %>%
    unnest_tokens(ngram, sentence, token='skip_ngrams', n=2, k=2)


##### Dense embeddings


