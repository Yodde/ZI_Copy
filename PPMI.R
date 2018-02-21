library(Matrix)
library(MASS)
# Nie nalezy do zbioru
'%ni%' <- Negate('%in%')
# Wczytanie danych
wacky.table <- read.table("test.txt", sep = '\t', quote = "", col.names = c('word', 'default', 'type', 'v1', 'v2', 'v3'))

# Wczytanie stop words, brak stop wordsow, frazy
stop.words <- read.table("stopwordsSmall.txt", sep = "\n", quote = "")
# Usuniecie stop wordsow z korpusu
selected <- wacky.table[wacky.table$default %ni% stop.words$V1,]
# Reset numeracji
rownames(selected) <- NULL
# Reset wartosci zawartych w strukturze
selected <- droplevels(selected)
# Liczba wystapien kazdego wyrazu
selected.unique <- (table(unlist(selected$default)))
# Utworzenie zbioru slow wystepujacych w slowniku
selected.names <- rownames(selected.unique)
# Utworzenie macierzy kookurancji
selected.coocur <- sparseMatrix(as.numeric(selected$default),
                                as.numeric(selected$default),
                                dimnames = list(
                                    rownames(selected.unique),
                                    rownames(selected.unique)),
                                x = 1)
# Wyzerowanie glownej przekatnej macierzy
#diag(selected.coocur) <- 0

#okno
wind <- 3

get.words <- function(n, words, rows.numbers, sentence.ended, decreased = FALSE) {
    #reset
    sentence.ended <- sentence.ended | T
    window <- c(1:n) * (-1) ^ decreased
    if ((rows.numbers < n) && decreased)
        window <- c(1:rows.numbers) * (-1)

    for (i in window) {
        coocurence <- (
            selected$default[rows.numbers + i])
        sentence.ended <- (coocurence != '.' & !is.na(coocurence)) & sentence.ended
        words <- c(as.character(words), as.character(droplevels(coocurence[sentence.ended])))
    }
    words
}

# Filtracja 
for (dict in selected.names) {
    words <- vector(mode = 'character')
    rows.numbers <- as.integer(rownames(selected[selected$default == dict,]))
    # wektor logiczny sprawdzajacy czy zadnie sie nie skonczylo ('.') lub jest nieokreslone NA
    sentence.ended <- vector(mode = 'logical', length = selected.unique[dict]) | T

    words <- get.words(wind, words, rows.numbers, sentence.ended, F)
    words <- get.words(wind, words, rows.numbers, sentence.ended, decreased = T)
    coocurence <- table(words)

    for (close in names(words)) {
        selected.coocur[dict, close] <- selected.coocur[dict, close] + coocurence[close]
    }
}

# coocurence <- table(droplevels(selected$default[as.integer(rownames(selected[selected$default == selected.names[209],])) + 1]))



ppmi <- sparseMatrix(as.numeric(selected$default),
                     as.numeric(selected$default),
                     dimnames = list(
                    rownames(selected.unique),
                    rownames(selected.unique)),
                symmetric = T,
                x = 1)


p <- function(i, n) {
    i / n
}

n <- nrow(selected[selected$default != '.',])
for (x in selected.names) {
    if (x != '.') {
        coocurence <- selected.coocur[x, selected.coocur[x,] > 0]
        for (y in names(coocurence)) {
            if (y != '.') {
                ppmi[x, y] <- 2 ^ (log10(p(coocurence[y], n) / (p(selected.unique[x], n) * p(selected.unique[y], n)))
                                  - (-log10(p(coocurence[y], n))))
            }
        }
    }
}
#write.table(selected.unique, 'dict', quote = F, sep = ' ', col.names = F)

