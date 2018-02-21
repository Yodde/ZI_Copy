#neural.function.activation.function = function(vec1, w1, vec2, w2) {
#z <- vec1 * w1 + vec2 * w2
#}
#learn.network <- function(input, output, beta, learning.rate) {

#}

#learn.network.step.2
#function(input, output, beta, learning.rate, repeated, values.to.take = 2) {
#range.min <- repeated * values.to.take - (values.to.take - 1)
#range.max <- repeated * values.to.take
#invec <- input[range.min:range.max]
#outvec <- output[range.min:range.max]
#estimated <- neural.activation.function.simple(invec, beta)
#beta - (sum(estimated - outvec) / values.to.take * sum(invec) / values.to.take) * learning.rate
#}

input.vector <- c(0.5, 3, 5, 0.1, 6)
output.vector <- input.vector * 3 + 7
neural.beta1 <- 1
neural.beta2 <- 1
learning.rate <- 0.01

estimate.y <- function(x, b1, b2) {
    b1 * x + b2
}

learn.function.two.parameters <- function(input, output, b1, b2, lr) {
    for (i in 1:length(input)) {
        y.est = estimate.y(input[i], b1, b2)
        #print(paste("estimated y = ",y.est))
        dfbd1 <- input[i] * y.est - input[i] * output[i]
        dfdb2 <- y.est - output[i]
        b1 <- b1 - lr * dfbd1
        b2 <- b2 - lr * dfdb2
        #print(paste(neural.beta1, ";", neural.beta2))
    }
    list(b1 = b1, b2 = b2)
}

evaluate.betas <- function() {
    for (i in 1:1000) {
        values <- learn.function.two.parameters(input.vector, output.vector, neural.beta1, neural.beta2, learning.rate)
        neural.beta1 = values$b1
        neural.beta2 = values$b2
    }
    print(paste("Beta1: ", neural.beta1, " Beta2: ", neural.beta2))
}





#leksykon
label.vector <- read.table("paragram.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
lexicon <- read.table("lexicon.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE)
#label.vector[label.vector$V1 == "sonja", -1]
label.vector[label.vector$V1 == lexicon[1, "V1"], -1] * label.vector[label.vector$V1 == lexicon[1, "V2"], -1]

choosen.vector1 <- label.vector[label.vector$V1 == lexicon[1, "V1"], -1]
choosen.vector2 <- label.vector[label.vector$V1 == lexicon[1, "V2"], -1]

theta <- acos(sum(choosen.vector1 * choosen.vector2) /
    (sqrt(sum(choosen.vector1 * choosen.vector1)) * sqrt(sum(choosen.vector2 * choosen.vector2))))

acos(pmin(pmax(sum(choosen.vector1 * choosen.vector1) /
(sqrt(sum(choosen.vector1 * choosen.vector1)) * sqrt(sum(choosen.vector1 * choosen.vector1))), -1.0), 1.0))



# PMI
library(readr)
library(XML)
wacky <- xmlParseDoc("test.txt")
wacky.tree <- xmlTreeParse("test.txt")
wacky.root <- xmlRoot(wacky.tree)
wacky.sentences <- getNodeSet(wacky.root, '//s', namespaces = xmlNamespaceDefinitions(wacky.root, simplify = T))


library(Matrix)
# Nie nalezy do zbioru
'%ni%' <- Negate('%in%')
# Wczytanie danych
wacky.table <- read.table("test.txt", sep = '\t', quote = "", col.names = c('word', 'default', 'type', 'v1', 'v2', 'v3'))

# Wczytanie stop words, brak stop wordsow, frazy
stop.words <- c('') #read.table("stopwords.txt", sep = "\n", quote = "")
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

# Filtracja 
for (dict in selected.names) {
    coocurence <- table(droplevels(
        selected$default[as.integer(
                            rownames(selected[selected$default == dict,])
                            ) + 1]))
    #[T,T,T,T,F,F(.)]
    for (close in names(coocurence)) {
        if (close != '.')
            selected.coocur[dict, close] <- selected.coocur[dict, close] + coocurence[close]
    }
}

# coocurence <- table(droplevels(selected$default[as.integer(rownames(selected[selected$default == selected.names[209],])) + 1]))

for (dict in selected.names) {
    if (dict != '.') {
        coocurence <- table(droplevels(
                        selected$default[as.integer(
                            rownames(selected[selected$default == dict,])
                        ) + 1]))
        for (close in names(coocurence)) {
            if (close != '.') {
                selected.coocur[dict, close] <- selected.coocur[dict, close] + coocurence[close]
            }
        }
    }
}

for (dict in selected.names) {
    if (dict != '.') {
        coocurence <- table(droplevels(
                        selected$default[as.integer(
                            rownames(selected[selected$default == dict,])
                        ) + 1]))
        for (close in names(coocurence)) {
            if (close != '.') {
                selected.coocur[dict, close] <- selected.coocur[dict, close] + coocurence[close]
            }
        }
    }
}



ppmi <- sparseMatrix(dimnames = list(
                    rownames(selected.unique),
                    rownames(selected.unique)),
                x = 1)


p <- function(i, n) {
    i / n
}

n <- nrow(selected[selected$default != '.',])
for (x in selected.names) {
    if (x != '.') {
        coocurence <- selected.coocur[x,selected.coocur[x,] > 0]
        for (y in names(coocurence)) {
            if (y != '.') {
                ppmi[x, y] <- 2 ^ (log10(p(coocurence[y], n) / (p(selected.unique[x], n) * p(selected.unique[y], n)))
                                  -(-log10(p(coocurence[y], n))))
            }
        }
    }
}
write.table(selected.unique, 'dict', quote = F, sep = ' ', col.names = F)