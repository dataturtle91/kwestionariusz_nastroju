library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

plik <- "kwestionariusz_nastroju.csv"

# Lista emocji

emocje <- c("radość", 
            "spokój", 
            "zainteresowanie", 
            "smutek", 
            "niepokój", 
            "złość",
            "strach",
            "wstręt",
            "zmęczenie",
            "nuda",
            "energia",
            "napięcie",
            "motywacja")

# ===== Podstawowe funkcje ===

## Wypelnij kwestionariusz

wypelnij_kwestionariusz <- function() {
    cat("Kwestionariusz samoopisu nastroju -",as.character(Sys.Date()),"\n")    
    oceny <- numeric(length(emocje)) # tworzymy pusty wektor na odpowiedzi
  
    for (i in seq_along(emocje)) {
        repeat {
            wpis <- readline(paste0("Na ile dziś odczuwasz emocję: ", 
                                    emocje[i], " (1 - 10): \n "))
            
            if (wpis %in% as.character(1:10)) break
            cat("Podaj liczbę od 1 - 10 \n")
        }
    oceny[i] <- as.integer(wpis)
    }
    
    wpis_df <- data.frame(
        data = Sys.Date(),
        matrix(oceny, nrow = 1)
    )
    colnames(wpis_df)[-1] <- emocje
    
    if (file.exists(plik)) {
        write_csv(wpis_df, plik, append = TRUE)
    } else {
        write_csv(wpis_df, plik)
    }
    animacja_ladowania("Zapisywanie do bazy", czas = 2)
    cat("Zapisano dane dnia. \n")
}

## Animacja ładowania

animacja_ladowania <- function(tekst = "Przetwarzanie", czas = 3, krok = 0.3) {
    cat(tekst)
    flush.console() # natchmiast pokaz tekst w konsoli
    t0 <- Sys.time()
    while (as.numeric(Sys.time() - t0) < czas) {
        cat(".")
        flush.console()
        Sys.sleep(krok)
    }
    cat("\n")
}

## Analizuj nastroj

analizuj_nastroj <- function() {
    cat("\n")
    animacja_ladowania("Analiza w toku", czas = 3)
    cat("Funkcja w budowie\n\n")
}

## Uruchom menu

uruchom_menu <- function() {
    repeat{
        cat("\n=== Dziennik samooceny nastroju ===\n")
        cat("1. Wypełnij dziennik \n")
        cat("2. Pokaż analizę nastroju \n")
        cat("3. Wyjście \n")
        
        wybor <- readline("Wybierz opcje (1-3): ")
        
        if (wybor == "1") {
            wypelnij_kwestionariusz()
        } else if (wybor == "2") {
            analizuj_nastroj()
        } else if (wybor == "3") {
            animacja_ladowania("Kończenie...",2,0.3)
            cat("Do zobaczenia! \n ")
            break
        } else {
            cat("Nieprawidłowy wybór. Spróbuj ponownie.\n")
        }
        
        }
}

uruchom_menu()
  



