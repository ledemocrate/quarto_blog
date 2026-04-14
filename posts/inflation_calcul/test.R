
library(tidyverse)
library(pdftools)

path <- getwd()

liste_facture <- paste0(path,"/FACTURE/",list.files(paste0(path,"/FACTURE/")))
analyse_ticket_u <- function(pdf_file) {
  
  # --- Code ticket basé sur le nom du fichier ---
  file_name <- basename(pdf_file)
  
  # Extraction JJMMYYYY-HHMMSS
  m <- str_match(file_name, ".*_(\\d{8})-(\\d{6})\\.pdf$")
  if (!is.na(m[1])) {
    date_raw <- m[2]  # JJMMYYYY
    heure_raw <- m[3] # HHMMSS
    
    # Conversion JJMMYYYY -> YYYYMMDD
    date_fmt <- paste0(substr(date_raw, 5, 8), substr(date_raw, 3, 4), substr(date_raw, 1, 2))
    
    code_ticket <- paste0(date_fmt, "_", heure_raw)
  } else {
    code_ticket <- NA_character_
  }
  
  # Lecture PDF
  txt   <- pdf_text(pdf_file)
  full  <- paste(txt, collapse = "\n")
  lines <- unlist(strsplit(full, "\n"))
  
  # --- Infos magasin ---
  tva_magasin  <- str_extract(full, "TVA:\\s*FR[0-9A-Z]+") |>
    str_remove("TVA: ")
  code_magasin <- str_extract(full, "CODE MAGASIN\\s*:\\s*[0-9]+")  |>
    str_remove("CODE MAGASIN : ")
  
  # --- Date & heure ---
  date  <- str_extract(full, "\\b[0-3][0-9]/[0-1][0-9]/[0-9]{2}\\b")
  heure <- str_extract(full, "\\b[0-2][0-9]:[0-5][0-9]:[0-5][0-9]\\b")
  
  # --- Totaux ticket (TTC = sous_total + total_tva) ---
  sous_total_str <- str_extract(full, "SOUS-TOTAL\\s*-?[0-9]+,[0-9]{2}") |>
    str_extract("-?[0-9]+,[0-9]{2}")
  total_tva_str  <- str_extract(full, "TOTAL TVA\\s*-?[0-9]+,[0-9]{2}") |>
    str_extract("-?[0-9]+,[0-9]{2}")
  
  sous_total_num <- as.numeric(gsub(",", ".", sous_total_str))
  total_tva_num  <- as.numeric(gsub(",", ".", total_tva_str))
  total_ttc_num  <- sous_total_num + total_tva_num
  
  # --- Tableau TVA (code -> taux) ---
  pattern_tva <- "^(\\d+)\\s+([0-9]+,[0-9]{2})"
  tva_lines   <- str_subset(lines, pattern_tva)
  
  df_tva <- str_match(tva_lines, pattern_tva)
  df_tva <- data.frame(
    tva_code = as.numeric(df_tva[, 2]),
    tva_taux = as.numeric(gsub(",", ".", df_tva[, 3])) / 100,
    stringsAsFactors = FALSE
  )
  
  # --- Articles ---
  # Correction : prix négatifs, noms complets, etc.
  pattern_article <- "^(.+?)\\s+(-?[0-9]+,[0-9]{2}) €\\s+(-?[0-9]+)$"
  idx_articles    <- grep(pattern_article, lines)
  
  # Détection robuste des catégories
  is_categorie <- function(x) {
    x <- trimws(x)
    if (x == "") return(FALSE)
    if (grepl("€", x)) return(FALSE)
    if (grepl("-?[0-9]+,[0-9]{2}", x)) return(FALSE)
    if (grepl("-?\\d+ x", x)) return(FALSE)
    if (grepl("\\(", x)) return(FALSE)
    if (grepl("\\d{2,}", x)) return(FALSE)
    grepl("^[A-ZÀÂÄÇÉÈÊËÎÏÔÖÙÛÜ '&.-]+$", x)
  }
  
  
  get_categorie_raw <- function(idx) {
    j <- idx - 1
    while (j > 0 && trimws(lines[j]) == "") j <- j - 1
    if (j <= 0) return(NA_character_)
    cand <- trimws(lines[j])
    if (is_categorie(cand)) return(cand)
    return(NA_character_)
  }
  
  articles <- list()
  
  for (k in seq_along(idx_articles)) {
    
    i      <- idx_articles[k]
    ligne1 <- lines[i]
    ligne2 <- lines[i + 1]
    
    # Catégorie brute
    categorie <- get_categorie_raw(i)
    
    # Ligne 1 : nom + total TTC + code TVA
    m1        <- str_match(ligne1, pattern_article)
    nom       <- trimws(m1[2])
    total_ttc <- as.numeric(gsub(",", ".", m1[3]))
    tva_code  <- as.numeric(m1[4])
    
    # Ligne 2 : quantité + prix unitaire (gère négatifs)
    qty       <- as.numeric(str_extract(ligne2, "-?\\d+"))
    prix_unit <- str_extract(ligne2, "-?[0-9]+,[0-9]{2}")
    prix_unit <- as.numeric(gsub(",", ".", prix_unit))
    
    
    articles[[k]] <- data.frame(
      code_ticket  = code_ticket,
      categorie_raw = categorie,
      nom           = nom,
      quantite      = qty,
      prix_unitaire = prix_unit,
      total_ttc     = total_ttc,
      tva_code      = tva_code,
      stringsAsFactors = FALSE
    )
  }
  
  df_articles <- bind_rows(articles)
  
  # --- Propagation correcte des catégories ---
  last_cat <- NA_character_
  for (i in seq_len(nrow(df_articles))) {
    if (!is.na(df_articles$categorie_raw[i]) && df_articles$categorie_raw[i] != "") {
      last_cat <- df_articles$categorie_raw[i]
    }
    df_articles$categorie_raw[i] <- last_cat
  }
  df_articles <- df_articles |>
    rename(categorie = categorie_raw)
  
  # --- Résultat final ---
  list(
    ticket = list(
      tva          = tva_magasin,
      code_magasin = code_magasin,
      code_ticket  = code_ticket,
      date        = date,
      heure       = heure,
      sous_total  = sous_total_num,
      total_tva   = total_tva_num,
      total_ttc   = total_ttc_num
    ),
    articles  = df_articles %>%
      left_join(df_tva, by="tva_code") %>%
      mutate(ht = total_ttc / (1 + tva_taux))  %>%
      mutate(mt_tva = total_ttc - ht)
  )
}

result <- lapply(liste_facture, analyse_ticket_u)
facture <- bind_rows(result[[]]$facture)

## Bind.rows


ticket <- bind_rows(lapply(result, `[[`, "ticket"))
articles <- bind_rows(lapply(result, `[[`, "articles"))

lapply(result, `[[`, "ticket")
`[[`(result[1] "ticket")
