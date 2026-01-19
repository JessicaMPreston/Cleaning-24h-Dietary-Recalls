## ALL FUNCTIONS USED TO FORMAT THE 24H Diet recall documents

########################################################################
#' Separate Out Metadata into a Separate CSV
#' seperate food data and diet recall metadata
#' @param df
#'
#' @returns df_food and metadata(env. and CSV)

seperate_metadata <- function(df) {
  metadata <- df[, c(1:33, 165:ncol(df)), drop = FALSE]

  readr::write_csv(
    metadata,
    here::here("data", "24hdietrecalls_participantmetadata.csv")
  )

  df_food <- df[, c(1, 8, 9, 19:164), drop = FALSE]

  list(
    metadata = metadata,
    df_food  = df_food
  )
}

########################################################################
########################################################################
#' Clean DF Food
#'
#' @param df_food
#'
#' @returns individuals_dffood
#' @export CSV of individuals_dffood

clean_dffood <- function(df_food) {
  #  name from cols 1:3, then split, then drop cols 1:3
  date_clean <- gsub("\\D+", "", as.character(df_food[[3]]))
  date_clean[is.na(date_clean) | date_clean == ""] <- "0000"

  nm <- paste("id",
    gsub("\\D+", "", as.character(df_food[[1]])),
    gsub("\\D+", "", as.character(df_food[[2]])),
    date_clean,
    sep = "_"
  ) |>
    make.unique(sep = "_")

  individuals_dffood <- purrr::map(seq_len(nrow(df_food)), \(i) {
    df_i <- df_food[i, , drop = FALSE]
    df_i[, -c(1:3), drop = FALSE]
  })
  names(individuals_dffood) <- nm

  # relocate X9_k / X10_k before q_(10+k)
  individuals_dffood <- purrr::map(individuals_dffood, \(df) {
    purrr::reduce(
      1:7,
      .init = df,
      .f = \(d, k) dplyr::relocate(
        d,
        dplyr::any_of(c(paste0("X9_", k), paste0("X10_", k))),
        .before = paste0("q_", 10 + k)
      )
    )
  })

  # pivot each 1-row df into the long structure
  individuals_dffood <- purrr::map(individuals_dffood, \(df) {
    stopifnot(nrow(df) == 1)

    get1 <- function(d, col) {
      if (col %in% names(d)) d[[col]] else NA
    }
    meal_labels <- c(
      "q_11" = "Breakfast",
      "q_12" = "Morning snack",
      "q_13" = "Lunch",
      "q_14" = "Snack afternoon",
      "q_15" = "Supper",
      "q_16" = "Snack evening",
      "q_17" = "Night snack"
    )

    purrr::map_dfr(1:7, \(k) {
      q <- 10 + k
      q_name <- paste0("q_", q)

      meal_core <- tibble::tibble(
        sharedplate = get1(df, paste0("X9_", k)),
        n_shared = get1(df, paste0("X10_", k)),
        Meal = unname(meal_labels[q_name]),
        meal_accuracy = get1(df, paste0("q_", q)),
        time = get1(df, paste0("q_", q, "_1")),
        hunger = get1(df, paste0("q_", q, "_2")),
        fullness = get1(df, paste0("q_", q, "_3")),
        place_eaten = get1(df, paste0("q_", q, "a"))
      )

      dish_tbl <- tibble::tibble(
        dish = purrr::map_chr(1:5, \(i) as.character(get1(df, paste0("q_", q, "b", i)))),
        ingredient = purrr::map_chr(1:5, \(i) as.character(get1(df, paste0("q_", q, "c", i)))),
        preperation_method = purrr::map_chr(1:5, \(i) as.character(get1(df, paste0("q_", q, "d", i)))),
        servingsize = purrr::map_chr(1:5, \(i) as.character(get1(df, paste0("q_", q, "e", i))))
      ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            \(x) dplyr::na_if(trimws(x), "")
          )
        ) |>
        dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))

      if (nrow(dish_tbl) == 0) {
        # keep meal row even if no dishes recorded
        return(dplyr::bind_cols(
          meal_core,
          tibble::tibble(dish = NA, ingredient = NA, preperation_method = NA, servingsize = NA)
        ))
      }

      dplyr::bind_cols(
        meal_core[rep(1, nrow(dish_tbl)), , drop = FALSE],
        dish_tbl
      )
    })
  })
  # save each individual's df as a csv
  out_dir <- "data/individual 24h recalls"

  purrr::iwalk(
    individuals_dffood,
    \(df, nm) {
      utils::write.csv(
        df,
        file = file.path(out_dir, paste0(nm, ".csv")),
        row.names = FALSE,
        na = ""
      )
    }
  )

  individuals_dffood
}

########################################################################
########################################################################
#' Combine DFs Food
#'
#' @param individuals_dffood
#'
#' @returns h24recalls_combined
#' @export all_24hrecalls.csv

combined_dffood <- function(individuals_dffood) {
  # helper: parse "id_<AutoID>_<IdentificationNo>_<Date>" from list element name
#' Title
#'
#' @param nm
#'
#' @returns
#' @export
#'
#' @examples
  parse_id_fields <- function(nm) {
    parts <- strsplit(nm, "_", fixed = TRUE)[[1]]

    list(
      AutoID = parts[2],
      IdentificationNo = parts[3],
      Date = paste(parts[4:length(parts)], collapse = "_")
    )
  }

  # add ID columns to each df
  dfs_tagged <- purrr::imap(individuals_dffood, \(df, nm) {
    ids <- parse_id_fields(nm)

    dplyr::mutate(
      df,
      AutoID = ids$AutoID,
      IdentificationNo = ids$IdentificationNo,
      Date = ids$Date,
      .before = 1
    )
  })

  h24recalls_combined <- dplyr::bind_rows(dfs_tagged)

  # save combined df
  utils::write.csv(
    h24recalls_combined,
    file = file.path("data", "all_24hrecalls.csv"),
    row.names = FALSE,
    na = ""
  )

  h24recalls_combined
}
