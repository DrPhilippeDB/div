#' Create an overview of the data eventually categorised
#'
#' @param d data frame, the team data in the documented form with columns grade, gender, age, tenure_firm, tenure_grade, tenure_job
#' @param x character, the column to be used to split the data (apart from grade). This must be a categorical variable (use first div_add_median_label() for numeric variables). Set to NULL to only group per grade
#' @param y character, the column names of the dependent variables (e.g. c('salary', 'age', 'tenure_firm'))
#' @return knitr::kable object to be visualised in html or pdf
#' @export
#' @importFrom dplyr summarise
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling collapse_rows
#' @examples
#' d <- div_fake_team()
#' div_table(d)
div_table <- function(d, x = 'gender', y = c('salary', 'age')) {

  #TODO: allow y of length < 2

  if (length(y) < 2) stop("y in div::div_table() must at least be of length 2")

  # global bindings for CRAN:
  g <- t1 <- t2 <- t <- NULL
  grade <- n <- y1 <- ymax <- NULL


  for (k in 1:length(y)) {d[[paste0("y",k)]] <- d[[y[k]]]}
  colnames(d)[ncol(d)] <- "ymax"
  d$x <- d[[x]]

  if (is.null(x)) {
    g <- d %>%  group_by(x)
  } else {
    g <- d %>%  group_by(grade, x)
  }

  t1 <- g %>%  summarise(n())

  # TODO: te following does not work in older versions --> first select rows and then sumarise_all
  t2 <- g %>%  summarise(across(y1:ymax, ~ median(.x, na.rm = TRUE)))

  t <- cbind(t1, t2)
  t[, 4:5] <- NULL

  colnames(t) <- c("grade", x, "Nbr.", y)
  t %>%
  knitr::kable(digits = c(0,0,0, seq(from = 2, to = 2, length.out = length(y))),
               format.args = list(big.mark = ",", scientific = FALSE, digits = 3),
               caption = paste0("Per group: numbers and medians for ", paste0(y, collapse=", "))
               )  %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),
                position = "center", full_width = F) %>%
  kableExtra::collapse_rows(columns = 1)
}
