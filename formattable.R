# library(formattable)

library(htmltools)
library(webshot)    

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

# df <- data.frame(
#   id = 1:10,
#   name = c("Bob", "Ashley", "James", "David", "Jenny", 
#            "Hans", "Leo", "John", "Emily", "Lee"), 
#   age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
#   grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
#   test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
#   test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
#   final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
#   registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
#   stringsAsFactors = FALSE)
# 
# 
# formattable(df, list(
#   age = color_tile("white", "orange"),
#   grade = formatter("span", style = x ~ ifelse(x == "A", 
#                                                style(color = "green", font.weight = "bold"), NA)),
#   area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
#   final_score = formatter("span",
#                           style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
#                           x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
#   registered = formatter("span",
#                          style = x ~ style(color = ifelse(x, "green", "red")),
#                          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
# ))
