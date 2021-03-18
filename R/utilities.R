flextable::set_flextable_defaults(font.family = "Roboto", font.size = 11, 
                                  theme_fun = theme_vanilla, font.color = "#666666",
                                  na_str = "<na>")

do_ft <- function(x) {
  if( is.null(getOption("max-ft-n")) ) 
    n <- 10
  else n <- getOption("max-ft-n")
  nro <- nrow(x)
  x <- head(x, n = n)
  ft <- flextable(x)
  ft <- add_footer_lines(ft, values = sprintf("# lines : %.0f - # columns : %.0f", nro, ncol(x)))
  autofit(ft)
}

add_table_print <- function(){
  table_print = function(x, ...) {
    knitr::knit_print(do_ft(x))
  }
  # register the method
  registerS3method("knit_print", "data.frame", table_print)
  registerS3method("knit_print", "grouped_df", table_print)
  registerS3method("knit_print", "spec_tbl_df", table_print)
  registerS3method("knit_print", "tbl", table_print)

}

knitr_opt_set <- function(root, fig.height=6, fig.width=10){
  add_table_print()
  img_path <- file.path("static/img", root, "img-")
  if( dir.exists(img_path)) unlink(img_path, recursive = TRUE, force = TRUE)
  options(max.print="75")
  opts_chunk$set(width=75, echo = TRUE,
                 message=FALSE,
                 warning=FALSE,
                 fig.path = img_path, fig.height=fig.height, fig.width=fig.width,
                 dev.args=list(bg='transparent', pointsize=10))
}

theme_set(theme_minimal())

