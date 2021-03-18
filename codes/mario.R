library(magrittr)
library(flextable)
library(officer)

set_flextable_defaults(font.family = "Arial", font.size = 9, padding = 2, digits = 1)

mario_kart <- readxl::read_excel("../data/mario-kart.xlsx", skip = 1)
mario_kart$image <- file.path("../static/assets/img/mario/", paste0(mario_kart$image, ".png"))
mario_kart$star <- file.path("../static/assets/img/mario/", mario_kart$star)

var_stat <- setdiff(names(mario_kart), c("image", "star", "color", "Characters"))


ft <- flextable(mario_kart, col_keys = c("Characters", var_stat)) %>% 
  set_header_labels(Land_speed = "Land", `Anti-G_speed` = "Anti-G", 
                    Water_speed = "Water", Gliding_speed = "Gliding") %>% 
  add_header_row(values = c("Characters", "Speed", "Accel", "Weight", 
                            "Handling", "Traction", "M-turbo"), 
                 colwidths = c(1, 4, 1, 1, 
                               4, 1, 1)) %>% theme_box() %>% 
  merge_v(part = "header") %>% bold(part = "header") %>% 
  valign(valign = "center", part = "header") %>% 
  compose(j = "Characters", 
          value = as_paragraph(
            as_b(
              colorize(Characters, color = mario_kart$color)), 
            as_chunk(" "), 
            as_image(image, width = 0.2, height = 0.2)) 
  ) %>% 
  colformat_double(digits = 1) %>% 
  theme_zebra(odd_header = "#c7254e", even_header = "#c7254e",
              odd_body = "#fff5f5", even_body = "#f8f9fa") %>% 
  color(part = "header", color = "white") %>% 
  align(align = "right", part = "all")
ft

for(column in var_stat){
  i_selector <- as.formula(paste0("~`", column, "`>= max(`", column, "`, na.rm = TRUE)"))
  ft <- compose(ft, 
            i = i_selector, j = column, 
            value = as_paragraph(
              as_image(star, width = .15, height = .15), 
              as_chunk(" "), 
              as_chunk(.)), use_dot = TRUE
    )
}
ft <- ft %>% autofit() 
ft

