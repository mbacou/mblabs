require(yaml)

# Default Bootstrap branding
BRAND <- read_yaml("./data-raw/brand.yml", eval.expr=TRUE)[
  c("meta", "color", "typography")]

usethis::use_data(BRAND, overwrite=TRUE)

