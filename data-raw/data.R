require(yaml)

# Default Bootstrap branding
BRAND <- read_yaml("./data-raw/_brand.yml", eval.expr=TRUE)

usethis::use_data(BRAND, overwrite=TRUE)

