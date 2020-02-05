# Downloads the table from online repository and resaves to use in the package
item_categories <- googlesheets4::read_sheet("1giZ9l69lV5Mc0weM1B66CeOoevuVOLiy_IrK6s5_MnQ", sheet="Main")
item_translations <- googlesheets4::read_sheet("1giZ9l69lV5Mc0weM1B66CeOoevuVOLiy_IrK6s5_MnQ", sheet="Translations")
usethis::use_data(item_categories, overwrite = TRUE)
usethis::use_data(item_translations, overwrite = TRUE)
