packagename <- "expenditR"

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
# Set author names
my_desc$set_authors(c(
  person("Lorenz", "Kapsner", email = "lorenz.kapsner@gmail.com", role = c('cre', 'aut')))) #,
#  person("Name2", "Surname2", email = "mail@2", role = 'aut')))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.0.0.9001")
# The title of your package
my_desc$set(Title = "A shiny base app")
# The description of your package
my_desc$set(Description = "Organize expenditures with your roommates.")
# The description of your package
my_desc$set("Date/Publication" = paste(as.character(Sys.time()), "UTC"))
# The urls
my_desc$set("URL", "https://github.com/kapsner/expenditR")
my_desc$set("BugReports",
            "https://github.com/kapsner/expenditR/issues")
# License
my_desc$set("License", "GPL-3")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
usethis::use_gpl3_license(name="Lorenz Kapsner")


# add Imports and Depends
# Listing a package in either Depends or Imports ensures that it’s installed when needed
# Imports just loads the package, Depends attaches it
# Loading will load code, data and any DLLs; register S3 and S4 methods; and run the .onLoad() function.
##      After loading, the package is available in memory, but because it’s not in the search path,
##      you won’t be able to access its components without using ::.
##      Confusingly, :: will also load a package automatically if it isn’t already loaded.
##      It’s rare to load a package explicitly, but you can do so with requireNamespace() or loadNamespace().
# Attaching puts the package in the search path. You can’t attach a package without first loading it,
##      so both library() or require() load then attach the package.
##      You can see the currently attached packages with search().

# Depends

# Imports
usethis::use_package("data.table", type="Imports")
usethis::use_package("shiny", type="Imports")
usethis::use_package("shinydashboard", type="Imports")
usethis::use_package("shinyjs", type="Imports")
usethis::use_package("magrittr", type="Imports")
usethis::use_package("stats", type="Imports")
usethis::use_package("DT", type="Imports")
usethis::use_package("jsonlite", type="Imports")

# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("processx", type = "Suggests")


# buildignore and gitignore
usethis::use_build_ignore("docker")
usethis::use_build_ignore("inst/application/_settings/")
usethis::use_git_ignore("inst/application/_settings/")
usethis::use_git_ignore(".Rhistory")
usethis::use_git_ignore("*.Rproj")
usethis::use_git_ignore(".Rproj*")


# code coverage
#covr::package_coverage()
