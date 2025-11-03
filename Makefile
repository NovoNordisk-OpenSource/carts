PKG = $(shell cat DESCRIPTION | grep Package: | cut -d":" -f2 | tr -d '[:blank:]')
R = R --silent --no-save --no-echo
RCMD := $(R) CMD
CLIFF_CFG = .cliff.toml
CHANGELOG = NEWS.md

BUILD_DIR = build
make_build_dir = rm -Rf $(BUILD_DIR) && mkdir -p $(BUILD_DIR)
GETVER = $(shell cat DESCRIPTION | grep Version | cut -d":" -f2 | tr -d '[:blank:]')

default: check

# generate changelog for unreleased commits
cliff-unreleased:
	@git cliff --unreleased -c $(CLIFF_CFG)

cliff-unreleased-prepend:
	@git cliff --unreleased -c $(CLIFF_CFG) -p $(CHANGELOG)


cov: coverage
coverage:
	@echo 'covr::report(file="tests/coverage-report.html")' | $(R)
	@open tests/coverage-report.html

rcpp:
	@echo 'Rcpp::compileAttributes(".")' | $(R)

readme:
	@echo 'options(warnPartialMatchArgs = FALSE); rmarkdown::render("inst/README.Rmd",output_format="md_document")' | $(R)
	@cp inst/README.md README.md

roxygen:
	@echo 'devtools::document(".")' | $(R)

doc: documentation
documentation: roxygen rcpp readme

pkgdown:
	@echo 'pkgdown::build_site(override = list(destination = "docs", template = list(params = list(bootswatch = "s3"))))' | $(R)

.PHONY: build
build:
	@$(make_build_dir)
	@echo 'pkgbuild::build(path=".", dest_path="$(BUILD_DIR)", args="--compact-vignettes=qpdf --resave-data=best")' | $(R)

test: # tests installed package (as installed to .libPaths)
	@echo 'future::plan("multicore"); tinytest::test_package("$(PKG)")' | $(R)

test-loadall:
	@echo 'future::plan("multicore"); devtools::load_all("."); tinytest::test_all(".")' | $(R)

slowtest:
	@echo 'future::plan("multicore"); res <- tinytest::run_test_dir("inst/slowtest"); if (summary(res)["Total", "fails"] > 0) {print(res); quit(status = 2)}' | $(R)

clean:
	@find vignettes inst '(' -name "*.html" -o -name "*_cache" -o -name "*_files" ')' -exec rm -Rf {} +
	@rm -rf build $(PKG).Rcheck

lint:
	@echo 'lintr::lint_package(".", show_progress = TRUE)' | $(R)

rmd:
	@echo 'devtools::build_rmd(list.files("./vignettes", pattern="Rmd$$", full.names=TRUE))' | $(R)

check: lint
	@_R_CHECK_FORCE_SUGGESTS_=0 echo 'future::plan("multicore"); res <- rcmdcheck::rcmdcheck(".", build_args=c("--no-build-vignettes"), args=c("--ignore-vignettes"))' | $(R)

check-cran: build
	@$(R) CMD check $(BUILD_DIR)/$(PKG)_$(GETVER).tar.gz --timings --as-cran --no-multiarch --run-donttest

in: install
install:
	@echo 'devtools::install(".", upgrade = "never")' | $(R)

upgrade:
	@echo 'devtools::install(".", upgrade = "always")' | $(R)

install-deps:
	@echo 'devtools::install_deps(".", dependencies = TRUE)' | $(R)
