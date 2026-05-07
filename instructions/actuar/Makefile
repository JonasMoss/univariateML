### -*-Makefile-*- to build and check the R package actuar
##
## Copyright (C) 2023-2026 Vincent Goulet
##
## 'make build' builds the package.
##
## 'make build-no-vignettes' builds the package without vignettes.
##
## 'make check' checks the package.
##
## 'make check-no-vignettes' checks the package without vignettes.
##
## 'make check-as-cran' checks the package with the option '--as-cran'.
##
## 'make install' installs the package locally.
##
## 'make vignettes' compiles the vignettes.
##
## 'make clean' cleans up the vignettes directory.
##
## 'make check-url' checks the validity of all urls in vignettes.
##
## 'make all' is equivalent to 'make build check'
##
## 'make no-vignettes' is equivalent to 'make build-no-vignettes check-no-vignettes'
##
## Author: Vincent Goulet
##
## This file is part of actuar
## <https://gitlab.com/vigou3/actuar>.

## Package name
PKG = actuar

## Toolset
RCMD = R CMD
Rscript = Rscript
Rbuild = ${RCMD} build
Rcheck = ${RCMD} check
Rinstall = ${RCMD} install
RM = rm -rf

## List of source files
SOURCES = $(addprefix src/,DESCRIPTION NAMESPACE) \
	$(wildcard src/data/*.rda) \
	src/demo/00Index $(wildcard src/demo/*.R) \
	src/inst/CITATION $(wildcard src/inst/*.Rd) \
	$(wildcard src/inst/include/*.h) $(shell find src/inst/po -type f) \
	$(wildcard src/man/*.Rd) \
	$(wildcard src/po/*.po) $(wildcard src/po/*.pot) \
	$(wildcard src/R/*.R) \
	$(wildcard src/src/*.c) $(wildcard src/src/*.h) $(wildcard src/src/*.def) src/src/Makevars \
	$(wildcard src/tests/*.R) \
	${VIGSRC}
VIGSRC = $(wildcard src/vignettes/*.Rnw) $(wildcard src/vignettes/*.bib) \
	$(wildcard src/vignettes/share/*.tex) src/vignettes/Makefile
VIGNETTES = $(filter %.Rnw,${VIGSRC})

## Extract current version number from DESCRIPTION
VERSION=$(shell ${Rscript} -e 'cat(tools:::.read_description(file.path("src", "DESCRIPTION"))["Version"])')

## Set name of archive
ARCHIVE = ${PKG}_${VERSION}.tar.gz

all: build check

no-vignettes: build-no-vignettes check-no-vignettes

%.tex: %.Rnw
	cd src/vignettes && ${RCMD} Sweave $(notdir $<)

.PHONY: build
build ${ARCHIVE}: ${SOURCES}
	${MAKE} -C src/vignettes clean
ifneq ("$(wildcard src/vignettes/*.pdf)","")
	${RM} src/vignettes/*.pdf
endif
	${Rbuild} src

.PHONY: build-no-vignettes
build-no-vignettes: ${SOURCES}
	${Rbuild} --no-build-vignettes src

.PHONY: check
check: ${ARCHIVE}
	${Rcheck} $<

.PHONY: check-no-vignettes
check-no-vignettes: ${ARCHIVE}
	${Rcheck} --ignore-vignettes $<

.PHONY: check-as-cran
check-as-cran: ${ARCHIVE}
	${Rcheck} --as-cran $<

.PHONY: install
install: ${ARCHIVE}
	${Rinstall} $<

.PHONY: vignettes
vignettes: ${VIGNETTES:.Rnw=.tex} ${VIGSRC}
	${MAKE} -C src/vignettes

.PHONY: clean
clean:
	${MAKE} -C src/vignettes clean
	rm src/vignettes/*.pdf

.PHONY: check-url
check-url: $(filter %.Rnw %.bib,${VIGSRC})
	@printf "%s\n" "checking urls in the sources"
	@grep -E -o -h 'https?:\/\/[^./]+(?:\.[^ ./})"]+)+(?:\/[^ })"]*)?' $^ | \
	grep -v '/\\' | \
	sed -E 's/([()])/\\\1/g' | \
	sort | uniq | \
	while IFS=" " read url; \
	do \
	    printf "%s... " "$$url"; \
	    if curl --output /dev/null --silent --head --fail --max-time 5 "$$url"; then \
	        printf "%s\n" "ok"; \
	    else \
		printf "%s\n" "not found or server error"; \
	    fi; \
	done
