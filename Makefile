%.md : %.Rmd
	Rscript --slave -e "rmarkdown::render('$<')"
