all:
	Rscript -e 'bookdown::render_book("")'

clean:
	rm -r lectures
	rm *.rds
	rm -r _bookdown_files