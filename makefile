start:
	docker run --rm -p 8787:8787 -v $(pwd)/src/:/home/rstudio/scripts rstudio 
help:
	@echo start - Will start the docker container