## vizLearn - a Shiny app container
This project was intended to Dockerize an updated version of a Shiny app used to learn basic `ggplot2` code for data visualization in R. The container was created using [rocker/shiny](https://hub.docker.com/r/rocker/shiny) as a base container.

The vizLearn project is based on the work previously done as part of my undergraduate thesis. The paper discussing data science education and R Shiny apps for data visualization can be found in [this repository](https://github.com/devaneyJE/tidyTouch_thesis).

### Using this Repository
This repo may see updates with configuration files to simplify the installation of this container. Currently, the simplest way to use this container is to view instructions on [DockerHub](https://hub.docker.com/r/devaneyje/vizlearn). To build the image from the Dockerfile in the repository, follow the guide below. 

#### Building Image from Repository
To access the necessary files, clone the repo to your local machine.
```
git clone https://github.com/devaneyJE/vizLearn-docker.git
```
Navigate to the main directory containing the Dockerfile and run the following command:
```
docker build -t vizlearn .
```
Once complete, the image will be listed under `docker images`. A temporary container can now be run with:
```
docker run --rm -p 3838:3838 vizlearn:latest
```
The app UI can be accessed by navigating to `localhost:3838` in your web browser.

#### Continual Use
If you would like to create a container to continually access, the previous `run` command can be adjusted as follows.
```
docker run -p 3838:3838 vizlearn:latest
```
This container will persist in your list of local containers, and its status is viewable with:
```
docker container list -a
```
To start this container, the container's name contained in the output of the above command should be used.
```
docker start <container_name>
```

