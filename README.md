# SOS

## V1.0 Instruction

1. change directory to SOS

2. invoke docker with

```
    ./docker_connect.sh
```

the above command would automatically pull image if the given image doesn't exist

or 

pull/build the lastest image `sheronw1174/sos-env` with

```sh
    ./docker_image_fetching.sh pull # or
    ./docker_image_fetching.sh build
```

<del>**it's like 7GB so both options take a long while!!!**</del>

3. inside docker, compile the SOS language with

```
    make
```

4. run helloworld test with

```
    make test
```

should see

```
    ...
    helloworld...OK
```

5. clean the file with

```
    make clean
```

## docker environment

1. compile Dockerfile (not necessary)

```
    docker docker build . -t [tagname]
```

2. run docker image (to be pushed)

```
    docker run --rm -it -v `pwd`:/home/sos -w=/home/sos sheronw1174/sos-env
```

3. copy file inside of a running docker image

```
    docker cp [image id]:[src_path_inside_docker] [desc_path]
```

4. pull docker image

```
    docker pull sheronw1174/sos-env
```

## MESA & OpenGL

1. compile code with GL and GLU libraries linked (we did not use GLU)

```
    gcc -o [filename] [filename].c -I/usr/local/include/ -L/user/local/lib/ -lOSMesa -lGLU -lm
```
