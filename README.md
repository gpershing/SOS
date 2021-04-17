# SOS

## V1.0 Instruction

1. change directory to SOS

2. (given image `sheronw1174/sos-env` exists) invoke docker with

```
    ./docker_connect.sh
```

pull/build image `sheronw1174/sos-env` with

```sh
    ./docker_image_fetching.sh pull # or
    ./docker_image_fetching.sh build
```

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
    docker run --rm -it -v `pwd`:/home/sos -w=/home/sos columbiasedwards/plt
```

3. copy file inside of a running docker image

```
    docker cp [image id]:[src_path_inside_docker] [desc_path]
```

4. compile a OSMesa file with GL & GLU libaries

```
    gcc -o [filename] [filename].c -I/usr/local/include/ -L/user/local/lib/ -lOSMesa -lGLU -lm
```
