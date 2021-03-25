# SOS 

V1.0 Instruction:

1. change directory to SOS

2. invoke docker with 
````
    ./docker_connect.sh
````

3. inside docker, compile the SOS language with
````
    make
````

4. run helloworld test with
````
    make test
````

should see
````
    ...
    helloworld...OK
````
5. clean the file with
````
    make clean
````