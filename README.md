Training application simulating the operation of elevators. Consist of two parts: server on scala, which calculates the location of the elevators and js client for visualization.

## How to launch 

I am using a simple static web server:

    $ npm install --global http-server
    $ http-server src/main/resources/

Next, the server side is presented in several variations, you can run one of these commands:

    $ sbt "runMain catz.Main monolite"
    $ sbt "runMain catz.Main separate"
    $ sbt "runMain actor.oop_style.Main"
    $ sbt "runMain actor.functional_style.Main"
