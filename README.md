# Maze Generator

[![](https://img.shields.io/badge/Maven-3.6.1-red)](https://maven.apache.org)
[![](https://img.shields.io/badge/Jinterface-1.6.1-yellow)](https://mvnrepository.com/artifact/org.erlang.otp/jinterface)
[![](https://img.shields.io/badge/Project_Lombok-1.18.12-blue)](https://mvnrepository.com/artifact/org.projectlombok/lombok)
[![](https://img.shields.io/badge/JavaFX_Graphics-14.0.1-green)](https://mvnrepository.com/artifact/org.openjfx/javafx-graphics)

Small project showcasing usage of jinterface - library made for connecting with erlang application with java

## [Maze api](./maze_api)

Standalone erlang gen_server application with three functions:

- `createEmptyMaze(Height, Width)` - creates maze structure on server
- `getMaze()` - return maze structure
- `step()` - does a algorithm step and returns new maze

[maze_generator.erl](./maze_api/apps/maze_api/src/maze_generator.erl) is a main well documented file responsible for implementation of algorithm

### Build

To run a server run

```shell
cd maze_api
rebar3 shell --sname apiNode --setcookie erljava
```

## [Maze client](./maze_client)

Java application responsible for communicating with erlang server, parse, interprete data to POJO and showcase it using JavaFx library
