# NetLogo extension: games

This tool provides a convenient way to define normal-form game-theoretic situations in **NetLogo**. Optimal points and Nash equilibria are calculated and returned to NetLogo in a well-arranged form. The following software packages were used:

* [NetLogo 5.2](https://ccl.northwestern.edu/netlogo/)
* [Java 7](http://openjdk.java.net)
* [Scala 2.9.3](http://www.scala-lang.org)
* [typesafe/config 1.2.0](https://github.com/typesafehub/config)
+ [Gamut 1.0.1](http://gamut.stanford.edu)

The source code is found at [https://github.com/JZschache/NetLogo-Extensions](https://github.com/JZschache/NetLogo-Extensions)

Since ql is an extension of NetLogo, the latter must be installed first ([NetLogo 5.2.1](https://ccl.northwestern.edu/netlogo/5.2.1/)). 

The games-extension is installed by creating a directory named `games` in the `extensions` subdirectory of the `NetLogo` program. All files from 
`https://github.com/JZschache/NetLogo-games/tree/master/extensions/games` have to be downloaded and moved to the newly created directory `extensions/games`. For example:

    git clone https://github.com/JZschache/NetLogo-games.git
    mv NetLogo-games/extensions/games path-to-netlogo/extensions

If the games-extension is used in combination with the ql-extension, the jars `games.jar` and `gamut.jar` must be added to the variable `additional-jars` in the file 
`extensions/ql/application.conf`:

    netlogo {
      ...
      parallel {
        ...    
        # all additional jars that must be loaded by NetLogo
        additional-jars = ["extensions/games/games.jar",
                           "extensions/games/gamut.jar"]
        ...
      }
    }

After starting NetLogo, a sample model from `NetLogo-games/models` can be loaded, e.g. `NetLogo-games/models/two-pers-games.nlogo`.

For more information see [gamesextension.pdf](https://github.com/JZschache/NetLogo-games/blob/master/gamesextension.pdf)
