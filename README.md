# CDL Parser
This tool was built as part of my research ([Kivikangas, Petri; Ishizuka, Mitsuru; "Improving Semantic Queries by Utilizing UNL Ontology and a Graph Database", Proc. IEEE 6th Int'l Conf. on Semantic Computing (ICSC 2012), Palermo, Italy, pp.83-86, Sep. 2012](http://www.miv.t.u-tokyo.ac.jp/papers/petri-ICSC2012.pdf)). You can use the tool as you like (see [LICENSE.txt](https://github.com/Valafar/cdl-parser/blob/master/LICENSE.txt)), but I don't take any responsibilities (see [DISCLAIMER.txt](https://github.com/Valafar/cdl-parser/blob/master/DISCLAIMER.txt)).

# Concept Description Language (CDL)
Concept Description Language, or CDL, is a declarative formal language for representing semantic data. It is a machine-understandable language aiming at becoming the next generation language for the intelligent Web. For more information about CDL and its ecosystem, see ["Common Web Language - W3C Incubator Group Report 31 March 2008"](http://www.w3.org/2005/Incubator/cwl/XGR-cwl/).

# Installation
For using the toolkit, you need following:
- [sbt](http://www.scala-sbt.org/)
- [Java 1.5+](http://www.oracle.com/technetwork/java/javase/downloads/index.html)

When running sbt for the first time, it will download the dependencies, including Scala. By typing 'run' in sbt console, the main editor should launch.

# Usage
The main accessor to the parser is `cdl.parser.CDLParser.parseDocument(dataSource: Any): CDLDocument`. Currently supported data source types are `java.lang.CharSequence` and `java.io.File`. `CDLDocument` is a container representing a single CDL document, and containing all the parsed entities.

The parser has been tested on Windows 7 (x64) and Linux Mint 13 Maya (x64).