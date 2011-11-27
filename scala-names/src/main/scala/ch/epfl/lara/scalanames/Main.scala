package ch.epfl.lara.scalanames

object Main extends App {
  println("Main does nothing. Run as a compiler plugin for better results.")
}

/*
System.setProperty("wordnet.database.dir", "C:\WordNet-3.0\dict\");
The technique for setting the wordnet.database.dir property externally (outside your code) depends on how you're executing your application. If you're running your code from within an Integrated Development Environment (IDE) such as Eclipse you'll need to use that IDE's support for setting system properties. For example, Eclipse allows you to specify "VM Arguments" and you would need to include an entry like the following in the list of arguments to use when running your code:

-Dwordnet.database.dir=C:\WordNet-3.0\dict\

    You've downloaded the JAR file containing the JAWS executable code to a Windows machine and saved it as jaws-bin.jar in your C:\mywork\code directory.
    You installed WordNet to a directory named C:\WordNet-3.0\ which in turn would mean that the database files are located in C:\WordNet-3.0\dict\.

In this case, you could start a Java Virtual Machine from the command line like the one shown below, which assumes that you also have defined a class called MyApp that contains a main() method:

java -classpath .;C:\mywork\code\jaws-bin.jar -Dwordnet.database.dir=C:\WordNet-3.0\dict MyApp
*/
