Modler
======
*generate model source code from xml definitions*
***

Modler is a Programm to generate class model source code from xml model definitions.
Any language with a basic class system is supported. Your can generate multiple languages with the same model definition.

There are different template projects available:

//todo add list of projects

## Usage

### run from source with [leiningen 2][lein]

Make sure you have installed [leiningen][lein_install] on your system.
Run modler like this:

    lein run -- [options] path/to/model-definition.xml

### run from modler.jar

You can eather build or download the modler.jar.
A complete jar with all dependencies can be found [here][modlerJar]: 

    java -jar path/to/modle.jar -- [options] path/to/model-definition.xml


### Options

use the option -h or --help when you run modler to see a help page with all available options

    usage: modler [options] [model PATH]

    Switches                     Default       Desc                                                                  
    --------                     -------       ----                                                                  
    -lang, --languages           ["*"]         comma separated    list of languages to generate example 'as3,java,objc' 
    -t, --template-path          ./templates/  Path to template files                                                
    -o, --output-path            ./generated/  The output path                                                       
    -v, --no-verbose, --verbose  false                                                                               
    -h, --no-help, --help        false         Show help


### Model XML

You define your class definitions with a simple xml structure. You can define classes and interfaces. You can set a lang attributes to all entities to generate only language specific classes, interfaces, methods, properties, … and so on. 

**example:**

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE model SYSTEM "http://www.modeler.tslarusso.de/generator.dtd">
    <model xmlns="http://www.modeler.tslarusso.de/modelerSchema"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://www.modeler.tslarusso.de/modeler.xsd">
    
    	<class type="SimpleClass" lang="java">
    		<property name="name" type="String"/>
    		<method returns="object" name="gimmeAllYouGot"/>
    	</class>

        <class type="OtherSimpleClass" lang="cpp">
    		<property name="name" type="String"/>
    		<method returns="object" name="gimmeAllYouGot"/>
    	</class>
    </model>

For a full modler xml description see: //todo add link to wiki here

### Generated Model

Modler transforms the xml definitions to clojure data. Modler applies the designated template to all entries after the conversion. The [mustache][mustache] template will be provided with a hashmap containing all the converted information

-- add example here

For a full description see: //todo add link to wiki here

### Class Templates
Templates are written in the logic less [mustache][mustache] style.
Each template is referenced by its template key and language.

**example:**

* __java class template__ java.class.mustache
* __java iface template__ java.iface.mustache

class and face are the default template keys for class and iface model definitions. You can set any template key for your class or iface definition.

**example:**
* __as3 decorator class template__ as3.decoratorClass.mustache

## run 'modler'

### run with lein2

Make sure you have installed [leiningen][lein] on your system.
Run modler like this:

    lein run -- path/to/model.xml

### create uberjar

Create a uberjar with leiningen:

    lein uberjar



## License

Copyright (C) 2012 Manfred Endres

Distributed under the Eclipse Public License, the same as Clojure.

[lein]: https://github.com/technomancy/leiningen "leiningen 2"
[lein_install]: https://github.com/technomancy/leiningen/wiki/Upgrading "leiningen installation"
[mustache_man]: http://mustache.github.com/mustache.5.html "mustache manual"
[mustache]: http://mustache.github.com "mustache -- Logic-less templates."
[modlerJar]: http://www.google.de