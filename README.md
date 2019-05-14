# AML-GEN

## Command line usage

#### Installation

* [Download the latest release (.zip file)](https://repository-master.mulesoft.org/nexus/content/repositories/releases/com/github/amlorg/aml-gen_2.12/) 
* Unzip it
* Locate the bin folder
* Execute the amlgen script (.bat and .sh cmd file provided)

#### Script usage documentation

```
AML 1.0
Usage: amlgen [generate] [options] dialect output

Command: generate [options]
Generates dialect instances
  -s, --seed <value>       the seed used when generating files. Default is random
  dialect                  dialect is the path to the dialect
  output                   output is the path to the folder where the generated files will be dumped
  -i, --instances <value>  instances is the number of dialect instances you want to generate. Default is 1000
  -c, --cardinality <value>
                           cardinality is the maximum length of generated lists and maps. Default is 3
```

#### Example

The following example will generate 5 dialect instances files given the dialect.yaml and leave them in the output file. 
```
./amlgen generate "./dialect.yaml" "./output" -instances 5 -cardinality 2 -seed 7
```

## Library usage

#### Installation

###### Maven
```
<dependency>
    <groupId>com.github.amlorg</groupId>
    <artifactId>aml-gen_2.12</artifactId>
    <version>1.0.0</version>
</dependency>
```

###### Gradle
`compile group: 'com.github.amlorg', name: 'aml-gen_2.12', version: '1.0.0'`

###### SBT
`libraryDependencies += "com.github.amlorg" %% "aml-gen_2.12" % "1.0.0"`