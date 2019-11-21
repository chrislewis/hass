# hass

Above all, `hass` is a Haskell novice's attempt to become less of a novice
while building something debatably practical. Intended to recreate some subset
of the [javap](https://docs.oracle.com/en/java/javase/11/tools/javap.html)
utility, `hass` is currently able to read a subset of informational structures
from the [constant pool](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4)
of a JVM class file, and then print them to the terminal.

Not all structures are supported yet, and the code is still very much in
cut-corners mode.

## Running

This project uses `[stack]`(https://www.haskellstack.org/), so you should be
able to run `hass` from the project root via `stack run <class file path>`, or
building the binary and using it directly. Currently it expects a single
argument, which is the path of a JVM class file.

## Working Example

As stated above, not all constant strucutres are supported yet, and if a class
contains unsupported constants, the program will fail. For a working example,
consider the simplest Java class:

```java
public class Test {
        public static void main(String[] args) {
                System.out.println("Hello");
        }
}
```

If you were to run `javap -verbose` on the class file resulting from compiling
that code, you would see this constant table:

```
Constant pool:
   #1 = Methodref          #6.#15         // java/lang/Object."<init>":()V
   #2 = Fieldref           #16.#17        // java/lang/System.out:Ljava/io/PrintStream;
   #3 = String             #18            // Hello
   #4 = Methodref          #19.#20        // java/io/PrintStream.println:(Ljava/lang/String;)V
   #5 = Class              #21            // Test
   #6 = Class              #22            // java/lang/Object
   #7 = Utf8               <init>
   #8 = Utf8               ()V
   #9 = Utf8               Code
  #10 = Utf8               LineNumberTable
  #11 = Utf8               main
  #12 = Utf8               ([Ljava/lang/String;)V
  #13 = Utf8               SourceFile
  #14 = Utf8               Test.java
  #15 = NameAndType        #7:#8          // "<init>":()V
  #16 = Class              #23            // java/lang/System
  #17 = NameAndType        #24:#25        // out:Ljava/io/PrintStream;
  #18 = Utf8               Hello
  #19 = Class              #26            // java/io/PrintStream
  #20 = NameAndType        #27:#28        // println:(Ljava/lang/String;)V
  #21 = Utf8               Test
  #22 = Utf8               java/lang/Object
  #23 = Utf8               java/lang/System
  #24 = Utf8               out
  #25 = Utf8               Ljava/io/PrintStream;
  #26 = Utf8               java/io/PrintStream
  #27 = Utf8               println
  #28 = Utf8               (Ljava/lang/String;)V
```

Currently, `hass` will print the following for the same class file:

```
MethodRefInfo 6 15
FieldRefInfo 16 17
ClassInfo 18
MethodRefInfo 19 20
ClassInfo 21
ClassInfo 22
Utf8Info 6 [60,105,110,105,116,62]
Utf8Info 3 [40,41,86]
Utf8Info 4 [67,111,100,101]
Utf8Info 15 [76,105,110,101,78,117,109,98,101,114,84,97,98,108,101]
Utf8Info 4 [109,97,105,110]
Utf8Info 22 [40,91,76,106,97,118,97,47,108,97,110,103,47,83,116,114,105,110,103,59,41,86]
Utf8Info 10 [83,111,117,114,99,101,70,105,108,101]
Utf8Info 9 [84,101,115,116,46,106,97,118,97]
NameAndTypeInfo 7 8
ClassInfo 23
NameAndTypeInfo 24 25
Utf8Info 5 [72,101,108,108,111]
ClassInfo 26
NameAndTypeInfo 27 28
Utf8Info 4 [84,101,115,116]
Utf8Info 16 [106,97,118,97,47,108,97,110,103,47,79,98,106,101,99,116]
Utf8Info 16 [106,97,118,97,47,108,97,110,103,47,83,121,115,116,101,109]
Utf8Info 3 [111,117,116]
Utf8Info 21 [76,106,97,118,97,47,105,111,47,80,114,105,110,116,83,116,114,101,97,109,59]
Utf8Info 19 [106,97,118,97,47,105,111,47,80,114,105,110,116,83,116,114,101,97,109]
Utf8Info 7 [112,114,105,110,116,108,110]
Utf8Info 21 [40,76,106,97,118,97,47,108,97,110,103,47,83,116,114,105,110,103,59,41,86]
```

More to come...