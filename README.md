# PersistentGraph

A Persistent Graph representation in Scala.

The Graph is represented as a adjacency list. There is accompanying tests and some sample code. 

I have also implemented a [Krager Min-Cut](https://en.wikipedia.org/wiki/Karger's_algorithm) 
implementation to find the min cut.

## Future improvements
The algorithim can defenitely be made more efficient. Since it is a Persistent implementation it 
generates alot of objects. Future work would be to code the implementation/graph operations in a 
more efficient manner then return an immutable Graph object.
