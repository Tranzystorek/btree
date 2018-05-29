# B-tree

**Functional** implementation of B-tree structure in Scala. 

## General Info

Functional implementation of the B-tree structure in the Scala language. It is a project implemented as part of the JPS (Symbolic processing languages) course at the EiTI Faculty of Warsaw University of Technology. Project implemented in a two-person team.

### Requirements

* Scala

## Build Project

* [sbt](https://www.scala-sbt.org/) 

### Run

Using command ***sbt run*** in terminal, or running in IntelliJ IDEA.

## Main Loop

Running project starts main loop, which allow to use simple methods. It can show that b-tree really works. Those methods allow to use only integers, but b-tree can contain other comparable types.

##### Available methods:

* add [Int] - add value to BTree
* rm [Int] - remove value from BTree
* is [Int] - check if a value exists in BTree
* print - print a BST view of BTree
* help - print help prompt
* exit - exit program

## Tests

In src/test/scala directory are Unit Tests for implmented b-tree.


#### Samples of tests

```
When many values are inserted, BTree contains them.
```
```
When there are many values, and one is removed, then Btree no longer contains it.
```
```
When value is removed, BTree no longer contains it.
```

#### Running Tests:

Using command ***sbt test*** in terminal, or running IntelliJ IDEA.


## Scaladoc

Scaladoc in html version are stored in [docs/html](/docs/html/) directory


## How B-tree works

#### Example of b-tree

![btree_example](https://upload.wikimedia.org/wikipedia/commons/thumb/6/65/B-tree.svg/400px-B-tree.svg.png)

#### Insert Method

![Insert](http://staff.ustc.edu.cn/~csli/graduate/algorithms/book6/393_a.gif)

#### Delete Method

![Delete](http://www.euroinformatica.ro/documentation/programming/!!!Algorithms_CORMEN!!!/images/fig472_01_0.jpg)


## Authors

* **Marcin Puc** 
* **Mateusz Wasiak**

