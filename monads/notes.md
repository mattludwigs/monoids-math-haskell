*Following notes are from "From Simple IO to Monad Transformers" by J Adrian Zimmer.*

Think of a Monad `m` as a workshop for objects with type `m a`

All workshops have the

`>>` function

`(>>) :: Monad m => m a -> m b -> m b`

Often called combinator

`(>>) :: IO a -> IO b -> IO b`


`getLine :: IO String`

`putStrLn :: String -> IO ()`

The type of String is "covered" up by the IO or the Monad so other
functions cannot see the String value in the monad. Thus `putStrLn`
does not know how to handle the monad.


The string that `getLine` gets is only available in the context of that
monad.


`(>>=) :: Monad m => m a -> (a -> m b) -> m b`

mon >> f

where `mon = m a`
and   `f = a -> m b`

`a` and `b` can be the same type or not

it is `f`'s job to make an object of type `m b`

```
main =
  getLine >>= putStrLn
```

`getLine   >>= putStrLn`
`IO String >>= String -> IO String`


`>>` vs `>>=`

`>>` ignores the argument given to its expression

`(>>) m a -> m b = m a \_ -> m b`


> All forms of `return` create a monadic object whose interior objects (if any) are of the same type as their parameters.



`say_hello >> be_nice == be_nice . say_hello`


`(f . g) . h = f . (g . h)`



`(mon1 >>= f) >>= g = mon1 >>= (f >>= g)`


`f` must be a monad creating function on the left of the `==` but a monadic object on the right side

`f == \x -> f x`

`(mon >>= f) >>= g == mon >>= (\x -> f x >>= g)`




## Do Block Rules


### **1** Sequencing: Within a do block and for monadic objects these are equal

```
f = monad1 >> monad2

f = do
  monad1
  monad2
```


### **2** Conversion between parameter passing varialbe binding

In a `do` block with a monadic object `m` and type compatible function `f` these are the same

```
fun = mon >>= f

fun = mon >>= (\x -> f x)

fun = do
  x <- mon
  f x
```


```
(-> r) >>= f = \n -> f ((-> r) n) n
(+1) >>= (+) = \n -> (+ ((+1) n) n)
fun = do
  n <- (+1)
  return (n + n)
```


### **3** Do is Mon

```
do mon

mon
```
