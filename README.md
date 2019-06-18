# haskell-todo


## Usage


### Info

```
stack exec -- haskell-todo-exe -p ./example.txt info
```

### Init

```
stack exec -- haskell-todo-exe -p ./example.txt init
```

### List

```
stack exec -- haskell-todo-exe -p ./example.txt list"
```

### Add

```
stack exec -- haskell-todo-exe -p ./example.txt add TITLE -d DESCRIPTION -p 2 -b "2010/08/21 12:00:00"
```

### Remove

```
stack exec -- haskell-todo-exe -p ./example.txt remove 0"
```

### Update

```
stack exec -- haskell-todo-exe -p example.txt update 0 --title Bier
```
