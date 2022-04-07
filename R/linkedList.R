# linked lists 链表

## 创建一个空环境
create_emptyenv <- function() {
  emptyenv()
}

# 判断列表是不是空的
isEmpty <- function(llist) {
  if (class(llist) != "linkList")
    warning("Not linkList class") # 判断是不是linear linked list
  identical(llist, create_emptyenv()) # identical 函数是用来检测两个对象是不是完全相等
}

# 创建链表节点
linkListNode <- function(val, node = NULL) {
  llist <-  new.env(parent = create_emptyenv())
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- "linkList"
  llist
}

# 例子
LList <- linkListNode(5, linkListNode(2, create_emptyenv()))

# 获取下一个节点
setNextNode <- function(llist) {
  llist$nextnode
}

# 获取元素
setNextElement <- function(llist) {
  llist$element
}

# 获取节点的大小，这里使用了递归的思想

sizeLinkList <- function(llist, size = 0) {
  if (isEmpty(llist))
  {
    return(size)
  } else
  {
    size <- size + 1L
    sizeLinkList(llist$nextnode, size)
  }
}

# 添加item

addElement <- function(new, llist)
{
  if (isEmpty(llist)) {
    llist <- linkedlist(new)
  } else
  {
    llist <- linkListNode(llist, new)
  }
  llist
}

# 删除item
delElement <- function(llist, pos = NULL) {
  if (is.null(pos))
    warning("Nothing to delete")
  listsize <- sizeLinkList(llist)
  if (pos > listsize)
    stop("Position greater than size of list")
  if (isEmpty(llist)) {
    warning("Empty List")
  } else if (pos == 1) {
    PreviousNode <- llist$nextnode
  } else
  {
    PreviousNode <- linkListNode(llist$element)
    for (i in 1:(listsize - 1)) {
      if (pos == (i + 1)) {
        PreviousNode$nextnode <- setNextNode(llist$nextnode)
      } else
      {
        PreviousNode$nextnode <- llist$nextnode
        llist <- llist$nextnode
      }
    }
  }
  return(PreviousNode)
}

# 查找某一个item
findItem <- function(llist,
                     item,
                     pos = 0,
                     itemFound = FALSE) {
  if (itemFound == TRUE)
  {
    return(itemFound)
  } else if (isEmpty(llist)) {
    return(FALSE)
  } else
  {
    pos <- pos + 1L
    if (llist$element == item)
      itemFound <- TRUE
    findItem(llist$nextnode, item, size, itemFound)
  }
}


# Doubly linked list 双向链表

dlinkListNode <- function(val,
                          prevnode = NULL,
                          node = NULL) {
  llist <- new.env(parent = create_emptyenv())
  llist$prevnode <- prevnode
  llist$element <- val # 上一个元素
  llist$nextnode <- node # 下一个元素
  class(llist) <- "dlinkList"
  llist
}


# Circular linked list 循环链表

cicularLinkList <- function(llist, val) {
  if (isEmpty(llist)) {
    llist <- linkListNode(val)
    head <- llist
  } else
  {
    llistNew <- linkListNode(val)
    llistNew$nextnode <- head
    llist <- linkListNode(llist, llistNew)
  }
  llist
}


# Array based list

ALinkList <- setRefClass(
  Class = "ALinkList",
  fields = list(
    Alist = "array",
    listsize = "integer",
    arraySize = "integer",
    maxSize = "integer"
  ),
  methods = list(
    initialize = function(...) {
      listsize <<- 0L
      arraySize <<- 100L
      Alist <<- array(dim = arraySize)
      maxSize <<- arraySize
    }
  )
)
listlen = function()
{
  return(listsize)
}

updateArrayList = function() {
  Alist <<- c(Alist, array(dim = arraySize))
  maxSize <<- maxSize + arraySize
}

addItem = function(item) {
  if (maxSize <= listsize) {
    updateArrayList()
  }
  listsize <<- listsize + 1L
  Alist[listsize] <- item
  return(listsize)
}

removeItem = function(i)
{
  Alist[i] <<- NULL
  listsize <<- listsize - 1L
}

searchItem = function(val) {
  pointer <- 1L
  while (pointer != listsize) {
    if (Alist[pointer] == val) {
      break
    }
    pointer <- pointer + 1L
  }
  return(pointer)
}


