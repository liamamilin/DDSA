# stack

Astack <- setRefClass(
  Class = "Astack",
  fields = list(
    Maxsize = "integer",
    topPos = "integer",
    ArrayStack = "array"
  ),
  methods = list(
    # Initialization function
    initialize = function(defaultSize = 100L, ...)
    {
      topPos <<- 0L
      Maxsize <<- defaultSize # 100L
      ArrayStack <<- array(dim = Maxsize)
    },

    # Check if stack is empty
    isEmpty = function() {
    },

    # push value to stack
    push = function(pushval) {
    },

    # Pop value from stack
    pop = function() {
    },

    # Function to get size of stack
    stacksize = function() {
    },

    # Function to get top value of stack
    top = function() {
    }
  )
)


isEmpty = function() {
  if (topPos == 0) {
    cat("Empty Stack!")
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}


push = function(pushval) {
  if ((topPos + 1L) > Maxsize)
    stop("Stack is OUT OF MEMORY!")
  topPos <<- topPos + 1L
  ArrayStack[topPos] <<- pushval
}

pop = function() {
  isEmpty() # Check if stack is empty
  popval <- ArrayStack[topPos]
  ArrayStack[topPos] <<- NA
  topPos <<- topPos - 1L
  return(popval)
}

stacksize = function() {
  stackIsEmpty <- isEmpty()
  ifelse(stackIsEmpty, return(0), return(topPos))
}

top = function() {
  stackIsEmpty <- isEmpty()
  if (stackIsEmpty) {
    cat("Empty Stack")
  } else
  {
    return(ArrayStack[topPos])
  }
}


# Linked stacks

Linkstack <- setRefClass(
  Class = "Linkstack",
  fields = list(Lsize = "integer",
                Lstacktop = "environment"),
  methods = list(
    # Initialization function
    initialize = function(...) {
      Lsize <<- 0L
    },

    # Check if stack is empty
    isEmpty = function() {
    },

    # Function to create empty R environment
    create_emptyenv = function() {
    },

    # Function to create node
    Node = function(val, node = NULL) {
    },

    # push value to stack
    push = function(pushval) {
    },

    # Pop value from stack
    pop = function() {
    },

    # Function to get top value of stack
    top = function() {
    }
  )
)


isEmpty = function() {
  if (Lsize == 0) {
    cat("Empty Stack!")
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

create_emptyenv = function() {
  emptyenv()
}

Node = function(val, node = NULL) {
  llist <- new.env(parent = create_emptyenv())
  llist$element <- val
  llist$nextnode <- node
  llist
}

push = function(val) {
  stackIsEmpty <- isEmpty()
  if (stackIsEmpty) {
    Lstacktop <<- Node(val)
    Lsize <<- Lsize + 1L
  } else
  {
    Lstacktop <<- Node(val, Lstacktop)
    Lsize <<- Lsize + 1L
  }
}

pop = function() {
  stackIsEmpty <- isEmpty()
  if (stackIsEmpty) {
    cat("Empty Stack")
  } else
  {
    Lstacktop <<- Lstacktop$nextnode
    Lsize <<- Lsize - 1L
  }
}

topVal = function() {
  stackIsEmpty <- isEmpty()
  if (stackIsEmpty) {
    cat("Empty Stack")
  } else
  {
    return(Lstacktop$element)
  }
}



# Array-based queues

aqueue <- setRefClass(
  Class = "aqueue",
  fields = list(
    Alist = "array",
    queuesize = "integer",
    maxSize = "integer",
    rear = "integer",
    top = "integer"
  ),
  methods = list(
    initialize = function(qSize, ...) {
      queuesize <<- 0L
      rear <<- 1L
      top <<- 0L
      maxSize <<- as.integer(qSize)
      Alist <<- array(dim = maxSize)
    },
    # Queue is empty
    isEmpty = function() {
      return(queuesize == 0L)
    },
    # Add element to the queue
    enqueue = function(val) {
      if (queuesize < maxSize) {
        if (top == maxSize)
          top <<- 0L
        top <<- top + 1L
        Alist[top] <<- val
        queuesize <<- queuesize + 1L
      } else{
        cat("Queue Full!")
      }
    },

    # remove element from queue
    dequeue = function() {
      if (queuesize > 0L) {
        Alist[rear] <<- NA
        ifelse(rear == maxSize, rear <<-
                 1L, rear <<- rear + 1L)
        queuesize <<- queuesize - 1L
      } else{
        cat("Empty Queue!")
      }
    },

    # size of queue
    size = function() {
      Lsize
    }
  )
)

# Linked queues

ListQueue <- setRefClass(
  Class = "ListQueue",
  fields = list(
    Lsize = "integer",
    front = "environment",
    rear = "environment",
    Lqueue = "environment"
  ),

  methods = list(
    initialize = function(...) {
      Lsize <<- 0L
    },

    # Check if list is empty
    isEmpty = function() {
      if (Lsize == 0) {
        cat("Empty Stack!")
        return(TRUE)
      } else
      {
        return(FALSE)
      }
    },

    # create empty environment
    create_emptyenv = function() {
      emptyenv()
    } ,

    # Create node
    Node = function(val, node = NULL) {
      llist <- new.env(parent = create_emptyenv())
      llist$element <- val
      llist$nextnode <- node
      llist
    },

    # Function to add value to link list
    enqueue = function(val) {
      ListIsEmpty <- isEmpty()
      if (ListIsEmpty) {
        Lqueue <<- Node(val)
        Lsize <<- Lsize + 1L
        rear <<- Lqueue
      } else
      {
        newNode <- Node(val)
        assign("nextnode", newNode, envir = rear)
        rear <<- newNode
        Lsize <<- Lsize + 1L
      }
    },

    # Function to remove node from link list
    dequeue = function() {
      stackIsEmpty <- isEmpty()
      if (stackIsEmpty) {
        cat("Empty Queue")
      } else
      {
        Lqueue <<- Lqueue$nextnode
        Lsize <<- Lsize - 1L
      }
    },

    # Function to get link list size
    size = function() {
      Lsize
    }
  )
)

# dictionaries

Adict <- setRefClass(Class = "Adict",
                     fields = list(
                       Alist = "list",
                       listsize = "integer",
                       key = "integer"
                     ),
                     methods = list(
                       # Re-initialize dictionary
                       initialize = function(...) {
                         listsize <<- 0L
                         Alist <<- list()
                       },

                       # Check length of value
                       size = function() {
                         return(listsize)
                       },

                       # Add following key value pair in Array
                       addElement = function(key, val) {
                         Alist[[key]] <<- val
                         listsize <<- listsize + 1L
                       },

                       # remove value with defined
                       removeElement = function(key) {
                         Alist[[key]] <<- NULL
                         listsize <<- listsize - 1L
                       },

                       # remove value with following
                       findElement = function(key) {
                         return(key %in% names(Alist))
                       }
                     )
)


