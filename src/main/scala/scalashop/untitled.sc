val numtasks = 32
val w = 720
val ntm1 = numtasks max 1
val r = if numtasks <= 1 then w else w / numtasks max 1
val listOfFrom = (0 to w by (r)).toList
val listOfEnd = listOfFrom map (n => if n + (r - 1) > w then w else n + (r - 1))
val listOfRanges = listOfFrom zip listOfEnd
