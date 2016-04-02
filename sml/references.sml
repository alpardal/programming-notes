
val x = ref "before"

val before_mutation = !x

val _ = x := "after"

val after_mutation = !x
