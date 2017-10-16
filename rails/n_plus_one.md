
# fonte: https://goiabada.blog/to-join-or-not-to-join-an-act-of-includes-f6728fcefea3

* `preload`, `eager_load` and `includes` are birds of a feather:
  they are all eager loading strategies.
* `joins` is different from the three amigos: uses inner join
  to filter queries without loading relations.
* `preload`: loads associated tables using always a separate query.
* `eager_load`: loads associated tables using always a left join.
* `includes`: Since Rails 4 it uses preloading unless explicitly told to use
  left joins with references.
* `references`: cannot be used without includes, but the inverse can happen
  and will call preload.
