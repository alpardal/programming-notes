
# Law of Demeter

The Law of Demeter requires that a method `m` of an object `O` may only
invoke the methods of the following kinds of objects:

  * `O` itself;
  * `m`'s parameters;
  * any objects created/instantiated within `m`;
  * `O`'s direct component objects;
  * global variables, accessible by `O`, in the scope of `m`.

An analogy:

  "when one wants a dog to walk, one does not command the dog's legs
   to walk directly; instead one commands the dog which then commands
   its own legs."


# Class Cohesion

Class cohesion is the measure of interrelatedness of the methods in the
*external* interface of a class.

A class with low cohesion has a set of methods that don't belong together;
a class with high cohesion has a set of methods that all contribute to the
type abstraction implemented by the class.

## Cohesion Problems

Classes with

* mixed-instance cohesion:

  have some components (typically methods) that are undefined for some
  objects of the class, i.e. "partial function"-methods.

* mixed-domain cohesion:

  have component(s) that depend on classes of a different domain.
  e.g. floats with a `celsius-to-fahrenheit` method.

* mixed-role cohesion have component(s) that cause dependencies on an
  extrinsic class in the same domain. E.g. a `Person` class with a
  `number-of-dogs` field: `Dog` is extrinsic to `Person`.

  (class B is extrinsic to A if A can be fully defined with no notion of B)
