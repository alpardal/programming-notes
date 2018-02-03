
# Connascence: coupling between components

Connascence between A and B mean that either:

* changes in A require B to be changed (or at least carefully checked);
* there are changes that require A and B to be changed;

Exists in a continuum from explicit to implicit - the more implicit the harder
it is to detect.

* two components needn't to communicate to each other in order to be connascent;
* connascence can be directional (e.g. when A refers to B) or nondirectional
  (B does'nt refer to A, A doesn't refer to B);
* can be static (arising from the lexical structure of the code), or dynamic
  (based on execution pattern of running code).

## Varieties of static connascence

* of name: components must always have the same name to work;
* of type/class: one component must have the same type as other
  (e.g. two variables that refer to the same object);
* of meaning: one component depends on implicit meaning atributed another
  (e.g. -1 to mean 'not found' in searches)
* of algorithm: e.g. new items added to end of list in A, always use last item
  in B to get most recent item;
* of position: e.g. lines of code that must be run in sequence, order of
  function arguments;

## Varieties of dynamic connascence

* of execution: e.g. "always do x before doing y";
* of timing: e.g. "B must run at most 20ms after A";
* of value: e.g. "x must always be smaller than y", denormalized values that
  must be kept in sync, etc;
* of identity: e.g. variables that must point to same object, "if x points to y
  then u must point to v";

## Contranascence

"Connascence of difference": a form of connascence in which difference rather
than equality must be preserved.

e.g. two variables in the same scope must have different names.

## Connascence and maintainability

* minimize overall connascence/contranascence by breaking the system into
  encapsulated componentes;
* minimize any remaining connascence that crosses encapsulation boundaries;
* maximize the connascence within encapsulation boundaries.

"Keep like things together and unlike things apart", where 'things' are
components with mutual connascence.
