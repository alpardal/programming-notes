
# Grouping methods into classes

  * you will never know less than you know at the beginning of a project,
    if your application succeeds many of the decisions you make today will
    need to be changed later;
  * design is more the art of preserving changeability than it is the act
    of achieving perfection;

# Organizing code to allow for easy changes

Code is easy to change if:

  * changes have no unexpected side effects;
  * small changes in requirements require correspondingly small changes in code;
  * existing code is easy to reuse;
  * the easiest way to make a change is to add code that in itself is easy to change;

Then, code should be *TRUE*:

  * (T)ransparent: consequences of change should be obvious in the code that is changing
    and in distant code that relies upon it;
  * (R)easonable: the cost of any change should be proportional to the benefits the change
    achieves;
  * (U)sable: existing code should be usable in new and unexpected contexts;
  * (E)xemplary: the code itself should encourage those who change it to perpetuate these
    qualities.

The first step in creating code that is TRUE is to ensure that each class has a single,
well-defined responsibility.

# Creating classes that have a single responsibility

A class should do the smallest possible useful thing.

# Why single responsibility matters

  * applications that are easy to change consist of classes that are easy to reuse;
  * like a box of building blocks: you can select just the pieces you need and assemble
    them in unanticipated ways;

Classes with multiple responsibilities:

  * make it impossible to reuse some (but not all) of its behaviors - it is impossible
    to use only the parts you need;
  * have many reasons to change;
  * may change for a reason that is unrelated to your use of it;
  * risk breaking every class that depend on it when it changes;

You increase your application's chance of breaking unexpectedly if you depend on
classes that do too much.

# Determining if a class has a single responsibility

  * pretend that it's sentient and ask it:
      - "Please Mr. Gear, what is your *ratio*?" - ok
      - "Please Mr. Gear, what are your *gear_inches*?" - shaky ground
      - "Please Mr. Gear, what is your *tire_size*?" - doesn't make sense.
  * attempt to describe it in a single sentence:
      - if the description uses "and", likely more than one responsibility;
      - if it uses "or" probably more than one -and- unrelated responsibilities;

# Determining when to make design decisions

  * don't feel compelled to make design decisions prematurely;
  * when faced with an imperfect and muddled class, ask yourself: "what is the
    future cost of doing nothing?";
  * the most cost-effective course of action may be to wait for more information;
  * make the decision only when you must with the information you have at that time;
  * is there a change that s.o. will reuse the class, or create new code that
    follows its pattern?
  * when code lie you must be alert to programmers believing and then propagating
    that lie;
  * applications are never perfect - a good designer understands the tension and
    minimizes costs by making informed tradeoffs between the needs of the present
    and the possibilities of the future.

# Writing code that embraces change

Because change is inevitable, coding in a changeable style has big future payoffs.
Techniques to create code that embrace changes:

## Depend on behavior, not data

Behavior is captured in methods and invoked by sending messages. Every tiny bit
of behavior (should) live in one and only one place.

### Hide instance variables:

  ```ruby
    attr_reader :cog
  ```

  * the cog method is now the only place that understands what _cog_ means. _Cog_
    becomes the result of a message send - data (which is referenced all over)
    changes to behavior (which is defined once);
  * because it's possible to wrap every instance var in a method, the distinction
    between data and a regular object can begin to disappear. While it's sometimes
    expedient to think of parts of your application as behavior-less data, most
    things are better thought of as plain old objects;
  * regardless (...) you should [still] hide data from yourself to protect the
    code from unexpected changes - data very often has behavior that you don't
    yet know about, send messages to access variables, even if you think of them
    as data.

### Hide data structures:

  * if being attached to an instance variable is bad, depending on a complicated
    data structure is worse;
  * just hiding the ivar is not enough - to do anything useful each sender will
    have to have complete knowledge of the structure of the data held by the ivar;
  * if this structure changes, the code will have to change. References will be leaky:
    they escape encapsulation and insinuate themselves throughout the code;
  * direct references into complicated structure are confusing because they obscure
    the data really is, and they are a maintenance nightmare;
  * if you don't own the data wrap it with custom classes to protect against changes
    and make your code more readable and intention revealing;

## Enforce single responsibility everywhere

### Extract extra responsibilities from methods:

  * methods, like classes, should have a single responsibility;
  * same techniques apply: ask them questions about what they do, describe in a
    single sentence;
  * do these refactorings even when you do not know the ultimate design; they are
    needed not because the design is clear, but because it isn't - good practices
    reveal design;
  * methods that have a single responsibility:
    - expose previously hidden qualities (making the set of things the class does
      more obvious);
    - avoid the need for comments;
    - encourage reuse and act as example for new methods - the code style propagates
      itself;
    - are easy to move to another class - small methods lower the barriers to
      improving your code;

### Isolate extra responsibilities in classes:

Sometimes you may not want to create a new class (e.g. because you are uncertain about
where you are going and don't want to create a new class that others might start
depending on). Yet, it may seem impossible to the class to have a single responsibility
unless you extract another class.

However, casting the design choice in either/or terms is shortsighted - there are other
choices. Your goal is to preserve single responsibility while making the fewest design
commitments possible.

Postpone decisions until you are absolutely forced to make them. Decisions made in
advance of an explicit requirement are just guesses - don't decide, preserve your
ability to make a decision _later_.

If you have a muddled class with too many responsibilities, separate those responsibilities
into different classes. If you identify extra responsibilities that you cannot yet remove,
isolate them.

(e.g. inner _Struct_ with relevant behavior)
