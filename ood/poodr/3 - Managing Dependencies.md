
# Understanding dependencies

An object depends on another if, when one changes, the other might
be forced to change in turn. [connascence.md]

## Recognizing Dependencies

An object has a dependency when it knows:

  * the name of another class;
  * the name of a message that it intends to send to s.o. other than self;
  * the arguments that a message requires;
  * the order of those arguments.

Some degrees of dependency between two collaborating classes is inevitable,
but most of the ones listed above are unnecessary.

The design challenge is to manage dependencies so that each class has the
fewest possible: "a class should know just enough to do its job".

## Coupling between objects (cbo)

The more tightly couple two objects are, the more they  behave like a single
entity. When you change one, you might have to change the other; when you
want you reuse one, you'll have to bring the other along; when you test one,
you will be testing the other too.

Left unchecked, unmanaged dependencies cause an entire application to become
an entangled mess. A day will come when it's easier to rewrite everything
than to change anything.

# Writing loosely coupled code

Every dependency is like a little dot of glue that causes your class to
stick to the things it touches. A few dots are necessary, but apply too
much glue and your application will harden into a solid block.

Techniques to reduce dependency:

## Inject dependencies

Referring to another class by its name creates a major sticky spot. When
class A hard-codes a reference to class B, it is explicitly declaring that
it's only willing to work with instances of B. A refuses to work with any
other kind of object.

It's not the class of the object that is important, it's the *message*.

## Isolate dependencies

When working  on an existing application you may find yourself under severe
constraints about how much you can actually change. If you cannot remove
unnecessary dependencies, you should isolate them within your class.

Dependencies are foreign invaders that represent vulnerabilities, and they
should be concise, explicit, and isolated.

  * Isolate instance creation: if you cannot inject a dependency, isolate the
    creation of instances (e.g. in a private method).

The way you manage dependencies on external class names has profound effects
on your application. Routinely injecting them leads to classes that are
naturally decoupled;

  * Isolate vulnerable external messages:

    ```ruby
      def gear_inches
        ratio * wheel.diameter
      end
    ```
  vs

    ```ruby
      def gear_inches
        ratio * wheel_diameter
      end

      private

      def wheel_diameter
        wheel.diameter
      end
    ```

Reduces your chance of being forced to change *gear_inches* by removing the
external dependency and encapsulating it in a method of its own.

In the original code, *gear_inches* knew that *wheel* had a diameter. This
knowledge is a dangerous dependency that couples *gear_inches* to an external
object and one of _its_ methods. After the change, *gear_inches* is more

abstract and can depend on a message sent to `self`.

This technique becomes necessary when a class contains embedded references
to a _message_ that is likely to change. Isolating the reference provides
some insurance against being affected by that change.

## Remove argument-order dependencies

  * use hashes/keyword args for initialization arguments: dependency on names
    is more stable than on order, and acts as documentation;
  * explicitly define defaults;
  * isolate multiparameter initialization: when you do not control the signature
    of the method that needs to change, extract a factory method/module to
    initialize object with a better signature.

# Managing dependency direction

## Reversing dependencies

  ```ruby
    def gear_inches
      ratio * wheel_diameter
    end
  ```

  vs

  ```ruby
    def gear_inches(diameter)
      ratio * diameter
    end
  ```

The choices you make about the direction of dependencies have far
reaching consequences that manifest themselves for the life of your
application. If you get this right, your application will be pleasant
to work on and easy to maintain. If you get it wrong, then the
dependencies will gradually take over and the application will become
harder and harder to change.

## Choosing dependency direction

_Depend on things that change less often than you_.

  * some classes are more likely than others to have changes in
    requirements;
  * concrete classes are more likely to change than abstract classes;
  * changing a class that has many dependents will result in
    widespread consequences.

### Understanding likelihood of change

Every class used in your application can be ranked along a scale of
how likely it is to undergo a change relative to all other classes.
This ranking is one key piece of information to consider when choosing
the direction of dependencies.

### Recognizing concretions and abstractions

Abstractions represent common, stable qualities. They are less likely
to change than the concrete classes from which they were extracted.

Interfaces can have dependents which must be taken into account
during design.

### Avoiding dependent-laden classes

A class that, if changed, will cause changes to ripple through the
application, will be under enormous pressure to _never_ change. Ever.
Your application may be permanently handicapped by your reluctance
to pay the price required to make a change to this class.

### Finding dependencies that matter

  * A) many dependents, less likely to undergo requirements change:
     "Abstract Zone": changes are unlikely but will have broad effects;
  * B) few dependents, less likely to change:
     "Neutral Zone": unlikely changes with few side effects;
  * C) few dependents, more likely to change:
     "Neutral Zone": likely changes, but with few side effects;
  * D) many dependents, very likely to change:
     "Danger Zone": classes that _will_ change and the changes will
     cascade into dependents.

Zone A usually consists of abstract classes and interfaces. In a
thoughtfully designed application this is inevitable: dependencies
cluster around abstractions because the are more stable.

Zone B classes don't require much concern during design.

Zone C is the opposite of A: more concrete classes, likely to change;
but that's not a problem because few other classes depend on them.

Zone D classes represent a danger to the future health of the application.
These are the classes that make the application painful to change. When a
change breaks some far away an seemingly unrelated bit of code, the design
flaw originated here.

# Summary

Dependency management is core to creating future-proof applications.
Injecting dependencies creates loosely couple objects that can be reused
in novel ways. Isolating dependencies allows objects to quickly adapt to
unexpected changes. Depending on abstractions decreases the likelihood of
facing these changes.

The key to manage dependencies is to control their direction. The road to
maintenance nirvana is paved with classes that depend on things that
change less often than they do.
