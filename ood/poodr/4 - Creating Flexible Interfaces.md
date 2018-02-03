
# Understanding interfaces

Dependency injection and single responsibility, while necessary techniques,
are not enough to prevent the construction of an application whose design
causes you pain.

Besides what a class _does_, another source of problems is what it _reveals_.
Each object has a clearly defined set of methods that it expects others
to use - these exposed methods comprise the class's _public interface_.

## Public interfaces

  * reveal its primary responsibility;
  * are expected to be invoked by others;
  * will not change on a whim;
  * are safe for others to depend on;
  * are thoroughly documented in the tests.

## Private interfaces

  * handle implementation details;
  * are not expected to be sent by other objects;
  * can change for any reason whatsoever;
  * are unsafe for others to depend on;
  * may not even be referenced in the tests.

## Responsibilities, dependencies, and interfaces

The public interface is a contract that articulates the responsibilities
of your class. Public parts are the stable parts; private parts are the
changeable parts. When your classes use the public methods of others, you
trust those methods to be stable.

# Finding the public interface

The design goal, as always, is to retain maximum future flexibility while
writing only enough code to meet today's requirements. Good public interfaces
reduce the cost of unanticipated change; bad public interfaces raise it.

## Example app: bicycle touring company

Use case: "A customer, in order to choose a trip, would like to see a list
of available trips of appropriate difficulty, on a specific date, where
rental bikes are available."

## Constructing an intention

Classes representing nouns in the use cases description (e.g. Customer, Trip,
Bike, etc), which have both data and behavior (aka _domain objects_, d.o.)
are obvious because they are persistent: they stand for big, visible real-world
things that will end up with a representation in the database.

But d.o. are not at the design center of your application; instead, they are a
trap for the unwary. If you fixate on d.o., you will tend to coerce behavior
into them. Design experts notice d.o. without concentrating on them - they
focus on the *messages* that pass between them. These messages are guides that
lead you to discover other objects, ones that are just as necessary but far
less obvious.

## Using sequence diagrams

UML sequence diagrams provide a simple way to experiment with different
object arrangements and message passing schemes. They bring clarity to
your thoughts and provide a vehicle to collaborate and communicate with
others.

pp 66
