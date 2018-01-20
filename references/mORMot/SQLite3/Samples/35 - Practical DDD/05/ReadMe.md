
  Practical Domain Driven Design
 ================================
 
Welcome to the Progressive samples folders!
  
In the sub-folders, we will write a almost-complete DDD sample, using TDD.
Folders are enumerated to follow the iterations on the project: 01, 02, 03...
  
Since it was started during EKON 21 conferences, it will modelize a conference booking system.

Don't forget to check out the associated Slides from https://synopse.info/files/ekon21


01 Iteration
------------

Contains the core units, following the "Clean Architecture" patterns.

Implements 
  * TestAll.dpr to run the regression tests;
  * ServBook.dpr to run a Booking service, using the Conference Domain objects.
  

02 Iteration
------------

We added some Domain objects, and a basic booking service.
This has been implemented via the participation of all attendees to the EKON 21 Conference, just after the slides.
Nice first attempt, even it may be really mind-breaking from a classical DB-centric approach! ;)

Introduces a Repository dependency contract, which will be implemented in the test with a `TSynStub`, to let the test pass.


03 Iteration
------------

Minor refactoring:
  * Cleaning the code to use mORMot registration for interface TypeInfo();
  * Refactored IConferenceBooking for a more realistic use of parameters;
  * Include session days to the regression tests.
  

04 Iteration
------------

Adding ORM persistence, as used in ServBook process.
With booking service unit test, of course.


05 Iteration
------------

Enhanced booking service to search for a registration, via the repository service.
