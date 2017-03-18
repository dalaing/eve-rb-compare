This is an attempt to port the Eve tutorial to reactive-banana. 

Aside from having to set up the host, a reflex version would be pretty similar - and there is at least one project out there that aims to make setting up the reflex hosts as easy as working with IO integration in reactive-banana.

It's probably best to go through the various files in order.

So far I haven't:
- gone through the final version and split it up into modules
- separated the IO from the pure bits of the network
  - used the pure bits of the network to write QuickCheck for the code
- used Writer to accumulate the various stati/statuses to better match the Eve tutorial

