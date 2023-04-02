# RCS

### Rubik’s Cube Solver (Old Pochmann)

----------

## Description

This repository contains multiple implementations of a Rubik’s Cube solver. The solver is based on the Old Pochmann method.

>> The Old Pochmann Method, which is a method that solves one piece at a time, is a method typically used by beginner blindsolvers.
>> Blindfolded solvers use letter patterns to help memorize sequences of moves in order to solve the cube.
> 
> https://en.wikipedia.org/wiki/Speedcubing Blindfolded methods

----------

### Implementations

- [x] [Python3](./py/)
- [ ] [C++](./cpp/)
- [ ] [Haskell](./hs/)
- [ ] [Rust](./rs/)

<!-- no i will not add a java implementation!!! -->

----------

## Move notation 

>> Many 3×3×3 Rubik's Cube enthusiasts use a notation developed by David Singmaster
>> to denote a sequence of moves, referred to as "Singmaster notation". Its relative
>> nature allows algorithms to be written in such a way that they can be applied
>> regardless of which side is designated the top or how the colours are organised
>> on a particular cube.
>>
>> - F (Front): the side currently facing the solver
>> - B (Back): the side opposite the front
>> - U (Up): the side above or on top of the front side
>> - D (Down): the side opposite the top, underneath the Cube
>> - L (Left): the side directly to the left of the front
>> - R (Right): the side directly to the right of the front
>> - ...
>>
>> When a prime symbol ( ' ) follows a letter, it denotes an anticlockwise face turn;
>> while a letter without a prime symbol denotes a clockwise turn. These directions
>> are as one is looking at the specified face. A letter followed by a 2 
>> (occasionally a superscript 2) denotes two turns, or a 180-degree turn. R is right
>> side clockwise, but R' is right side anticlockwise. ...
>
> https://en.wikipedia.org/wiki/Rubik%27s_Cube Singmaster notation

----------
