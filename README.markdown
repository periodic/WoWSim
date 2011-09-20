# WoWSim

WoWSim is a combat simuation framework for World of Warcraft combat simulations.  It is built on a descrete event simulator and is designed originally for DPS simulations against combat dummies.  For an example, see WoWSim.

## Features

The system should be able to support any number of entities fighting eachother.  I'm working on supporting buffs, debuffs, talents, and all abilities.  The main feature is programmable AI so that many different combat rotations or priority lists can be tested.  You can see an example of a class implemented in AI/Warrior.hs.

## Background

The system uses DisEvSim to register a set of handlers which respond to events and generate additional events.

## Performance

Performance is acceptable, but could be better.  More work should be done profiling to improve performance.
