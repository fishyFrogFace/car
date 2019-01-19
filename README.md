# car
Calculates the validity and/or price of a car rental from JSON.

You need [Stack](https://www.haskell.org/platform/) to build the project.

- Cars can be small, sport or SUV and the price depends on the type of car (40USD/day for small cars, 60USD/day for sport cars, 100USD/day for SUV cars).
- There is a discount on weekdays (10%).
- There is a discount for number of rental days (3 or more).
  * For 3 to 5 days 5%.
  * For 6 to 10 days 10%
  * 11 or more 15%
- People subscribed to a membership plan gets a discount (5%).
- An insurance policy is also added to the final price: 5USD a day for the small car, 7USD a day for the sport car, 10USD a day for the SUV (with a 25% increase for younger people). No discount applies over the insurance total.
The person renting the car has to be at least 18 years old.

## Usage

```
stack build
```
```
stack exec car-exe content.txt
```
