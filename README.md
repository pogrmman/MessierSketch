# Messier Sketching Project
One of my personal goals for the year 2021 is to sketch all 110 Messier objects at the eyepiece of my
telescope. This repo will host my planning materials for the project. The bulk of the code is a small
library for astronomical calculations.

I'm planning on doing most of the sketches between 9 pm and midnight Central Time, so the first step
for the project is to calculate what days of the year each of the Messier Objects crosses the meridian
between 9:30 pm and 11:30 pm (this narrower window ensures they're high during the whole session).

To do this, I calculate the local apparent sidereal time at 9:30 pm and 11:30 pm for every day in 2021
and compare it to the right ascension of each object. Because local apparent sidereal time gives you the
line of right ascension on the meridian, if an object's RA lies between the sidereal time at 9:30 and
11:30, it transits the meridian sometime between those two times. This allows me to calculate a range of
dates at which each object has the best visibility.

Because I'm observing from suburban Austin, TX, my night sky has a fair degree of light pollution: on a
good night, I can only see down to magnitude ~5 at the zenith with the naked eye. While I plan to do
most of the sketches from my backyard, I may plan to travel to darker skies for objects with a lower
surface brightness. Additionally, although I'm located fairly far to the south, my southern horizon has
many tall trees that block the sky, so I'm going to have to sketch some objects from a location without
such obstructions.

## Files
`catalog.txt` -- the raw catalog data


`difficulties.txt` -- objects that may pose special difficulties (low surface brightness or far enough
to the south to be obstructed


`observing_dates.txt` -- the catalog, arranged by observation window, with notes marking low surface
brightness (B) or a southernly position (S). Modified from the output of `calculator.scm` for formatting
and to include difficulty notes


`moon_position.txt` -- Moon position at 10:30 local time every day of 2021, calculated using 
[Cartes du Ciel](https://www.ap-i.net/skychart/en/start)


`moon_below_horizon.txt` -- dates on which the Moon lies more than 10 degrees below the horizon, making
conditions good for observing deep sky objects


`observing_dates_moon.txt` -- similar to `observing_dates.txt`, only with additional windows that are the
overlap of the best visibility window with the windows of when the Moon is more than 10 degrees below the
horizon at 10:30 pm


`moon_observation_windows.txt` -- a list of which objects transit the meridian sometime during each period
where the Moon is more than 10 degrees below the horizzon at 10:30 pm 


`observing_plan.txt` -- my first pass at an observing plan


`calculator.scm` -- the script that actually runs the calculations to generate the observing windows


`lib/astro` -- the astronomical calculation library

## References
[Solar Position Algorithm for Solar Radiation Applications](https://www.nrel.gov/docs/fy08osti/34302.pdf)
-- I took many of my astronomical calculations from here. Some are currently unused, but I plan to
create a full-fledged astronomical calculation library using many of them.


[Messier Guide by Tony Flanders](https://tonyflanders.wordpress.com/messier-guide-index-by-number/) --
I used this to identify objects of low surface brightness that may be a challenge


[Messier Object Data, sorted by Right Ascension](https://www.messier.seds.org/dataRA.html) --
The actual catalog I'm using to plan my observations. (Yes, it has 111 distinct objects, but I plan to
sketch all of them.)
