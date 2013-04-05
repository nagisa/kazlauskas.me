---
title: Dysfunctional systems and small miscalculations
published: 2013-04-05
tags: Natural Sciences, Otaku Jinsei
---

This article spoils the game I've
[reviewed here](/entries/dysfunctional-systems.html).

# Miscalculations

I'll discuss only the path in which Winter follows Cyrus. I also have to point
out some assumptions I've made before the actual calculations:

1. The Sule is similar to the Earth. Be it size, atmosphere or chemical
   composition of soil;
2. The bomb exploded somewhere over Gabrea;
3. Both the capital of Brighton and the city over which the nuke exploded are
   as close as possible;
4. Accuracy of the map is reasonable.

![The map of the Sule with a line portraying the shortest distance between the
two countries](/images/dysfunctional-systems/sule_gb.jpg)

The map is 3000 pixels wide which makes one pixel represent about 13.35km. The
reddish line is $\sqrt{228^2 + 213^2} \approx 312$ pixels long, therefore
distance between the countries in question turns out to be $312 \times 13.35
\approx 4168$ kilometres.

# Shock wave

The citizens of Brighton could observe the blinding explosion just
$\frac{4168}{229.8} \approx 14$ milliseconds after the event. Between the
explosion visuals and a sound effect, which supposedly meant the arrival of
shock wave, there was just a few lines of dialogue I'd give *at most* a minute to
act out. Too bad the shock wave needs whole $\frac{4168}{480} \approx 8.7$
minutes[^1] to travel all the way from Gabrea to Brighton, therefore in the
game shock wave comes at least 8.7 times faster than it should. A small
mistake, oops.

In further calculations I'll assume that whole 8.7 minutes have already passed
after the explosion when the shock wave sound effect has played.

[speed_of_sound]: http://hypertextbook.com/facts/2001/PamelaSpiegel.shtml

[^1]: [Speed of the sound in earth crust][speed_of_sound]; I took
8$\frac{\mathrm{km}}{\mathrm{s}}$ here.

# Sickness

Shock wave is followed by Cyrus' explanation about how dire the situation
actually is after which  they finally teleport out. Given length of the
explanation I rate it at (*at most*) 5 minutes. Teleportation, as can be seen
earlier in game, takes only 5 seconds to complete, which puts Winter at exactly
$8.7\mathrm{min} + 5\mathrm{min} + 5\mathrm{sec} = 826$ seconds of exposure to
radiation.

826 seconds only and strictly only if there was any other radiation besides
background radiation in the first place! The strength of radiation is inversely
proportional to the distance from the explosion centre[^sqinv], thus even at
relatively short distances from the explosion centre the radiation decreases to
negligible levels. For comparison one cannot even measure the change of
radiation background 200 kilometres away from the explosion of 20Mt nuke[^2].

Looking at the [effects table][effects_table] to experience the same symptoms
Winter had (vomiting and diarrhoea to be exact), her body had to absorb from 2
to 6 Gy of ionising radiation. Given 826 seconds of exposure the dose rate had
to be anything between $8.717\frac{\mathrm{Gy}}{\mathrm{h}}$ and
$26.15\frac{\mathrm{Gy}}{\mathrm{h}}$. I'm not sure I need to point out I'm
having a hard time thinking up specifications of a bomb which would induce such
strong radiation 4168 kilometres away from it's explosion location and not
break the Sule apart.

[^2]: At least until fallout arrives. [Source][nuke]. Oh, and the bomb used in
      the game was a lot stronger.
[^sqinv]: See [inverse-square law][sqinv-law].

[nuke]: http://kitsune.addr.com/Rifts/Rifts-Missiles/nuke.htm
[effects_table]: https://en.wikipedia.org/wiki/Radiation_poisoning#Signs_and_symptoms
[sqinv-law]: https://en.wikipedia.org/wiki/Inverse-square_law


# Final words

Even though there certainly were some small mistakes in the story, those
mistakes didn't prevent me from having fun playing this game. What actually
saddens me is how people are still afraid of nuclear as if it was the most
fearful thing conceived by our hard-working scientists. It's not in the scope of
this article though.
