# proposal 1: team builder rpg

each unit in the team has an activated effect.
effects are meant to be combo pieces and happen automatically.
the overworld map is really small (one screen)
the battles are 1v1 and happen in a combat screen.
there is dialogue and the story is very simple.

## combat

each player starts by placing the units.
the placement is automatic.

### unit resources

each unit has as a base:
- speed
- defence
- damage
- energy
- life

then each unit can accumulate:
- current energy
- current life
- additional/less defence
- additional/less damage
- additional/less speed
- exp

level ups grant random stats increases and can happen during combat.

### player combat turn

each unit gains full energy. 
starting from the speediest, each unit triggers their abilities.
if there is a tie the tiebreaker is exp, then name.
each unit has 0 to n abilities.
every ability has a energy cost to activate.
until every unit has energy to activate abilities, the turn repeats.

example:
unit A has "attack the speediest using 3 energy" then "defence + 1 (2)"
unit B has "when a unit attacks, that unit gains 2 energy (2)"
unit C has "when a unit gains energy it also gains attack (2)" then "attack the healthiest (1)"

### ability design

i have to be very careful not to include deterministic infinite combos.
i will have to handle this case by truncating the turn if it goes on too long.
but that won't be a problem if the combo also does damage.

ability types:
- *action*: like "attack at random (2)", "gain 1 defence (3)", "gain 1 life (2)"
- triggered: "when this happens, *action* (2)"
    - the trigger can be anything: enemy losing life, enemy gaining defence, friendly attacking

ability list (exploratory):
- gain 1 life
- gain 1 attack
- all allies lose 1 defence
- all enemies lose life
- when an ally gains life, attack the strongest enemy
- when this unit loses life, gain 2 mana (1)
- when an enemy gains damage, gain 2 damage (2)
- all units lose 2 life (5)
- when an ally loses life, all units lose 1 life (1)

balancing this game will be impossible. :))))))))))

### when a unit dies

it's removed from combat, you can use it again next combat. enemies flee.

## at the end of combat

gain some kind of way to modify your units or your team.
you gain exp, do i need to gain money/prestige too?

