use bevy::prelude::*;

fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins);

    app.add_event::<OnCombatStep>();

    app.insert_resource(CombatState::default());

    app.add_systems(Startup, spawn_units);
    app.add_systems(
        Update,
        (combat_next_step, combat_remove_dead_units).run_if(on_event::<OnCombatStep>),
    );
    app.add_systems(Update, manual_combat_step);

    app.run();
}

fn manual_combat_step(keys: Res<ButtonInput<KeyCode>>, mut event: EventWriter<OnCombatStep>) {
    if keys.just_pressed(KeyCode::Space) {
        event.write(OnCombatStep);
    }
    if keys.pressed(KeyCode::Enter) {
        event.write(OnCombatStep);
    }
}

#[derive(Component, Clone, Debug)]
struct Unit {
    life: UnitValue,
    attack: UnitValue,
    energy: UnitValue,
    abilities: Vec<CombatAbility>,
    owner: Owner,
}

impl Unit {
    fn value(&self, valuetype: &UnitValues) -> UnitValue {
        match valuetype {
            UnitValues::Life => self.life.clone(),
            UnitValues::Attack => self.attack.clone(),
            UnitValues::Energy => self.energy.clone(),
        }
    }

    fn value_mut(&mut self, valuetype: &UnitValues) -> &mut UnitValue {
        match valuetype {
            UnitValues::Life => &mut self.life,
            UnitValues::Attack => &mut self.attack,
            UnitValues::Energy => &mut self.energy,
        }
    }
}
impl ToString for Unit {
    fn to_string(&self) -> String {
        format!(
            "own: {}, lif {}, atk {}, nrg {}",
            match self.owner {
                Owner::Player => "P",
                Owner::Enemy => "E",
            },
            self.life.to_string(),
            self.attack.to_string(),
            self.energy.to_string()
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum UnitValues {
    Life,
    Attack,
    Energy,
}

#[derive(Clone, Debug)]
enum CombatTarget {
    All,
    This,
    AllEnemy,
    AllAlly,
    EnemyWithMost(UnitValues),
    EnemyWithLeast(UnitValues),
    AllyWithMost(UnitValues),
    AllyWithLeast(UnitValues),
    Specific(Entity),
}

impl CombatTarget {
    fn get(
        &self,
        (source_entity, source_unit): (Entity, &Unit),
        units: &Vec<(Entity, &Unit)>,
    ) -> Vec<Entity> {
        match self {
            CombatTarget::All => units.iter().map(|(e, _)| e).cloned().collect(),
            CombatTarget::This => vec![source_entity],
            CombatTarget::AllEnemy => units
                .iter()
                .filter_map(|(e, u)| (u.owner != source_unit.owner).then_some(e))
                .cloned()
                .collect(),
            CombatTarget::AllAlly => units
                .iter()
                .filter_map(|(e, u)| (u.owner == source_unit.owner).then_some(e))
                .cloned()
                .collect(),
            CombatTarget::EnemyWithMost(v) => units
                .iter()
                .filter(|(_, u)| u.owner != source_unit.owner)
                .max_by(|a, b| a.1.value(v).current.cmp(&b.1.value(v).current))
                .map(|(e, _)| e)
                .into_iter()
                .cloned()
                .collect(),
            CombatTarget::EnemyWithLeast(v) => units
                .iter()
                .filter(|(_, u)| u.owner != source_unit.owner)
                .min_by(|a, b| a.1.value(v).current.cmp(&b.1.value(v).current))
                .map(|(e, _)| e)
                .into_iter()
                .cloned()
                .collect(),
            CombatTarget::AllyWithMost(v) => units
                .iter()
                .filter(|(_, u)| u.owner == source_unit.owner)
                .max_by(|a, b| a.1.value(v).current.cmp(&b.1.value(v).current))
                .map(|(e, _)| e)
                .into_iter()
                .cloned()
                .collect(),
            CombatTarget::AllyWithLeast(v) => units
                .iter()
                .filter(|(_, u)| u.owner == source_unit.owner)
                .min_by(|a, b| a.1.value(v).current.cmp(&b.1.value(v).current))
                .map(|(e, _)| e)
                .into_iter()
                .cloned()
                .collect(),
            CombatTarget::Specific(entity) => units
                .iter()
                .filter_map(|(e, _)| (e == entity).then_some(e))
                .cloned()
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
struct CombatNumber {
    amount: i32,
    values: UnitValues,
}

#[derive(Clone, Debug)]
struct CombatEffect {
    target: CombatTarget,
    gains: Vec<CombatNumber>,
}

#[derive(Clone, Debug)]
enum CombatTriggerWatch {
    Targeted,
    ValueIncrease(UnitValues),
    ValueDecrease(UnitValues),
}

impl CombatTriggerWatch {
    fn check(&self, activated_ability: &ActivatedAbility) -> bool {
        match self {
            Self::Targeted => true,
            Self::ValueIncrease(v) => activated_ability
                .gains
                .iter()
                .any(|g| g.amount > 0 && &g.values == v),
            Self::ValueDecrease(v) => activated_ability
                .gains
                .iter()
                .any(|g| g.amount < 0 && &g.values == v),
        }
    }
}

#[derive(Clone, Debug)]
struct CombatTrigger {
    target: CombatTarget,
    watch: CombatTriggerWatch,
}

#[derive(Clone, Debug)]
struct CombatAbility {
    trigger: Option<CombatTrigger>,
    effects: Vec<CombatEffect>,
    costs: Vec<CombatNumber>,
    // activation_contitions: Vec<CombatCondition>
}

#[derive(Clone, Debug)]
struct ActivatedAbility {
    source: Entity,
    targets: Vec<Entity>,
    gains: Vec<CombatNumber>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Owner {
    Player,
    Enemy,
}

#[derive(Clone, Debug)]
struct UnitValue {
    current: i32,
    base: i32,
}
impl UnitValue {
    fn full(f: i32) -> Self {
        Self {
            current: f,
            base: f,
        }
    }
}
impl ToString for UnitValue {
    fn to_string(&self) -> String {
        format!("{}/{}", self.current, self.base)
    }
}

#[derive(Resource, Default, Debug)]
struct CombatState {
    turn_owner: Option<Owner>,
    pass_ordering: Vec<Entity>,
    pass_current_unit: usize,
    pass_current_ability: usize,
    pass_no_more_abilities: bool,
    response_ordering: Vec<Entity>,
    response_current_unit: usize,
    response_current_ability: usize,
    response_no_more_abilities: bool,
    stack_height: usize,
    stack: Vec<ActivatedAbility>,
}

#[derive(Event)]
struct OnCombatStep;

const MAX_ITERS: i32 = 50;

fn combat_remove_dead_units(mut commands: Commands, units_query: Query<(Entity, &Unit)>) {
    for (entity, unit) in units_query {
        if unit.life.current <= 0 {
            commands.entity(entity).despawn();
        }
    }
}

/// start of combat, the most energy team goes first, tiebreaker most stats, otherwise player
/// start of turn:
/// define a pass order: sort turn units by energy
/// execute in order the units non-trigger abilities
/// after an ability is executed, run all trigger abilities
/// apply each ability effects in stack order
/// to walk up the stack, define a response ordering
/// loop through the response ordering and activate any triggered ability
/// then move up the stack by one
/// once at the top of the stack and a full loop has been done, apply the topmost ability
/// move on to the next ability (current unit, last ability resolved)
/// once all units in a pass resolved all abilities, start another pass
/// once no abiliies in a pass can be resolved, pass the turn
fn combat_next_step(mut state: ResMut<CombatState>, mut units_query: Query<(Entity, &mut Unit)>) {
    debug!(target: "abilities", "start step!");
    for (entity, unit) in &units_query {
        debug!(target: "abilities", "{}: {}", entity, unit.to_string());
    }
    debug!(target: "abilities", "{:?}", &state);

    let (players, enemies): (Vec<&Unit>, Vec<&Unit>) = units_query
        .iter()
        .map(|(_, u)| u)
        .partition(|u| u.owner == Owner::Player);

    if state.turn_owner.is_none() {
        let initiative = |v: &Vec<&Unit>| v.iter().map(|u| u.energy.current).sum::<i32>();
        if initiative(&players) >= initiative(&enemies) {
            state.turn_owner = Some(Owner::Player);
        } else {
            state.turn_owner = Some(Owner::Enemy);
        }
    }

    if players.is_empty() {
        debug!(target: "abilities", "lost combat!");
        return;
    }

    if enemies.is_empty() {
        debug!(target: "abilities", "won combat!");
        return;
    }

    if state.pass_ordering.is_empty() {
        let mut order: Vec<(Entity, Unit)> = units_query
            .iter()
            .filter(|(_, unit)| Some(unit.owner.clone()) == state.turn_owner)
            .map(|(e, unit)| (e, unit.clone()))
            .collect();
        order.sort_by(|(_, a), (_, b)| b.energy.current.cmp(&a.energy.current));
        debug!(target: "abilities", "new pass ordering: {:?}", order.iter().map(|(e, _)|
            e).collect::<Vec<_>>());
        state.pass_ordering = order.into_iter().map(|(e, _)| e).collect();
        state.pass_current_unit = 0;
        state.pass_current_ability = 0;
        state.pass_no_more_abilities = true;
    }

    if !state.stack.is_empty() {
        if state.stack_height == state.stack.len() {
            // top of the stack reached, start resolving abilities
            state.stack_height -= 1;
            let activated_ability = state.stack.pop().unwrap();
            for target in &activated_ability.targets {
                if let Ok((_, mut target_unit)) = units_query.get_mut(*target) {
                    for gain in &activated_ability.gains {
                        target_unit.value_mut(&gain.values).current += gain.amount;
                    }
                }
            }
            debug!(target: "abilities", "resolved ability, stack height {:?}", state.stack_height);
        } else {
            for _ in 0..MAX_ITERS {
                let activated = activate_next_response(&mut state, &mut units_query);
                if activated {
                    break;
                }
            }
        }
    } else {
        for _ in 0..MAX_ITERS {
            let activated = activate_next_ability(&mut state, &mut units_query);
            if activated {
                start_responses(&mut state, &mut units_query);
                break;
            }
        }
    }
}

fn next_turn(state: &mut CombatState, units_query: &mut Query<(Entity, &mut Unit)>) -> bool {
    state.turn_owner = state.turn_owner.as_ref().map(|owner| match owner {
        Owner::Enemy => Owner::Player,
        Owner::Player => Owner::Enemy,
    });
    state.pass_ordering.clear();
    state.pass_current_unit = 0;
    state.pass_current_ability = 0;

    for (_, mut unit) in units_query.iter_mut() {
        if Some(unit.owner.clone()) == state.turn_owner {
            unit.energy.current = unit.energy.base;
        }
    }

    debug!(target: "abilities", "next turn, turn owner: {:?}", state.turn_owner);
    return true;
}

fn next_pass(state: &mut CombatState, units_query: &mut Query<(Entity, &mut Unit)>) -> bool {
    if !(0..state.pass_ordering.len()).contains(&state.pass_current_unit) {
        if state.pass_no_more_abilities {
            return next_turn(state, units_query);
        } else {
            state.pass_current_unit = 0;
            state.pass_current_ability = 0;
            state.pass_no_more_abilities = true;
            debug!(target: "abilities", "do another pass, someone had activated at least one ability");
        }
    } else {
        debug!(target: "abilities", "next pass unit");
        debug!(target: "abilities", "next pass unit: {}", state.pass_current_unit);
        state.pass_current_unit += 1;
    }
    return false;
}

fn next_pass_ability(state: &mut CombatState, unit: &Unit) {
    if !(0..unit.abilities.len()).contains(&state.pass_current_ability) {
        state.pass_current_ability = 0;
        state.pass_current_unit += 1;
        debug!(target: "abilities", "next pass unit: {}", state.pass_current_unit);
    } else {
        state.pass_current_ability += 1;
        debug!(target: "abilities", "next pass ability: {}", state.pass_current_ability);
    }
}

fn start_responses(state: &mut CombatState, units_query: &mut Query<(Entity, &mut Unit)>) {
    let mut order: Vec<(Entity, &Unit)> = units_query.iter().collect();
    order.sort_by(|(_, a), (_, b)| b.energy.current.cmp(&a.energy.current));
    debug!(target: "abilities", "new responses ordering: {:?}", order.iter().map(|(e, _)|
            e).collect::<Vec<_>>());
    state.response_ordering = order.into_iter().map(|(e, _)| e).collect();
    state.stack_height = 0;
    state.response_current_unit = 0;
    state.response_no_more_abilities = true;
}

fn activate_next_ability(
    state: &mut CombatState,
    units_query: &mut Query<(Entity, &mut Unit)>,
) -> bool {
    let Some(unit_entity) = state.pass_ordering.get(state.pass_current_unit).cloned() else {
        debug!(target: "abilities", "no unit");
        return next_pass(state, units_query);
    };

    let Ok((_, unit)) = units_query.get(unit_entity) else {
        debug!(target: "abilities", "no unit");
        return next_pass(state, units_query);
    };

    let Some(ability) = unit.abilities.get(state.pass_current_ability).cloned() else {
        debug!(target: "abilities", "no ability");
        next_pass_ability(state, unit);
        return false;
    };

    // check if all costs can be paid
    if !ability
        .costs
        .iter()
        .all(|cost| unit.value(&cost.values).current >= cost.amount)
    {
        debug!(target: "abilities", "unit {} can't pay {:?}", unit.to_string(), ability.costs);
        next_pass_ability(state, unit);
        return false;
    }

    // activating only non-triggered abilities
    if ability.trigger.is_some() {
        debug!(target: "abilities", "this is a triggered ability");
        next_pass_ability(state, unit);
        return false;
    }

    let units: Vec<(Entity, &Unit)> = units_query.iter().collect();

    for effect in &ability.effects {
        let targets = effect.target.get((unit_entity, unit), &units);
        let act = ActivatedAbility {
            source: unit_entity,
            targets,
            gains: effect.gains.clone(),
        };
        debug!(target: "abilities", "activated {:?}, stack height: {}", act, state.stack.len());
        state.stack.push(act);
    }

    if let Ok((_, mut unit_mut)) = units_query.get_mut(unit_entity) {
        for cost in &ability.costs {
            unit_mut.value_mut(&cost.values).current -= cost.amount;
        }
    };

    state.pass_no_more_abilities = false;
    state.pass_current_ability += 1;
    return true;
}

fn next_response(state: &mut CombatState) {
    if !(0..state.response_ordering.len()).contains(&state.response_current_unit) {
        if state.response_no_more_abilities {
            state.stack_height += 1;
            debug!(target: "abilities", "no more responses, walking up: {}", state.stack_height);
        } else {
            state.response_current_unit = 0;
            state.response_current_ability = 0;
            state.response_no_more_abilities = true;
            debug!(target: "abilities", "do another response, someone had activated at least one ability");
        }
    } else {
        state.response_current_unit += 1;
        debug!(target: "abilities", "next response unit: {}", state.response_current_unit);
    }
}

fn next_response_ability(state: &mut CombatState, unit: &Unit) {
    if !(0..unit.abilities.len()).contains(&state.response_current_ability) {
        state.response_current_ability = 0;
        state.response_current_unit += 1;
        debug!(target: "abilities", "next response unit: {}", state.response_current_unit);
    } else {
        state.response_current_ability += 1;
        debug!(target: "abilities", "next response ability: {}", state.response_current_ability);
    }
}

fn activate_next_response(
    state: &mut CombatState,
    units_query: &mut Query<(Entity, &mut Unit)>,
) -> bool {
    if state.stack_height >= state.stack.len() {
        debug!(target: "abilities", "over the top of the stack, start resolving");
        return true;
    }

    let Some(unit_entity) = state
        .response_ordering
        .get(state.response_current_unit)
        .cloned()
    else {
        debug!(target: "abilities", "no unit");
        next_response(state);
        return false;
    };

    let Ok((_, unit)) = units_query.get(unit_entity) else {
        debug!(target: "abilities", "no unit");
        next_response(state);
        return false;
    };

    let Some(ability) = unit.abilities.get(state.response_current_ability).cloned() else {
        debug!(target: "abilities", "no ability");
        next_response_ability(state, unit);
        return false;
    };

    // check if all costs can be paid
    if !ability
        .costs
        .iter()
        .all(|cost| unit.value(&cost.values).current >= cost.amount)
    {
        debug!(target: "abilities", "unit {} can't pay {:?}", unit.to_string(), ability.costs);
        next_response_ability(state, unit);
        return false;
    }

    // look at the activated ability at the current stack height
    let activated_ability = state.stack[state.stack_height].clone();

    let Some(trigger) = &ability.trigger else {
        debug!(target: "abilities", "no trigger");
        next_response_ability(state, unit);
        return false;
    };

    let units: Vec<(Entity, &Unit)> = units_query.iter().collect();

    let mut any_activation = false;

    // activate the trigger
    let watched_targets = trigger.target.get((unit_entity, unit), &units);
    for target in watched_targets {
        if !activated_ability.targets.contains(&target) {
            continue;
        }

        if units.iter().find(|(e, _)| *e == target).is_none() {
            continue;
        };

        if trigger.watch.check(&activated_ability) {
            for effect in &ability.effects {
                any_activation = true;
                let targets = effect.target.get((unit_entity, unit), &units);
                let act = ActivatedAbility {
                    source: unit_entity,
                    targets,
                    gains: effect.gains.clone(),
                };
                debug!(target: "abilities", "activated {:?}, stack height: {}", act, state.stack.len());
                state.stack.push(act);
            }
        }
    }

    state.response_current_ability += 1;
    if any_activation {
        state.response_no_more_abilities = false;
        if let Ok((_, mut unit_mut)) = units_query.get_mut(unit_entity) {
            for cost in &ability.costs {
                unit_mut.value_mut(&cost.values).current -= cost.amount;
            }
        };
    }

    return true;
}

fn spawn_units(mut commands: Commands) {
    commands.spawn((Unit {
        life: UnitValue::full(10),
        attack: UnitValue::full(10),
        energy: UnitValue::full(10),
        abilities: vec![
            CombatAbility {
                effects: vec![CombatEffect {
                    target: CombatTarget::This,
                    gains: vec![CombatNumber {
                        amount: 1,
                        values: UnitValues::Attack,
                    }],
                }],
                costs: vec![CombatNumber {
                    amount: 2,
                    values: UnitValues::Energy,
                }],
                trigger: None,
            },
            CombatAbility {
                effects: vec![CombatEffect {
                    target: CombatTarget::EnemyWithMost(UnitValues::Life),
                    gains: vec![CombatNumber {
                        amount: -1,
                        values: UnitValues::Life,
                    }],
                }],
                costs: vec![CombatNumber {
                    amount: 2,
                    values: UnitValues::Energy,
                }],
                trigger: None,
            },
        ],
        owner: Owner::Player,
    },));
    commands.spawn((Unit {
        life: UnitValue::full(10),
        attack: UnitValue::full(10),
        energy: UnitValue::full(10),
        abilities: vec![
            CombatAbility {
                effects: vec![CombatEffect {
                    target: CombatTarget::This,
                    gains: vec![CombatNumber {
                        amount: 1,
                        values: UnitValues::Attack,
                    }],
                }],
                costs: vec![CombatNumber {
                    amount: 2,
                    values: UnitValues::Energy,
                }],
                trigger: None,
            },
            CombatAbility {
                effects: vec![CombatEffect {
                    target: CombatTarget::EnemyWithMost(UnitValues::Life),
                    gains: vec![CombatNumber {
                        amount: -1,
                        values: UnitValues::Life,
                    }],
                }],
                costs: vec![CombatNumber {
                    amount: 2,
                    values: UnitValues::Energy,
                }],
                trigger: None,
            },
        ],
        owner: Owner::Enemy,
    },));
}
