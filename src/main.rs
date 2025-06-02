use std::time::Duration;

use bevy::{
    asset::AssetMetaCheck, ecs::spawn::SpawnWith, prelude::*, render::camera::ScalingMode,
    text::TextBounds, window::WindowResolution,
};
use bevy_asset_loader::prelude::*;
use rand::{Rng, SeedableRng, thread_rng};
use rand_chacha::ChaCha8Rng;

fn main() {
    let mut app = App::new();

    app.add_plugins(
        DefaultPlugins
            .set(AssetPlugin {
                meta_check: AssetMetaCheck::Never,
                ..default()
            })
            .set(WindowPlugin {
                primary_window: Window {
                    title: "bevy jam 6".to_string(),
                    fit_canvas_to_parent: true,
                    resolution: WindowResolution::new(1280., 720.),
                    resizable: true,
                    ..default()
                }
                .into(),
                ..default()
            }),
    );

    app.add_event::<OnCombatStep>();

    app.insert_resource(CombatState::default());
    app.insert_resource(ClearColor(Color::srgb(0.1, 0.1, 0.1)));

    app.init_state::<AppStates>();

    app.add_loading_state(
        LoadingState::new(AppStates::AssetLoading)
            .continue_to_state(AppStates::Combat)
            .load_collection::<JamAssets>(),
    );
    app.add_systems(OnExit(AppStates::AssetLoading), prepare_atlases);

    app.add_systems(OnEnter(AppStates::Combat), spawn_units);
    app.add_systems(
        Update,
        (combat_next_step, combat_remove_dead_units, stack_animation)
            .chain()
            .run_if(on_event::<OnCombatStep>)
            .run_if(in_state(AppStates::Combat)),
    );
    app.add_systems(
        Update,
        manual_combat_step.run_if(in_state(AppStates::Combat)),
    );

    app.add_systems(
        Update,
        (ui_ability_trackers, ui_value_trackers, ui_name_trackers)
            .run_if(in_state(AppStates::Combat)),
    );

    app.add_systems(Update, update_particles.run_if(in_state(AppStates::Combat)));

    app.add_systems(Update, debug_resolution);

    app.run();
}

fn debug_resolution(mut gizmos: Gizmos, time: Res<Time>) {
    gizmos.rect_2d(Vec2::ZERO, Vec2::new(1280., 720.), Color::WHITE);
}

#[derive(AssetCollection, Resource, Clone)]
struct JamAssets {
    #[asset(path = "images/units.png")]
    units_image: Handle<Image>,
    units_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "images/icons.png")]
    icons_image: Handle<Image>,
    icons_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "images/ability_background.png")]
    ability_background_image: Handle<Image>,

    #[asset(path = "images/unit_background.png")]
    unit_background_image: Handle<Image>,

    #[asset(path = "images/unit_name.png")]
    unit_name_image: Handle<Image>,

    #[asset(path = "fonts/IosevkaFixed-Medium.subset.ttf")]
    font: Handle<Font>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
enum AppStates {
    #[default]
    AssetLoading,
    Combat,
}

fn prepare_atlases(
    mut handles: ResMut<JamAssets>,
    mut texture_atlas_layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
    handles.units_layout = texture_atlas_layouts.add(TextureAtlasLayout::from_grid(
        UVec2::splat(256),
        2,
        2,
        None,
        None,
    ));

    handles.icons_layout = texture_atlas_layouts.add(TextureAtlasLayout::from_grid(
        UVec2::splat(128),
        4,
        4,
        None,
        None,
    ));
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
    name: String,
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

impl UnitValues {
    fn sprite_index(&self) -> usize {
        match self {
            UnitValues::Life => 0,
            UnitValues::Attack => 1,
            UnitValues::Energy => 2,
        }
    }
    fn describe(&self) -> String {
        match &self {
            UnitValues::Life => format!("life"),
            UnitValues::Attack => format!("attack"),
            UnitValues::Energy => format!("energy"),
        }
    }
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
    fn describe(&self) -> String {
        match self {
            CombatTarget::All => format!("any"),
            CombatTarget::This => format!("this"),
            CombatTarget::AllEnemy => format!("any enemy"),
            CombatTarget::AllAlly => format!("any ally"),
            CombatTarget::EnemyWithMost(v) => format!("the enemy with the most {}", v.describe()),
            CombatTarget::EnemyWithLeast(v) => format!("the enemy with the least {}", v.describe()),
            CombatTarget::AllyWithMost(v) => format!("the enemy with the most {}", v.describe()),
            CombatTarget::AllyWithLeast(v) => format!("the enemy with the least {}", v.describe()),
            CombatTarget::Specific(_) => format!("that unit"),
        }
    }

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
enum CombatNumberSource {
    Immediate(i32),
    Unit(UnitValues),
    UnitNegated(UnitValues),
}
impl CombatNumberSource {
    fn solve(&self, unit: &Unit) -> i32 {
        match &self {
            Self::Immediate(amt) => *amt,
            Self::Unit(v) => unit.value(&v).current,
            Self::UnitNegated(v) => -unit.value(&v).current,
        }
    }
}

#[derive(Clone, Debug)]
struct CombatNumber {
    source: CombatNumberSource,
    values: UnitValues,
}

impl CombatNumber {
    fn describe(&self) -> String {
        match &self.source {
            CombatNumberSource::Immediate(amt) => {
                format!(
                    "{} {} {}",
                    if amt > &0 {
                        "gains"
                    } else if amt == &0 {
                        ""
                    } else {
                        "loses"
                    },
                    amt.abs(),
                    self.values.describe()
                )
            }
            CombatNumberSource::Unit(source) => format!(
                "gains {} equal to this unit's {}",
                self.values.describe(),
                source.describe(),
            ),
            CombatNumberSource::UnitNegated(source) => {
                format!(
                    "loses {} equal to this unit's {}",
                    self.values.describe(),
                    source.describe(),
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
struct CombatEffect {
    target: CombatTarget,
    gains: Vec<CombatNumber>,
}

impl CombatEffect {
    fn describe(&self) -> String {
        let mut s = format!("{} ", self.target.describe());
        for (i, gain) in self.gains.iter().enumerate() {
            let comma = if i < self.gains.len() - 1 { ", " } else { "" };
            s = format!("{}{}{}", s, gain.describe(), comma);
        }
        s
    }
}

#[derive(Clone, Debug)]
enum CombatTriggerWatch {
    Targeted,
    ValueIncrease(UnitValues),
    ValueDecrease(UnitValues),
}

impl CombatTriggerWatch {
    fn describe(&self) -> String {
        match &self {
            Self::Targeted => format!("is targeted"),
            Self::ValueIncrease(unit_values) => format!("gains {}", unit_values.describe()),
            Self::ValueDecrease(unit_values) => format!("loses {}", unit_values.describe()),
        }
    }
}

impl CombatTriggerWatch {
    fn check(&self, activated_ability: &ActivatedAbility) -> bool {
        match self {
            Self::Targeted => true,
            Self::ValueIncrease(v) => activated_ability.gains.iter().any(|g| &g.values == v),
            Self::ValueDecrease(v) => activated_ability.gains.iter().any(|g| &g.values == v),
        }
    }
}

#[derive(Clone, Debug)]
struct CombatTrigger {
    target: CombatTarget,
    watch: CombatTriggerWatch,
}

impl CombatTrigger {
    fn describe(&self) -> String {
        format!("when {} {}", self.target.describe(), self.watch.describe())
    }
}

#[derive(Clone, Debug)]
struct CombatAbility {
    trigger: Option<CombatTrigger>,
    effects: Vec<CombatEffect>,
    costs: Vec<CombatNumber>,
    // activation_contitions: Vec<CombatCondition>
}

impl CombatAbility {
    fn describe(&self) -> String {
        let mut s = String::new();
        if let Some(trigger) = &self.trigger {
            s = format!("{}{}: ", s, trigger.describe());
        }
        for (i, effect) in self.effects.iter().enumerate() {
            let comma = if i < self.effects.len() - 1 { ", " } else { "" };
            s = format!("{}{}{}", s, effect.describe(), comma);
        }
        s
    }
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
            if let Ok(unit) = units_query
                .get(activated_ability.source)
                .map(|(_, u)| u.clone())
            {
                for target in &activated_ability.targets {
                    if let Ok((_, mut target_unit)) = units_query.get_mut(*target) {
                        for gain in &activated_ability.gains {
                            target_unit.value_mut(&gain.values).current += gain.source.solve(&unit);
                        }
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
        .all(|cost| unit.value(&cost.values).current >= cost.source.solve(unit))
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

    let unit = unit.clone();
    if let Ok((_, mut unit_mut)) = units_query.get_mut(unit_entity) {
        for cost in &ability.costs {
            unit_mut.value_mut(&cost.values).current -= cost.source.solve(&unit);
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
        .all(|cost| unit.value(&cost.values).current >= cost.source.solve(unit))
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
        let unit = unit.clone();
        if let Ok((_, mut unit_mut)) = units_query.get_mut(unit_entity) {
            for cost in &ability.costs {
                unit_mut.value_mut(&cost.values).current -= cost.source.solve(&unit);
            }
        };
    }

    return true;
}

fn unit_value_icon_bundle(handles: &JamAssets, values: &UnitValues) -> impl Bundle {
    (children![
        (
            Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::new(0., -35., 3.)),
            Sprite::from_atlas_image(
                handles.icons_image.clone(),
                TextureAtlas {
                    layout: handles.icons_layout.clone(),
                    index: values.sprite_index(),
                },
            ),
        ),
        (
            Transform::from_translation(Vec3::new(20., -20., 2.)),
            Visibility::Visible,
            children![
                (
                    Transform::from_translation(Vec3::Z * 2.),
                    TextColor(Color::WHITE),
                    Text2d::new("webs"),
                    TextFont {
                        font: handles.font.clone(),
                        font_size: 20.0,
                        ..default()
                    },
                    UiValueTracker::base(values.clone())
                ),
                (
                    Transform::from_translation(Vec3::Z).with_scale(Vec3::splat(0.2)),
                    Sprite {
                        color: Color::BLACK,
                        ..Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 4,
                            },
                        )
                    }
                ),
            ],
        ),
        (
            Transform::from_translation(Vec3::splat(0.)),
            Visibility::Visible,
            children![
                (
                    Transform::from_translation(Vec3::Z * 2.),
                    TextColor(Color::BLACK),
                    Text2d::new("webs"),
                    TextFont {
                        font: handles.font.clone(),
                        font_size: 30.0,
                        ..default()
                    },
                    UiValueTracker::current(values.clone())
                ),
                (
                    Transform::from_translation(Vec3::Z).with_scale(Vec3::splat(0.4)),
                    Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 4,
                        },
                    ),
                ),
            ],
        ),
    ],)
}

#[derive(Component, Debug, Clone, Default)]
struct UiAbilityTracker {
    ability: Option<CombatAbility>,
    index: usize,
}

fn ui_ability_cost_single_bundle(
    handles: &JamAssets,
    cost: &CombatNumber,
    index: usize,
) -> impl Bundle {
    (
        Transform::from_translation(Vec3::new(0. - (index as f32) * 25., 0., 0.)),
        Visibility::Visible,
        children![
            (
                Transform::from_translation(Vec3::Z * 3.),
                TextColor(Color::BLACK),
                Text2d::new(format!(
                    "{}",
                    match &cost.source {
                        CombatNumberSource::Immediate(amt) => format!("{}", amt),
                        CombatNumberSource::Unit(v) => match v {
                            UnitValues::Life => "+".to_string(),
                            UnitValues::Attack => "+".to_string(),
                            UnitValues::Energy => "+".to_string(),
                        },
                        CombatNumberSource::UnitNegated(v) => match v {
                            UnitValues::Life => "-".to_string(),
                            UnitValues::Attack => "-".to_string(),
                            UnitValues::Energy => "-".to_string(),
                        },
                    }
                )),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 16.0,
                    ..default()
                },
            ),
            (
                Transform::from_translation(Vec3::new(0., -20., 2.)).with_scale(Vec3::splat(0.2)),
                Sprite::from_atlas_image(
                    handles.icons_image.clone(),
                    TextureAtlas {
                        layout: handles.icons_layout.clone(),
                        index: cost.values.sprite_index(),
                    },
                ),
            ),
            (
                Transform::from_translation(Vec3::Z * 1.).with_scale(Vec3::splat(0.25)),
                Sprite::from_atlas_image(
                    handles.icons_image.clone(),
                    TextureAtlas {
                        layout: handles.icons_layout.clone(),
                        index: 4,
                    },
                )
            ),
        ],
    )
}

fn ui_ability_cost_bundle(handles: &JamAssets, ability: CombatAbility) -> impl Bundle {
    let moved_handles: JamAssets = handles.clone();
    (
        Transform::from_translation(Vec3::new(100., 10., 0.)),
        Visibility::Visible,
        Children::spawn(SpawnWith(move |parent: &mut ChildSpawner| {
            let captured_handles = moved_handles;
            for (i, cost) in ability.costs.iter().enumerate() {
                parent.spawn(ui_ability_cost_single_bundle(&captured_handles, &cost, i));
            }
        })),
    )
}

fn ui_filled_ability_bundle(handles: &JamAssets, ability: CombatAbility) -> impl Bundle {
    (
        Transform::from_translation(Vec3::Z * 2.),
        Visibility::Visible,
        children![
            (
                Transform::from_translation(Vec3::new(-10., 0., 0.)),
                TextColor(Color::BLACK),
                Text2d::new(ability.describe()),
                TextLayout::new(JustifyText::Left, LineBreak::WordBoundary),
                TextBounds::from(Vec2::new(180., 50.)),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 12.,
                    line_height: bevy::text::LineHeight::RelativeToFont(0.8),
                    ..default()
                },
            ),
            ui_ability_cost_bundle(handles, ability),
            (
                Transform::from_scale(Vec3::splat(0.5)),
                Sprite::from_image(handles.ability_background_image.clone(),),
            ),
        ],
    )
}

fn ui_ability_bundle(handles: &JamAssets, index: usize) -> impl Bundle {
    (UiAbilityTracker {
        ability: None,
        index,
    },)
}

fn ui_ability_trackers(
    mut commands: Commands,
    mut trackers_query: Query<(Entity, &mut UiAbilityTracker)>,
    unit_query: Query<&Unit>,
    child_of_query: Query<&ChildOf>,
    handles: Res<JamAssets>,
) {
    for (tracker_entity, mut ui_tracker) in &mut trackers_query {
        if ui_tracker.ability.is_none() {
            if let Some(unit) = child_of_query
                .iter_ancestors(tracker_entity)
                .find_map(|e| unit_query.get(e).ok())
            {
                if let Some(ability) = unit.abilities.get(ui_tracker.index) {
                    let e = commands
                        .spawn(ui_filled_ability_bundle(&handles, ability.clone()))
                        .id();
                    commands.entity(tracker_entity).add_child(e);
                    ui_tracker.ability = Some(ability.clone());
                }
            } else {
                panic!("despawn");
            }
        }
    }
}

#[derive(Component, Debug, Clone)]
struct UiValueTracker {
    values: UnitValues,
    amount: Option<i32>,
    last_amount: Option<i32>,
    track_current: bool,
}
impl UiValueTracker {
    fn current(values: UnitValues) -> Self {
        Self {
            values,
            amount: None,
            last_amount: None,
            track_current: true,
        }
    }
    fn base(values: UnitValues) -> Self {
        Self {
            values,
            amount: None,
            last_amount: None,
            track_current: false,
        }
    }
}

fn ui_value_trackers(
    mut trackers_query: Query<(Entity, &mut UiValueTracker, &mut Text2d, &GlobalTransform)>,
    unit_query: Query<&Unit>,
    child_of_query: Query<&ChildOf>,
    mut commands: Commands,
    handles: Res<JamAssets>,
) {
    let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

    for (tracker_entity, mut ui_tracker, mut text, gtr) in &mut trackers_query {
        if let Some(unit) = child_of_query
            .iter_ancestors(tracker_entity)
            .find_map(|e| unit_query.get(e).ok())
        {
            ui_tracker.last_amount = ui_tracker.amount;
            let current = if ui_tracker.track_current {
                unit.value(&ui_tracker.values).current
            } else {
                unit.value(&ui_tracker.values).base
            };
            ui_tracker.amount = Some(current);

            text.0 = format!("{}", current);

            // spawn some particles
            if let (Some(amt), Some(last)) = (ui_tracker.amount, ui_tracker.last_amount) {
                if amt != last {
                    for _ in 0..(amt - last).abs() {
                        commands.spawn((
                            Transform::from_translation(gtr.translation())
                                .with_scale(Vec3::splat(0.2)),
                            Visibility::Visible,
                            particle_value_bundle(&handles, &ui_tracker.values, &mut rng),
                        ));
                    }
                }
            }
        } else {
            panic!("despawn");
        }
    }
}

#[derive(Component, Debug, Clone)]
struct Particle {
    velocity: Vec3,
    drag: f32,
    lifetime: Timer,
}

fn particle_value_bundle(
    handles: &JamAssets,
    values: &UnitValues,
    rng: &mut ChaCha8Rng,
) -> impl Bundle {
    (
        Particle {
            velocity: Vec3::new(
                rng.gen_range(-100.0..100.0),
                rng.gen_range(150.0..200.0),
                0.,
            ),
            drag: rng.gen_range(0.97..0.99),
            lifetime: Timer::new(Duration::from_millis(800), TimerMode::Once),
        },
        Sprite::from_atlas_image(
            handles.icons_image.clone(),
            TextureAtlas {
                layout: handles.icons_layout.clone(),
                index: values.sprite_index(),
            },
        ),
    )
}

fn update_particles(
    mut particles_query: Query<(Entity, &mut Particle, &mut Transform, &mut Sprite)>,
    mut commands: Commands,
    time: Res<Time>,
) {
    for (entity, mut particle, mut tr, mut sprite) in &mut particles_query {
        particle.lifetime.tick(time.delta());
        if particle.lifetime.finished() {
            commands.entity(entity).despawn();
            continue;
        }

        sprite.color.set_alpha(1. - particle.lifetime.fraction());

        tr.translation += particle.velocity * time.delta_secs();

        let drag = particle.drag;
        particle.velocity *= drag;
    }
}

#[derive(Component, Debug, Clone)]
struct UiNameTracker;

fn ui_name_trackers(
    mut trackers_query: Query<(Entity, &UiNameTracker, &mut Text2d)>,
    unit_query: Query<&Unit>,
    child_of_query: Query<&ChildOf>,
) {
    for (tracker_entity, _ui_tracker, mut text) in &mut trackers_query {
        if let Some(unit) = child_of_query
            .iter_ancestors(tracker_entity)
            .find_map(|e| unit_query.get(e).ok())
        {
            text.0 = unit.name.clone();
        } else {
            panic!("despawn");
        }
    }
}

fn unit_bundle(handles: &JamAssets, index: usize, unit: Unit) -> impl Bundle {
    (
        children![
            (
                Transform::from_translation(Vec3::new(-50., -100., 1.)),
                unit_value_icon_bundle(&handles, &UnitValues::Life),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(0., -100., 1.)),
                unit_value_icon_bundle(&handles, &UnitValues::Attack),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(50., -100., 1.)),
                unit_value_icon_bundle(&handles, &UnitValues::Energy),
                Visibility::Visible,
            ),
            (
                Transform::from_scale(Vec3::splat(0.7)),
                Sprite::from_atlas_image(
                    handles.units_image.clone(),
                    TextureAtlas {
                        layout: handles.units_layout.clone(),
                        index,
                    },
                ),
            ),
            (
                Transform::from_translation(Vec3::new(0., -200., 0.)),
                ui_ability_bundle(&handles, 0),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(0., -260., 0.)),
                ui_ability_bundle(&handles, 1),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(0., -320., 0.)),
                ui_ability_bundle(&handles, 2),
                Visibility::Visible,
            ),
            (
                Transform::from_scale(Vec3::splat(0.5))
                    .with_translation(Vec3::new(0., -160., -10.)),
                Sprite {
                    color: Color::BLACK,
                    ..Sprite::from_image(handles.unit_background_image.clone(),)
                }
            ),
            (
                Transform::from_translation(Vec3::new(0., 80., 2.)),
                Visibility::Visible,
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.5)),
                        Sprite::from_image(handles.unit_name_image.clone()),
                    ),
                    (
                        UiNameTracker,
                        TextColor(Color::BLACK),
                        Text2d::new(""),
                        TextFont {
                            font: handles.font.clone(),
                            font_size: 30.,
                            ..default()
                        },
                    )
                ]
            ),
        ],
        Visibility::Visible,
        unit,
    )
}

#[derive(Component, Debug, Clone)]
struct ActivatedAbilityFx {
    ability: ActivatedAbility,
    stack_height: usize,
}

fn stack_animation(
    state: Res<CombatState>,
    mut commands: Commands,
    fx_query: Query<(Entity, &ActivatedAbilityFx)>,
    units_query: Query<(Entity, &Unit, &Transform)>,
    mut gizmo_assets: ResMut<Assets<GizmoAsset>>,
) {
    for (h, activated_ability) in state.stack.iter().enumerate() {
        if let None = fx_query.iter().find(|(_, ab)| ab.stack_height == h) {
            // find source
            let Some((unit_entity, unit, source_tr)) = units_query
                .iter()
                .find(|(u, _, _)| u == &activated_ability.source)
            else {
                eprintln!("no source");
                continue;
            };

            for target in &activated_ability.targets {
                // find target
                let Some((unit_entity, unit, target_tr)) =
                    units_query.iter().find(|(u, _, _)| u == target)
                else {
                    eprintln!("no target");
                    continue;
                };

                let mut gizmo = GizmoAsset::new();
                gizmo
                    .arrow(
                        source_tr.translation,
                        target_tr.translation,
                        Color::srgb(1., 0., 0.),
                    )
                    .with_tip_length(5.);

                commands.spawn((
                    ActivatedAbilityFx {
                        ability: activated_ability.clone(),
                        stack_height: state.stack.len(),
                    },
                    Gizmo {
                        handle: gizmo_assets.add(gizmo),
                        line_config: GizmoLineConfig {
                            width: 5.,
                            ..default()
                        },
                        ..default()
                    },
                ));
            }
        }
    }

    for (entity, fx) in &fx_query {
        if fx.stack_height >= state.stack.len() {
            commands.entity(entity).despawn();
        }
    }
}

fn spawn_units(mut commands: Commands, handles: Res<JamAssets>) {
    commands.spawn((
        Camera2d,
        Projection::Orthographic(OrthographicProjection {
            scaling_mode: ScalingMode::FixedVertical {
                viewport_height: 720.,
            },
            ..OrthographicProjection::default_2d()
        }),
    ));

    commands.spawn((
        Transform::from_translation(Vec3::new(0., -100., 0.)).with_scale(Vec3::splat(0.5)),
        unit_bundle(
            &handles,
            2,
            Unit {
                name: format!("Lucius"),
                life: UnitValue::full(10),
                attack: UnitValue::full(1),
                energy: UnitValue::full(10),
                abilities: vec![
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::EnemyWithMost(UnitValues::Life),
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::UnitNegated(UnitValues::Attack),
                                values: UnitValues::Life,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::EnemyWithMost(UnitValues::Life),
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::Immediate(-1),
                                values: UnitValues::Life,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                ],
                owner: Owner::Player,
            },
        ),
    ));

    commands.spawn((
        Transform::from_translation(Vec3::new(120., -100., 0.)).with_scale(Vec3::splat(0.5)),
        unit_bundle(
            &handles,
            0,
            Unit {
                name: format!("Serral"),
                life: UnitValue::full(10),
                attack: UnitValue::full(1),
                energy: UnitValue::full(10),
                abilities: vec![
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::EnemyWithMost(UnitValues::Life),
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::UnitNegated(UnitValues::Attack),
                                values: UnitValues::Life,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::EnemyWithMost(UnitValues::Life),
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::Immediate(-1),
                                values: UnitValues::Life,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                ],
                owner: Owner::Player,
            },
        ),
    ));

    commands.spawn((
        Transform::from_translation(Vec3::new(0., 250., 0.)).with_scale(Vec3::splat(0.5)),
        unit_bundle(
            &handles,
            1,
            Unit {
                name: format!("Dane"),
                life: UnitValue::full(10),
                attack: UnitValue::full(10),
                energy: UnitValue::full(10),
                abilities: vec![
                    // cost 2, gain 1 attack
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::This,
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::Immediate(1),
                                values: UnitValues::Attack,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                    // cost 2, enemy with most life loses 1 life
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::EnemyWithMost(UnitValues::Life),
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::Immediate(-1),
                                values: UnitValues::Life,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                    // cost 2, enemy with most life loses 1 life
                    CombatAbility {
                        effects: vec![CombatEffect {
                            target: CombatTarget::EnemyWithMost(UnitValues::Life),
                            gains: vec![CombatNumber {
                                source: CombatNumberSource::Immediate(-1),
                                values: UnitValues::Life,
                            }],
                        }],
                        costs: vec![CombatNumber {
                            source: CombatNumberSource::Immediate(2),
                            values: UnitValues::Energy,
                        }],
                        trigger: None,
                    },
                ],
                owner: Owner::Enemy,
            },
        ),
    ));
}
