use rand::seq::IteratorRandom;

use crate::*;

pub struct CombatPlugin;
impl Plugin for CombatPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(CombatState::default());
        app.insert_resource(AutoStepper {
            timer: Timer::new(Duration::from_millis(500), TimerMode::Repeating),
            slider_value: 0.,
        });

        app.add_systems(
            OnEnter(GameStates::Combat),
            (
                setup_combo_animation,
                spawn_units,
                spawn_gold,
                spawn_turn_counter,
                reset_combat,
                spawn_auto_step_controls,
            ),
        );

        app.add_systems(
            Update,
            (
                combat_next_step,
                combat_remove_dead_units,
                stack_animation,
                detect_end,
            )
                .chain()
                .run_if(on_event::<OnCombatStep>)
                .run_if(in_state(GameStates::Combat)),
        );
        app.add_systems(
            Update,
            (
                manual_combat_step,
                update_combo_animation,
                update_particles,
                end_combat_sequence,
                auto_step,
                update_ui_turn_speed_trackers,
            )
                .run_if(in_state(GameStates::Combat)),
        );
    }
}

#[derive(Component)]
struct UiTurnTracker;

pub fn spawn_turn_counter(mut commands: Commands, handles: Res<JamAssets>) {
    commands.spawn((
        DestroyBetweenStates,
        Transform::from_translation(Vec3::new(-500., 240., 200.)),
        Visibility::Visible,
        children![
            (
                Transform::from_scale(Vec3::splat(0.45)).with_translation(Vec3::new(0., 0., -1.)),
                Sprite::from_image(handles.combat_back_image.clone()),
            ),
            (
                Transform::from_translation(Vec3::new(0., 30., 2.)),
                TextColor(Color::WHITE),
                Text2d::new("Turn"),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 30.0,
                    ..default()
                },
            ),
            (
                Transform::from_translation(Vec3::new(0., -30., 2.)),
                TextColor(Color::WHITE),
                Text2d::new("0/100"),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 30.0,
                    ..default()
                },
                UiTurnTracker,
            )
        ],
    ));
}

#[derive(Resource)]
struct AutoStepper {
    timer: Timer,
    slider_value: f32,
}

#[derive(Component)]
struct AutoStepperSlider {
    min: f32,
    max: f32,
}

fn on_slider_drag(
    trigger: Trigger<Pointer<Drag>>,
    mut query: Query<(&AutoStepperSlider, &mut Transform)>,
    mut stepper: ResMut<AutoStepper>,
) {
    if let Ok((slider, mut tr)) = query.single_mut() {
        stepper.slider_value =
            (stepper.slider_value - trigger.delta.y).clamp(slider.min, slider.max);
        tr.translation.y = stepper.slider_value;
        let amp = slider.max - slider.min;
        let t = (stepper.slider_value - slider.min) / amp;
        if t < 0.1 {
            stepper.timer.pause()
        } else {
            stepper.timer.unpause();
            stepper
                .timer
                .set_duration(Duration::from_secs_f32((1.01 - t).max(0.016)));
        }
    }
}

fn update_ui_turn_speed_trackers(
    mut turn_trackers_query: Query<&mut Text2d, With<UiTurnTracker>>,
    mut speed_trackers_query: Query<&mut Text2d, (With<UiSpeedTracker>, Without<UiTurnTracker>)>,
    state: Res<CombatState>,
    stepper: Res<AutoStepper>,
) {
    for mut text in &mut turn_trackers_query {
        text.0 = format!("{}/100", state.turn_number);
    }
    for mut text in &mut speed_trackers_query {
        if stepper.timer.paused() {
            text.0 = format!("Paused");
        } else {
            text.0 = format!("{}ms", stepper.timer.duration().as_millis());
        }
    }
}

#[derive(Component)]
struct UiSpeedTracker;

fn spawn_auto_step_controls(
    mut commands: Commands,
    handles: Res<JamAssets>,
    stepper: Res<AutoStepper>,
) {
    commands.spawn((
        DestroyBetweenStates,
        Transform::from_translation(Vec3::new(-500., 0., 200.)),
        Visibility::Visible,
        children![
            (
                Transform::from_scale(Vec3::splat(0.45)).with_translation(Vec3::new(0., 0., -1.)),
                Sprite::from_image(handles.combat_back_image.clone()),
            ),
            (
                Transform::from_translation(Vec3::new(0., 50., 0.)),
                TextColor(Color::WHITE),
                Text2d::new("Speed"),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 30.0,
                    ..default()
                },
            ),
            (
                Transform::from_translation(Vec3::new(0., 20., 0.)),
                TextColor(Color::WHITE),
                Text2d::new("30ms"),
                UiSpeedTracker,
                TextFont {
                    font: handles.font.clone(),
                    font_size: 20.0,
                    ..default()
                },
            ),
            (
                Transform::from_translation(Vec3::new(20., -70., 2.)),
                TextColor(Color::srgb(0.5, 0.5, 0.5)),
                TextLayout::new(JustifyText::Left, LineBreak::WordBoundary),
                TextBounds::from(Vec2::new(120., 140.)),
                Text2d::new("Move the handle to set the speed.\n\nBackspace: step once\nSpace: pause\nEnter: max speed!"),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 10.0,
                    ..default()
                },
            )
        ],
    ));

    commands.spawn((
        DestroyBetweenStates,
        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::new(-580., 100., 200.)),
        Visibility::Visible,
        children![
            (
                Transform::from_translation(Vec3::Z),
                Sprite {
                    color: Color::BLACK,
                    ..Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 4,
                        },
                    )
                },
            ),
            (
                Transform::from_translation(Vec3::Z * 2.),
                Sprite {
                    color: Color::WHITE,
                    ..Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 9,
                        },
                    )
                },
            )
        ],
    ));
    commands.spawn((
        DestroyBetweenStates,
        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::new(-580., -0., 200.)),
        Visibility::Visible,
        children![(
            Transform::from_translation(Vec3::ZERO),
            Sprite {
                color: Color::WHITE,
                ..Sprite::from_image(handles.speed_dial_image.clone())
            },
        )],
    ));
    commands
        .spawn((
            DestroyBetweenStates,
            Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::new(
                -580.,
                stepper.slider_value,
                201.,
            )),
            Visibility::Visible,
            ButtonColorTarget,
            AutoStepperSlider {
                min: -60.,
                max: 60.,
            },
            Pickable::default(),
            Sprite {
                color: Color::WHITE,
                ..Sprite::from_atlas_image(
                    handles.icons_image.clone(),
                    TextureAtlas {
                        layout: handles.icons_layout.clone(),
                        index: 10,
                    },
                )
            },
        ))
        .observe(on_over_color)
        .observe(on_out_color)
        .observe(on_slider_drag);
    commands.spawn((
        DestroyBetweenStates,
        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::new(-580., -100., 200.)),
        Visibility::Visible,
        children![
            (
                Transform::from_translation(Vec3::Z),
                Sprite {
                    color: Color::BLACK,
                    ..Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 4,
                        },
                    )
                },
            ),
            (
                Transform::from_translation(Vec3::Z * 2.),
                Sprite {
                    color: Color::WHITE,
                    ..Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 8,
                        },
                    )
                },
            )
        ],
    ));
}

fn auto_step(
    mut event: EventWriter<OnCombatStep>,
    mut stepper: ResMut<AutoStepper>,
    time: Res<Time>,
) {
    stepper.timer.tick(time.delta());
    if stepper.timer.finished() {
        event.write(OnCombatStep);
    }
}

fn reset_combat(mut state: ResMut<CombatState>) {
    *state = CombatState::default();
}

fn manual_combat_step(
    keys: Res<ButtonInput<KeyCode>>,
    mut stepper: ResMut<AutoStepper>,
    mut event: EventWriter<OnCombatStep>,
    mut query: Query<(&AutoStepperSlider, &mut Transform)>,
) {
    if let Ok((slider, mut tr)) = query.single_mut() {
        tr.translation.y = stepper.slider_value;
        if keys.just_pressed(KeyCode::Space) {
            stepper.slider_value = -80.;
            let amp = slider.max - slider.min;
            let t = (stepper.slider_value - slider.min) / amp;
            if t < 0.1 {
                stepper.timer.pause()
            } else {
                stepper.timer.unpause();
                stepper
                    .timer
                    .set_duration(Duration::from_secs_f32(1.01 - t));
            }
        }
        if keys.pressed(KeyCode::Enter) {
            stepper.slider_value = 80.;
            let amp = slider.max - slider.min;
            let t = (stepper.slider_value - slider.min) / amp;
            if t < 0.1 {
                stepper.timer.pause()
            } else {
                stepper.timer.unpause();
                stepper
                    .timer
                    .set_duration(Duration::from_secs_f32(1.01 - t));
            }
        }
        if keys.just_pressed(KeyCode::Backspace) {
            event.write(OnCombatStep);
        }
    }
}

#[derive(Component, Clone, Debug)]
pub struct Unit {
    pub name: String,
    pub life: UnitValue,
    pub attack: UnitValue,
    pub energy: UnitValue,
    pub abilities: Vec<CombatAbility>,
    pub owner: Owner,
    pub sprite_index: usize,
    pub level: i32,
    pub experience: i32,
    pub max_experience: i32,
}

impl Default for Unit {
    fn default() -> Self {
        Self {
            name: format!("Nameless"),
            life: UnitValue::full(1),
            attack: UnitValue::full(1),
            energy: UnitValue::full(1),
            abilities: vec![],
            owner: Owner::Enemy,
            sprite_index: 0,
            level: 1,
            experience: 0,
            max_experience: 1,
        }
    }
}

impl Unit {
    pub fn value(&self, valuetype: &UnitValues) -> UnitValue {
        match valuetype {
            UnitValues::Life => self.life.clone(),
            UnitValues::Attack => self.attack.clone(),
            UnitValues::Energy => self.energy.clone(),
        }
    }

    pub fn value_mut(&mut self, valuetype: &UnitValues) -> &mut UnitValue {
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
pub enum UnitValues {
    Life,
    Attack,
    Energy,
}

impl UnitValues {
    pub fn sprite_index(&self) -> usize {
        match self {
            UnitValues::Life => 0,
            UnitValues::Attack => 1,
            UnitValues::Energy => 2,
        }
    }
    pub fn describe(&self) -> String {
        match &self {
            UnitValues::Life => format!("life"),
            UnitValues::Attack => format!("attack"),
            UnitValues::Energy => format!("energy"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CombatTarget {
    All,
    This,
    AllOther,
    AllEnemy,
    AllAlly,
    AllOtherAlly,
    EnemyWithMost(UnitValues),
    EnemyWithLeast(UnitValues),
    AllyWithMost(UnitValues),
    AllyWithLeast(UnitValues),
    Specific(Entity),
    AbilitySource,
    AbilityTarget,
}

impl CombatTarget {
    pub fn describe(&self, is_trigger: bool) -> String {
        let mut s = match self {
            CombatTarget::All => format!("any"),
            CombatTarget::This => format!("this"),
            CombatTarget::AllOther => format!("any other"),
            CombatTarget::AllEnemy => format!("any enemy"),
            CombatTarget::AllAlly => format!("any ally"),
            CombatTarget::AllOtherAlly => format!("any other ally"),
            CombatTarget::EnemyWithMost(v) => format!("the enemy with the most {}", v.describe()),
            CombatTarget::EnemyWithLeast(v) => format!("the enemy with the least {}", v.describe()),
            CombatTarget::AllyWithMost(v) => format!("the ally with the most {}", v.describe()),
            CombatTarget::AllyWithLeast(v) => format!("the ally with the least {}", v.describe()),
            CombatTarget::AbilitySource => format!("the targeter"),
            CombatTarget::AbilityTarget => format!("the target"),
            CombatTarget::Specific(_) => format!("that unit"),
        };
        if !is_trigger {
            s = s.replace("any", "all");
        }
        s
    }

    pub fn get(
        &self,
        (source_entity, source_unit): (Entity, &Unit),
        activated_ability: Option<&ActivatedAbility>,
        units: &Vec<(Entity, &Unit)>,
    ) -> Vec<Entity> {
        match self {
            CombatTarget::AbilitySource => activated_ability.iter().map(|ab| ab.source).collect(),
            CombatTarget::AbilityTarget => activated_ability
                .iter()
                .map(|ab| ab.targets.clone())
                .flatten()
                .collect(),
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
            CombatTarget::AllOther => units
                .iter()
                .filter_map(|(e, _)| (e != &source_entity).then_some(e))
                .cloned()
                .collect(),
            CombatTarget::AllOtherAlly => units
                .iter()
                .filter(|(e, _)| e != &source_entity)
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
pub enum CombatNumberSource {
    Immediate(i32),
    Unit(UnitValues),
    UnitNegated(UnitValues),
}
impl CombatNumberSource {
    pub fn solve(&self, unit: &Unit) -> i32 {
        match &self {
            Self::Immediate(amt) => *amt,
            Self::Unit(v) => unit.value(&v).current,
            Self::UnitNegated(v) => -unit.value(&v).current,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CombatNumber {
    pub source: CombatNumberSource,
    pub values: UnitValues,
}

impl CombatNumber {
    pub fn describe_short(&self) -> String {
        match &self.source {
            CombatNumberSource::Immediate(amt) => {
                format!("{} {}", amt, self.values.describe())
            }
            _ => format!("n/a"),
        }
    }

    pub fn describe(&self) -> String {
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
pub struct CombatEffect {
    pub target: CombatTarget,
    pub gains: Vec<CombatNumber>,
}

impl CombatEffect {
    pub fn describe(&self) -> String {
        let mut s = format!("{} ", self.target.describe(false));
        for (i, gain) in self.gains.iter().enumerate() {
            let comma = if i < self.gains.len() - 1 { ", " } else { "" };
            s = format!("{}{}{}", s, gain.describe(), comma);
        }
        s
    }
}

#[derive(Clone, Debug)]
pub enum CombatTriggerWatch {
    Targeted,
    ValueIncrease(UnitValues),
    ValueDecrease(UnitValues),
}

impl CombatTriggerWatch {
    pub fn describe(&self) -> String {
        match &self {
            Self::Targeted => format!("is targeted"),
            Self::ValueIncrease(unit_values) => format!("gains {}", unit_values.describe()),
            Self::ValueDecrease(unit_values) => format!("loses {}", unit_values.describe()),
        }
    }
}

impl CombatTriggerWatch {
    pub fn check(&self, activated_ability: &ActivatedAbility, source: &Unit) -> bool {
        match self {
            Self::Targeted => true,
            Self::ValueIncrease(v) => activated_ability
                .gains
                .iter()
                .any(|g| &g.values == v && g.source.solve(source) > 0),
            Self::ValueDecrease(v) => activated_ability
                .gains
                .iter()
                .any(|g| &g.values == v && g.source.solve(source) < 0),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CombatTrigger {
    pub target: CombatTarget,
    pub watch: CombatTriggerWatch,
}

impl CombatTrigger {
    pub fn describe(&self) -> String {
        format!(
            "when {} {}",
            self.target.describe(true),
            self.watch.describe()
        )
    }
}

#[derive(Clone, Debug)]
pub struct CombatAbility {
    pub trigger: Option<CombatTrigger>,
    pub effects: Vec<CombatEffect>,
    pub costs: Vec<CombatNumber>,
    // activation_contitions: Vec<CombatCondition>
}

impl CombatAbility {
    pub fn describe(&self) -> String {
        let mut s = String::new();
        if let Some(trigger) = &self.trigger {
            s = format!("{}{}: ", s, trigger.describe());
        }
        for (i, effect) in self.effects.iter().enumerate() {
            let comma = if i < self.effects.len() - 1 { ", " } else { "" };
            s = format!("{}{}{}", s, effect.describe(), comma);
        }
        if !s.is_empty() {
            s = s.remove(0).to_uppercase().to_string() + &s;
        }
        s
    }
}

#[derive(Clone, Debug)]
pub struct ActivatedAbility {
    pub source: Entity,
    pub targets: Vec<Entity>,
    pub gains: Vec<CombatNumber>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Owner {
    Player,
    Enemy,
}

#[derive(Clone, Debug)]
pub struct UnitValue {
    pub current: i32,
    pub base: i32,
}
impl UnitValue {
    pub fn full(f: i32) -> Self {
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

#[derive(Resource, Default, Debug, Clone)]
pub struct CombatState {
    pub turn_number: u32,
    pub turn_owner: Option<Owner>,
    pub pass_ordering: Vec<Entity>,
    pub pass_current_unit: usize,
    pub pass_current_ability: usize,
    pub pass_no_more_abilities: bool,
    pub response_ordering: Vec<Entity>,
    pub response_current_unit: usize,
    pub response_current_ability: usize,
    pub response_no_more_abilities: bool,
    pub response_already_responded: Vec<(Entity, usize)>,
    pub stack_height: usize,
    pub stack: Vec<ActivatedAbility>,
}

#[derive(Event)]
pub struct OnCombatStep;

pub const MAX_ITERS: i32 = 1000;

pub fn combat_remove_dead_units(mut commands: Commands, units_query: Query<(Entity, &Unit)>) {
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
pub fn combat_next_step(
    mut state: ResMut<CombatState>,
    mut units_query: Query<(Entity, &mut Unit)>,
) {
    let draw = state.turn_number > 100;
    if draw {
        return;
    }

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
        state.turn_number = 0;
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
                            let current = target_unit.value_mut(&gain.values).current;
                            target_unit.value_mut(&gain.values).current =
                                (current + gain.source.solve(&unit)).max(0);
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
    state.turn_number += 1;

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
        let targets = effect.target.get((unit_entity, unit), None, &units);
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
        state.response_current_unit = 0;
        state.response_current_ability = 0;
        state.response_no_more_abilities = true;
        state.response_already_responded.clear();
        if state.response_no_more_abilities {
            state.stack_height += 1;
            debug!(target: "abilities", "no more responses, walking up: {}", state.stack_height);
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

    if state
        .response_already_responded
        .contains(&(unit_entity, state.response_current_ability))
    {
        debug!(target: "abilities", "already responded");
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

    let mut unit = unit.clone();

    // activate the trigger
    let watched_targets =
        trigger
            .target
            .get((unit_entity, &unit), Some(&activated_ability), &units);
    for target in watched_targets {
        if !activated_ability.targets.contains(&target) {
            continue;
        }

        if units.iter().find(|(e, _)| *e == target).is_none() {
            continue;
        };

        if trigger.watch.check(&activated_ability, &unit) {
            // check if all costs can be paid
            if !ability
                .costs
                .iter()
                .all(|cost| unit.value(&cost.values).current >= cost.source.solve(&unit))
            {
                debug!(target: "abilities", "unit {} can't pay {:?}", unit.to_string(), ability.costs);
                break;
            }

            for effect in &ability.effects {
                any_activation = true;
                let targets =
                    effect
                        .target
                        .get((unit_entity, &unit), Some(&activated_ability), &units);
                let act = ActivatedAbility {
                    source: unit_entity,
                    targets,
                    gains: effect.gains.clone(),
                };
                debug!(target: "abilities", "activated {:?}, stack height: {}", act, state.stack.len());
                state.stack.push(act);
            }

            for cost in &ability.costs {
                unit.value_mut(&cost.values).current -= cost.source.solve(&unit);
            }

            assert!(unit.energy.current >= 0);

            // pay here
        }
    }

    if any_activation {
        state
            .response_already_responded
            .push((unit_entity, state.response_current_ability));
        state.response_no_more_abilities = false;
        let unit = unit.clone();
        if let Ok((_, mut unit_mut)) = units_query.get_mut(unit_entity) {
            *unit_mut = unit;
        };
        return true;
    }
    state.response_current_ability += 1;

    return false;
}

#[derive(Component)]
pub struct UiExperienceTracker;

pub fn ui_experience_trackers(
    mut trackers_query: Query<(Entity, &UiExperienceTracker, &mut Transform)>,
    unit_query: Query<&Unit>,
    child_of_query: Query<&ChildOf>,
) {
    for (tracker_entity, _, mut tr) in &mut trackers_query {
        if let Some(unit) = child_of_query
            .iter_ancestors(tracker_entity)
            .find_map(|e| unit_query.get(e).ok())
        {
            let f = unit.experience as f32 / unit.max_experience as f32;
            tr.scale.x = f;
        } else {
            panic!("despawn");
        }
    }
}

fn unit_experience_bundle(handles: &JamAssets) -> impl Bundle {
    (children![
        (
            Transform::from_scale(Vec3::new(0.8, 1.2, 1.)).with_translation(Vec3::new(0., 0., 0.9)),
            Sprite {
                color: Color::srgb(0., 0., 0.),
                ..Sprite::from_image(handles.exp_bar_image.clone(),)
            },
        ),
        (
            Transform::from_translation(Vec3::new(-70., 0., 0.)),
            Visibility::Visible,
            UiExperienceTracker,
            children![(
                Transform::from_scale(Vec3::new(0.75, 0.8, 1.))
                    .with_translation(Vec3::new(70., 0., 1.)),
                Sprite {
                    color: Color::srgb(0.2, 0.8, 0.),
                    ..Sprite::from_image(handles.exp_bar_image.clone(),)
                }
            )]
        ),
    ],)
}

fn unit_level_icon_bundle(handles: &JamAssets) -> impl Bundle {
    (children![
        (
            Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::new(0., -35., 3.)),
            Sprite::from_atlas_image(
                handles.icons_image.clone(),
                TextureAtlas {
                    layout: handles.icons_layout.clone(),
                    index: 3,
                },
            ),
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
                    UiLevelTracker {
                        last_amount: None,
                        amount: None
                    }
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

#[derive(Component)]
pub struct UiLevelTracker {
    last_amount: Option<i32>,
    amount: Option<i32>,
}

pub fn ui_level_trackers(
    mut trackers_query: Query<(Entity, &mut UiLevelTracker, &mut Text2d, &GlobalTransform)>,
    unit_query: Query<&Unit>,
    child_of_query: Query<&ChildOf>,
    mut commands: Commands,
    handles: Res<JamAssets>,
) {
    for (tracker_entity, mut ui_tracker, mut text, gtr) in &mut trackers_query {
        if let Some(unit) = child_of_query
            .iter_ancestors(tracker_entity)
            .find_map(|e| unit_query.get(e).ok())
        {
            ui_tracker.last_amount = ui_tracker.amount;
            ui_tracker.amount = Some(unit.level);

            text.0 = format!("{}", unit.level);

            // spawn the level particle
            if let (Some(amt), Some(last)) = (ui_tracker.amount, ui_tracker.last_amount) {
                if amt != last {
                    for _ in 0..(amt - last).abs() {
                        commands.spawn((
                            DestroyBetweenStates,
                            Transform::from_translation(gtr.translation() + Vec3::X * 100.),
                            Visibility::Visible,
                            particle_level_bundle(&handles),
                        ));
                    }
                }
            }
        } else {
            panic!("despawn");
        }
    }
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
pub struct UiAbilityTracker {
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
                parent.spawn((
                    DestroyBetweenStates,
                    ui_ability_cost_single_bundle(&captured_handles, &cost, i),
                ));
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

fn ui_ability_bundle(index: usize) -> impl Bundle {
    (UiAbilityTracker {
        ability: None,
        index,
    },)
}

#[derive(Component)]
pub struct AbilityZoomed;
#[derive(Component)]
pub struct AbilityZoomedText;

#[derive(Component)]
pub struct AbilityZoom {
    pub ability: CombatAbility,
}

pub fn spawn_ability_zoomed(mut commands: Commands, handles: Res<JamAssets>) {
    commands.spawn((
        Transform::from_translation(Vec3::Z * 2.),
        Visibility::Hidden,
        AbilityZoomed,
        children![
            (
                Visibility::Inherited,
                AbilityZoomedText,
                Transform::from_translation(Vec3::new(0., 0., 0.)),
                TextColor(Color::BLACK),
                Text2d::new(""),
                TextLayout::new(JustifyText::Left, LineBreak::WordBoundary),
                TextBounds::from(Vec2::new(280., 80.)),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 16.,
                    ..default()
                },
            ),
            (
                Visibility::Inherited,
                Transform::from_scale(Vec3::splat(0.8)),
                Sprite::from_image(handles.ability_background_image.clone(),),
            ),
        ],
    ));
}

pub fn update_ability_zoom(
    zoom_query: Query<(&GlobalTransform, &AbilityZoom)>,
    mut zoomed_query: Query<(Entity, &mut Transform, &AbilityZoomed)>,
    mut zoomed_text_query: Query<(&mut Text2d, &AbilityZoomedText)>,
    mut commands: Commands,
) {
    let Ok((entity, mut tr, _)) = zoomed_query.single_mut() else {
        return;
    };

    if let Ok((gtr, zoom)) = zoom_query.single() {
        commands.entity(entity).insert(Visibility::Visible);

        // show bigger ability
        let Ok((mut text, _)) = zoomed_text_query.single_mut() else {
            return;
        };

        tr.translation = gtr.translation() + Vec3::Z * 1.1;

        let mut costs = String::new();
        for cost in &zoom.ability.costs {
            costs += &(cost.describe_short() + " ");
        }
        if costs.is_empty() {
            costs = format!("Nothing");
        }

        text.0 = format!("Activate by paying: {}\n{}", costs, zoom.ability.describe());
    } else {
        commands.entity(entity).insert(Visibility::Hidden);
    }
}

pub fn ui_ability_trackers(
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
                        .spawn((ui_filled_ability_bundle(&handles, ability.clone()),))
                        .id();
                    commands.entity(tracker_entity).add_child(e);

                    let cloned_ability = ability.clone();
                    let e = commands
                        .spawn((
                            Pickable {
                                should_block_lower: false,
                                is_hoverable: true,
                            },
                            Sprite::from_color(Color::WHITE.with_alpha(0.0), Vec2::new(180., 50.)),
                            Transform::from_translation(Vec3::Z * 10.),
                        ))
                        .observe(
                            move |trigger: Trigger<Pointer<Over>>, mut commands: Commands| {
                                commands.entity(trigger.target()).insert(AbilityZoom {
                                    ability: cloned_ability.clone(),
                                });
                            },
                        )
                        .observe(|trigger: Trigger<Pointer<Out>>, mut commands: Commands| {
                            commands.entity(trigger.target()).remove::<AbilityZoom>();
                        })
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
pub struct UiValueTracker {
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

pub fn ui_value_trackers(
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
                    let sign = (amt - last).signum();
                    for _ in 0..(amt - last).abs() {
                        commands.spawn((
                            DestroyBetweenStates,
                            Transform::from_translation(gtr.translation())
                                .with_scale(Vec3::splat(0.2)),
                            Visibility::Visible,
                            particle_value_bundle(
                                &handles,
                                &mut rng,
                                sign,
                                ui_tracker.values.sprite_index(),
                            ),
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
pub struct Particle {
    pub velocity: Vec3,
    pub drag: f32,
    pub lifetime: Timer,
    pub starting_alpha: f32,
    pub half_alpha: f32,
}

pub fn particle_level_bundle(handles: &JamAssets) -> impl Bundle {
    (
        Particle {
            velocity: Vec3::new(0., 100., 0.),
            drag: 0.99,
            lifetime: Timer::new(Duration::from_millis(1000), TimerMode::Once),
            starting_alpha: 1.0,
            half_alpha: 0.5,
        },
        children![
            (
                Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::new(-100., 0., 0.1)),
                Sprite::from_atlas_image(
                    handles.icons_image.clone(),
                    TextureAtlas {
                        layout: handles.icons_layout.clone(),
                        index: 3,
                    },
                ),
            ),
            (
                Transform::from_scale(Vec3::splat(0.4)),
                Sprite::from_image(handles.ability_background_image.clone()),
            ),
            (
                TextColor(Color::BLACK),
                Text2d::new("level up!"),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 25.,
                    ..default()
                },
            )
        ],
    )
}

pub fn particle_value_bundle(
    handles: &JamAssets,
    rng: &mut ChaCha8Rng,
    sign: i32,
    index: usize,
) -> impl Bundle {
    (
        Particle {
            velocity: Vec3::new(
                rng.gen_range(-100.0..100.0),
                rng.gen_range(150.0..200.0) * sign as f32,
                0.,
            ),
            drag: rng.gen_range(0.97..0.99),
            lifetime: Timer::new(Duration::from_millis(800), TimerMode::Once),
            starting_alpha: 1.0,
            half_alpha: 0.5,
        },
        Sprite::from_atlas_image(
            handles.icons_image.clone(),
            TextureAtlas {
                layout: handles.icons_layout.clone(),
                index,
            },
        ),
    )
}

pub fn update_particles(
    mut particles_query: Query<(Entity, &mut Particle, &mut Transform, Option<&mut Sprite>)>,
    mut commands: Commands,
    time: Res<Time>,
    handles: Res<JamAssets>,
) {
    for (entity, mut particle, mut tr, sprite) in &mut particles_query {
        particle.lifetime.tick(time.delta());
        if particle.lifetime.finished() {
            commands.entity(entity).despawn();

            // level up fest
            if let None = sprite {
                let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();
                for _ in 0..20 {
                    let pos = Vec3::new(
                        rng.gen_range(-100.0..100.0),
                        rng.gen_range(-50.0..50.0),
                        0.0,
                    );
                    commands.spawn((
                        DestroyBetweenStates,
                        Transform::from_translation(tr.translation + pos)
                            .with_scale(Vec3::splat(0.2)),
                        Visibility::Visible,
                        particle_value_bundle(&handles, &mut rng, 1, 3),
                    ));
                }
            }
            continue;
        }

        if let Some(mut sprite) = sprite {
            let t = particle.lifetime.fraction();
            let interp = particle.starting_alpha * (1. - t)
                + particle.half_alpha * (1. - (t * 2. - 1.).abs());
            sprite.color.set_alpha(interp);
        }

        tr.translation += particle.velocity * time.delta_secs();

        let drag = particle.drag;
        particle.velocity *= drag;
    }
}

#[derive(Component, Debug, Clone)]
pub struct UiNameTracker;

pub fn ui_name_trackers(
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

pub fn unit_bundle(handles: &JamAssets, index: usize, unit: Unit) -> impl Bundle {
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
                Transform::from_translation(Vec3::new(-100., 75., 1.)),
                unit_level_icon_bundle(&handles),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(0., 62., 1.)),
                unit_experience_bundle(&handles),
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
                ui_ability_bundle(0),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(0., -260., 0.)),
                ui_ability_bundle(1),
                Visibility::Visible,
            ),
            (
                Transform::from_translation(Vec3::new(0., -320., 0.)),
                ui_ability_bundle(2),
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
struct ComboFx;
#[derive(Component, Debug, Clone)]
struct ComboTextFx;

fn setup_combo_animation(mut commands: Commands, handles: Res<JamAssets>) {
    commands.spawn((
        DestroyBetweenStates,
        Transform::default(),
        Visibility::Visible,
        ComboFx,
        children![
            (
                Transform::from_scale(Vec3::splat(0.4)),
                Sprite::from_image(handles.ability_background_image.clone()),
            ),
            (
                ComboTextFx,
                TextColor(Color::BLACK),
                Text2d::new(""),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 30.,
                    ..default()
                },
            )
        ],
    ));
}

fn update_combo_animation(
    mut commands: Commands,
    mut query_text: Query<(&ComboTextFx, &mut Text2d)>,
    mut query_fx: Query<(Entity, &mut Transform), With<ComboFx>>,
    state: Res<CombatState>,
) {
    if let Ok((fx, mut tr)) = query_fx.single_mut() {
        if state.stack.len() > 1 {
            commands.entity(fx).insert(Visibility::Visible);
        } else {
            commands.entity(fx).insert(Visibility::Hidden);
        }
        for (_, mut text) in &mut query_text {
            text.0 = format!("{} combo!", state.stack.len());
        }
        tr.scale = Vec3::splat(1. + 0.01 * state.stack.len() as f32);
    }
}

#[derive(Component, Debug, Clone)]
struct ActivatedAbilityFx {
    stack_height: usize,
}

fn stack_animation(
    state: Res<CombatState>,
    mut commands: Commands,
    fx_query: Query<(Entity, &ActivatedAbilityFx)>,
    units_query: Query<(Entity, &Unit, &Transform)>,
    handles: Res<JamAssets>,
    muted: Res<SoundMuted>,
) {
    for (h, activated_ability) in state.stack.iter().enumerate() {
        if let None = fx_query.iter().find(|(_, ab)| ab.stack_height == h) {
            // find source
            let Some((_, unit, source_tr)) = units_query
                .iter()
                .find(|(u, _, _)| u == &activated_ability.source)
            else {
                continue;
            };

            if h > 2 {
                play_sound_speed(
                    &mut commands,
                    &handles.stack_up_sound,
                    0.5 + 0.1 * h as f32,
                    &muted,
                );
            }

            for target in &activated_ability.targets {
                // find target
                let Some((_, _, target_tr)) = units_query.iter().find(|(u, _, _)| u == target)
                else {
                    continue;
                };

                let src = source_tr.translation.xy();
                let dst = target_tr.translation.xy();
                let arcing = 100. + 10. * (h % 10) as f32 + 3. * (h / 10) as f32;
                let mut t1 = (src + src + dst) / 3.;
                t1 += t1.normalize_or_zero() * arcing;
                let mut t2 = (src + dst + dst) / 3.;
                t2 += t2.normalize_or_zero() * arcing;

                if src == dst {
                    let axis = (src - t1).normalize_or_zero();
                    t1 += axis.perp() * 100.;
                    t2 -= axis.perp() * 100.;
                }

                let points = [[src, t1, t2, dst]];

                let segments_len = points[0]
                    .windows(2)
                    .map(|v| v[0].distance(v[1]))
                    .sum::<f32>();

                let segments_num = (segments_len / 45.) as usize;

                let bezier = CubicBezier::new(points).to_curve().unwrap();
                let positions: Vec<_> = bezier.iter_positions(segments_num).collect();
                let velocities: Vec<_> = bezier.iter_velocities(segments_num).collect();

                let fx = commands
                    .spawn((
                        DestroyBetweenStates,
                        Transform::default(),
                        Visibility::Visible,
                        ActivatedAbilityFx { stack_height: h },
                    ))
                    .id();

                let thirds: Vec<_> = bezier.iter_positions(3).collect();
                for (i, gain) in activated_ability.gains.iter().enumerate() {
                    let sg = commands
                        .spawn((
                            Transform::from_translation(
                                thirds[2].extend(30.3 + h as f32)
                                    + Vec3::new(30. * i as f32, 0., 0.),
                            ),
                            Visibility::Visible,
                            children![
                                (
                                    Transform::from_translation(Vec3::Z * 0.2),
                                    TextColor(Color::BLACK),
                                    Text2d::new(format!("{}", gain.source.solve(unit))),
                                    TextFont {
                                        font: handles.font.clone(),
                                        font_size: 25.,
                                        ..default()
                                    },
                                ),
                                (
                                    Transform::from_translation(Vec3::new(20., -20., 0.2)),
                                    TextColor(Color::BLACK),
                                    Text2d::new(format!("{}", h)),
                                    TextFont {
                                        font: handles.font.clone(),
                                        font_size: 16.,
                                        ..default()
                                    },
                                ),
                                (
                                    Transform::from_translation(Vec3::new(20., -20., 0.1))
                                        .with_scale(Vec3::splat(0.2)),
                                    Sprite {
                                        color: Color::WHITE,
                                        ..Sprite::from_image(handles.activated_ability_back.clone())
                                    },
                                ),
                                (
                                    Transform::from_scale(Vec3::splat(0.4))
                                        .with_translation(Vec3::Z * 0.2 - Vec3::Y * 30.),
                                    Sprite {
                                        color: Color::WHITE,
                                        ..Sprite::from_atlas_image(
                                            handles.icons_image.clone(),
                                            TextureAtlas {
                                                layout: handles.icons_layout.clone(),
                                                index: gain.values.sprite_index(),
                                            },
                                        )
                                    },
                                ),
                                (
                                    Transform::from_scale(Vec3::splat(0.4))
                                        .with_translation(Vec3::Z * 0.1),
                                    Sprite {
                                        color: Color::WHITE,
                                        ..Sprite::from_image(handles.activated_ability_back.clone())
                                    },
                                ),
                                (
                                    Transform::from_scale(Vec3::splat(0.5)),
                                    Sprite {
                                        color: Color::BLACK,
                                        ..Sprite::from_image(handles.activated_ability_back.clone())
                                    },
                                )
                            ],
                        ))
                        .id();
                    commands.entity(fx).add_child(sg);
                }

                for i in 0..positions.len() {
                    let pos = positions[i];
                    let vel = velocities[i];
                    let angle = Vec2::X.angle_to(vel);
                    let rot = Quat::from_rotation_z(angle);
                    let sprite_image = if i == 0 {
                        handles.arrow_start_image.clone()
                    } else if i == positions.len() - 1 {
                        handles.arrow_head_image.clone()
                    } else {
                        handles.arrow_segment_image.clone()
                    };
                    let sg = commands
                        .spawn((
                            Transform::from_translation(pos.extend(30. + h as f32))
                                .with_rotation(rot),
                            Visibility::Visible,
                            children![
                                (
                                    Transform::from_scale(Vec3::splat(0.5))
                                        .with_translation(Vec3::Z * 0.01),
                                    Sprite {
                                        color: Color::WHITE,
                                        ..Sprite::from_image(sprite_image.clone())
                                    },
                                ),
                                (
                                    Transform::from_scale(Vec3::splat(0.8)),
                                    Sprite {
                                        color: Color::BLACK,
                                        ..Sprite::from_image(sprite_image.clone())
                                    },
                                )
                            ],
                        ))
                        .id();
                    commands.entity(fx).add_child(sg);
                }
            }
        }
    }

    let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

    for (entity, fx) in &fx_query {
        if fx.stack_height >= state.stack.len() {
            play_sound_rng(
                &mut commands,
                [&handles.ow2_sound, &handles.ow3_sound, &handles.ow_sound]
                    .iter()
                    .choose(&mut rng)
                    .unwrap(),
                &mut rng,
                &muted,
            );

            commands.entity(entity).despawn();
        }
    }
}

#[derive(Component)]
pub struct ButtonColorTarget;

pub fn on_over_color(
    trigger: Trigger<Pointer<Over>>,
    mut query: Query<&mut Sprite, With<ButtonColorTarget>>,
    children_query: Query<&Children>,
) {
    if let Ok(mut sprite) = query.get_mut(trigger.target()) {
        sprite.color = Color::srgb(0.5, 0.5, 0.5);
        return;
    }
    for child in children_query.iter_descendants(trigger.target()) {
        if let Ok(mut sprite) = query.get_mut(child) {
            sprite.color = Color::srgb(0.5, 0.5, 0.5);
        }
    }
}
pub fn on_out_color(
    trigger: Trigger<Pointer<Out>>,
    mut query: Query<&mut Sprite, With<ButtonColorTarget>>,
    children_query: Query<&Children>,
) {
    if let Ok(mut sprite) = query.get_mut(trigger.target()) {
        sprite.color = Color::WHITE;
        return;
    }
    for child in children_query.iter_descendants(trigger.target()) {
        if let Ok(mut sprite) = query.get_mut(child) {
            sprite.color = Color::WHITE;
        }
    }
}

fn on_click_goto_map(
    _: Trigger<Pointer<Click>>,
    mut next_game_state: ResMut<NextState<GameStates>>,
) {
    next_game_state.set(GameStates::Map);
}

fn spawn_units(
    mut commands: Commands,
    handles: Res<JamAssets>,
    map: Res<MapState>,
    party: Res<PartyState>,
) {
    for (i, unit) in map.combat_enemies.iter().enumerate() {
        let len = map.combat_enemies.len();
        let shift = i as f32 - len as f32 * 0.5 + 0.5;
        let pos = Vec3::new(shift as f32 * 150., 250., 0.);
        commands.spawn((
            DestroyBetweenStates,
            Transform::from_translation(pos).with_scale(Vec3::splat(0.6)),
            unit_bundle(&handles, unit.sprite_index, unit.clone()),
        ));
    }

    for (i, unit) in party.units.iter().enumerate() {
        let len = party.units.len();
        let shift = i as f32 - len as f32 * 0.5 + 0.5;
        let pos = Vec3::new(shift as f32 * 150., -100., 10.);
        commands.spawn((
            DestroyBetweenStates,
            SeqUnitMarker(i),
            Transform::from_translation(pos).with_scale(Vec3::splat(0.6)),
            unit_bundle(&handles, unit.sprite_index, unit.clone()),
        ));
    }
}

#[derive(Component)]
struct EndFx;

fn detect_end(
    state: Res<CombatState>,
    mut commands: Commands,
    handles: Res<JamAssets>,
    units_query: Query<(Entity, &Unit)>,
    fx_query: Query<&EndFx>,
    mut party: ResMut<PartyState>,
    mut map_state: ResMut<MapState>,
) {
    if !fx_query.is_empty() {
        return;
    }

    let (players, enemies): (Vec<&Unit>, Vec<&Unit>) = units_query
        .iter()
        .map(|(_, u)| u)
        .partition(|u| u.owner == Owner::Player);

    // on return click: pass results to the map, show results
    // then map is shown

    let win = enemies.is_empty();
    let lose = players.is_empty();
    let draw = state.turn_number > 100;
    if win || lose || draw {
        commands.spawn((
            DestroyBetweenStates,
            Transform::default()
                .with_translation(Vec3::new(
                    0.,
                    if win {
                        180.
                    } else if lose {
                        -180.
                    } else {
                        0.
                    },
                    300.,
                ))
                .with_scale(Vec3::splat(0.8)),
            Visibility::Visible,
            EndFx,
            children![
                (
                    Transform::default(),
                    Sprite {
                        color: if win {
                            Color::srgb(0.1, 0.6, 0.0)
                        } else if lose {
                            Color::srgb(1.0, 0.1, 0.0)
                        } else {
                            Color::WHITE
                        },
                        ..Sprite::from_image(handles.combat_end_back_image.clone())
                    }
                ),
                (
                    Transform::default().with_translation(Vec3::new(0., 0., 1.)),
                    Sprite::from_image(match (win, lose, draw) {
                        (true, false, _) => handles.combat_win_image.clone(),
                        (false, true, _) => handles.combat_lose_image.clone(),
                        _ => handles.combat_draw_image.clone(),
                    }),
                ),
            ],
        ));
        commands
            .spawn((
                DestroyBetweenStates,
                EndFx,
                Transform::default().with_translation(Vec3::new(
                    0.,
                    if win {
                        120.
                    } else if lose {
                        -60.
                    } else {
                        -40.
                    },
                    302.,
                )),
                Sprite::from_color(Color::WHITE.with_alpha(0.0), Vec2::new(180., 50.)),
                Pickable::default(),
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.4)),
                        Sprite::from_image(handles.ability_background_image.clone()),
                        ButtonColorTarget,
                    ),
                    (
                        TextColor(Color::BLACK),
                        Text2d::new("onwards"),
                        TextFont {
                            font: handles.font.clone(),
                            font_size: 24.,
                            ..default()
                        },
                    )
                ],
            ))
            .observe(on_over_color)
            .observe(on_out_color)
            .observe(on_click_goto_map);

        if lose || draw {
            map_state.player_pos = map_state.last_town;
        } else if win {
            let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

            let player_pos = map_state.player_pos;
            map_state.enemy_parties.remove(&player_pos);
            map_state.enemy_respawn_timer.insert(player_pos, 3);

            // units gain exp
            // enemy level total and player level total are used as additional bonus
            let enemy_levels = map_state
                .combat_enemies
                .iter()
                .map(|u| u.level)
                .sum::<i32>();
            let player_levels = party.units.iter().map(|u| u.level).sum::<i32>();
            let underdog_bonus = (enemy_levels - player_levels).max(0);
            let outnumbered_bonus =
                (map_state.combat_enemies.len() as i32 - party.units.len() as i32).max(0) * 2;
            let experience_gained = 2 + underdog_bonus + outnumbered_bonus;

            let mut gains = vec![];

            for unit in party.units.iter_mut() {
                unit.experience += experience_gained;

                let mut unit_gains = vec![];

                let exp = (
                    unit.experience.min(unit.max_experience),
                    unit.max_experience,
                );
                unit_gains.push((vec![], exp));

                for _ in 0..100 {
                    if unit.experience >= unit.max_experience {
                        unit.experience -= unit.max_experience;
                        unit.level += 1;

                        unit.max_experience = 11 + (unit.level + 3).pow(2) / 10;

                        let mut level_gains = vec![];

                        let gains = level_up(&mut rng);
                        for gain in gains {
                            unit.value_mut(&gain.values).current += gain.source.solve(&unit);
                            unit.value_mut(&gain.values).base += gain.source.solve(&unit);
                            level_gains.push(gain);
                        }

                        let exp = (
                            unit.experience.min(unit.max_experience),
                            unit.max_experience,
                        );
                        unit_gains.push((level_gains, exp));
                    }
                }

                gains.push(unit_gains);
            }

            let gold_gained = 5 + enemy_levels;
            party.gold += gold_gained;

            // end combat animation sequence

            for (e, unit) in &units_query {
                if unit.owner == Owner::Enemy {
                    commands.entity(e).despawn();
                }
            }
            commands.spawn((
                DestroyBetweenStates,
                EndCombatSequence {
                    experience_shown: false,
                    experience_gained,
                    gold_shown: false,
                    gold_gained,
                    heal_everybody: false,
                    gains,
                    current_unit_shown: 0,
                    current_gain_shown: 0,
                    timer: Timer::default(),
                },
            ));
        }

        map_state.combat_enemies.clear();
    }
}

#[derive(Component)]
struct SeqUnitMarker(usize);

#[derive(Component)]
struct EndCombatSequence {
    gold_shown: bool,
    gold_gained: i32,
    //
    experience_shown: bool,
    experience_gained: i32,
    //
    heal_everybody: bool,
    //
    // per unit, per level, (list of gains, current exp/max exp)
    gains: Vec<Vec<(Vec<CombatNumber>, (i32, i32))>>,
    current_unit_shown: usize,
    current_gain_shown: usize,
    // done
    timer: Timer,
}

fn end_combat_sequence(
    mut query: Query<&mut EndCombatSequence>,
    mut units_query: Query<(Entity, &mut Unit)>,
    seq_unit_marker_query: Query<(Entity, &SeqUnitMarker)>,
    mut commands: Commands,
    handles: Res<JamAssets>,
    party: Res<PartyState>,
    time: Res<Time>,
    muted: Res<SoundMuted>,
) {
    let Ok(mut seq) = query.single_mut() else {
        return;
    };

    seq.timer.tick(time.delta());
    if !seq.timer.finished() {
        return;
    }

    if !seq.gold_shown {
        seq.gold_shown = true;
        commands.spawn((
            DestroyBetweenStates,
            Transform::from_translation(Vec3::new(-150., 50., 0.)),
            Visibility::Visible,
            children![
                (
                    Transform::from_scale(Vec3::splat(0.5)),
                    Sprite::from_image(handles.ability_background_image.clone()),
                ),
                (
                    TextColor(Color::BLACK),
                    Text2d::new(format!("+{} gold", seq.gold_gained)),
                    TextFont {
                        font: handles.font.clone(),
                        font_size: 30.,
                        ..default()
                    },
                ),
                (
                    Transform::from_scale(Vec3::splat(0.8))
                        .with_translation(Vec3::Z * 0.1 + Vec3::X * 120.),
                    Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 6,
                        },
                    )
                ),
            ],
        ));

        seq.timer = Timer::from_seconds(0.3, TimerMode::Once);
        return;
    }

    if !seq.experience_shown {
        seq.experience_shown = true;
        commands.spawn((
            DestroyBetweenStates,
            Transform::from_translation(Vec3::new(150., 50., 0.)),
            Visibility::Visible,
            children![
                (
                    Transform::from_scale(Vec3::splat(0.5)),
                    Sprite::from_image(handles.ability_background_image.clone()),
                ),
                (
                    TextColor(Color::BLACK),
                    Text2d::new(format!("+{} exp", seq.experience_gained)),
                    TextFont {
                        font: handles.font.clone(),
                        font_size: 30.,
                        ..default()
                    },
                ),
                (
                    Transform::from_scale(Vec3::splat(0.8))
                        .with_translation(Vec3::Z * 0.1 - Vec3::X * 110.),
                    Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 3,
                        },
                    )
                ),
            ],
        ));

        seq.timer = Timer::from_seconds(0.8, TimerMode::Once);
        return;
    }

    if !seq.heal_everybody {
        seq.heal_everybody = true;
        for (i, unit) in party.start_of_combat_units.iter().enumerate() {
            if let Some((_, _)) = seq_unit_marker_query.iter().find(|(_, m)| m.0 == i) {
                /*
                if let Ok((_, mut existing_unit)) = units_query.get_mut(e) {
                    *existing_unit = unit.clone();
                }
                */
            } else {
                let len = party.start_of_combat_units.len();
                let shift = i as f32 - len as f32 * 0.5 + 0.5;
                let pos = Vec3::new(shift as f32 * 150., -100., 10.);
                commands.spawn((
                    DestroyBetweenStates,
                    SeqUnitMarker(i),
                    Transform::from_translation(pos).with_scale(Vec3::splat(0.6)),
                    unit_bundle(&handles, unit.sprite_index, unit.clone()),
                ));
            }
        }

        seq.timer = Timer::from_seconds(0.2, TimerMode::Once);
        return;
    }

    if let Some(_) = seq.gains.get(seq.current_unit_shown) {
        if let Some((gains, (exp, max_exp))) = seq.gains[seq.current_unit_shown]
            .get(seq.current_gain_shown)
            .cloned()
        {
            if let Some((unit_entity, _)) = seq_unit_marker_query
                .iter()
                .find(|(_, m)| m.0 == seq.current_unit_shown)
            {
                if let Ok((_, mut unit)) = units_query.get_mut(unit_entity) {
                    seq.current_gain_shown += 1;

                    if !gains.is_empty() {
                        unit.level += 1;

                        play_sound(&mut commands, &handles.level_up_sound, &muted);
                    } else {
                        play_sound(&mut commands, &handles.exp_up_sound, &muted);
                    }

                    for gain in gains {
                        let current = unit.value(&gain.values).current;
                        unit.value_mut(&gain.values).current =
                            (current + gain.source.solve(&unit)).max(0);
                    }

                    unit.experience = exp;
                    unit.max_experience = max_exp;
                } else {
                    println!("huh")
                }
            } else {
                seq.current_gain_shown += 1;
            }
        } else {
            seq.current_gain_shown = 0;
            seq.current_unit_shown += 1;
        }

        seq.timer = Timer::from_seconds(0.4, TimerMode::Once);
        return;
    }
}
