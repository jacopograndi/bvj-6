use bevy::{ecs::spawn::SpawnIter, platform::collections::HashMap};
use rand::seq::SliceRandom;

use crate::*;

fn get_map() -> Vec<Vec<&'static str>> {
    #[rustfmt::skip]
    let tiles = vec![
        vec!["f", "p", "f", "f", "m"],
        vec!["p", "1", "p", "p", "p"],
        vec!["p", "p", "f", "m", "2"],
        vec!["f", "f", "m", "f", "m"],
        vec!["f", "3", "t", "p", "c"],
    ];
    tiles
}

fn weird_map_index(map: &Vec<Vec<&'static str>>, x: i32, y: i32) -> &'static str {
    map[map[0].len() - 1 - x as usize][y as usize]
}

pub struct MapPlugin;
impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<OnOpenShop>();
        app.add_event::<OnCloseShop>();
        app.add_event::<OnOpenCamp>();
        app.add_event::<OnCloseCamp>();

        let bp = app
            .world()
            .get_resource::<Blueprints>()
            .expect("insert bp first");

        app.insert_resource(MapState::construct(&bp));
        app.insert_resource(HandCooldown {
            timer: Timer::from_seconds(0.05, TimerMode::Once),
        });

        app.add_systems(OnEnter(GameStates::Map), (spawn_map, spawn_gold).chain());
        app.add_systems(
            Update,
            (
                pull_up,
                update_party_marker,
                update_party_member_marker,
                move_player_marker,
                open_shop,
                close_shop,
                open_camp,
                close_camp,
                update_hand,
                update_zone_shop,
                update_zone_player,
                update_zone_camp,
                update_zone_tocamp,
                refresh_hand_cooldown,
                update_notes,
            )
                .chain()
                .run_if(in_state(GameStates::Map)),
        );
    }
}

#[derive(Clone, Debug)]
pub struct PoolUnit {
    pub base: Unit,
    pub min_level: i32,
    pub max_level: i32,
}

impl PoolUnit {
    fn new(unit: Unit, min: i32, max: i32) -> Self {
        Self {
            base: unit,
            min_level: min,
            max_level: max,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SpawnPool {
    pub base_units: Vec<PoolUnit>,
    pub always_units: Vec<PoolUnit>,
    pub min_amount: i32,
    pub max_amount: i32,
}

impl SpawnPool {
    fn spawn(&self, rng: &mut ChaCha8Rng) -> Vec<Unit> {
        let amount = if self.max_amount == self.min_amount {
            self.max_amount
        } else {
            rng.gen_range(self.min_amount..self.max_amount)
        };
        let mut spawned = vec![];
        for _ in 0..amount {
            let pool_unit = self.base_units.choose(rng).unwrap();
            let level = if pool_unit.max_level == pool_unit.min_level {
                pool_unit.max_level
            } else {
                rng.gen_range(pool_unit.min_level..pool_unit.max_level)
            };
            let mut base = pool_unit.base.clone();
            base.level = level;
            for _ in 1..level {
                let gains = level_up(rng);
                for gain in gains {
                    base.value_mut(&gain.values).current += gain.source.solve(&base);
                    base.value_mut(&gain.values).base += gain.source.solve(&base);
                }
            }
            spawned.push(base);
        }
        for pool_unit in &self.always_units {
            let level = if pool_unit.max_level == pool_unit.min_level {
                pool_unit.max_level
            } else {
                rng.gen_range(pool_unit.min_level..pool_unit.max_level)
            };
            let mut base = pool_unit.base.clone();
            base.level = level;
            for _ in 1..level {
                let gains = level_up(rng);
                for gain in gains {
                    base.value_mut(&gain.values).current += gain.source.solve(&base);
                    base.value_mut(&gain.values).base += gain.source.solve(&base);
                }
            }
            spawned.push(base);
        }
        spawned
    }
}

#[derive(Resource)]
pub struct MapState {
    pub player_pos: IVec2,
    pub last_town: IVec2,
    pub enemy_parties: HashMap<IVec2, Vec<Unit>>,
    pub enemy_pools: HashMap<IVec2, Vec<SpawnPool>>,
    pub enemy_respawn_timer: HashMap<IVec2, i32>,
    pub shop_pools: HashMap<IVec2, SpawnPool>,
    pub combat_enemies: Vec<Unit>,
    pub tile_can_walk: HashMap<IVec2, bool>,
}

impl MapState {
    fn construct(bp: &Blueprints) -> Self {
        let u = |index: usize| bp.units[index].clone();
        let w = |index: usize| {
            let mut unit = u(index);
            unit.owner = Owner::Player;
            unit
        };
        let n = |s: &str, min, max| {
            PoolUnit::new(
                bp.units.iter().find(|u| u.name == s).unwrap().clone(),
                min,
                max,
            )
        };
        let m = |s: &str, min, max| {
            let mut unit = bp.units.iter().find(|u| u.name == s).unwrap().clone();
            unit.owner = Owner::Player;
            PoolUnit::new(unit, min, max)
        };

        let tiles = get_map();

        let mut shop_pools = HashMap::new();
        let mut tile_can_walk = HashMap::new();
        for y in 0..tiles.len() {
            for x in 0..tiles[y].len() {
                let xy = ivec2(x as i32, y as i32);
                let i = weird_map_index(&tiles, x as i32, y as i32);

                tile_can_walk.insert(
                    xy,
                    match i {
                        "m" => false,
                        _ => true,
                    },
                );

                match i {
                    "1" => {
                        shop_pools.insert(
                            xy,
                            SpawnPool {
                                base_units: vec![
                                    m("Elena", 1, 3),
                                    m("Spark", 1, 3),
                                    m("Brandon", 1, 3),
                                    m("Jade", 1, 3),
                                    m("Poppy", 1, 3),
                                ],
                                min_amount: 5,
                                max_amount: 5,
                                always_units: vec![],
                            },
                        );
                    }
                    "2" => {
                        shop_pools.insert(
                            xy,
                            SpawnPool {
                                base_units: vec![
                                    m("Kramer", 4, 6),
                                    m("Roth", 4, 6),
                                    m("Thelonius", 1, 3),
                                    m("Scarlet", 4, 6),
                                    m("Luna", 4, 6),
                                ],
                                min_amount: 5,
                                max_amount: 5,
                                always_units: vec![],
                            },
                        );
                    }
                    "3" => {
                        shop_pools.insert(
                            xy,
                            SpawnPool {
                                base_units: vec![
                                    m("Luna", 6, 9),
                                    m("Spark", 6, 9),
                                    m("Brandon", 6, 9),
                                    m("Sylther", 6, 9),
                                    m("Joker", 6, 9),
                                    m("Abad", 6, 9),
                                    m("Joker", 1, 1),
                                ],
                                min_amount: 5,
                                max_amount: 5,
                                always_units: vec![],
                            },
                        );
                    }
                    _ => {}
                }
            }
        }

        let pool = |(min, max), units| SpawnPool {
            base_units: units,
            min_amount: min,
            max_amount: max,
            always_units: vec![],
        };

        let pool_fixed = |units| SpawnPool {
            base_units: vec![],
            min_amount: 0,
            max_amount: 0,
            always_units: units,
        };

        // easy pools
        let mut enemy_pools = HashMap::new();
        enemy_pools.insert(
            ivec2(4, 1),
            vec![pool((1, 2), vec![n("Spark", 1, 1), n("Brandon", 1, 1)])],
        );
        enemy_pools.insert(
            ivec2(3, 2),
            vec![pool(
                (2, 3),
                vec![n("Spark", 1, 2), n("Brandon", 1, 2), n("Jade", 1, 3)],
            )],
        );
        enemy_pools.insert(
            ivec2(3, 0),
            vec![pool(
                (3, 4),
                vec![
                    n("Spark", 2, 3),
                    n("Brandon", 2, 5),
                    n("Jade", 1, 3),
                    n("Kramer", 3, 5),
                ],
            )],
        );
        enemy_pools.insert(
            ivec2(2, 1),
            vec![pool((2, 2), vec![n("Spark", 4, 7), n("Brandon", 4, 7)])],
        );
        enemy_pools.insert(
            ivec2(3, 3),
            vec![pool(
                (4, 4),
                vec![n("Spark", 2, 4), n("Roth", 2, 4), n("Scarlet", 2, 4)],
            )],
        );
        enemy_pools.insert(
            ivec2(3, 4),
            vec![pool(
                (4, 4),
                vec![
                    n("Brandon", 4, 6),
                    n("Brandon", 4, 6),
                    n("Brandon", 4, 6),
                    n("Poppy", 4, 6),
                ],
            )],
        );
        enemy_pools.insert(
            ivec2(4, 3),
            vec![pool(
                (3, 5),
                vec![
                    n("Brandon", 4, 6),
                    n("Spark", 4, 6),
                    n("Poppy", 4, 6),
                    n("Jade", 4, 6),
                    n("Kramer", 4, 6),
                    n("Roth", 4, 6),
                    n("Scarlet", 4, 6),
                ],
            )],
        );
        enemy_pools.insert(
            ivec2(4, 2),
            vec![pool(
                (3, 5),
                vec![
                    n("Jade", 2, 6),
                    n("Jade", 2, 6),
                    n("Jade", 2, 6),
                    n("Kramer", 2, 6),
                    n("Poppy", 2, 6),
                    n("Roth", 2, 6),
                    n("Scarlet", 2, 6),
                ],
            )],
        );

        // mediums
        enemy_pools.insert(
            ivec2(4, 0),
            vec![
                pool((3, 5), vec![n("Spark", 4, 7), n("Roth", 4, 7)]),
                pool((3, 4), vec![n("Brandon", 4, 7), n("Roth", 4, 7)]),
            ],
        );
        enemy_pools.insert(
            ivec2(2, 0),
            vec![pool(
                (3, 5),
                vec![n("Spark", 4, 7), n("Roth", 4, 7), n("Jade", 5, 10)],
            )],
        );
        enemy_pools.insert(
            ivec2(2, 2),
            vec![pool(
                (5, 5),
                vec![
                    n("Spark", 7, 7),
                    n("Spark", 7, 7),
                    n("Spark", 5, 9),
                    n("Scarlet", 4, 10),
                    n("Luna", 6, 8),
                    n("Poppy", 6, 8),
                ],
            )],
        );
        enemy_pools.insert(
            ivec2(1, 1),
            vec![pool(
                (4, 5),
                vec![
                    n("Sylther", 7, 12),
                    n("Joker", 7, 12),
                    n("Spark", 7, 12),
                    n("Scarlet", 7, 12),
                    n("Luna", 7, 12),
                    n("Poppy", 7, 12),
                ],
            )],
        );
        enemy_pools.insert(
            ivec2(1, 0),
            vec![pool(
                (4, 5),
                vec![
                    n("Sylther", 7, 12),
                    n("Joker", 7, 12),
                    n("Spark", 7, 12),
                    n("Scarlet", 7, 12),
                    n("Luna", 7, 12),
                    n("Poppy", 7, 12),
                ],
            )],
        );
        enemy_pools.insert(
            ivec2(0, 0),
            vec![pool(
                (5, 5),
                vec![
                    n("Thelonius", 7, 12),
                    n("Joker", 7, 12),
                    n("Spark", 7, 12),
                    n("Scarlet", 7, 12),
                    n("Luna", 7, 12),
                    n("Poppy", 7, 12),
                    n("Roth", 7, 12),
                    n("Abad", 7, 12),
                ],
            )],
        );

        // final 3 bosses
        enemy_pools.insert(
            ivec2(0, 2),
            vec![pool_fixed(vec![
                n("Trion", 16, 16),
                n("Thelonius", 12, 15),
                n("Joker", 12, 15),
                n("Scarlet", 12, 15),
                n("Scarlet", 12, 15),
            ])],
        );
        enemy_pools.insert(
            ivec2(0, 3),
            vec![pool_fixed(vec![
                n("Abad", 16, 16),
                n("Abad", 16, 16),
                n("Poppy", 12, 15),
                n("Poppy", 12, 15),
                n("Roth", 12, 15),
            ])],
        );
        enemy_pools.insert(
            ivec2(0, 4),
            vec![pool_fixed(vec![
                n("Trion", 10, 18),
                n("Trion", 10, 18),
                n("Kramer", 15, 18),
                n("Roth", 15, 18),
                n("Zener", 20, 20),
            ])],
        );

        Self {
            player_pos: ivec2(3, 1),
            last_town: ivec2(3, 1),
            enemy_parties: HashMap::new(),
            enemy_respawn_timer: HashMap::new(),
            combat_enemies: vec![],
            enemy_pools,
            shop_pools,
            tile_can_walk,
        }
    }
}

#[derive(Component, Clone)]
struct TilePos {
    xy: IVec2,
}

#[derive(Component, PartialEq)]
enum MarkerType {
    Player,
    Enemy(IVec2),
}

#[derive(Component)]
struct PartyMarker {
    marker_type: MarkerType,
}

fn update_party_marker(
    mut commands: Commands,
    query: Query<(Entity, &PartyMarker)>,
    query_members: Query<(Entity, &PartyMemberMarker, &ChildOf)>,
    tiles_query: Query<(Entity, &TilePos)>,
    mut tr_query: Query<&mut Transform>,
    map: Res<MapState>,
    banners_query: Query<(Entity, &UnitBanner)>,
    player_unit_zone: Query<(&PlayerUnitZone, &Children)>,
) {
    for (marker_entity, marker) in &query {
        if let Some((tile, tile_pos)) = tiles_query.iter().find(|(_, t)| {
            t.xy == match marker.marker_type {
                MarkerType::Player => map.player_pos,
                MarkerType::Enemy(pos) => pos,
            }
        }) {
            let tile_tr = if let Ok(tile_tr) = tr_query.get(tile) {
                tile_tr.clone()
            } else {
                return;
            };

            if let Ok(mut marker_tr) = tr_query.get_mut(marker_entity) {
                *marker_tr = tile_tr;

                if (tile_pos.xy.x - map.player_pos.x).abs()
                    + (tile_pos.xy.y - map.player_pos.y).abs()
                    > 1
                {
                    marker_tr.translation = Vec3::new(10000., 0., 0.);
                }
            };
        }

        let spread = vec![
            vec![vec2(0., 0.)],
            vec![vec2(-0.5, 0.), vec2(0.5, 0.)],
            vec![vec2(-1., 0.), vec2(0., -0.5), vec2(1., 0.)],
            vec![vec2(-1., 0.), vec2(0., -0.5), vec2(0., 0.5), vec2(1., 0.)],
            vec![
                vec2(-1., 0.3),
                vec2(-0.5, -0.5),
                vec2(0.5, -0.5),
                vec2(0., 0.5),
                vec2(1., 0.3),
            ],
        ];

        let Ok((_, zone_children)) = player_unit_zone.single() else {
            continue;
        };

        let units: Vec<Unit> = match marker.marker_type {
            // get them from the banners
            MarkerType::Player => banners_query
                .iter()
                .filter(|(e, _)| zone_children.contains(e))
                .map(|(_, b)| b.unit.clone())
                .collect(),
            MarkerType::Enemy(pos) => {
                if let Some(vec) = map.enemy_parties.get(&pos) {
                    vec.clone()
                } else {
                    continue;
                }
            }
        };

        for (member_entity, member, child_of) in &query_members {
            if child_of.0 != marker_entity {
                continue;
            }

            if member.marker_index >= units.len() - 1 {
                commands.entity(member_entity).insert(Visibility::Hidden);
                // HIDE IT HIDE IT HIDE IT COMEON AAAAH
                commands
                    .entity(member_entity)
                    .insert(Transform::from_translation(Vec3::new(10000., 0., 0.)));
            } else {
                commands.entity(member_entity).insert(Visibility::Visible);
            }

            if let Some(off) = spread[units.len() - 1].get(member.marker_index) {
                let res = (off * 30.).extend(2.);
                commands
                    .entity(member_entity)
                    .insert(Transform::from_translation(res));
            } else {
            }
        }
    }
}

#[derive(Component)]
struct PartyMemberSmallFace;

#[derive(Component)]
struct PartyMemberMarker {
    pub marker_index: usize,
    pub marker_type: MarkerType,
}

#[derive(Component)]
struct PartyMemberLevel;

fn update_party_member_marker(
    mut commands: Commands,
    members_query: Query<(Entity, &PartyMemberMarker)>,
    children_query: Query<&Children>,
    mut small_query: Query<(&PartyMemberSmallFace, &mut Sprite)>,
    mut level_query: Query<(&PartyMemberLevel, &mut Text2d)>,
    handles: Res<JamAssets>,
    map: Res<MapState>,
    banners_query: Query<(Entity, &UnitBanner)>,
    player_unit_zone: Query<(&PlayerUnitZone, &Children)>,
) {
    for (member_entity, member) in &members_query {
        let Ok((_, zone_children)) = player_unit_zone.single() else {
            continue;
        };

        let units: Vec<Unit> = match member.marker_type {
            // get them from the banners
            MarkerType::Player => banners_query
                .iter()
                .filter(|(e, _)| zone_children.contains(e))
                .map(|(_, b)| b.unit.clone())
                .collect(),
            MarkerType::Enemy(pos) => {
                if let Some(vec) = map.enemy_parties.get(&pos) {
                    vec.clone()
                } else {
                    continue;
                }
            }
        };

        let Some(unit) = units.get(member.marker_index) else {
            continue;
        };
        if let Some(e) = children_query
            .iter_descendants(member_entity)
            .find(|e| small_query.contains(*e))
        {
            if let Ok((_, mut sprite)) = small_query.get_mut(e) {
                // keep updated small unit sprite
                if let Some(atlas) = &mut sprite.texture_atlas {
                    atlas.index = unit.sprite_index;
                }
            }

            if let Some(e) = children_query
                .iter_descendants(member_entity)
                .find(|e| level_query.contains(*e))
            {
                if let Ok((_, mut text)) = level_query.get_mut(e) {
                    // keep updated small unit level
                    text.0 = format!("{}", unit.level);
                }
            }

            continue;
        }

        let bob_pos = Vec3::Y * 14.;
        let bob_timer = Timer::from_seconds(0.5, TimerMode::Repeating);

        let child = commands
            .spawn((
                DestroyBetweenStates,
                Transform::default(),
                Visibility::Visible,
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.4)).with_translation(-Vec3::Z * 0.3),
                        Sprite {
                            color: Color::BLACK,
                            ..Sprite::from_atlas_image(
                                handles.icons_image.clone(),
                                TextureAtlas {
                                    layout: handles.icons_layout.clone(),
                                    index: 5,
                                },
                            )
                        },
                    ),
                    (
                        Transform::from_translation(Vec3::ZERO),
                        Visibility::Inherited,
                        BobUpAndDown {
                            source: bob_pos,
                            destination: bob_pos + Vec3::Y * 10.,
                            to_source: true,
                            timer: bob_timer,
                        },
                        children![
                            (
                                Transform::from_scale(Vec3::splat(0.32))
                                    .with_translation(-Vec3::Z * 0.2),
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
                                PartyMemberSmallFace,
                                Transform::from_scale(Vec3::splat(0.4))
                                    .with_translation(-Vec3::Z * 0.1),
                                Sprite::from_atlas_image(
                                    handles.units_small_image.clone(),
                                    TextureAtlas {
                                        layout: handles.units_small_layout.clone(),
                                        index: unit.sprite_index,
                                    },
                                ),
                            ),
                            (
                                Transform::from_scale(Vec3::splat(0.1))
                                    .with_translation(Vec3::new(12., -12., 0.1)),
                                Sprite::from_atlas_image(
                                    handles.icons_image.clone(),
                                    TextureAtlas {
                                        layout: handles.icons_layout.clone(),
                                        index: 4,
                                    },
                                ),
                            ),
                            (
                                PartyMemberLevel,
                                Transform::from_translation(Vec3::new(12., -12., 0.11)),
                                Visibility::Inherited,
                                TextColor(Color::BLACK),
                                Text2d::new("1"),
                                TextFont {
                                    font: handles.font.clone(),
                                    font_size: 14.0,
                                    ..default()
                                },
                            )
                        ]
                    )
                ],
            ))
            .id();
        commands.entity(member_entity).add_child(child);
    }
}

#[derive(Component)]
pub struct BobUpAndDown {
    source: Vec3,
    destination: Vec3,
    to_source: bool,
    timer: Timer,
}

#[derive(Component)]
pub struct BobUpAndDownFast;

pub fn bob_up_and_down(
    mut query: Query<(&mut Transform, &mut BobUpAndDown, Option<&BobUpAndDownFast>)>,
    time: Res<Time>,
) {
    for (mut tr, mut bob, fast) in &mut query {
        bob.timer
            .tick(time.delta() * if fast.is_some() { 2 } else { 1 });
        if bob.timer.finished() {
            bob.to_source = !bob.to_source;
        }

        let (dest, pos) = if bob.to_source {
            (bob.source, bob.destination)
        } else {
            (bob.destination, bob.source)
        };

        let t = (bob.timer.fraction() * 2.0).powi(2).min(1.0);
        tr.translation = dest * t + pos * (1. - t);
    }
}

#[derive(Component)]
pub struct GoldTracker {
    last_amount: Option<i32>,
    amount: Option<i32>,
}

pub fn update_gold_tracker(
    mut trackers_query: Query<(Entity, &mut GoldTracker, &mut Text2d, &GlobalTransform)>,
    mut commands: Commands,
    handles: Option<Res<JamAssets>>,
    party: Option<Res<PartyState>>,
) {
    let (Some(handles), Some(party)) = (handles, party) else {
        return;
    };
    let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

    for (_, mut ui_tracker, mut text, gtr) in &mut trackers_query {
        ui_tracker.last_amount = ui_tracker.amount;
        let current = party.gold;
        ui_tracker.amount = Some(current);
        text.0 = format!("{}", current);

        // spawn some particles
        if let (Some(amt), Some(last)) = (ui_tracker.amount, ui_tracker.last_amount) {
            if amt != last {
                let sign = (amt - last).signum();
                for _ in 0..(amt - last).abs() {
                    commands.spawn((
                        DestroyBetweenStates,
                        Transform::from_translation(gtr.translation()).with_scale(Vec3::splat(0.2)),
                        Visibility::Visible,
                        particle_value_bundle(&handles, &mut rng, sign, 6),
                    ));
                }
            }
        }
    }
}

pub fn spawn_gold(mut commands: Commands, handles: Res<JamAssets>) {
    commands.spawn((
        DestroyBetweenStates,
        Transform::from_translation(Vec3::new(-500., -200., 200.)),
        Visibility::Visible,
        (children![
            (
                Transform::from_scale(Vec3::splat(0.8)).with_translation(Vec3::new(0., -100., 3.)),
                Sprite::from_atlas_image(
                    handles.icons_image.clone(),
                    TextureAtlas {
                        layout: handles.icons_layout.clone(),
                        index: 6,
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
                        Text2d::new(""),
                        TextFont {
                            font: handles.font.clone(),
                            font_size: 30.0,
                            ..default()
                        },
                        GoldTracker {
                            amount: None,
                            last_amount: None
                        }
                    ),
                    (
                        Transform::from_translation(Vec3::Z).with_scale(Vec3::splat(0.8)),
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
        ],),
    ));
}

fn spawn_map(
    mut map: ResMut<MapState>,
    mut commands: Commands,
    handles: Res<JamAssets>,
    party: Res<PartyState>,
    player_zone_query: Query<Entity, With<PlayerUnitZone>>,
) {
    let scale = 0.70;
    let tile_size = Vec3::new(256., 128., 0.) * scale;

    let tiles = get_map();

    let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

    // spawn parties from pools
    let mut enemy_parties = map.enemy_parties.clone();
    for (xy, pools) in &map.enemy_pools {
        if !enemy_parties.get(xy).is_some() && map.enemy_respawn_timer.get(xy).is_none() {
            let pool = pools.choose(&mut rng).unwrap();
            enemy_parties.insert(*xy, pool.spawn(&mut rng));
        }
    }
    map.enemy_parties = enemy_parties;

    for y in 0..5 {
        for x in 0..5 {
            let xy = Vec3::new(x as f32, y as f32, 0.);
            let isox = xy.x * tile_size.x * 0.5 + xy.y * tile_size.x * 0.5;
            let isoy = -xy.x * tile_size.y * 0.5 + xy.y * tile_size.y * 0.5;
            let pos = Vec3::new(isox, isoy, 0.0);
            let pos = pos + tile_size * Vec3::new(-2., 0.4, 0.) - Vec3::Z * isoy / 100.;

            let image = match weird_map_index(&tiles, x, y) {
                "f" => handles.tile_forest_image.clone(),
                "m" => handles.tile_mountain_image.clone(),
                "1" => handles.tile_home_image.clone(),
                "2" => handles.tile_town1_image.clone(),
                "3" => handles.tile_town2_image.clone(),
                "t" => handles.tile_tower_image.clone(),
                "c" => handles.tile_castle_image.clone(),
                _ => handles.tile_plains_image.clone(),
            };

            let tilepos = ivec2(x as i32, y as i32);
            commands
                .spawn((
                    DestroyBetweenStates,
                    Transform::from_scale(Vec3::splat(scale)).with_translation(pos),
                    Sprite::from_image(image),
                    Pickable::default(),
                    TilePos { xy: tilepos },
                ))
                .observe(on_over_tile)
                .observe(on_click_tile)
                .observe(on_out_tile);

            if tilepos == map.player_pos {
                // spawn party in map

                commands.spawn((
                    DestroyBetweenStates,
                    Transform::from_translation(pos + Vec3::Z),
                    Visibility::Visible,
                    PartyMarker {
                        marker_type: MarkerType::Player,
                    },
                    Children::spawn(SpawnIter((0..5).map(|i| {
                        (
                            Transform::default(),
                            Visibility::Visible,
                            PartyMemberMarker {
                                marker_index: i,
                                marker_type: MarkerType::Player,
                            },
                        )
                    }))),
                ));
            } else if let Some(_) = map.enemy_parties.get(&tilepos) {
                commands.spawn((
                    DestroyBetweenStates,
                    Transform::from_translation(pos + Vec3::Z),
                    Visibility::Visible,
                    PartyMarker {
                        marker_type: MarkerType::Enemy(tilepos),
                    },
                    Children::spawn(SpawnIter((0..5).map(move |i| {
                        (
                            Transform::default(),
                            Visibility::Visible,
                            PartyMemberMarker {
                                marker_index: i,
                                marker_type: MarkerType::Enemy(tilepos),
                            },
                        )
                    }))),
                ));
            }
        }
    }

    let Ok(zone) = player_zone_query.single() else {
        return;
    };

    for (_, unit) in party.units.iter().enumerate() {
        // offload party.units to entity banners
        let entity = spawn_unit_banner(&mut commands, &handles, unit);
        commands.entity(zone).add_child(entity);
    }
}

fn spawn_unit_banner(commands: &mut Commands, handles: &JamAssets, unit: &Unit) -> Entity {
    let entity = commands
        .spawn((
            DestroyBetweenStates,
            UnitBanner { unit: unit.clone() }, // is not modified in map
            Visibility::Visible,
            Transform::default(),
            children![(
                PullUp {
                    source: Vec3::ZERO,
                    destination: Vec3::Y * 140.,
                    to_source: true,
                    timer: {
                        let mut timer = Timer::from_seconds(0.1, TimerMode::Once);
                        timer.set_elapsed(Duration::from_millis(100));
                        timer
                    },
                },
                Transform::default().with_scale(Vec3::splat(0.6)),
                unit_bundle(&handles, unit.sprite_index, unit.clone()),
            ),],
        ))
        .id();
    let hitbox = commands
        .spawn((
            UnitPickupMarker,
            DestroyBetweenStates,
            Pickable::default(),
            Transform::from_translation(Vec3::new(0., -80., -10.)),
            Sprite::from_color(Color::WHITE.with_alpha(0.0), Vec2::new(150., 300.)),
        ))
        .observe(on_over_pull_up)
        .observe(on_out_pull_down)
        .observe(on_click_pickup)
        .id();
    commands.entity(entity).add_child(hitbox);
    entity
}

#[derive(Component)]
struct PullUp {
    source: Vec3,
    destination: Vec3,
    to_source: bool,
    timer: Timer,
}

#[derive(Component)]
struct ArrowMoveFx;

#[derive(Component)]
struct PlayerMovementFx {
    from: Vec3,
    to: Vec3,
    from_tilepos: IVec2,
    to_tilepos: IVec2,
    timer: Timer,
}

fn move_player_marker(
    mut party_marker_query: Query<(Entity, &PartyMarker, &mut Transform, &mut PlayerMovementFx)>,
    time: Res<Time>,
    mut commands: Commands,
    mut map: ResMut<MapState>,
    mut next_game_state: ResMut<NextState<GameStates>>,
    mut party: ResMut<PartyState>,
    mut shop_event: EventWriter<OnOpenShop>,
    banners_query: Query<(Entity, &UnitBanner)>,
    child_of_query: Query<&ChildOf>,
    player_zone_query: Query<Entity, With<PlayerUnitZone>>,
) {
    for (entity, _marker, mut tr, mut fx) in &mut party_marker_query {
        fx.timer.tick(time.delta());
        tr.translation = fx.to * fx.timer.fraction() + fx.from * (1. - fx.timer.fraction());
        if fx.timer.finished() {
            map.player_pos = fx.to_tilepos;
            commands.entity(entity).remove::<PlayerMovementFx>();

            // tile events
            // if there is an enemy squad -> combat
            // if there is a town -> shop

            // decrement spawn timer
            map.enemy_respawn_timer.retain(|_, t| {
                *t -= 1;
                *t > 0
            });

            if let Some(enemies) = map.enemy_parties.get(&fx.to_tilepos) {
                // upload party.units from the banners
                let units = banners_query
                    .iter()
                    .filter_map(|(e, b)| {
                        child_of_query
                            .get(e)
                            .ok()
                            .filter(|parent| Some(parent.0) == player_zone_query.single().ok())
                            .and_then(|_| Some(b.unit.clone()))
                    })
                    .collect();
                party.units = units;
                party.start_of_combat_units = party.units.clone();
                map.combat_enemies = enemies.clone();
                next_game_state.set(GameStates::Combat);
            }

            if let Some(_) = map.shop_pools.get(&fx.to_tilepos) {
                map.last_town = map.player_pos;
                shop_event.write(OnOpenShop { pos: fx.to_tilepos });
            }
        }
    }
}

fn on_click_tile(
    trigger: Trigger<Pointer<Click>>,
    query: Query<(Entity, &TilePos)>,
    arrow_query: Query<Entity, With<ArrowMoveFx>>,
    mut commands: Commands,
    map: Res<MapState>,
    party_marker_query: Query<(Entity, &PartyMarker)>,
    tr_query: Query<&Transform>,
    movement_fx: Query<&PlayerMovementFx>,
    shop_opened_query: Query<&ShopOpened>,
    camp_opened_query: Query<&CampOpened>,
) {
    if !shop_opened_query.is_empty() || !camp_opened_query.is_empty() || !movement_fx.is_empty() {
        return;
    }

    let Ok((tile_dst, tp_dst)) = query.get(trigger.target()) else {
        return;
    };
    let Some((tile_src, tp_src)) = query.iter().find(|(_, tp)| tp.xy == map.player_pos) else {
        return;
    };

    if let Some(false) = map.tile_can_walk.get(&tp_dst.xy) {
        return;
    }

    if (tp_dst.xy.x - tp_src.xy.x).abs() + (tp_dst.xy.y - tp_src.xy.y).abs() != 1 {
        return;
    }

    let Some((marker_entity, _)) = party_marker_query
        .iter()
        .find(|(_, m)| m.marker_type == MarkerType::Player)
    else {
        return;
    };

    let Ok(source_tr) = tr_query.get(tile_src) else {
        return;
    };
    let Ok(target_tr) = tr_query.get(tile_dst) else {
        return;
    };

    for arrow in &arrow_query {
        commands.entity(arrow).despawn();
    }

    commands.entity(marker_entity).insert(PlayerMovementFx {
        from: source_tr.translation + Vec3::Z,
        to: target_tr.translation + Vec3::Z,
        from_tilepos: tp_src.xy,
        to_tilepos: tp_dst.xy,
        timer: Timer::from_seconds(0.5, TimerMode::Once),
    });
}

fn on_over_tile(
    trigger: Trigger<Pointer<Over>>,
    query: Query<(Entity, &TilePos)>,
    arrow_query: Query<Entity, With<ArrowMoveFx>>,
    mut commands: Commands,
    handles: Res<JamAssets>,
    tr_query: Query<&Transform>,
    map: Res<MapState>,
    movement_fx: Query<&PlayerMovementFx>,
    shop_opened_query: Query<&ShopOpened>,
    camp_opened_query: Query<&CampOpened>,
) {
    if !shop_opened_query.is_empty() || !camp_opened_query.is_empty() || !arrow_query.is_empty() {
        return;
    }

    let Ok((tile_dst, tp_dst)) = query.get(trigger.target()) else {
        return;
    };
    let Some((tile_src, tp_src)) = query.iter().find(|(_, tp)| tp.xy == map.player_pos) else {
        return;
    };

    let mut tp_src: TilePos = tp_src.clone();

    let Ok(mut source_tr) = tr_query.get(tile_src).cloned() else {
        return;
    };
    let Ok(target_tr) = tr_query.get(tile_dst) else {
        return;
    };

    if let Ok(movement) = movement_fx.single() {
        if (tp_dst.xy.x - movement.to_tilepos.x).abs() + (tp_dst.xy.y - movement.to_tilepos.y).abs()
            != 1
        {
            return;
        } else {
            source_tr.translation = movement.to;
            tp_src.xy = movement.to_tilepos;
        }
    }

    if let Some(false) = map.tile_can_walk.get(&tp_dst.xy) {
        return;
    }

    if (tp_dst.xy.x - tp_src.xy.x).abs() + (tp_dst.xy.y - tp_src.xy.y).abs() != 1 {
        return;
    }

    let src = source_tr.translation.xy();
    let dst = target_tr.translation.xy();
    let arcing = 40.;
    let mut t1 = (src + src + dst) / 3.;
    t1 += Vec2::Y * arcing;
    let mut t2 = (src + dst + dst) / 3.;
    t2 += Vec2::Y * arcing;

    let points = [[src, t1, t2, dst]];

    let segments_len = points[0]
        .windows(2)
        .map(|v| v[0].distance(v[1]))
        .sum::<f32>();

    let segments_num = (segments_len / 32.) as usize;

    let bezier = CubicBezier::new(points).to_curve().unwrap();
    let positions: Vec<_> = bezier.iter_positions(segments_num).collect();
    let velocities: Vec<_> = bezier.iter_velocities(segments_num).collect();

    let fx = commands
        .spawn((
            DestroyBetweenStates,
            Transform::default(),
            Visibility::Visible,
            ArrowMoveFx,
        ))
        .id();

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
                DestroyBetweenStates,
                Transform::from_translation(pos.extend(30.)).with_rotation(rot),
                Visibility::Visible,
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::Z * 0.01),
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

fn on_out_tile(
    trigger: Trigger<Pointer<Out>>,
    arrow_query: Query<Entity, With<ArrowMoveFx>>,
    mut commands: Commands,
) {
    for arrow in &arrow_query {
        commands.entity(arrow).despawn();
    }
}

fn on_over_pull_up(
    trigger: Trigger<Pointer<Over>>,
    mut query: Query<&mut PullUp>,
    child_of_query: Query<&ChildOf>,
    children_query: Query<&Children>,
    bob_query: Query<Entity, With<BobUpAndDown>>,
    party_member_marker_query: Query<&PartyMemberMarker>,
    mut commands: Commands,
) {
    if let Ok(parent) = child_of_query.get(trigger.target()) {
        if let Ok(children) = children_query.get(parent.0) {
            if let Some(pullup_entity) = children.iter().find(|c| query.contains(*c)) {
                if let Ok(mut pullup) = query.get_mut(pullup_entity) {
                    pullup.to_source = false;
                    pullup.timer.reset();

                    // find map marker and bob faster
                    if let Some(bob) = bob_query.iter().find(|e| {
                        child_of_query
                            .iter_ancestors(*e)
                            .find(|a| {
                                party_member_marker_query
                                    .get(*a)
                                    .ok()
                                    .is_some_and(|marker| {
                                        marker.marker_type == MarkerType::Player
                                        //&& marker.marker_index == pullup.unit_index
                                    })
                            })
                            .is_some()
                    }) {
                        commands.entity(bob).insert(BobUpAndDownFast);
                    }
                }
            }
        }
    }
}

fn on_out_pull_down(
    trigger: Trigger<Pointer<Out>>,
    mut query: Query<&mut PullUp>,
    child_of_query: Query<&ChildOf>,
    children_query: Query<&Children>,
    bob_query: Query<Entity, With<BobUpAndDown>>,
    party_member_marker_query: Query<&PartyMemberMarker>,
    mut commands: Commands,
) {
    if let Ok(parent) = child_of_query.get(trigger.target()) {
        if let Ok(children) = children_query.get(parent.0) {
            if let Some(pullup_entity) = children.iter().find(|c| query.contains(*c)) {
                if let Ok(mut pullup) = query.get_mut(pullup_entity) {
                    pullup.to_source = true;
                    pullup.timer.reset();

                    // find map marker and bob faster
                    if let Some(bob) = bob_query.iter().find(|e| {
                        child_of_query
                            .iter_ancestors(*e)
                            .find(|a| {
                                party_member_marker_query
                                    .get(*a)
                                    .ok()
                                    .is_some_and(|marker| {
                                        marker.marker_type == MarkerType::Player
                                        //&& marker.marker_index == pullup.unit_index
                                    })
                            })
                            .is_some()
                    }) {
                        commands.entity(bob).remove::<BobUpAndDownFast>();
                    }
                }
            }
        }
    }
}

fn pull_up(
    mut query: Query<(Entity, &mut Transform, &mut PullUp)>,
    child_of_query: Query<&ChildOf>,
    player_unit_zone: Query<&PlayerUnitZone>,
    hand_query: Query<&UnitInHand>,
    time: Res<Time>,
) {
    return;
    for (entity, mut tr, mut pullup) in &mut query {
        if let Ok(parent) = child_of_query.get(entity) {
            if let Ok(zone) = child_of_query.get(parent.0) {
                if hand_query.contains(parent.0) && player_unit_zone.contains(zone.0) {
                    pullup.timer.tick(time.delta());

                    let (dest, pos) = if pullup.to_source {
                        (pullup.source, pullup.destination)
                    } else {
                        (pullup.destination, pullup.source)
                    };

                    if pullup.timer.finished() {
                        tr.translation = dest;
                        continue;
                    }

                    let t = pullup.timer.fraction().powi(2);
                    tr.translation = dest * t + pos * (1. - t);
                }
            }
        }

        pullup.to_source = true;
        pullup.timer.reset();
        tr.translation = pullup.source;
    }
}

#[derive(Event)]
struct OnOpenShop {
    pos: IVec2,
}
#[derive(Event)]
struct OnCloseShop;

#[derive(Event)]
struct OnOpenCamp;
#[derive(Event)]
struct OnCloseCamp;

#[derive(Component)]
struct ShopOpened;

#[derive(Component)]
struct CampOpened;

fn open_shop(
    mut commands: Commands,
    handles: Res<JamAssets>,
    mut event: EventReader<OnOpenShop>,
    map: Res<MapState>,
    shop_zone_query: Query<Entity, With<ShopUnitZone>>,
) {
    for e in event.read() {
        let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

        let Some(pool) = map.shop_pools.get(&e.pos) else {
            continue;
        };
        let Ok(zone) = shop_zone_query.single() else {
            return;
        };

        let units = pool.spawn(&mut rng);

        commands.spawn((DestroyBetweenStates, ShopOpened));

        for (_, unit) in units.iter().enumerate() {
            let entity = spawn_unit_banner(&mut commands, &handles, unit);
            commands.entity(zone).add_child(entity);

            let p = commands
                .spawn((
                    DestroyBetweenStates,
                    Visibility::Visible,
                    Transform::default().with_translation(Vec3::new(0., 10., 0.)),
                    unit_price_bundle(&handles, unit),
                ))
                .id();
            commands.entity(entity).add_child(p);
        }

        commands.spawn((
            DestroyBetweenStates,
            ShopOpened,
            Transform::default().with_translation(Vec3::new(0., 180., 290.)),
            children![
                (
                    Transform::from_scale(Vec3::splat(1.0)),
                    Sprite {
                        color: Color::srgb(1., 1., 0.),
                        ..Sprite::from_image(handles.combat_end_back_image.clone())
                    },
                    ButtonColorTarget,
                ),
                (
                    Transform::from_translation(Vec3::new(0., 40., 1.)),
                    TextColor(Color::BLACK),
                    Text2d::new("Units for hire"),
                    TextFont {
                        font: handles.font.clone(),
                        font_size: 24.,
                        ..default()
                    },
                )
            ],
        ));

        commands
            .spawn((
                DestroyBetweenStates,
                ShopOpened,
                Transform::default().with_translation(Vec3::new(500., 150., 302.)),
                Sprite::from_color(Color::WHITE.with_alpha(0.0), Vec2::new(220., 80.)),
                Pickable::default(),
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.5)),
                        Sprite::from_image(handles.ability_background_image.clone()),
                        ButtonColorTarget,
                    ),
                    (
                        Transform::from_translation(Vec3::new(0., 10., 1.)),
                        TextColor(Color::BLACK),
                        Text2d::new("close shop"),
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
            .observe(
                |_: Trigger<Pointer<Click>>, mut event: EventWriter<OnCloseShop>| {
                    event.write(OnCloseShop);
                },
            );
    }
}

fn close_shop(
    mut commands: Commands,
    mut event: EventReader<OnCloseShop>,
    shop_zone_query: Query<&Children, With<ShopUnitZone>>,
    shop_opened_query: Query<(Entity, &ShopOpened)>,
) {
    for _ in event.read() {
        for children in &shop_zone_query {
            for child in children.iter() {
                commands.entity(child).despawn();
            }
        }
        for (e, _) in shop_opened_query.iter() {
            commands.entity(e).despawn();
        }
    }
}

fn open_camp(
    mut commands: Commands,
    handles: Res<JamAssets>,
    mut event: EventReader<OnOpenCamp>,
    mut camp_zone_query: Query<(Entity, &mut Transform), With<CampUnitZone>>,
    camp_unit_query: Query<(Entity, &CampUnit)>,
) {
    for _ in event.read() {
        commands.spawn((DestroyBetweenStates, CampOpened));

        let Ok((zone, mut tr)) = camp_zone_query.single_mut() else {
            return;
        };

        tr.translation.x = 0.;

        for (camp_unit_entity, camp_unit) in &camp_unit_query {
            let entity = spawn_unit_banner(&mut commands, &handles, &camp_unit.unit);
            commands.entity(zone).add_child(entity);
            commands.entity(entity).insert(CampUnitBanner {
                camp_unit: camp_unit_entity,
            });
        }

        commands.spawn((
            DestroyBetweenStates,
            CampOpened,
            Transform::default().with_translation(Vec3::new(0., 180., 290.)),
            children![
                (
                    Transform::from_scale(Vec3::splat(1.0)),
                    Sprite {
                        color: Color::srgb(0., 1., 1.),
                        ..Sprite::from_image(handles.combat_end_back_image.clone())
                    },
                    ButtonColorTarget,
                ),
                (
                    Transform::from_translation(Vec3::new(0., 40., 1.)),
                    TextColor(Color::BLACK),
                    Text2d::new("Your units in reserve"),
                    TextFont {
                        font: handles.font.clone(),
                        font_size: 24.,
                        ..default()
                    },
                )
            ],
        ));

        commands
            .spawn((
                DestroyBetweenStates,
                CampOpened,
                Transform::default().with_translation(Vec3::new(500., 300., 302.)),
                Sprite::from_color(Color::WHITE.with_alpha(0.0), Vec2::new(220., 80.)),
                Pickable::default(),
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.6)).with_translation(-Vec3::Z * 0.1),
                        Sprite {
                            color: Color::srgb(0., 0., 0.),
                            ..Sprite::from_image(handles.ability_background_image.clone())
                        },
                    ),
                    (
                        Transform::from_scale(Vec3::splat(0.5)),
                        Sprite {
                            color: Color::WHITE,
                            ..Sprite::from_image(handles.ability_background_image.clone())
                        },
                        ButtonColorTarget,
                    ),
                    (
                        Transform::from_translation(Vec3::new(0., 10., 1.)),
                        TextColor(Color::BLACK),
                        Text2d::new("close reserves"),
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
            .observe(
                |_: Trigger<Pointer<Click>>, mut event: EventWriter<OnCloseCamp>| {
                    event.write(OnCloseCamp);
                },
            );

        commands
            .spawn((
                DestroyBetweenStates,
                CampOpened,
                Transform::default().with_translation(Vec3::new(500., 220., 302.)),
                Sprite::from_color(Color::WHITE.with_alpha(0.1), Vec2::new(80., 80.)),
                Pickable::default(),
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::Z * 0.1),
                        Sprite {
                            color: Color::srgb(0., 0., 0.),
                            ..Sprite::from_atlas_image(
                                handles.icons_image.clone(),
                                TextureAtlas {
                                    layout: handles.icons_layout.clone(),
                                    index: 8,
                                },
                            )
                        },
                    ),
                    (
                        Transform::from_scale(Vec3::splat(0.5)),
                        Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 4,
                            },
                        ),
                        ButtonColorTarget,
                    ),
                ],
            ))
            .observe(on_over_color)
            .observe(on_out_color)
            .observe(
                |_: Trigger<Pointer<Click>>,
                 mut camp_zone_query: Query<&mut Transform, With<CampUnitZone>>| {
                    if let Ok(mut tr) = camp_zone_query.single_mut() {
                        tr.translation.x = 0.;
                    }
                },
            );

        commands
            .spawn((
                DestroyBetweenStates,
                CampOpened,
                Transform::default().with_translation(Vec3::new(420., 220., 302.)),
                Sprite::from_color(Color::WHITE.with_alpha(0.1), Vec2::new(80., 80.)),
                Pickable::default(),
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::Z * 0.1),
                        Sprite {
                            color: Color::srgb(0., 0., 0.),
                            flip_x: true,
                            ..Sprite::from_atlas_image(
                                handles.icons_image.clone(),
                                TextureAtlas {
                                    layout: handles.icons_layout.clone(),
                                    index: 12,
                                },
                            )
                        },
                    ),
                    (
                        Transform::from_scale(Vec3::splat(0.5)),
                        Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 4,
                            },
                        ),
                        ButtonColorTarget,
                    ),
                ],
            ))
            .observe(on_over_color)
            .observe(on_out_color)
            .observe(
                |trigger: Trigger<Pointer<Click>>,
                 mut camp_zone_query: Query<&mut Transform, With<CampUnitZone>>| {
                    if let Ok(mut tr) = camp_zone_query.single_mut() {
                        tr.translation.x += 150.;
                    }
                },
            );

        commands
            .spawn((
                DestroyBetweenStates,
                CampOpened,
                Transform::default().with_translation(Vec3::new(580., 220., 302.)),
                Sprite::from_color(Color::WHITE.with_alpha(0.1), Vec2::new(80., 80.)),
                Pickable::default(),
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.5)).with_translation(Vec3::Z * 0.1),
                        Sprite {
                            color: Color::srgb(0., 0., 0.),
                            ..Sprite::from_atlas_image(
                                handles.icons_image.clone(),
                                TextureAtlas {
                                    layout: handles.icons_layout.clone(),
                                    index: 12,
                                },
                            )
                        },
                    ),
                    (
                        Transform::from_scale(Vec3::splat(0.5)),
                        Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 4,
                            },
                        ),
                        ButtonColorTarget,
                    ),
                ],
            ))
            .observe(on_over_color)
            .observe(on_out_color)
            .observe(
                |trigger: Trigger<Pointer<Click>>,
                 mut camp_zone_query: Query<&mut Transform, With<CampUnitZone>>| {
                    if let Ok(mut tr) = camp_zone_query.single_mut() {
                        tr.translation.x -= 150.;
                    }
                },
            );
    }
}

fn close_camp(
    mut commands: Commands,
    mut event: EventReader<OnCloseCamp>,
    camp_zone_query: Query<&Children, With<CampUnitZone>>,
    camp_opened_query: Query<(Entity, &CampOpened)>,
) {
    for _ in event.read() {
        for children in &camp_zone_query {
            for child in children.iter() {
                commands.entity(child).despawn();
            }
        }
        for (e, _) in camp_opened_query.iter() {
            commands.entity(e).despawn();
        }
    }
}

#[derive(Component)]
struct UnitPickupMarker;

#[derive(Component)]
struct UnitInHand {
    source_zone: Entity,
}

#[derive(Resource)]
struct HandCooldown {
    timer: Timer,
}

fn refresh_hand_cooldown(mut hand_cooldown: ResMut<HandCooldown>, time: Res<Time>) {
    hand_cooldown.timer.tick(time.delta());
}

fn on_click_pickup(
    trigger: Trigger<Pointer<Click>>,
    query: Query<(Entity, &UnitPickupMarker)>,
    child_of_query: Query<&ChildOf>,
    mut commands: Commands,
    hand_query: Query<&UnitInHand>,
    mut hand_cooldown: ResMut<HandCooldown>,
    price_query: Query<(Entity, &UnitPriceMarker)>,
    mut party: ResMut<PartyState>,
    camp_zone_query: Query<Entity, With<ToCampUnitZone>>,
    player_children_query: Query<&Children, With<PlayerUnitZone>>,
    handles: Res<JamAssets>,
) {
    if !hand_query.is_empty() {
        return;
    }

    if !hand_cooldown.timer.finished() {
        return;
    }

    let Ok(_) = query.get(trigger.target()) else {
        return;
    };
    let Ok(parent) = child_of_query.get(trigger.target()) else {
        return;
    };

    let Ok(source_zone) = child_of_query.get(parent.0) else {
        return;
    };

    if let Ok(children) = player_children_query.get(source_zone.0) {
        if children.len() <= 1 {
            commands.spawn((
                DestroyBetweenStates,
                Transform::from_translation(Vec3::new(0., 0., 800.)),
                note_bundle(&handles, format!("You can't remove your last unit.")),
            ));
            return;
        }
    }

    if let Some((price_entity, price_marker)) = price_query.iter().find(|(e, _)| {
        if let Ok(p) = child_of_query.get(*e) {
            p.0 == parent.0
        } else {
            false
        }
    }) {
        if price_marker.price > party.gold {
            commands.spawn((
                DestroyBetweenStates,
                Transform::from_translation(Vec3::new(0., 0., 800.)),
                note_bundle(&handles, format!("Not enough gold.")),
            ));
            return;
        }

        if let Ok(camp) = camp_zone_query.single() {
            hand_cooldown.timer.reset();
            commands.entity(price_entity).despawn();
            commands.entity(parent.0).remove::<ChildOf>();
            commands
                .entity(parent.0)
                .insert(UnitInHand { source_zone: camp });
            party.gold -= price_marker.price;
        }
    } else {
        println!("picked up");
        hand_cooldown.timer.reset();
        commands.entity(parent.0).remove::<ChildOf>();
        commands.entity(parent.0).insert(UnitInHand {
            source_zone: source_zone.0,
        });
    }
}

fn update_hand(
    mut query: Query<(&UnitInHand, &mut Transform)>,
    camera_query: Single<(&Camera, &GlobalTransform)>,
    window: Query<&Window>,
) {
    let (camera, camera_transform) = *camera_query;
    let Ok(window) = window.single() else {
        return;
    };

    let Some(cursor_position) = window.cursor_position() else {
        return;
    };

    let Ok(world_pos) = camera.viewport_to_world_2d(camera_transform, cursor_position) else {
        return;
    };

    let Ok((_, mut tr)) = query.single_mut() else {
        return;
    };

    tr.translation = world_pos.xy().extend(100.);
}

#[derive(Component)]
pub struct PlayerUnitZone;

#[derive(Component)]
pub struct ShopUnitZone;

#[derive(Component)]
pub struct CampUnitZone;

#[derive(Component)]
pub struct ToCampUnitZone;

pub fn spawn_zones(mut commands: Commands, handles: Res<JamAssets>) {
    commands
        .spawn((
            PlayerUnitZone,
            Transform::from_translation(Vec3::new(0., -250., 400.)),
            Pickable {
                should_block_lower: false,
                is_hoverable: true,
            },
            Sprite::from_color(Color::WHITE.with_alpha(0.0), vec2(750., 200.)),
        ))
        .observe(on_click_zone);

    commands.spawn((
        ShopUnitZone,
        Transform::from_translation(Vec3::new(0., 100., 400.)),
        Pickable {
            should_block_lower: false,
            is_hoverable: true,
        },
        Sprite::from_color(Color::WHITE.with_alpha(0.0), vec2(750., 200.)),
    ));

    commands
        .spawn((
            CampUnitZone,
            Transform::from_translation(Vec3::new(0., 100., 400.)),
            Pickable {
                should_block_lower: false,
                is_hoverable: true,
            },
            Sprite::from_color(Color::WHITE.with_alpha(0.0), vec2(750., 200.)),
        ))
        .observe(on_click_zone);

    commands
        .spawn((
            ToCampUnitZone,
            Transform::from_translation(Vec3::new(500., -250., 400.)),
            Pickable {
                should_block_lower: false,
                is_hoverable: true,
            },
            Sprite::from_color(Color::WHITE.with_alpha(0.0), vec2(250., 200.)),
            children![(
                Transform::from_translation(Vec3::new(0., 0., -390.1)).with_scale(Vec3::splat(0.7)),
                Sprite::from_image(handles.camp_image.clone())
            )],
        ))
        .observe(on_click_zone);
}

fn on_click_zone(
    trigger: Trigger<Pointer<Click>>,
    player_zone_query: Query<(Entity, &PlayerUnitZone, &Children)>,
    camp_zone_query: Query<(Entity, &CampUnitZone, Option<&Children>)>,
    to_camp_zone_query: Query<(Entity, &ToCampUnitZone, Option<&Children>)>,
    hand_query: Query<(Entity, &UnitInHand)>,
    mut commands: Commands,
    mut hand_cooldown: ResMut<HandCooldown>,
    handles: Res<JamAssets>,
    mut close_shop_event: EventWriter<OnCloseShop>,
    mut open_camp_event: EventWriter<OnOpenCamp>,
    mut close_camp_event: EventWriter<OnCloseCamp>,
    camp_opened_query: Query<(Entity, &CampOpened)>,
    banner_camp_query: Query<&CampUnitBanner>,
    banners_query: Query<&UnitBanner>,
) {
    if !hand_cooldown.timer.finished() {
        return;
    }

    let Ok((_, hand)) = hand_query.single() else {
        if let Ok(_) = to_camp_zone_query.get(trigger.target()) {
            if camp_opened_query.is_empty() {
                close_shop_event.write(OnCloseShop);
                open_camp_event.write(OnOpenCamp);
            } else {
                close_camp_event.write(OnCloseCamp);
            }
        }
        return;
    };

    let Ok((entity, _)) = hand_query.single() else {
        return;
    };

    let mut put_down = |zone_entity| {
        hand_cooldown.timer.reset();
        commands.entity(entity).insert(ChildOf(zone_entity));
        for (e, _) in &hand_query {
            commands.entity(e).remove::<UnitInHand>();
        }
    };

    let Ok((zone_entity, _, zone_children)) = player_zone_query.get(trigger.target()) else {
        if let Ok((zone_entity, _, _)) = to_camp_zone_query.get(trigger.target()) {
            if let None = camp_zone_query.get(hand.source_zone).ok() {
                println!("put in camp");
                put_down(zone_entity);
            }
            return;
        };

        if let Ok((zone_entity, _, _)) = camp_zone_query.get(trigger.target()) {
            if !camp_opened_query.is_empty() {
                // spawn the little guy too
                println!("put down in camp");
                if let Ok(banner) = banners_query.get(entity) {
                    if let Ok((camp, _, _)) = to_camp_zone_query.single() {
                        put_down(zone_entity);

                        if !camp_zone_query.contains(hand.source_zone) {
                            let e = commands
                                .spawn(camp_unit_bundle(&handles, &banner.unit))
                                .id();
                            commands.entity(camp).add_child(e);
                            commands
                                .entity(entity)
                                .insert(CampUnitBanner { camp_unit: e });
                        }
                    }
                }
            }
            return;
        };

        println!("put back");
        put_down(hand.source_zone);
        return;
    };

    if zone_children.len() >= 5 {
        println!("put back");
        put_down(hand.source_zone);
        commands.spawn((
            DestroyBetweenStates,
            Transform::from_translation(Vec3::new(0., 0., 800.)),
            note_bundle(&handles, format!("Max 5 units, moved to camp")),
        ));
        return;
    }

    println!("put down");
    put_down(zone_entity);
    if let Ok(camp_entity) = banner_camp_query.get(entity) {
        commands.entity(camp_entity.camp_unit).despawn();
        commands.entity(entity).remove::<CampUnitBanner>();
    }
}

fn update_zone_shop(
    shop_zone_query: Query<(&ShopUnitZone, &Children)>,
    mut banners_query: Query<(&UnitBanner, &mut Transform)>,
) {
    let Ok((_, children)) = shop_zone_query.single() else {
        return;
    };

    for (i, child) in children.iter().enumerate() {
        if let Ok((_, mut tr)) = banners_query.get_mut(child) {
            let len = children.len();
            let shift = i as f32 - len as f32 * 0.5 + 0.5;
            tr.translation.x = shift * 150.;
            tr.translation.y = 0.;
        }
    }
}

fn update_zone_player(
    player_zone_query: Query<(&PlayerUnitZone, &Children)>,
    mut banners_query: Query<(&UnitBanner, &mut Transform)>,
) {
    let Ok((_, children)) = player_zone_query.single() else {
        return;
    };

    for (i, child) in children.iter().enumerate() {
        if let Ok((_, mut tr)) = banners_query.get_mut(child) {
            let len = children.len();
            let shift = i as f32 - len as f32 * 0.5 + 0.5;
            tr.translation.x = shift * 150.;
            tr.translation.y = 0.;
        }
    }
}

fn update_zone_camp(
    camp_zone_query: Query<(&CampUnitZone, &Children)>,
    mut banners_query: Query<(&UnitBanner, &mut Transform)>,
) {
    let Ok((_, children)) = camp_zone_query.single() else {
        return;
    };

    for (i, child) in children.iter().enumerate() {
        if let Ok((_, mut tr)) = banners_query.get_mut(child) {
            let len = children.len();
            let shift = i as f32 - len as f32 * 0.5 + 0.5;
            tr.translation.x = shift * 150.;
            tr.translation.y = 0.;
        }
    }
}

fn update_zone_tocamp(
    camp_zone_query: Query<(Entity, &CampUnitZone)>,
    to_camp_zone_query: Query<(Entity, &ToCampUnitZone, &Children)>,
    banners_query: Query<&UnitBanner>,
    mut commands: Commands,
    handles: Res<JamAssets>,
    camp_opened_query: Query<(Entity, &CampOpened)>,
) {
    let Ok((zone_entity, _, children)) = to_camp_zone_query.single() else {
        return;
    };

    for (_, child) in children.iter().enumerate() {
        if let Ok(banner) = banners_query.get(child) {
            if !camp_opened_query.is_empty() {
                if let Ok((camp_banner, _)) = camp_zone_query.single() {
                    commands.entity(child).insert(ChildOf(camp_banner));
                };
            }
            commands.entity(child).despawn();
            let e = commands
                .spawn(camp_unit_bundle(&handles, &banner.unit))
                .id();
            commands.entity(zone_entity).add_child(e);
        }
    }
}

#[derive(Component)]
struct CampUnitBanner {
    camp_unit: Entity,
}

#[derive(Component)]
pub struct CampUnit {
    pub unit: Unit,
    destination: Vec3,
    timer: Timer,
}

fn camp_unit_bundle(handles: &JamAssets, unit: &Unit) -> impl Bundle {
    let bob_pos = Vec3::Y * 14.;
    let bob_timer = Timer::from_seconds(0.5, TimerMode::Repeating);
    (
        CampUnit {
            unit: unit.clone(),
            destination: Vec3::ZERO,
            timer: Timer::from_seconds(0.1, TimerMode::Once),
        },
        Transform::default(),
        Visibility::Visible,
        children![
            (
                Transform::from_scale(Vec3::splat(0.4)).with_translation(-Vec3::Z * 0.3),
                Sprite {
                    color: Color::BLACK,
                    ..Sprite::from_atlas_image(
                        handles.icons_image.clone(),
                        TextureAtlas {
                            layout: handles.icons_layout.clone(),
                            index: 5,
                        },
                    )
                },
            ),
            (
                Transform::from_translation(Vec3::ZERO),
                Visibility::Visible,
                BobUpAndDown {
                    source: bob_pos,
                    destination: bob_pos + Vec3::Y * 10.,
                    to_source: true,
                    timer: bob_timer,
                },
                children![
                    (
                        Transform::from_scale(Vec3::splat(0.32)).with_translation(-Vec3::Z * 0.2),
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
                        PartyMemberSmallFace,
                        Transform::from_scale(Vec3::splat(0.4)).with_translation(-Vec3::Z * 0.1),
                        Sprite::from_atlas_image(
                            handles.units_small_image.clone(),
                            TextureAtlas {
                                layout: handles.units_small_layout.clone(),
                                index: unit.sprite_index,
                            },
                        ),
                    ),
                ]
            )
        ],
    )
}

pub fn update_camp(mut camp_unit_query: Query<(&mut CampUnit, &mut Transform)>, time: Res<Time>) {
    let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();
    for (mut camp_unit, mut tr) in &mut camp_unit_query {
        camp_unit.timer.tick(time.delta());
        if camp_unit.timer.finished() {
            camp_unit
                .timer
                .set_duration(Duration::from_millis(rng.gen_range(3000..20000)));
            camp_unit.timer.reset();

            let x = rng.gen_range(-1.0..1.0);
            let y = rng.gen_range(-1.0..1.0);
            let isox = x * 128. * 0.5 + y * 128. * 0.5;
            let isoy = -x * 64. * 0.5 + y * 64. * 0.5;
            camp_unit.destination.x = isox;
            camp_unit.destination.y = isoy;
        }

        let pos = tr.translation;
        let d = camp_unit.destination - pos;
        if d.length_squared() > 50. {
            tr.translation += d.normalize_or_zero() * time.delta_secs() * 30.;
        }
    }
}

#[derive(Component)]
struct UnitBanner {
    unit: Unit,
}

#[derive(Component)]
struct Note {
    lifetime: Timer,
}

fn update_notes(
    mut notes_query: Query<(Entity, &mut Note)>,
    mut commands: Commands,
    time: Res<Time>,
) {
    for (entity, mut note) in &mut notes_query {
        note.lifetime.tick(time.delta());
        if note.lifetime.finished() {
            commands.entity(entity).despawn();
            continue;
        }
    }
}

fn note_bundle(handles: &JamAssets, text: String) -> impl Bundle {
    (
        Note {
            lifetime: Timer::from_seconds(1., TimerMode::Once),
        },
        Visibility::Visible,
        children![
            (
                Transform::from_translation(Vec3::Z * 2.),
                TextColor(Color::BLACK),
                Text2d::new(text),
                TextFont {
                    font: handles.font.clone(),
                    font_size: 20.0,
                    ..default()
                },
                TextLayout::new(JustifyText::Center, LineBreak::WordBoundary),
                TextBounds::from(Vec2::new(250., 80.)),
            ),
            (
                Transform::from_translation(Vec3::Z).with_scale(Vec3::splat(0.7)),
                Sprite::from_image(handles.ability_background_image.clone()),
            ),
        ],
    )
}

#[derive(Component)]
struct UnitPriceMarker {
    price: i32,
}

fn unit_price_bundle(handles: &JamAssets, unit: &Unit) -> impl Bundle {
    (
        UnitPriceMarker { price: price(unit) },
        children![
            (
                Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::new(0., -35., 3.)),
                Sprite::from_atlas_image(
                    handles.icons_image.clone(),
                    TextureAtlas {
                        layout: handles.icons_layout.clone(),
                        index: 6,
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
                        Text2d::new(format!("{}", price(unit))),
                        TextFont {
                            font: handles.font.clone(),
                            font_size: 30.0,
                            ..default()
                        },
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
        ],
    )
}
