use bevy::{ecs::spawn::SpawnIter, platform::collections::HashMap};

use crate::*;

pub struct MapPlugin;
impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        let bp = app
            .world()
            .get_resource::<Blueprints>()
            .expect("insert bp first");

        app.insert_resource(MapState::construct(&bp));

        app.add_systems(OnEnter(GameStates::Map), spawn_map);
        app.add_systems(
            Update,
            (
                pull_up,
                bob_up_and_down,
                update_party_marker,
                update_party_member_marker,
                move_player_marker,
            )
                .chain()
                .run_if(in_state(GameStates::Map)),
        );
    }
}

pub struct PoolUnit {
    pub base: Unit,
    pub min_level: i32,
    pub max_level: i32,
}

pub struct EnemySpawnPool {
    pub base_units: Vec<PoolUnit>,
}

#[derive(Resource)]
pub struct MapState {
    pub player_pos: IVec2,
    pub enemy_parties: HashMap<IVec2, Vec<Unit>>,
    pub enemy_pools: HashMap<IVec2, Vec<EnemySpawnPool>>,
    pub enemy_respawn_timer: HashMap<IVec2, i32>,
    pub combat_enemies: Vec<Unit>,
}

impl MapState {
    fn construct(bp: &Blueprints) -> Self {
        let mut enemy_parties = HashMap::new();

        enemy_parties.insert(ivec2(2, 1), vec![bp.units[4].clone(), bp.units[5].clone()]);
        enemy_parties.insert(
            ivec2(4, 1),
            vec![
                bp.units[2].clone(),
                bp.units[3].clone(),
                bp.units[6].clone(),
                bp.units[7].clone(),
            ],
        );
        enemy_parties.insert(ivec2(3, 2), vec![bp.units[3].clone()]);
        enemy_parties.insert(
            ivec2(3, 0),
            vec![
                bp.units[1].clone(),
                bp.units[1].clone(),
                bp.units[0].clone(),
                bp.units[0].clone(),
            ],
        );
        enemy_parties.insert(
            ivec2(1, 1),
            vec![
                bp.units[4].clone(),
                bp.units[4].clone(),
                bp.units[3].clone(),
                bp.units[6].clone(),
                bp.units[6].clone(),
            ],
        );

        Self {
            player_pos: ivec2(3, 1),
            enemy_parties,
            enemy_respawn_timer: HashMap::new(),
            combat_enemies: vec![],
            enemy_pools: HashMap::new(),
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
    party: Res<PartyState>,
    tiles_query: Query<(Entity, &TilePos)>,
    mut tr_query: Query<&mut Transform>,
    map: Res<MapState>,
) {
    for (marker_entity, marker) in &query {
        if let Some((tile, _)) = tiles_query.iter().find(|(_, t)| {
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

        let units: &Vec<Unit> = match marker.marker_type {
            MarkerType::Player => &party.units,
            MarkerType::Enemy(pos) => {
                if let Some(vec) = map.enemy_parties.get(&pos) {
                    vec
                } else {
                    continue;
                }
            }
        };

        for (member_entity, member, child_of) in &query_members {
            if child_of.0 != marker_entity {
                continue;
            }

            if member.marker_index >= units.len() {
                commands.entity(member_entity).insert(Visibility::Hidden);
            } else {
                commands.entity(member_entity).insert(Visibility::Visible);
            }

            if let Some(off) = spread[units.len() - 1].get(member.marker_index) {
                let res = (off * 30.).extend(2.);
                commands
                    .entity(member_entity)
                    .insert(Transform::from_translation(res));
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

fn update_party_member_marker(
    mut commands: Commands,
    members_query: Query<(Entity, &PartyMemberMarker, Option<&Children>)>,
    mut small_query: Query<(&PartyMemberSmallFace, &mut Sprite)>,
    party: Res<PartyState>,
    handles: Res<JamAssets>,
    map: Res<MapState>,
) {
    let mut rng = ChaCha8Rng::from_rng(thread_rng()).unwrap();

    for (member_entity, member, children) in &members_query {
        let units: &Vec<Unit> = match member.marker_type {
            MarkerType::Player => &party.units,
            MarkerType::Enemy(pos) => {
                if let Some(vec) = map.enemy_parties.get(&pos) {
                    vec
                } else {
                    continue;
                }
            }
        };

        let Some(unit) = units.get(member.marker_index) else {
            continue;
        };
        if let Some(children) = &children {
            // keep updated small unit sprite
            for child in children.iter() {
                if let Ok((_, mut sprite)) = small_query.get_mut(child) {
                    if let Some(atlas) = &mut sprite.texture_atlas {
                        atlas.index = unit.sprite_index;
                    }
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
                        Visibility::Visible,
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
                        ]
                    )
                ],
            ))
            .id();
        commands.entity(member_entity).add_child(child);
    }
}

#[derive(Component)]
struct BobUpAndDown {
    source: Vec3,
    destination: Vec3,
    to_source: bool,
    timer: Timer,
}

#[derive(Component)]
struct BobUpAndDownFast;

fn bob_up_and_down(
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

fn spawn_map(
    map: Res<MapState>,
    mut commands: Commands,
    handles: Res<JamAssets>,
    party: Res<PartyState>,
) {
    let scale = 0.70;
    let tile_size = Vec3::new(256., 128., 0.) * scale;

    #[rustfmt::skip]
    let tiles = vec![
        ["f", "p", "f", "f", "m"],
        ["p", "1", "p", "p", "p"],
        ["p", "p", "f", "m", "2"],
        ["f", "f", "m", "f", "m"],
        ["f", "3", "t", "p", "c"],
    ];

    for y in 0..5 {
        for x in 0..5 {
            let xy = Vec3::new(x as f32, y as f32, 0.);
            let isox = xy.x * tile_size.x * 0.5 + xy.y * tile_size.x * 0.5;
            let isoy = -xy.x * tile_size.y * 0.5 + xy.y * tile_size.y * 0.5;
            let pos = Vec3::new(isox, isoy, 0.0);
            let pos = pos + tile_size * Vec3::new(-2., 0.4, 0.) - Vec3::Z * isoy / 100.;

            let image = match tiles[4 - x][y] {
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

    for (i, unit) in party.units.iter().enumerate() {
        let len = party.units.len();
        let shift = i as f32 - len as f32 * 0.5 + 0.5;
        let pos = Vec3::new(shift as f32 * 150., -260., 10.);
        let entity = commands
            .spawn((
                DestroyBetweenStates,
                Transform::from_translation(pos).with_scale(Vec3::splat(0.6)),
                Visibility::Visible,
                PullUp {
                    unit_index: i,
                    source: pos,
                    destination: pos + Vec3::Y * 140.,
                    to_source: true,
                    timer: {
                        let mut timer = Timer::from_seconds(0.1, TimerMode::Once);
                        timer.set_elapsed(Duration::from_millis(100));
                        timer
                    },
                },
                children![(
                    Transform::default(),
                    unit_bundle(&handles, unit.sprite_index, unit.clone()),
                ),],
            ))
            .id();
        let hitbox = commands
            .spawn((
                DestroyBetweenStates,
                Pickable::default(),
                Transform::from_translation(Vec3::new(0., -160., -10.)),
                Sprite::from_color(Color::WHITE.with_alpha(0.0), Vec2::new(220., 512.)),
            ))
            .observe(on_over_pull_up)
            .observe(on_out_pull_down)
            .id();
        commands.entity(entity).add_child(hitbox);
    }
}

#[derive(Component)]
struct PullUp {
    unit_index: usize,
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

            if let Some(enemies) = map.enemy_parties.get(&fx.to_tilepos) {
                party.start_of_combat_units = party.units.clone();
                map.combat_enemies = enemies.clone();
                next_game_state.set(GameStates::Combat);
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
) {
    for arrow in &arrow_query {
        commands.entity(arrow).despawn();
    }

    if !movement_fx.is_empty() {
        return;
    }

    let Ok((tile_dst, tp_dst)) = query.get(trigger.target()) else {
        return;
    };
    let Some((tile_src, tp_src)) = query.iter().find(|(_, tp)| tp.xy == map.player_pos) else {
        return;
    };

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
) {
    if !arrow_query.is_empty() {
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
    bob_query: Query<Entity, With<BobUpAndDown>>,
    party_member_marker_query: Query<&PartyMemberMarker>,
    mut commands: Commands,
) {
    if let Ok(parent) = child_of_query.get(trigger.target()) {
        if let Ok(mut pullup) = query.get_mut(parent.0) {
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
                                    && marker.marker_index == pullup.unit_index
                            })
                    })
                    .is_some()
            }) {
                commands.entity(bob).insert(BobUpAndDownFast);
            }
        }
    }
}

fn on_out_pull_down(
    trigger: Trigger<Pointer<Out>>,
    mut query: Query<&mut PullUp>,
    child_of_query: Query<&ChildOf>,
    bob_query: Query<Entity, With<BobUpAndDown>>,
    party_member_marker_query: Query<&PartyMemberMarker>,
    mut commands: Commands,
) {
    if let Ok(parent) = child_of_query.get(trigger.target()) {
        if let Ok(mut pullup) = query.get_mut(parent.0) {
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
                                    && marker.marker_index == pullup.unit_index
                            })
                    })
                    .is_some()
            }) {
                commands.entity(bob).remove::<BobUpAndDownFast>();
            }
        }
    }
}

fn pull_up(mut query: Query<(&mut Transform, &mut PullUp)>, time: Res<Time>) {
    for (mut tr, mut pullup) in &mut query {
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
