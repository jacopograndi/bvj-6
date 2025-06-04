use bevy::ecs::spawn::SpawnIter;

use crate::*;

pub struct MapPlugin;
impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(MapState::construct());
        app.add_systems(OnEnter(AppStates::Map), spawn_map);
        app.add_systems(
            Update,
            (
                pull_up,
                bob_up_and_down,
                update_party_marker,
                update_party_member_marker,
            )
                .chain()
                .run_if(in_state(AppStates::Map)),
        );
        /*
        app.add_systems(OnEnter(AppStates::Combat), spawn_units);
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
        app.add_systems(
            Update,
            update_combo_animation.run_if(in_state(AppStates::Combat)),
        );
        */
    }
}

#[derive(Resource)]
struct MapState {
    player_pos: IVec2,
}

impl MapState {
    fn construct() -> Self {
        Self {
            player_pos: ivec2(3, 1),
        }
    }
}

#[derive(Component)]
struct TilePos {
    xy: IVec2,
}

#[derive(Component)]
struct PartyMarker;

fn update_party_marker(
    mut commands: Commands,
    query: Query<Entity, With<PartyMarker>>,
    query_members: Query<(Entity, &PartyMemberMarker)>,
    party: Res<PartyState>,
    tiles_query: Query<(Entity, &TilePos)>,
    mut tr_query: Query<&mut Transform>,
    map: Res<MapState>,
) {
    let Ok(marker) = query.single() else {
        return;
    };

    if let Some((tile, _)) = tiles_query.iter().find(|(_, t)| t.xy == map.player_pos) {
        let tile_tr = if let Ok(tile_tr) = tr_query.get(tile) {
            tile_tr.clone()
        } else {
            return;
        };

        if let Ok(mut marker_tr) = tr_query.get_mut(marker) {
            *marker_tr = tile_tr;
        };
    }

    let spread = vec![
        vec![vec2(0., 0.)],
        vec![vec2(-0.5, 0.), vec2(0.5, 0.)],
        vec![vec2(-1., 0.), vec2(0., -0.5), vec2(1., 0.)],
        vec![vec2(-1., -1.), vec2(1., -1.), vec2(-1., 1.), vec2(1., 1.)],
        vec![
            vec2(-1., -1.),
            vec2(1., -1.),
            vec2(-1., 1.),
            vec2(1., 1.),
            vec2(0., 0.),
        ],
    ];

    for (member_entity, member) in &query_members {
        if member.0 >= party.units.len() {
            commands.entity(member_entity).insert(Visibility::Hidden);
        } else {
            commands.entity(member_entity).insert(Visibility::Visible);
        }

        if let Some(off) = spread[party.units.len() - 1].get(member.0) {
            let res = (off * 30.).extend(2.);
            commands
                .entity(member_entity)
                .insert(Transform::from_translation(res));
        }
    }
}

#[derive(Component)]
struct PartyMemberSmallFace;

#[derive(Component)]
struct PartyMemberMarker(pub usize);

fn update_party_member_marker(
    mut commands: Commands,
    members_query: Query<(Entity, &PartyMemberMarker, Option<&Children>)>,
    mut small_query: Query<(&PartyMemberSmallFace, &mut Sprite)>,
    party: Res<PartyState>,
    handles: Res<JamAssets>,
) {
    for (member_entity, member, children) in &members_query {
        let Some(unit) = party.units.get(member.0) else {
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
        let child = commands
            .spawn((
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
                            timer: Timer::from_seconds(0.5, TimerMode::Repeating)
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

        let pos = tr.translation.clone();
        let dest = if bob.to_source {
            bob.source
        } else {
            bob.destination
        };

        let acc = if fast.is_some() { 30. } else { 20. };
        tr.translation += (dest - pos) * time.delta_secs() * acc;
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
            commands.spawn((
                Transform::from_scale(Vec3::splat(scale)).with_translation(pos),
                Sprite::from_image(image),
                TilePos { xy: tilepos },
            ));

            if tilepos == map.player_pos {
                // spawn party in map

                commands.spawn((
                    Transform::from_translation(pos),
                    Visibility::Visible,
                    PartyMarker,
                    Children::spawn(SpawnIter((0..5).map(|i| {
                        (
                            Transform::default(),
                            Visibility::Visible,
                            PartyMemberMarker(i),
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
                Transform::from_translation(pos).with_scale(Vec3::splat(0.6)),
                Visibility::Visible,
                PullUp {
                    unit_index: i,
                    source: pos,
                    destination: pos + Vec3::Y * 140.,
                    to_source: true,
                },
                children![(
                    Transform::default(),
                    unit_bundle(&handles, unit.sprite_index, unit.clone()),
                ),],
            ))
            .id();
        let hitbox = commands
            .spawn((
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

            // find map marker and bob faster
            if let Some(bob) = bob_query.iter().find(|e| {
                child_of_query
                    .iter_ancestors(*e)
                    .find(|a| {
                        party_member_marker_query
                            .get(*a)
                            .ok()
                            .is_some_and(|marker| marker.0 == pullup.unit_index)
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

            // find map marker and bob faster
            if let Some(bob) = bob_query.iter().find(|e| {
                child_of_query
                    .iter_ancestors(*e)
                    .find(|a| {
                        party_member_marker_query
                            .get(*a)
                            .ok()
                            .is_some_and(|marker| marker.0 == pullup.unit_index)
                    })
                    .is_some()
            }) {
                commands.entity(bob).remove::<BobUpAndDownFast>();
            }
        }
    }
}

fn pull_up(mut query: Query<(&mut Transform, &PullUp)>, time: Res<Time>) {
    for (mut tr, pullup) in &mut query {
        let pos = tr.translation.clone();
        let dest = if pullup.to_source {
            pullup.source
        } else {
            pullup.destination
        };
        tr.translation += (dest - pos) * time.delta_secs() * 30.;
    }
}
