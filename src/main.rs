use std::time::Duration;

use bevy::{
    asset::AssetMetaCheck, ecs::spawn::SpawnWith, prelude::*, render::camera::ScalingMode,
    text::TextBounds, window::WindowResolution,
};
use bevy_asset_loader::prelude::*;
use rand::{Rng, SeedableRng, thread_rng};
use rand_chacha::ChaCha8Rng;

mod blueprints;
use blueprints::*;

mod combat;
use combat::*;

mod map;
use map::*;

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

    app.insert_resource(ClearColor(Color::srgb(0.1, 0.1, 0.1)));
    app.insert_resource(SoundMuted { muted: false });

    app.init_state::<GameStates>();

    app.add_loading_state(
        LoadingState::new(GameStates::AssetLoading)
            .continue_to_state(GameStates::Map)
            .load_collection::<JamAssets>(),
    );

    app.add_systems(
        OnEnter(GameStates::AssetLoading),
        (spawn_camera, setup_loading_screen),
    );

    app.add_systems(
        OnExit(GameStates::AssetLoading),
        (
            remove_loading_screen,
            prepare_atlases,
            spawn_zones,
            setup_audio_tracks,
            spawn_ability_zoomed,
        )
            .chain(),
    );

    app.add_systems(OnExit(GameStates::Map), destroy_everything);
    app.add_systems(OnExit(GameStates::Combat), destroy_everything);

    app.add_systems(Update, (update_camp, bob_up_and_down, update_gold_tracker, update_ability_zoom));

    let bp = Blueprints::construct();
    app.insert_resource(PartyState::construct(&bp));
    app.insert_resource(bp);

    app.add_plugins((CombatPlugin, MapPlugin));

    app.add_systems(
        Update,
        (
            ui_ability_trackers,
            ui_value_trackers,
            ui_name_trackers,
            ui_level_trackers,
            ui_experience_trackers,
            update_particles,
        )
            .run_if(in_state(GameStates::Combat).or(in_state(GameStates::Map))),
    );

    app.run();
}

#[derive(Component)]
struct LoadingScreen;

fn setup_loading_screen(mut commands: Commands) {
    commands.spawn((
        LoadingScreen,
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            ..default()
        },
        children![(
            Text::new("Loading assets..."),
            Node {
                width: Val::Px(150.0),
                height: Val::Px(65.0),
                border: UiRect::all(Val::Px(5.0)),
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                ..default()
            },
        )],
    ));
}

fn remove_loading_screen(mut commands: Commands, query: Query<Entity, With<LoadingScreen>>) {
    for entity in &query {
        commands.entity(entity).despawn();
    }
}

#[derive(AssetCollection, Resource, Clone)]
struct JamAssets {
    #[asset(path = "images/units.png")]
    units_image: Handle<Image>,
    units_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "images/units_small.png")]
    units_small_image: Handle<Image>,
    units_small_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "images/icons.png")]
    icons_image: Handle<Image>,
    icons_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "images/exp_bar.png")]
    exp_bar_image: Handle<Image>,

    #[asset(path = "images/speed_dial.png")]
    speed_dial_image: Handle<Image>,

    #[asset(path = "images/ability_background.png")]
    ability_background_image: Handle<Image>,

    #[asset(path = "images/activated_ability_back.png")]
    activated_ability_back: Handle<Image>,

    #[asset(path = "images/unit_background.png")]
    unit_background_image: Handle<Image>,

    #[asset(path = "images/unit_name.png")]
    unit_name_image: Handle<Image>,

    #[asset(path = "images/arrow_start.png")]
    arrow_start_image: Handle<Image>,
    #[asset(path = "images/arrow_segment.png")]
    arrow_segment_image: Handle<Image>,
    #[asset(path = "images/arrow_head.png")]
    arrow_head_image: Handle<Image>,

    #[asset(path = "images/combat_end_back.png")]
    combat_end_back_image: Handle<Image>,
    #[asset(path = "images/combat_win.png")]
    combat_win_image: Handle<Image>,
    #[asset(path = "images/combat_lose.png")]
    combat_lose_image: Handle<Image>,
    #[asset(path = "images/combat_draw.png")]
    combat_draw_image: Handle<Image>,

    #[asset(path = "images/tile_forest.png")]
    tile_forest_image: Handle<Image>,
    #[asset(path = "images/tile_plains.png")]
    tile_plains_image: Handle<Image>,
    #[asset(path = "images/tile_mountain.png")]
    tile_mountain_image: Handle<Image>,
    #[asset(path = "images/tile_home.png")]
    tile_home_image: Handle<Image>,
    #[asset(path = "images/tile_town1.png")]
    tile_town1_image: Handle<Image>,
    #[asset(path = "images/tile_town2.png")]
    tile_town2_image: Handle<Image>,
    #[asset(path = "images/tile_tower.png")]
    tile_tower_image: Handle<Image>,
    #[asset(path = "images/tile_castle.png")]
    tile_castle_image: Handle<Image>,

    #[asset(path = "images/camp.png")]
    camp_image: Handle<Image>,

    #[asset(path = "images/combat_back.png")]
    combat_back_image: Handle<Image>,

    #[asset(path = "images/slash.png")]
    slash_image: Handle<Image>,

    #[asset(path = "fonts/IosevkaFixed-Medium.subset.ttf")]
    font: Handle<Font>,

    #[asset(path = "audio/waong.mp3")]
    map_track: Handle<AudioSource>,

    #[asset(path = "audio/ow.mp3")]
    ow_sound: Handle<AudioSource>,
    #[asset(path = "audio/ow2.mp3")]
    ow2_sound: Handle<AudioSource>,
    #[asset(path = "audio/ow3.mp3")]
    ow3_sound: Handle<AudioSource>,
    #[asset(path = "audio/click.mp3")]
    click_sound: Handle<AudioSource>,
    #[asset(path = "audio/level_up.mp3")]
    level_up_sound: Handle<AudioSource>,
    #[asset(path = "audio/stack_up.mp3")]
    stack_up_sound: Handle<AudioSource>,
    #[asset(path = "audio/exp_up.mp3")]
    exp_up_sound: Handle<AudioSource>,
}

#[derive(Component)]
struct MusicTrack;

#[derive(Resource)]
pub struct SoundMuted {
    pub muted: bool,
}

#[derive(Component)]
struct TrackSlash;

#[derive(Component)]
struct SoundSlash;

fn setup_audio_tracks(mut commands: Commands, handles: Res<JamAssets>) {
    commands.spawn((
        AudioPlayer(handles.map_track.clone()),
        MusicTrack,
        PlaybackSettings {
            mode: bevy::audio::PlaybackMode::Loop,
            volume: bevy::audio::Volume::Linear(0.8),
            ..default()
        },
    ));

    commands
        .spawn((
            Pickable::default(),
            Sprite::from_color(Color::BLACK.with_alpha(0.0), Vec2::new(60., 60.)),
            Transform::from_translation(Vec3::new(540., 320., 400.)),
            Visibility::Visible,
            children![
                (
                    Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::Z * 1.3),
                    Sprite {
                        color: Color::BLACK.with_alpha(0.0),
                        ..Sprite::from_image(handles.slash_image.clone(),)
                    },
                    TrackSlash,
                ),
                (
                    Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::Z * 1.2),
                    Sprite {
                        color: Color::WHITE,
                        ..Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 11,
                            },
                        )
                    },
                ),
                (
                    Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::Z),
                    Sprite {
                        color: Color::WHITE,
                        ..Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 4,
                            },
                        )
                    },
                ),
            ],
        ))
        .observe(on_click_toggle_mute_tracks);

    commands
        .spawn((
            Pickable::default(),
            Sprite::from_color(Color::BLACK.with_alpha(0.0), Vec2::new(60., 60.)),
            Transform::from_translation(Vec3::new(600., 320., 400.)),
            Visibility::Visible,
            children![
                (
                    Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::Z * 1.3),
                    Sprite {
                        color: Color::BLACK.with_alpha(0.0),
                        ..Sprite::from_image(handles.slash_image.clone(),)
                    },
                    SoundSlash,
                ),
                (
                    Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::Z * 1.2),
                    Sprite {
                        color: Color::WHITE,
                        ..Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 7,
                            },
                        )
                    },
                ),
                (
                    Transform::from_scale(Vec3::splat(0.4)).with_translation(Vec3::Z),
                    Sprite {
                        color: Color::WHITE,
                        ..Sprite::from_atlas_image(
                            handles.icons_image.clone(),
                            TextureAtlas {
                                layout: handles.icons_layout.clone(),
                                index: 4,
                            },
                        )
                    },
                ),
            ],
        ))
        .observe(on_click_toggle_mute_sounds);
}

fn on_click_toggle_mute_tracks(
    _: Trigger<Pointer<Click>>,
    mut music_controller: Query<&mut AudioSink, With<MusicTrack>>,
    mut sound_query: Query<&mut Sprite, With<TrackSlash>>,
) {
    let Ok(mut sink) = music_controller.single_mut() else {
        return;
    };
    sink.toggle_mute();
    if let Ok(mut sprite) = sound_query.single_mut() {
        if sink.is_muted() {
            sprite.color = sprite.color.with_alpha(1.0);
        } else {
            sprite.color = sprite.color.with_alpha(0.0);
        }
    }
}

fn on_click_toggle_mute_sounds(
    _: Trigger<Pointer<Click>>,
    mut muted: ResMut<SoundMuted>,
    mut sound_query: Query<&mut Sprite, With<SoundSlash>>,
) {
    muted.muted = !muted.muted;
    if let Ok(mut sprite) = sound_query.single_mut() {
        if muted.muted {
            sprite.color = sprite.color.with_alpha(1.0);
        } else {
            sprite.color = sprite.color.with_alpha(0.0);
        }
    }
}

pub fn play_sound(commands: &mut Commands, sound: &Handle<AudioSource>, muted: &SoundMuted) {
    if muted.muted {
        return;
    }
    commands.spawn((
        AudioPlayer(sound.clone()),
        PlaybackSettings {
            mode: bevy::audio::PlaybackMode::Despawn,
            volume: bevy::audio::Volume::Linear(0.3),
            ..default()
        },
    ));
}

pub fn play_sound_rng(
    commands: &mut Commands,
    sound: &Handle<AudioSource>,
    rng: &mut ChaCha8Rng,
    muted: &SoundMuted,
) {
    if muted.muted {
        return;
    }
    commands.spawn((
        AudioPlayer(sound.clone()),
        PlaybackSettings {
            mode: bevy::audio::PlaybackMode::Despawn,
            volume: bevy::audio::Volume::Linear(rng.gen_range(0.25..0.35)),
            speed: rng.gen_range(0.9..1.1),
            ..default()
        },
    ));
}

pub fn play_sound_speed(
    commands: &mut Commands,
    sound: &Handle<AudioSource>,
    speed: f32,
    muted: &SoundMuted,
) {
    if muted.muted {
        return;
    }
    commands.spawn((
        AudioPlayer(sound.clone()),
        PlaybackSettings {
            mode: bevy::audio::PlaybackMode::Despawn,
            volume: bevy::audio::Volume::Linear(0.5),
            speed,
            ..default()
        },
    ));
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
pub enum GameStates {
    #[default]
    AssetLoading,
    Combat,
    Map,
}

fn prepare_atlases(
    mut handles: ResMut<JamAssets>,
    mut texture_atlas_layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
    handles.units_layout = texture_atlas_layouts.add(TextureAtlasLayout::from_grid(
        UVec2::splat(256),
        4,
        4,
        None,
        None,
    ));

    handles.units_small_layout = texture_atlas_layouts.add(TextureAtlasLayout::from_grid(
        UVec2::splat(64),
        4,
        4,
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

#[derive(Component)]
struct DestroyBetweenStates;

fn destroy_everything(mut commands: Commands, query: Query<Entity, With<DestroyBetweenStates>>) {
    for entity in &query {
        commands.entity(entity).despawn();
    }
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn((
        Camera2d,
        Projection::Orthographic(OrthographicProjection {
            scaling_mode: ScalingMode::FixedVertical {
                viewport_height: 720.,
            },
            ..OrthographicProjection::default_2d()
        }),
    ));
}

#[derive(Resource)]
pub struct PartyState {
    pub units: Vec<Unit>,
    pub start_of_combat_units: Vec<Unit>,
    pub gold: i32,
}

impl PartyState {
    fn construct(bp: &Blueprints) -> Self {
        let mut units = vec![bp.units[1].clone(), bp.units[2].clone()];
        for unit in &mut units {
            unit.owner = Owner::Player
        }
        Self {
            units,
            gold: 25,
            start_of_combat_units: vec![],
        }
    }
}
