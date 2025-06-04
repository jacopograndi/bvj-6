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

    app.init_state::<AppStates>();

    app.add_plugins((CombatPlugin, MapPlugin));

    app.add_loading_state(
        LoadingState::new(AppStates::AssetLoading)
            .continue_to_state(AppStates::Map)
            .load_collection::<JamAssets>(),
    );
    app.add_systems(OnExit(AppStates::AssetLoading), prepare_atlases);
    app.add_systems(OnExit(AppStates::AssetLoading), spawn_camera);
    app.add_systems(OnExit(AppStates::Map), destroy_everything);
    app.add_systems(OnExit(AppStates::Combat), destroy_everything);

    app.add_systems(Update, debug_resolution);

    let bp = Blueprints::construct();
    app.insert_resource(PartyState::construct(&bp));
    app.insert_resource(bp);

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

    #[asset(path = "images/units_small.png")]
    units_small_image: Handle<Image>,
    units_small_layout: Handle<TextureAtlasLayout>,

    #[asset(path = "images/icons.png")]
    icons_image: Handle<Image>,
    icons_layout: Handle<TextureAtlasLayout>,

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

    #[asset(path = "fonts/IosevkaFixed-Medium.subset.ttf")]
    font: Handle<Font>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default, States)]
enum AppStates {
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
struct KeepBetweenStates;

fn destroy_everything(mut commands: Commands, query: Query<Entity, Without<KeepBetweenStates>>) {
    for entity in &query {
        commands.entity(entity).despawn();
    }
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn((
        KeepBetweenStates,
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
struct PartyState {
    units: Vec<Unit>,
}

impl PartyState {
    fn construct(bp: &Blueprints) -> Self {
        let mut units = vec![
            bp.units[0].clone(),
            bp.units[1].clone(),
            bp.units[2].clone(),
        ];
        for unit in &mut units {
            unit.owner = Owner::Player
        }
        Self { units }
    }
}
